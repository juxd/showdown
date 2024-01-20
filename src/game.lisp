(in-package :cl-user)
(defpackage :potato-srv.game
  (:use :cl)
  (:export :*singleton-game*
           :invalid-peg-placement
           :invalid-thaler-placement
           :invalid-move-for-phase
           #:create-state
           #:state-phase
           #:state-phase-seq
           #:echoose-thaler
           #:echoose-color
           #:echoose-peg-move
           #:get-table-html
           #:generate-move-form
           #:bad-move
           #:bad-move-phase
           #:bad-peg-color
           #:bad-peg-placement
           #:bad-peg-reason
           #:bad-thaler-placement
           #:bad-thaler-reason))

(in-package :potato-srv.game)

(define-condition invalid-thaler-placement (error)
  ((placement :initarg :placement
              :initform nil
              :reader bad-thaler-placement)
   (reason :initarg :reason
           :initform nil
           :reader bad-thaler-reason))
  (:documentation "When someone tried to place a thaler at a bad place"))

(define-condition invalid-peg-placement (error)
  ((peg :initarg :peg
        :initform nil
        :reader bad-peg-color)
   (placement :initarg :placement
              :initform nil
              :reader bad-peg-placement)
   (reason :initarg :reason
           :initform nil
           :reader bad-peg-reason))
  (:documentation "When someone tried to move a peg to a bad place"))

(define-condition invalid-move-for-phase (error)
  ((move :initarg :move
         :initform nil
         :reader bad-move)
   (phase :initarg :phase
          :initform nil
          :reader bad-move-phase))
  (:documentation "When someone tried to do something at a bad time"))

(defstruct state
  pegs
  thaler
  player-1-choice
  player-2-choice
  phase-seq
  phase
  valid-moves)

(defvar *singleton-game* nil)

(defun generate-seven-coordinates ()
  (mapcar (lambda (x) (multiple-value-bind (x y) (floor x 8) (cons x y)))
          (subseq (alexandria:shuffle (loop for i from 0 to 63 collect i))
                  0
                  7)))

(defvar *move-dirs* '((-1 . -1) ( 0 . -1) ( 1 . -1)
                      (-1 .  0)           ( 1 .  0)
                      (-1 .  1) ( 0 .  1) ( 1 .  1)))

(defun create-state ()
  (setf *singleton-game* (make-state :pegs (generate-seven-coordinates)
                                     :thaler nil
                                     :player-1-choice nil
                                     :player-2-choice nil
                                     :phase 'p1-thaler
                                     :phase-seq 0
                                     :valid-moves '())))

(defun update-valid-moves (state)
  (setf (state-valid-moves state)
        (loop for peg in (state-pegs state)
              collect (loop for dir in *move-dirs*
                            when (dir-is-valid-1 state peg dir)
                              collect (cons (+ (car peg) (car dir))
                                            (+ (cdr peg) (cdr dir)))
                            when (dir-is-valid-2 state peg dir)
                              collect (cons (+ (car peg) (car dir) (car dir))
                                            (+ (cdr peg) (cdr dir) (cdr dir)))))))

(defun find-color (state x y)
  (position (cons x y)
            (state-pegs state)
            :test #'equal))

(defun thaler-dist (state x y)
  (destructuring-bind (t-x . t-y) (state-thaler state)
    (let ((x (abs (- t-x x)))
          (y (abs (- t-y y))))
      (+ (min x y) (abs (- x y))))))

(defun dir-is-valid-1 (state peg-pos dir)
  (destructuring-bind ((p-x . p-y) . (d-x . d-y)) (cons peg-pos dir)
    (let ((x (+ p-x d-x))
          (y (+ p-y d-y)))
      (and (<= 0 x 7)
           (<= 0 y 7)
           (not (find-color state x y))
           (<= (thaler-dist state x y) (thaler-dist state p-x p-y))))))

(defun dir-is-valid-2 (state peg-pos dir)
  (destructuring-bind ((p-x . p-y) . (d-x . d-y)) (cons peg-pos dir)
    (let ((x1 (+ p-x d-x ))
          (y1 (+ p-y d-y))
          (x  (+ p-x d-x d-x))
          (y  (+ p-y d-y d-y)))
      (and (<= 0 x 7)
           (<= 0 y 7)
           (find-color state x1 y1)
           (not (find-color state x y))
           (<= (thaler-dist state x y) (thaler-dist state p-x p-y))))))

(defvar *color-inputs*
  (apply #'str:concat
         (loop for color in
               '("red" "orange" "yellow" "green" "blue" "pink" "white")
               collect
               (format nil
                       "
<input type=\"radio\" id=\"~a-radio\" name=\"color\" value=\"~a\" checked />
<label for=\"~a\">~a</label>"
                       color color color color))))

(defvar *coord-inputs*
  "<label for=\"x\">X</label>
<input type=\"text\" name=\"x\" id=\"x\" required/>
<label for=\"y\">Y</label>
<input type=\"text\" name=\"y\" id=\"y\" required/>")

(defun make-thaler-form ()
  (format nil
          "<form hx-get=\"/choose-thaler\" hx-swap=\"none\" id=\"player-choice\">
~a
<input type=\"submit\" value=\"Place Thaler\"/>
</form>"
          *coord-inputs*))

(defun make-color-choice-form (player)
  (format nil
          "<form hx-get=\"/choose-color\">
<input type=\"hidden\" name=\"player\" value=\"~a\">
~a
<input type=\"submit\" value=\"Choose Color\">
</form>"
          player
          *color-inputs*))

(defun make-move-choice-form (player)
  (format nil
          "<form hx-get=\"/choose-move\">
<input type=\"hidden\" name=\"player\" value=\"~a\">
~a
~a
<input type=\"submit\" value=\"Choose Move\">
</form>"
          player
          *color-inputs*
          *coord-inputs*))

(defun generate-move-form (state)
  (case (state-phase state)
    (p1-thaler (make-thaler-form))
    (p1-choice (make-color-choice-form "1"))
    (p2-choice (make-color-choice-form "2"))
    (p1-move   (make-move-choice-form  "1"))
    (p2-move   (make-move-choice-form  "2"))
    (t "<div>no move to be made my guy</div>")))

(defun change-phase (state phase)
  (setf (state-phase state) phase)
  (incf (state-phase-seq state)))

(defun echoose-color (state player color)
  (alexandria:switch ((cons (state-phase state) player)
                      :test #'equal)
    ('(p1-choice . 1)
      (setf (state-player-1-choice state) color)
      (change-phase state 'p2-move))
    ('(p2-choice . 2)
      (setf (state-player-2-choice state) color)
      (change-phase state 'p1-choice))
    (t (error 'invalid-move-for-phase
              :move player
              :phase (state-phase state)))))

(defun maybe-thaler (state x y)
  (cond ((equal (cons x y) (state-thaler state)) 'thaler)
        (t nil)))

(defun eset-peg-move (state color x y)
  (let ((index (position color
                         '(red orange yellow green blue pink white)
                         :test #'equal)))
    (if (find (cons x y)
              (nth index (state-valid-moves state))
              :test #'equal)
        (setf (nth index (state-pegs state)) (cons x y))
        (error 'invalid-peg-placement
               :placement (cons x y)
               :peg color
               :reason (format nil
                               "not one of valid moves: ~a~%"
                               (nth index (state-valid-moves state)))))))

(defun echoose-peg-move (state player color x y)
  (alexandria:switch ((cons (state-phase state) player) :test #'equal)
    ('(p1-move . 1)
      (eset-peg-move state color x y)
      (change-phase state 'p2-move))
    ('(p2-move . 2)
      (eset-peg-move state color x y)
      (change-phase state 'p1-move))
    (t (error 'invalid-move-for-phase
              :move player
              :phase (state-phase state)))))

(defun eset-thaler (state x y)
  (labels ((message-beside-peg (peg) (format nil "beside a peg: ~a" peg))
           (error-invalid-placement (thaler reason)
             (error 'invalid-thaler-placement
                    :placement thaler
                    :reason reason))
           (is-not-adjacent-to (thaler peg)
             (destructuring-bind
                 ((t-x . t-y) . (p-x . p-y))
                 (cons thaler peg)
               (if (and (<= (abs (- p-x t-x)) 1)
                        (<= (abs (- p-y t-y)) 1))
                   (error-invalid-placement thaler
                                            (message-beside-peg peg))))))
    (cond ((not (<= 0 x 7)) (error-invalid-placement (cons x y) "out of board"))
          ((not (<= 0 y 7)) (error-invalid-placement (cons x y) "out of board"))
          (t       nil))
    (loop for peg in (state-pegs state)
          do (is-not-adjacent-to (cons x y) peg))
    (setf (state-thaler state) (cons x y))
    (update-valid-moves state)))

(defun echoose-thaler (state x y)
  (case (state-phase state)
    (p1-thaler
     (eset-thaler state x y)
     (change-phase state 'p2-choice))
    (t (error 'invalid-move-for-phase
              :move 'choose-thaler
              :phase (state-phase state)))))

(defun get-table (state)
  (loop for y from 0 to 7
        collect (loop for x from 0 to 7
                      collect (case (find-color state x y)
                                (0 'red)
                                (1 'orange)
                                (2 'yellow)
                                (3 'green)
                                (4 'blue)
                                (5 'pink)
                                (6 'white)
                                (t (maybe-thaler state x y))))))

(defun piece-to-cell (piece x y)
  (flet ((format-td (css-class content &key (piece ""))
           (format nil
                   "<td class=\"~a\" x_pos=\"~a\" y_pos=\"~a\" ~a>~a</td>~%"
                   css-class
                   x
                   y
                   piece
                   content)))
    (case piece
      (red    (format-td "r-piece" "R" :piece "piece=\"r\""))
      (orange (format-td "o-piece" "O" :piece "piece=\"o\""))
      (yellow (format-td "y-piece" "Y" :piece "piece=\"y\""))
      (green  (format-td "g-piece" "G" :piece "piece=\"g\""))
      (blue   (format-td "b-piece" "B" :piece "piece=\"b\""))
      (pink   (format-td "p-piece" "P" :piece "piece=\"p\""))
      (white  (format-td "w-piece" "W" :piece "piece=\"w\""))
      (thaler (format-td "t-piece" "T" :piece "piece=\"t\""))
      (t (if (evenp (mod (+ x y) 2))
             (format nil
                     "<td class=\"empty1\" x_pos=\"~a\" y_pos=\"~a\"></td>~%"
                     x y)
             (format nil
                     "<td class=\"empty2\" x_pos=\"~a\" y_pos=\"~a\"></td>~%"
                     x y))))))

(defun get-table-html (state)
  (append '("<div id=\"game-table\" hx-swap-oob=\"true\">")
          '("<table id=\"game-table-contents\">")
          (list
           (format nil
                   "<tr><td></td>~a</tr>~%"
                   (apply #'str:concat
                          (loop for x from 0 to 7
                                collect (format nil "<td>~a</td>~%" x)))))
          (loop for y from 0
                for row in (get-table state)
                collect (let* ((cells (loop for x from 0
                                            for c in row
                                            collect (piece-to-cell c x y)))
                               (row (apply #'str:concat cells)))
                          (format nil "<tr><td>~%~a</td>~%~a</tr>~%" y row)))
          '("</table>")
          '("</div>")))
