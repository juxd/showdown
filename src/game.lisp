(in-package :cl-user)
(defpackage :potato-srv.game
  (:use :cl)
  (:export :*singleton-game*
           :invalid-thaler-placement
           :invalid-move-for-phase
           #:create-state
           #:state-phase
           #:echoose-thaler
           #:get-table-html
           #:bad-thaler-placement
           #:bad-thaler-reason
           #:bad-move
           #:bad-move-phase))

(in-package :potato-srv.game)

(define-condition invalid-thaler-placement (error)
  ((placement :initarg :placement
              :initform nil
              :reader bad-thaler-placement)
   (reason :initarg :reason
           :initform nil
           :reader bad-thaler-reason))
  (:documentation "When someone tried to place a thaler at a bad place"))

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
  phase)

(defvar *singleton-game* nil)

(defun generate-seven-coordinates ()
  (mapcar (lambda (x)
            (multiple-value-bind (x y) (floor x 8) (cons x y)))
          (subseq
           (alexandria:shuffle (loop for i from 0 to 63 collect i))
           0
           7)))

(defun create-state ()
  (setf *singleton-game* (make-state :pegs (generate-seven-coordinates)
                                     :thaler nil
                                     :player-1-choice nil
                                     :player-2-choice nil
                                     :phase :p1-thaler))
  *singleton-game*)

(defun find-color (state x y)
  (position (cons x y)
            (state-pegs state)
            :test #'equal))

(defun echoose-color (state player color)
  (alexandria:switch ((cons (state-phase state) player)
                      :test #'equal)
    ('(p1-choice . p1-choice)
      (setf (state-player-1-choice state) color)
      (setf (state-phase state) :player-2-move))
    ('(p2-choice . p2-choice)
      (setf (state-player-2-choice state) colot)
      (setf (state-phase state) :p1-choice))
    (t (error 'invalid-move-for-phase :move player
                                      :phase (state-phase state)))))

(defun maybe-thaler (state x y)
  (cond ((equal (cons x y) (state-thaler state)) :thaler)
        (t nil)))

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
    (let* ((message-out-of-board "out of board")
           (thaler (cons x y)))
      (cond ((< x 0) (error-invalid-placement thaler message-out-of-board))
            ((> x 7) (error-invalid-placement thaler message-out-of-board))
            ((< y 0) (error-invalid-placement thaler message-out-of-board))
            ((> y 7) (error-invalid-placement thaler message-out-of-board))
            (t       nil)))
    (loop for peg in (state-pegs state)
          do (is-not-adjacent-to (cons x y) peg))
    (setf (state-thaler state) (cons x y))))

(defun echoose-thaler (state x y)
  (case (state-phase state)
    (:p1-thaler
     (eset-thaler state x y)
     (setf (state-phase state) :p2-choice))
    (t (error 'invalid-move-for-phase :move :choose-thaler
                                      :phase (state-phase state)))))

(defun get-table (state)
  (loop for y from 0 to 7
        collect (loop for x from 0 to 7
                      collect (case (find-color state x y)
                                (0 :red)
                                (1 :orange)
                                (2 :yellow)
                                (3 :green)
                                (4 :blue)
                                (5 :pink)
                                (6 :white)
                                (t (maybe-thaler state x y))))))

(defun piece-to-cell (piece x y)
  (flet ((format-td (css-class content)
           (format nil
                   "<td class=\"~a\" x_pos=\"~a\" y_pos=\"~a\">~a</td>~%"
                   css-class x y content)))
    (case piece
      (:red    (format-td "r-piece" "R"))
      (:orange (format-td "o-piece" "O"))
      (:yellow (format-td "y-piece" "Y"))
      (:green  (format-td "g-piece" "G"))
      (:blue   (format-td "b-piece" "B"))
      (:pink   (format-td "p-piece" "P"))
      (:white  (format-td "w-piece" "W"))
      (:thaler (format-td "t-piece" "T"))
      (t (if (evenp (mod (+ x y) 2))
             (format nil
                     "<td class=\"empty1\" x_pos=\"~a\" y_pos=\"~a\"></td>~%"
                     x y)
             (format nil
                     "<td class=\"empty2\" x_pos=\"~a\" y_pos=\"~a\"></td>~%"
                     x y))))))

(defun get-table-html (state)
  (append (list "<table id=\"game-table\" hx-swap-oob=\"true\">")
          (list
           (format nil
                   "<tr><td></td>~a</tr>~%"
                   (apply #'str:concat
                          (loop for x from 0 to 7
                                collect (format nil "<td>~%~a</td>~%" x)))))
          (loop for y from 0
                for row in (get-table state)
                collect (let* ((cells (loop for x from 0
                                            for c in row
                                            collect (piece-to-cell c x y)))
                               (row (apply #'str:concat cells)))
                          (format nil "<tr><td>~%~a</td>~%~a</tr>~%" y row)))
          (list "</table>")))
