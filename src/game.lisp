(in-package :cl-user)
(defpackage :potato-srv.game
  (:use :cl)
  (:export #:create-state
           #:get-table-html))

(in-package :potato-srv.game)

(defstruct state
  pegs
  thaler
  player-1-choice
  player-2-choice
  phase)

(defvar *all-game-states* (create-state (:p1-thaler)))

(defun find-color (state x y)
  (position (cons x y)
            (state-pegs state)
            :test #'equal))

(defun maybe-thaler (state x y)
  (cond ((equal (cons x y) (state-thaler state)) :thaler)
        (t nil)))

(defun set-thaler (state x y)
  (flet ((is-not-adjacent-to (thaler peg)
           (destructuring-bind
               ((t-x . t-y) . (p-x . p-y))
               (cons thaler peg)
               (cond ((>= (abs (- p-x t-x)) 1) nil)
                     ((>= (abs (- p-y t-y)) 1) nil)
                     (t                        t)))))
    (alexandria:when-let
        ((is-in-board        (cond ((< x 0) nil)
                                   ((> x 7) nil)
                                   ((< x 0) nil)
                                   ((> x 7) nil)
                                   (t       t)))
         (is-not-beside-pegs (loop for peg in (state-pegs state)
                                   always (is-not-adjacent-to (cons x y) peg))))
      (setf (state-thaler state) (cons x y))
      t)))

(defun choose-thaler (state x y)
  (flet ((set-thaler () (setf (state-thaler state) (cons x y))))
    (case (state-phase state)
      (:p1-thaler (set-thaler))
      (:p2-thaler (set-thaler))
      (t nil))))

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
  (case piece
    (:red    "<td class=\"r-piece\">R</td>")
    (:orange "<td class=\"o-piece\">O</td>")
    (:yellow "<td class=\"y-piece\">Y</td>")
    (:green  "<td class=\"g-piece\">G</td>")
    (:blue   "<td class=\"b-piece\">B</td>")
    (:pink   "<td class=\"p-piece\">P</td>")
    (:white  "<td class=\"w-piece\">W</td>")
    (:thaler "<td class=\"t-piece\">T</td>")
    (t (cond ((evenp (mod (+ x y) 2)) "<td class=\"empty1\"></td>")
             (t "<td class=\"empty2\"></td>")))
    ))

(defun get-table-html (state)
  (append (list "<table>")
          (loop for y from 0
                for row in (get-table state)
                collect (let* ((cells (loop for x from 0
                                           for c in row
                                           collect (piece-to-cell c x y)))
                               (row (apply #'str:concat cells)))
                          (format nil "<tr>~a</tr>" row)))
          (list "</table>")))

(defun generate-seven-coordinates ()
  (mapcar (lambda (x)
            (multiple-value-bind (x y) (floor x 8) (cons x y)))
          (subseq
           (alexandria:shuffle (loop for i from 0 to 63 collect i))
           0
           7)))

(defun create-state (initial-phase)
  (make-state :pegs (generate-seven-coordinates)
              :thaler nil
              :player-1-choice nil
              :player-2-choice nil
              :phase initial-phase))
