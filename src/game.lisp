(in-package :cl-user)
(defpackage :potato-srv.game
  (:use :cl)
  (:export #:create-state
           #:get-table-html))

(in-package :potato-srv.game)

(defstruct state
  pegs
  thaler
  current-turn
  player-1-choice
  player-2-choice
  phase)

(defun find-color (state x y)
  (position (cons x y)
            (state-pegs state)
            :test #'equal))

(defun maybe-thaler (state x y)
  (cond ((equal (cons x y) (state-thaler state)) :thaler)
        (t nil)))

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
             :current-turn (case initial-phase
                             (:p1-thaler :player-1)
                             (:p2-thaler :player-2))
             :player-1-choice nil
             :player-2-choice nil
             :phase initial-phase))
