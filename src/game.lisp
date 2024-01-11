(in-package :potato-srv)

(defstruct state
  pegs
  thaler
  current-turn
  player-1-choice
  player-2-choice
  phase)

(defun generate-seven-coordinates ()
  (mapcar (lambda (x)
            (multiple-value-bind (x y) (floor x 8) (cons x y)))
          (subseq
           (alexandria:shuffle (loop for i from 0 to 63 collect i))
           0
           7)))
