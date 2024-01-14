(in-package :cl-user)
(defpackage potato-srv
  (:use :cl)
  (:export #:start))

(in-package :potato-srv)

(defparameter *application-root* (asdf:system-source-directory :potato-srv))
(defparameter *templates-directory* (merge-pathnames #P"templates/" *application-root*))

(defun ok-html (content &key (headers '()))
  (list 200 (nconc (list :content-type "text/html") headers) content))

(defun query-str-alist (query)
  (mapcar
   (lambda (q) (let ((p (str:split #\= q)))
                 (cons (car p) (cadr p))))
   (str:split #\& query)))

(defun style.css ()
  (list 200
        '(:content-type "text/css")
        (merge-pathnames #P"style.css" *templates-directory*)))

(defun Showdown_Banner.png ()
  (list 200
        '(:content-type "image/png")
        (merge-pathnames #P"Showdown_Banner.png" *templates-directory*)))

(defun game-status (state)
  (format nil
          "<div id=\"game-phase\" hx-swap-oob=\"true\">
<h3>Current Phase: ~a</h3>
</div>"
          (potato-srv.game:state-phase state)))

(defun message-to-client (message)
  (format nil
          "<p id=\"message-box\" hx-swap-oob=\"afterbegin\">~a</br></p>"
          message))

(defun redirect-to-main ()
  '(301 (:location "/") ()))

(defun handle-main ()
  (ok-html (merge-pathnames #P"main.html" *templates-directory*)))

(defun handle-error ()
  `(404
    (:content-type "text/plain") ("now thats a bruh")))

(defun make-poller (seq)
  (format nil
          "<div
id=\"game-poller\"
style=\"{ display: hidden; }\"
hx-get=\"/get-game?seq=~a\"
hx-swap=\"outerHTML\"
hx-trigger=\"load delay:1s\">
</div>"
          seq))

(defun get-game (query)
  (handler-case
      (let ((req-seq (parse-integer (cdr (assoc "seq"
                                                (query-str-alist query)
                                                :test #'string=)))))
        (cond
          ((not potato-srv.game:*singleton-game*)
           (ok-html (list (make-poller req-seq))))
          ((= (potato-srv.game:state-phase-seq
               potato-srv.game:*singleton-game*)
              req-seq)
           (ok-html (list (make-poller req-seq))))
          (t (ok-html `(,(make-poller (potato-srv.game:state-phase-seq
                                       potato-srv.game:*singleton-game*))
                        ,(game-status potato-srv.game:*singleton-game*)
                        ,(str:concat
                          "<div id=\"player-move-form\" hx-swap-oob=\"true\">"
                          (potato-srv.game:generate-move-form
                           potato-srv.game:*singleton-game*)
                          "</div>")
                        . ,(potato-srv.game:get-table-html
                            potato-srv.game:*singleton-game*))))))
    (error (cond)
      (ok-html (list (make-poller -1)
                     (game-status potato-srv.game:*singleton-game*)
                     (message-to-client (format nil "bruh moment: ~a" cond)))))))

(defun handle-make-game ()
  (potato-srv.game:create-state)
  (ok-html nil))

(defun handle-choose-thaler (query-alist)
  (let ((x (parse-integer (cdr (assoc "x" query-alist :test #'string=))))
        (y (parse-integer (cdr (assoc "y" query-alist :test #'string=)))))
    (potato-srv.game:echoose-thaler potato-srv.game:*singleton-game* x y)))

(defun handle-move (move query)
  (let ((error-message-to-client
          (handler-case
              (case move
                (:choose-thaler
                 (handle-choose-thaler (query-str-alist query))
                 nil)
                (t nil))
            (potato-srv.game:invalid-thaler-placement (cond)
              (message-to-client
               (format nil
                       "open your eyes pls: thaler@~a -- ~a"
                       (potato-srv.game:bad-thaler-placement cond)
                       (potato-srv.game:bad-thaler-reason cond))))
            (potato-srv.game:invalid-move-for-phase (cond)
              (message-to-client
               (format nil
                       "pls wait: phase:~a move:~a"
                       (potato-srv.game:bad-move-phase cond)
                       (potato-srv.game:bad-move cond))))
            (error (cond)
              (message-to-client
               (format nil "bruh moment: ~a" cond))))))
    (ok-html (list (if error-message-to-client
                       error-message-to-client
                       (message-to-client (format nil "We made the move ~a" query)))))))

(defun start ()
  (woo:run
   (lambda (env)
     (destructuring-bind
         (&key query-string path-info &allow-other-keys)
         env
       (let ((s (str:split #\/ path-info :omit-nulls t)))
         (format t "req: ~a ~a ~a~%" query-string path-info (length s))
         (case (length s)
           (0 (handle-main))
           (1 (let ((s (nth 0 s)))
                (alexandria:switch (s :test #'string=)
                  ("get-game" (get-game query-string))
                  ("make-game" (handle-make-game))
                  ("choose-thaler" (handle-move :choose-thaler query-string))
                  ("style.css" (style.css))
                  ("Showdown_Banner.png" (Showdown_Banner.png))
                  (t (handle-error)))))
           (t (redirect-to-main))))))))
