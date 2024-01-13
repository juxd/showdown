(in-package :cl-user)
(defpackage potato-srv
  (:use :cl)
  (:export #:start))

(in-package :potato-srv)

(defparameter *application-root* (asdf:system-source-directory :potato-srv))
(defparameter *templates-directory* (merge-pathnames #P"templates/" *application-root*))

(defun ok-html (content &key (headers '()))
  (list 200 (nconc (list :content-type "text/html") headers) content))

(defun style.css ()
  (list 200
        '(:content-type "text/css")
        (merge-pathnames #P"style.css" *templates-directory*)))

(defun message-to-client (message)
  (format nil
          "<p hx-swap-oob=\"afterbegin:#message-box\">~a</br></p>"
          message))

(defun bruh-html (num)
  (format nil
          "<button hx-get=\"/send?num=~a\" hx-swap=\"outerHTML\">bruh ~a</button>"
          (+ num 1)
          num))

(defun redirect-to-main ()
  '(301 (:location "/") ()))

(defun handle-main ()
  (ok-html (merge-pathnames #P"main.html" *templates-directory*)))

(defun handle-error ()
  `(404
    (:content-type "text/plain") ("now thats a bruh")))

(defun handle-make-game ()
  (ok-html (potato-srv.game:get-table-html
            (potato-srv.game:create-state :p1-thaler))))

(defun query-str-alist (query)
  (mapcar
   (lambda (q) (let ((p (str:split #\= q)))
                 (cons (car p) (cadr p))))
   (str:split #\& query)))

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
              (format nil "bruh moment: ~a" cond)))))
    (ok-html (cons (if error-message-to-client
                       error-message-to-client
                       (format nil "We made the move ~a" query))
                   (potato-srv.game:get-table-html potato-srv.game:*singleton-game*)))))

(defun start ()
  (woo:run
   (lambda (env)
     (destructuring-bind
         (&key query-string path-info &allow-other-keys)
         env
       (let ((s (str:split #\/ path-info :omit-nulls t)))
         (format t "we got this: ~a ~a ~a~%" query-string path-info (length s))
         (case (length s)
           (0 (handle-main))
           (1 (let ((s (nth 0 s)))
                (alexandria:switch (s :test #'string=)
                  ("make-game" (handle-make-game))
                  ("choose-thaler" (handle-move :choose-thaler query-string))
                  ("style.css" (style.css))
                  (t (handle-error)))))
           (t (redirect-to-main))))))))
