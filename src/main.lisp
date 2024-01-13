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

(defun handle-send (query)
  (let* ((per-kv (str:split #\& query))
         (query-alist (mapcar
                       (lambda (q) (let ((p (str:split #\= q)))
                                     (cons (car p) (cadr p))))
                       per-kv))
         (num (cdr (assoc "num" query-alist :test #'string=))))
    (ok-html (list (message-to-client (format nil
                                              "You requested for a bruh ~a"
                                              num))
                   (bruh-html (parse-integer num))))))

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
                  ("send" (handle-send query-string))
                  ("make-game" (handle-make-game))
                  ("style.css" (style.css))
                  (t (handle-error)))))
           (t (redirect-to-main))))))))
