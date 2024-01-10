(defpackage potato-srv
  (:use :cl :woo :alexandria)
  (:export #:start))

(in-package :potato-srv)

(defparameter *application-root* (asdf:system-source-directory :potato-srv))
(defparameter *templates-directory* (merge-pathnames #P"templates/" *application-root*))

(defun ok-html (content)
  `(200 (:content-type "text/html") ,content))

(defun bruh-html (num)
  (format nil
          "<button hx-get=\"/send?num=~a\" hx-swap=\"outerHTML\">bruh ~a</button>"
          (+ num 1)
          num))

(defun handle-name (name)
  (declare (ignore name))
  (ok-html (merge-pathnames #P"main.html" *templates-directory*)))

(defun handle-error ()
  `(404
    (:content-type "text/plain") ("now thats a bruh")))

(defun handle-send (query)
  (let* ((per-kv (str:split #\& query))
         (query-alist (mapcar
                       (lambda (q) (let ((p (str:split #\= q)))
                                     (cons (car p) (cadr p))))
                       per-kv))
         (num (cdr (assoc "num" query-alist :test #'string=))))
    (ok-html (list (bruh-html (parse-integer num))))))

(defun start ()
  (woo:run
   (lambda (env)
     (destructuring-bind
         (&key query-string path-info &allow-other-keys)
         env
       (let ((s (str:split #\/ path-info :omit-nulls t)))
         (format t "we got this: ~a ~a ~a~%" query-string path-info (length s))
         (case (length s)
           (1 (let ((s (nth 0 s)))
                (cond ((string= s "send") (handle-send query-string))
                      (t (handle-name s)))))
           (t (handle-error))))))))
