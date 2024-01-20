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

(defun Chessboard_Image.svg ()
  (list 200
        '(:content-type "image/svg+xml")
        (merge-pathnames #P"Chessboard_Image.svg" *templates-directory*)))

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

(defun make-poller (seq &key (oob nil))
  (format nil
          "<div
id=\"game-poller\"
style=\"{ display: hidden; }\"
hx-get=\"/get-game?seq=~a\"
~a=\"outerHTML\"
hx-trigger=\"load delay:1s\">
</div>"
          seq
          (if oob "hx-swap-oob" "hx-swap")))

(defun get-game (query)
  (flet ((game-status (state)
           (format nil
                   "<div id=\"game-phase\" hx-swap-oob=\"true\">
<h3>Current Phase: ~a</h3>
</div>"
                   (potato-srv.game:state-phase state))))
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
                       (message-to-client (format nil "bruh moment: ~a" cond))))))))

(defun handle-make-game ()
  (potato-srv.game:create-state)
  (ok-html (list (make-poller -1 :oob t))))

(defun handle-choose-thaler (query-alist)
  (let ((x (parse-integer (cdr (assoc "x" query-alist :test #'string=))))
        (y (parse-integer (cdr (assoc "y" query-alist :test #'string=)))))
    (potato-srv.game:echoose-thaler potato-srv.game:*singleton-game* x y)))

(defun handle-choose-color (query-alist)
  (let ((player (parse-integer (cdr (assoc "player" query-alist :test #'string=))))
        (color  (intern
                 (string-upcase
                  (cdr (assoc "color" query-alist :test #'string=)))
                 'potato-srv.game)))
    (potato-srv.game:echoose-color potato-srv.game:*singleton-game* player color)))

(defun handle-choose-move (query-alist)
  (let ((player (parse-integer (cdr (assoc "player" query-alist :test #'string=))))
        (x (parse-integer (cdr (assoc "x" query-alist :test #'string=))))
        (y (parse-integer (cdr (assoc "y" query-alist :test #'string=))))
        (color  (intern
                 (string-upcase
                  (cdr (assoc "color" query-alist :test #'string=)))
                 'potato-srv.game)))
    (potato-srv.game:echoose-peg-move potato-srv.game:*singleton-game*
                                      player
                                      color
                                      x y)))

(defun handle-guess-other-player (query-alist)
  (let ((player (parse-integer (cdr (assoc "player" query-alist :test #'string=))))
        (color  (intern
                 (string-upcase
                  (cdr (assoc "color" query-alist :test #'string=)))
                 'potato-srv.game)))
    (potato-srv.game:eguess-other-player potato-srv.game:*singleton-game*
                                         player
                                         color)))

(defun handle-move (move query)
  (let* ((query-alist (query-str-alist query))
         (error-message-to-client
           (handler-case
               (case move
                 (choose-thaler      (handle-choose-thaler      query-alist) nil)
                 (choose-color       (handle-choose-color       query-alist) nil)
                 (choose-move        (handle-choose-move        query-alist) nil)
                 (guess-other-player (handle-guess-other-player query-alist) nil)
                 (t nil))
             (potato-srv.game:invalid-peg-placement (cond)
               (message-to-client
                (format nil
                        "(only you): bad peg ~a@~a -- ~a"
                        (potato-srv.game:bad-peg-color cond)
                        (potato-srv.game:bad-peg-placement cond)
                        (potato-srv.game:bad-peg-reason cond))))
             (potato-srv.game:invalid-thaler-placement (cond)
               (message-to-client
                (format nil
                        "(only you): bad thaler@~a -- ~a"
                        (potato-srv.game:bad-thaler-placement cond)
                        (potato-srv.game:bad-thaler-reason cond))))
             (potato-srv.game:invalid-move-for-phase (cond)
               (message-to-client
                (format nil
                        "(only you): bad turn: phase:~a move:~a"
                        (potato-srv.game:bad-move-phase cond)
                        (potato-srv.game:bad-move cond))))
             (error (cond)
               (message-to-client
                (format nil "bruh moment: ~a" cond))))))
    (ok-html (list (if error-message-to-client
                       error-message-to-client
                       (message-to-client
                        (format nil
                                "(only you): handled move ~a"
                                query-alist)))))))

(defun start (&key (port 5000))
  (woo:run
   (lambda (env)
     (destructuring-bind
         (&key query-string path-info &allow-other-keys)
         env
       (let ((s (str:split #\/ path-info :omit-nulls t)))
         (case (length s)
           (0 (handle-main))
           (1 (let ((s (nth 0 s)))
                (alexandria:switch (s :test #'string=)
                  ("get-game"      (get-game query-string))
                  ("make-game"     (handle-make-game))
                  ("choose-thaler" (handle-move 'choose-thaler query-string))
                  ("choose-color"  (handle-move 'choose-color query-string))
                  ("choose-move"   (handle-move 'choose-move query-string))
                  ("guess-other-player"
                   (handle-move 'guess-other-player query-string))
                  ("style.css"     (style.css))
                  ("Showdown_Banner.png"  (Showdown_Banner.png))
                  ("Chessboard_Image.svg" (Chessboard_Image.svg))
                  (t (format t "req: ~a ~a ~a~%" query-string path-info (length s))
                     (handle-error)))))
           (t (redirect-to-main))))))
   :port port))
