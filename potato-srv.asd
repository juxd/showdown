(defsystem "potato-srv"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ("alexandria" "str" "woo")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("game"))
                 (:file "game"))))
  :description ""
  :in-order-to ((test-op (test-op "potato-srv/tests"))))

(defsystem "potato-srv/tests"
  :author ""
  :license ""
  :depends-on ("potato-srv"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for potato-srv"
  :perform (test-op (op c) (symbol-call :rove :run c)))
