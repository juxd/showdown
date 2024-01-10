(defpackage potato-srv/tests/main
  (:use :cl
        :potato-srv
        :rove))
(in-package :potato-srv/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
