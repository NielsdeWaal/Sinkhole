(defpackage sinkhole/tests/main
  (:use :cl
        :sinkhole
        :rove))
(in-package :sinkhole/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :sinkhole)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
