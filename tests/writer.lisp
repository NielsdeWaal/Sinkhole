(in-package :cl-user)
(defpackage writer-test
  (:use :cl
        :fiveam
        :sinkhole/writer))
(in-package :writer-test)

(fiveam:def-suite* writer-test)
(fiveam:test writer-simple
    (let ((writer (sinkhole/writer::make-writer 5)))
      (fiveam:finishes (sinkhole/writer::write-to-buffer writer 1))
      (fiveam:finishes (sinkhole/writer::write-to-buffer writer '(2 3 4 5)))
      (fiveam:is (= 5 (length (sinkhole/writer::write-to-buffer writer 6))))
      (fiveam:is (= 1 (sinkhole/writer::writer-head writer)))))
