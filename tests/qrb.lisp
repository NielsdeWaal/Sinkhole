(in-package :cl-user)
(defpackage qrb-test
  (:use :cl
        :fiveam
        :sinkhole))
(in-package :qrb-test)

(fiveam:def-suite* qrb-tests)
(fiveam:test qrb-simple
  (let ((qrb (sinkhole::make-qrb :quorum 4)))
    (is (= 4 (sinkhole::quorum qrb)))
    (is (= 16 (sinkhole::buffer-size (sinkhole::queue qrb))))

    (is (null (sinkhole::insert qrb (sinkhole::make-row 1243 42))))
    (is (null (sinkhole::insert qrb (sinkhole::make-row 1243 42))))
    (is (null (sinkhole::insert qrb (sinkhole::make-row 1243 42))))
    (is (null (sinkhole::insert qrb (sinkhole::make-row 1243 42))))
    ;; Get flushable component
    (let ((flushable (sinkhole::insert qrb (sinkhole::make-row 1243 42))))
      (is (and flushable))
      (is (= 2 (length flushable)))))
  (signals simple-error (sinkhole::make-qrb :quorum 3)))
