(in-package :cl-user)
(defpackage qrb-test
  (:use :cl
        :fiveam
        :sinkhole))
(in-package :qrb-test)

(defun same-elements (lst1 lst2)
  (and (= (length lst1) (length lst2))
       (every #'(lambda (x) (member x lst2 :test #'(lambda (x y) (and (= (sinkhole::timestamp x) (sinkhole::timestamp y))
                                                                      (= (sinkhole::row-values x) (sinkhole::row-values y))))))
              lst1)
       (every #'(lambda (x) (member x lst1 :test #'(lambda (x y) (and (= (sinkhole::timestamp x) (sinkhole::timestamp y))
                                                                      (= (sinkhole::row-values x) (sinkhole::row-values y))))))
              lst2)))

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

(fiveam:test qrb-sorting
  (let ((qrb (sinkhole::make-qrb :quorum 4)))
    (sinkhole::insert qrb (sinkhole::make-row 1234 1))
    (sinkhole::insert qrb (sinkhole::make-row 1235 1))
    (sinkhole::insert qrb (sinkhole::make-row 1233 1))
    (sinkhole::insert qrb (sinkhole::make-row 1232 1))

    (let ((flushable (sinkhole::insert qrb (sinkhole::make-row 1234 5))))
      (is (same-elements flushable
                         (list (sinkhole::make-row 1232 1)
                               (sinkhole::make-row 1233 1)))))))
