(in-package :cl-user)
(defpackage memtable-test
  (:use :cl
        :fiveam
        #:sinkhole/memtable))
(in-package :memtable-test)

(defun generate-timestamps ()
  (loop repeat (+ 10 (random 10))
        for ts = 1 then (incf ts (1+ (random 30)))
        collect ts))

(fiveam:def-suite* memtable)
(fiveam:test memtable-simple
  (let ((table (sinkhole::make-memtable)))
    (is (= 0 (sinkhole::index table)))
    (is (= 0 (sinkhole::val-count table)))
    (is (null (sinkhole::storage table)))

    (sinkhole::memtable-insert table 1234 42)
    (is (= 1234 (sinkhole::index table)))
    (is (= 1 (sinkhole::val-count table)))
    (is (= 1234 (sinkhole::prev-ts table)))
    (is (sinkhole::memtable-row= (sinkhole::make-memtable-row 0 42)
                                 (first (sinkhole::storage table))))

    (sinkhole::memtable-insert table 1235 43)
    (is (= 2 (sinkhole::val-count table)))
    (is (= 1235 (sinkhole::prev-ts table)))
    (is (sinkhole::memtable-row= (sinkhole::make-memtable-row 1 43)
                                 (first (last (sinkhole::storage table)))))))

(fiveam:test memtable-dod-encoding
  (let ((table (sinkhole::make-memtable))
        (ts (generate-timestamps)))
    (mapc #'(lambda (x) (sinkhole::memtable-insert table x 1))
          ts)
    (let* ((prev-ts (sinkhole::index table))
          (prev-delta 0)
          (decoded
            (mapcar #'(lambda (x)
                        (incf prev-delta (sinkhole::timestamp x))
                        (incf prev-ts prev-delta))
                    (sinkhole::storage table))))
      (loop for x in ts
            for y in decoded
            do (is (= x y))))))

(fiveam:test memtable-sealing
  (let ((table (sinkhole::make-memtable)))
    (dotimes (i 10)
      (sinkhole::memtable-insert table i (+ 10 i)))
    (is (= 10 (sinkhole::val-count table)))
    (is (= 3 (length (sinkhole::memtable-seal table))))))
