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
  (let ((table (sinkhole/memtable:make-memtable)))
    (is (zerop (sinkhole/memtable:index table)))
    (is (zerop (sinkhole/memtable::val-count table)))

    (sinkhole/memtable:memtable-insert table 1234 42)
    (is (= (sinkhole/memtable:index table) 1234))
    (is (= (sinkhole/memtable::val-count table) 1))
    (is (sinkhole/memtable::memtable-row= (sinkhole/memtable:make-memtable-row 1234 42)
                                          (first (sinkhole/memtable::storage table))))))

(fiveam:test memtable-flushing
  (let ((table (sinkhole/memtable:make-memtable)))
    (dotimes (i 10)
      (sinkhole/memtable:memtable-insert table i (+ 10 i)))
    (is (= 10 (sinkhole/memtable::val-count table)))
    ;; (is (= 3 (length (sinkhole::memtable-seal table))))

    (let* ((stream (sinkhole/memtable:memtable-flush-to-byte-stream
                   table
                   (make-array 50 :element-type 'unsigned-byte)))
          (data (coerce (sinkhole/byte-stream::data stream) 'list)))
      (is (= (first data) 10))
      (dotimes (i 10)
        (is (= (sinkhole/compressor:leb128i-decompress-array (subseq
                                                              data
                                                              (+ 1 (* 2 i))
                                                              (+ 2 (* 2 i)))) i)))
      (let* ((data (sinkhole/byte-stream::data stream))
             (new-table (sinkhole/memtable:memtable-load-from-byte-stream data 50))
             (old-storage (sinkhole/memtable::storage table))
             (new-storage (sinkhole/memtable::storage new-table)))
        (loop for x in old-storage
              for y in new-storage
              do
                 (is (sinkhole/memtable::memtable-row= x y)))))))
