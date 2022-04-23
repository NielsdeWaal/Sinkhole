(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cffi")
  (ql:quickload "fiveam"))

(require 'cffi)
(defpackage sinkhole
  (:use :cl :cffi))
(in-package :sinkhole)

(defclass sinkhole ()
  ((qrb
    :initform (make-qrb :quorum 32)
    :accessor qrb
    :documentation "qrb for incoming datapoints")
   (memtable-queue
    :initform (list (make-memtable))
    :accessor memtable-queue
    :documentation "Queue of sealed memtables. TODO These need to be converted to raw data, for use with storage")
   (memtable-tree
    :initform (make-interval-tree)
    :accessor memtable-tree
    :documentation "Storing pointers to memtables with the stored interval. TODO later to be used for storing offsets into raw storage")
   (sealing-limit
    :initform nil
    :initarg :sealing-limit
    :accessor sealing-limit
    :documentation "Limit for memtables. Determines how many values they hold before considered full")))

(defun make-sinkhole (&key sealing-limit)
  (make-instance 'sinkhole :sealing-limit sealing-limit))

(defun sinkhole-insert (sinkhole timestamp value)
  (with-accessors ((table memtable-queue)
                   (quantum qrb)) sinkhole
    (let ((flushable (insert quantum (make-memtable-row timestamp value))))
      (when (not (null flushable))
        (mapc #'(lambda (x) (memtable-insert (first (last table)) (timestamp x) (value x)))
              flushable)
        (when (memtable-fullp (first (last table)) :limit (sealing-limit sinkhole))
          (interval-tree-insert (memtable-tree sinkhole)
                                (make-interval (index (first (last table)))
                                               timestamp
                                               (last table)))
          (setf table (append table (list (make-memtable)))))))))

(defparameter database (make-sinkhole))

(defun main ()
  (setf database (make-sinkhole :sealing-limit 10))
  (let ((start-time 2000))
    (dotimes (i 40)
      (sinkhole-insert database (+ start-time i) (random 100)))))
