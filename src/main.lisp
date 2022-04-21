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
    :documentation "Storing pointers to memtables with the stored interval. TODO later to be used for storing offsets into raw storage")))

(defun make-sinkhole ()
  (make-instance 'sinkhole))

(defun sinkhole-insert (sinkhole timestamp value)
  (with-accessors ((table memtable-queue)
                   (quantum qrb)) sinkhole
    (let ((flushable (insert quantum (make-row timestamp value))))
      (when (not (null flushable))
        (mapc #'(lambda (x) (memtable-insert (last table) (timestamp x) (value x)))
              flushable)
        (when (memtable-fullp (last table))
          (interval-tree-insert (memtable-tree sinkhole)
                                (make-interval (index (last table))
                                               timestamp
                                               (last table)))
          (setf table (append table (list (make-memtable)))))))))

(defun main ()
  (format t "Hello World"))
