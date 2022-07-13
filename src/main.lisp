(in-package #:sinkhole)

(defclass sinkhole ()
  ((qrb
    :initform (sinkhole/qrb:make-qrb :quorum 32)
    :accessor qrb
    :documentation "qrb for incoming datapoints")
   (memtable-queue
    :initform (list (sinkhole/memtable:make-memtable))
    :accessor memtable-queue
    :documentation "Queue of sealed memtables. TODO These need to be converted to raw data, for use with storage")
   (memtable-tree
    :initform (sinkhole/interval-tree:make-interval-tree)
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
    (let ((flushable (sinkhole/qrb:insert quantum (sinkhole/memtable:make-memtable-row timestamp value))))
      (unless (null flushable)
        ;; TODO with new storage engine this will have to changed first to see if storage
        ;; is full or not before writing to the memtable
        (mapc #'(lambda (x) (sinkhole/memtable:memtable-insert (first (last table)) (sinkhole/memtable:timestamp x) (sinkhole/memtable:value x)))
              flushable)
        (when (sinkhole/memtable:memtable-fullp (first (last table)) :limit (sealing-limit sinkhole))
          (sinkhole/interval-tree:interval-tree-insert (memtable-tree sinkhole)
                                                       (sinkhole/interval-tree:make-interval (sinkhole/memtable:index (first (last table)))
                                                                                             (sinkhole/memtable:prev-ts (first (last table)))
                                                                                             (first (last table))))
          (setf table (append table (list (make-memtable)))))))))

(defparameter database (make-sinkhole))

(defun main ()
  (setf database (make-sinkhole :sealing-limit 10))
  (let ((start-time 2000))
    (dotimes (i 100)
      (sinkhole-insert database (+ start-time i) (random 100)))))
