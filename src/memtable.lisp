(in-package #:sinkhole/memtable)

(defclass memtable ()
  ((val-count
    :initform 0
    :accessor val-count
    :documentation "Number of entries in the table")
   (index
    :initform 0
    :accessor index
    :documentation "Starting timestamp of the memtable")
   (storage
    :initform '()
    :accessor storage
    :documentation "Value stored")
   (prev-ts
    :initform 0
    :accessor prev-ts
    :documentation "Previously stored timestamp")
   (prev-delta
    :initform 0
    :accessor prev-delta)))

(defclass memtable-row ()
  ((timestamp
    :initarg :timestamp
    :accessor timestamp)
   (value
    :initarg :value
    :accessor value)))

(defun make-memtable-row (timestamp value)
  (make-instance 'memtable-row :timestamp timestamp :value value))

(defun memtable-row= (lhs rhs)
  (and (= (timestamp lhs)
          (timestamp rhs))
       (= (value lhs)
          (value rhs))))

(defun make-memtable ()
  (make-instance 'memtable))

;; (defun memtable-insert (memtable timestamp value)
;;   (if (null (storage memtable)) ;; (= (val-count memtable) 0)
;;       (progn
;;         (setf (index memtable) timestamp)
;;         (setf (storage memtable) (list (make-memtable-row 0 value)))
;;         (setf (prev-ts memtable) timestamp)
;;         (incf (val-count memtable)))
;;       (let* (;; (prev-delta (- timestamp (prev-ts memtable)))
;;              (ts-delta (- timestamp (prev-ts memtable)))
;;              (delta-of-delta (- ts-delta (prev-delta memtable) ;; (timestamp (car (last (storage memtable))))
;;                                 )))
;;         (setf (storage memtable) (append (storage memtable) (list (make-memtable-row delta-of-delta value))))
;;         (setf (prev-ts memtable) timestamp)
;;         (setf (prev-delta memtable) ts-delta)
;;         (incf (val-count memtable)))))

(defun memtable-insert (memtable timestamp value)
  (declare (type memtable memtable))
  (when (zerop (index memtable))
    (setf (index memtable) timestamp))
  (setf (storage memtable) (append (storage memtable) (list (make-memtable-row timestamp value))))
  (incf (val-count memtable)))

;;TODO extract dod encoder and decoder - for testability
(defun memtable-decode (sealed-table)
  (let ((new-table (make-memtable))
        (prev-ts (first sealed-table))
        (prev-delta 0))
    (format t "Decoding sealed table:~% start ts: ~w, count: ~d~%" prev-ts (second sealed-table))
    (setf (index new-table) (first sealed-table))
    (setf (val-count new-table) (second sealed-table))
    (mapc #'(lambda (x)
              (incf prev-delta (timestamp x))
              (incf prev-ts prev-delta)
              (format t "decoded row to: ~d -> ~d~%" prev-ts (value x))
              (memtable-insert new-table prev-ts (value x)))
          (third sealed-table))
    new-table))

(defun memtable-seal (memtable)
  (list (index memtable) (val-count memtable) (storage memtable)))

(defun memtable-fullp (memtable &key (limit 256))
  (>= (val-count memtable) limit))

(defun memtable-flush-to-byte-stream (memtable &optional buf)
  (let* ((stream-size (or (if (or buf) (length buf) 4096)))
         (stream ;; (make-byte-stream 2048 (make-array 2048 :element-type 'unsigned-byte))
                (make-byte-stream stream-size (or buf ;; (make-array 2048 :element-type 'unsigned-byte)
                                           ))))
    (print (list :writing-to-stream (val-count memtable)))
    (write-sequence (coerce (leb128u-compress (val-count memtable)) 'vector) stream)
    (loop for pair in (storage memtable)
          do (with-accessors ((timestamp timestamp)
                              (value value))
                 pair
               (write-sequence (coerce
                                (nconc (leb128i-compress timestamp) (leb128u-compress value))
                                'vector)
                               stream)))
    stream))

;; TODO to make this work the size of the table has to be written at the start
;; otherwise there is no knowning where to stop, there could be 0 values stored in the table
(defun memtable-load-from-byte-stream (buffer buffer-size)
  (let* ((stream (make-byte-stream buffer-size buffer))
         (new-memtable (make-memtable))
         (nr-entries (values (leb128u-decompress-stream stream))))
    (print (list :reading-from-stream nr-entries))
    (loop repeat nr-entries
          do (let ((timestamp (leb128i-decompress-stream stream))
                   (value (leb128u-decompress-stream stream)))
               (memtable-insert new-memtable timestamp value)))
    new-memtable))

(defmethod print-object ((obj memtable-row) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((timestamp timestamp)
                     (value value))
        obj
      (format stream "~d -> ~d" timestamp value))))

(defmethod print-object ((obj memtable) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((val-count val-count)
                     (index index)
                     (storage storage))
        obj
      (format stream "Starting from: ~d, count: ~d,~%~{~w ~%~}" index val-count storage))))
