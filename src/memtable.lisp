(in-package :sinkhole)

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
    :initform nil
    :accessor storage
    :documentation "Value stored")
   (prev-ts
    :initform 0
    :accessor prev-ts
    :documentation "Previously stored timestamp")))

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

(defun memtable-insert (memtable timestamp value)
  (if (= (val-count memtable) 0)
      (progn
        (setf (index memtable) timestamp)
        (setf (storage memtable) (list (make-memtable-row 0 value)))
        (setf (prev-ts memtable) timestamp)
        (incf (val-count memtable)))
      (let* (;; (prev-delta (- timestamp (prev-ts memtable)))
             (ts-delta (- timestamp (prev-ts memtable)))
             (delta-of-delta (- ts-delta (timestamp (car (last (storage memtable)))))))
        (setf (storage memtable) (append (storage memtable) (list (make-memtable-row delta-of-delta value))))
        (setf (prev-ts memtable) timestamp)
        (incf (val-count memtable)))))

(defun memtable-seal (memtable)
  (list (index memtable) (val-count memtable) (storage memtable)))

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
