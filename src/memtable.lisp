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

(defun memtable-insert (memtable timestamp value)
  (if (null (storage memtable)) ;; (= (val-count memtable) 0)
      (progn
        (setf (index memtable) timestamp)
        (setf (storage memtable) (list (make-memtable-row 0 value)))
        (setf (prev-ts memtable) timestamp)
        (incf (val-count memtable)))
      (let* (;; (prev-delta (- timestamp (prev-ts memtable)))
             (ts-delta (- timestamp (prev-ts memtable)))
             (delta-of-delta (- ts-delta (prev-delta memtable) ;; (timestamp (car (last (storage memtable))))
                                )))
        (setf (storage memtable) (append (storage memtable) (list (make-memtable-row delta-of-delta value))))
        (setf (prev-ts memtable) timestamp)
        (setf (prev-delta memtable) ts-delta)
        (incf (val-count memtable)))))

;; TODO extract dod encoder and decoder - for testability
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
