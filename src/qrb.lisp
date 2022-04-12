(in-package :sinkhole)

(defclass row ()
  ((timestamp
    :initarg :timestamp
    :accessor timestamp)
   (row-values
    :initarg :values
    :accessor row-values)))

(defun make-row (timestamp row-values)
  (make-instance 'row :timestamp timestamp :values row-values))

(defun get-row (row)
  (map 'list #'(lambda (value) (list (timestamp row) value)) (row-values row)))

(defclass QRB ()
  ((quorum
    :initarg :quorum
    :accessor quorum)
   (queue
    :initarg :queue
    :initform nil
    :accessor queue)
   (input-collection
    :initarg :input-collection
    :initform '()
    :accessor input-collection)
   (minimum
    :initarg :minimum
    :initform 0
    :accessor minimum)))

(defun make-qrb (&key quorum)
  ;; (make-instance 'qrb :quorum quorum :queue (make-array quorum))
  (when (/= (ceiling (log quorum 2)) (floor (log quorum 2)))
    (error "Quorum needs to be power of 2"))
  (make-instance 'qrb :quorum quorum :queue (make-ringbuffer (* 4 quorum))))

(defun %insert (item lst &optional (key #'<))
  (if (null lst)
      (list item)
      (if (funcall key item (car lst))
          (cons item lst)
          (cons (car lst) (%insert item (cdr lst) key)))))

(defun %insertion-sort (lst &optional (key #'<))
  (if (null lst)
      lst
      (%insert (car lst) (%insertion-sort (cdr lst) key) key)))

;; TODO be able to insert multiple rows
(defun insert (qrb row)
  (ringbuffer-push-po2 (queue qrb) row)
  (when (> (ringbuffer-count-stored (queue qrb)) (quorum qrb))
    (let* ((sorted (%insertion-sort
                    (ringbuffer-collect-po2 (queue qrb) (quorum qrb))
                    #'(lambda (&rest args) (< (timestamp (first args)) (timestamp (second args))))))
           ;; TODO for now just half the quorum and flush that, maybe do something more intelligent here
           (flushable (subseq sorted 0 (ash (quorum qrb) -1)))
           (remaining (last sorted (- (quorum qrb) (ash (quorum qrb) -1)))))
      (mapc #'(lambda (x) (ringbuffer-push-po2 (queue qrb) x)) remaining)
      flushable)))
