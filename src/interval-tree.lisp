;; (in-package :sinkhole)

(in-package #:sinkhole/interval-tree)

(defclass interval ()
  ((start
    :initarg :start
    :accessor start)
   (end
    :initarg :end
    :accessor end)
   (max
    :initarg :max
    :initform 0
    :accessor interval-max)
   (left
    :initform nil
    :accessor left)
   (right
    :initform nil
    :accessor right)
   (data
    :initarg :data
    :initform nil
    :accessor interval-data)))

(defun make-interval (start end &optional data)
  (declare (type integer start end))
  (make-instance 'interval :start start :end end :max end :data data))

(defun interval< (lhs rhs)
  (declare (type interval lhs rhs))
  (< (start lhs) (start rhs)))

(defun interval= (lhs rhs)
  (declare (type interval lhs rhs))
  (and (= (start lhs) (start rhs))
       (= (end lhs) (end rhs))))

(defmethod print-object ((obj interval) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((start start)
                     (end end))
        obj
      (format stream "(~d - ~d)~%" start end))))

(defclass interval-tree ()
  ((root
    :initform nil
    :accessor root
    :type (or null interval))))

(defun make-interval-tree ()
  (make-instance 'interval-tree))

(defun interval-tree-insert (tree interval)
  (setf (root tree)
        (interval-tree-node-insert (root tree) interval)))

(defun interval-tree-node-insert (node interval)
  ;; (declare (type interval node interval))
  (when (null node)
    (return-from interval-tree-node-insert interval))
  (when (< (interval-max node) (interval-max interval))
    (setf (interval-max node) (end interval)))
  (if (interval< node interval)
      (if (null (right node))
          (setf (right node) interval)
          (interval-tree-node-insert (right node) interval))
      (if (null (left node))
          (setf (left node) interval)
          (interval-tree-node-insert (left node) interval)))
  node)

(defun dump-interval-tree (interval)
  (unless (null interval)
    (when (left interval)
      (dump-interval-tree (left interval)))
    (format t "~w" interval)
    (when (right interval)
      (dump-interval-tree (right interval)))))

(defparameter *find-all-accumulator* '())
(defun %interval-tree-find-all (node interval)
  (when (null node)
    (return-from %interval-tree-find-all))
  (when (not (or (> (start node) (end interval))
                 (< (end node) (start interval))))
    ;; (alexandria:appendf acc (list node))
    (setf *find-all-accumulator* (append *find-all-accumulator* (list node)))
    ;; (format t "found overlapping interval, acc: ~{~w ~}~%" acc)
    )
  (when (and (left node)
             (>= (interval-max (left node))
                 (start interval)))
    (%interval-tree-find-all (left node) interval)
    ;; (let ((res (%interval-tree-find-all acc (left node) interval)))
    ;;   ;; (setf acc (append acc res))
    ;;   (format t "res: ~{~w ~}~%" (union acc res)))
    )
  (%interval-tree-find-all (right node) interval))

(defun interval-tree-find-all (tree interval)
  (setf *find-all-accumulator* '())
  (%interval-tree-find-all (root tree) interval)
  *find-all-accumulator*
  ;; (let ((acc '()))
  ;;   (%interval-tree-find-all acc (root tree) interval)
  ;;   acc)
  )

;; (defun test ()
;;   (let ((tree (make-interval-tree)))
;;     (interval-tree-insert tree (make-interval 5 10))
;;     (interval-tree-insert tree (make-interval 15 25))
;;     (interval-tree-insert tree (make-interval 1 12))
;;     (interval-tree-insert tree (make-interval 8 16))
;;     (interval-tree-insert tree (make-interval 14 20))
;;     (interval-tree-insert tree (make-interval 18 21))
;;     (interval-tree-insert tree (make-interval 2 8))

;;     (dump-interval-tree (root tree))
;;     (format t "~%")

;;     (interval-tree-find-all tree (make-interval 8 10))))

;; (defmethod print-object ((obj interval-tree) stream)
;;   (print-unreadable-object (obj stream :type t)
;;     (with-accessors ((start start)
;;                      (end end))
;;         obj
;;       (format stream ""))))
