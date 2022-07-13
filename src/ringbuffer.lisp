;; (in-package :sinkhole)

;; TODO conditions for when ring is full or empty
;; TODO overload pop with callback argument for popped value

(in-package #:sinkhole/ringbuffer)

(defclass ringbuffer ()
  ((buffer-size
    :initarg :size
    :accessor buffer-size)
   (buffer
    :initarg :buffer
    :accessor buffer)
   (head
    :initform 0
    :accessor head)
   (tail
    :initform 0
    :accessor tail)
   (mask
    :accessor mask)))

(defmethod initialize-instance :after ((buf ringbuffer) &rest args)
  (declare (ignore args))
  (setf (mask buf) (1- (buffer-size buf))))

(defun %full-p (buffer)
  (= (head buffer) (tail buffer)))

(defun %wrap-index (value mask)
  (logand (1+ value) mask))

(defun ringbuffer-count (head tail mask)
  (logand (- head tail) mask))

(defun ringbuffer-space (buffer)
  (ringbuffer-count (tail buffer) (1+ (head buffer)) (mask buffer)))

(defun make-ringbuffer (size)
  (make-instance 'ringbuffer :size size :buffer (make-array size)))

(defun %ringbuffer-push (buffer val)
  (let* ((next-index (mod (1+ (head buffer)) (buffer-size buffer))))
    (when (= next-index (tail buffer))
      (error "ringbuffer is full"))
    (setf (aref (buffer buffer) (head buffer)) val)
    (setf (head buffer) next-index)))

(defun ringbuffer-push (buffer &rest values)
  (mapc #'(lambda (val) (%ringbuffer-push buffer val)) values))

(defun %ringbuffer-push-po2 (buffer val)
  (when (<= (ringbuffer-space buffer) 1)
    (error "ringbuffer is full"))
  (setf (aref (buffer buffer) (head buffer)) val)
  (setf (head buffer) (%wrap-index (head buffer) (mask buffer))))

(defun ringbuffer-push-po2 (buffer &rest values)
  (mapc #'(lambda (val) (%ringbuffer-push-po2 buffer val)) values))

(defun ringbuffer-pop (buffer)
  (when (eq (head buffer) (tail buffer))
    (error "ringbuffer already empty, cannot remove more"))
  (let* ((val (aref (buffer buffer) (tail buffer))))
    (when (= (tail buffer) (buffer-size buffer))
      (setf (head buffer) 0))
    (incf (tail buffer))
    val))

(defun ringbuffer-pop-po2 (buffer)
  (when (<= (ringbuffer-count (head buffer) (tail buffer) (mask buffer)) 1)
    (error "ringbuffer is empty"))
  (let* ((val (aref (buffer buffer) (tail buffer))))
    (setf (tail buffer) (%wrap-index (tail buffer) (mask buffer)))
    val))

(defun ringbuffer-collect-po2 (buffer amount)
  (loop repeat amount collect (ringbuffer-pop-po2 buffer)))

(defun ringbuffer-count-stored (buffer)
  (ringbuffer-count (head buffer) (tail buffer) (mask buffer)))
