(in-package :cl-user)

(defpackage :sinkhole.byte-vector
  (:use :cl)
  (:export #:make-byte-vector))

(in-package :sinkhole.byte-vector)

(defclass byte-stream (fundamental-output-stream)
  ((buffer
    :documentation "Buffer in which data is stored")))

(defvar *default-buffer-size* 16)

(defmethod initialize-instance ((self byte-stream) &key buffer-size &allow-other-keys)
  (call-next-method)
  (let ((*default-buffer-size* (or buffer-size *default-buffer-size*)))
    (with-slots (buffer) self
      (setf buffer (make-byte-vector *default-buffer-size*)))))

(defun byte-stream-write-byte (byte buffer)
  (declare (ignore byte buffer))
  (error "unimplemented"))

(defmethod stream-write-byte ((stream byte-stream) byte)
  (with-slots (buffer) stream
    (byte-stream-write-byte byte buffer)))

(deftype ub8 () '(unsigned-byte 8))

(defun make-byte-vector (size)
  (make-array size :element-type 'ub8))
