(in-package :sinkhole)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline system-relative-namestring))
  (defun system-relative-namestring (system name)
    (namestring (asdf:system-relative-pathname system name))))

(define-foreign-library malloc-wrapper
  ;; (:unix (merge-pathnames (uiop/os:getcwd) "c-interop/malloc-wrapper.so"))
  (:unix "/home/niels/Dev/LearningCommonLisp/sinkhole/src/c-interop/malloc-wrapper.so")
  (t (:default "malloc-wrapper")))

(use-foreign-library malloc-wrapper)

(defcfun "get_buffer" :pointer
  (size :unsigned-int))

(deftype ub8 () '(unsigned-byte 8))

(defclass byte-stream (fundamental-binary-output-stream)
  ((data
    :initarg :data
    :type (vector ub8))
   (position
    :initform 0)
   (size
    :initarg :size
    :type integer)))

(defmethod stream-element-type ((stream byte-stream))
  'ub8)

(defmethod stream-write-byte ((stream byte-stream) byte)
  (with-slots (data
               position) stream
    (if (< position (length data))
        (prog1
          (setf (mem-ref data :char position) byte)
          (incf position))
        :eof)))

(let* ((arr (get-buffer 32))
       (stream (make-instance 'byte-stream :data arr)))
  (dotimes (i 4)
    (write-char #\a stream)))
