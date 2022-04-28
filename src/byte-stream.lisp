(defpackage :sinkhole/byte-stream
  (:use :cl :cffi :trivial-gray-streams))
(in-package :sinkhole/byte-stream)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline system-relative-namestring))
  (defun system-relative-namestring (system name)
    (namestring (asdf:system-relative-pathname system name))))

(define-foreign-library malloc-wrapper
  ;; (:unix (merge-pathnames (uiop/os:getcwd) "c-interop/malloc-wrapper.so"))
  (:unix "/home/niels/Dev/LearningCommonLisp/sinkhole/src/c-interop/malloc-wrapper.so")
  ;; (:unix (namestring (truename (make-pathname :name "malloc-wrapper.so" :directory '(:relative "c-interop")))))
  (t (:default "malloc-wrapper")))

(use-foreign-library malloc-wrapper)

(defcfun "get_buffer" :pointer
  (size :unsigned-int))

(deftype ub8 () '(unsigned-byte 8))

(defclass byte-stream (fundamental-binary-stream)
  ((data
    :initarg :data
    ;; :type
    ;; (vector ub8)
    )
   (position
    :initform 0)
   (read-pointer
    :initform 0)
   (size
    :initarg :size
    :type integer)))

(defmethod stream-element-type ((stream byte-stream))
  'ub8)

(defun report-eof (stream eof-errorp eof-value)
  (if eof-errorp
      (error 'end-of-file :stream stream)
      eof-value))

(defun make-byte-stream (size)
  (make-instance 'byte-stream :data (make-array size :element-type 'ub8) :size size))

(defmethod stream-write-byte ((stream byte-stream) byte)
  (if (= (1+ (slot-value stream 'position)) (slot-value stream 'size))
      (progn
        ;; (setf (slot-value stream 'open-p) nil)
        (error 'end-of-file :stream stream))
      (progn
        (setf (aref (slot-value stream 'data) (slot-value stream 'position)) byte)
        (incf (slot-value stream 'position)))))

(defmacro with-output-to-buffer ((var &key (size 4096)) &body body)
  `(let ((,var (make-byte-stream ,size)))
     ,@body
     (slot-value ,var 'data)))

(defmethod stream-read-byte ((stream byte-stream))
  (handler-case
      (aref
       (slot-value stream 'data)
       (prog1
           (slot-value stream 'read-pointer)
         (incf (slot-value stream 'read-pointer))))
    (t ()
      :eof)))
;; (defmethod stream-write-byte ((stream byte-stream) byte)
;;   (with-slots (data
;;                position) stream
;;     (if (< position (length data))
;;         (prog1
;;           (setf (mem-ref data :char position) byte)
;;           (incf position))
;;         :eof)))

;; (let* ((arr (get-buffer 32))
;;        (stream (make-instance 'byte-stream :data arr)))
;;   (dotimes (i 4)
;;     (write-char #\a stream)))

(defun testing ()
  (let ((stream (make-byte-stream 10)))
    (write-byte 42 stream)
    (write-byte 42 stream)
    (write-byte 42 stream)
    stream))
