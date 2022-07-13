(in-package #:sinkhole/byte-stream)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline system-relative-namestring))
  (defun system-relative-namestring (system name)
    (namestring (asdf:system-relative-pathname system name))))

(define-foreign-library malloc-wrapper
  ;; (:unix (merge-pathnames (uiop/os:getcwd) "c-interop/malloc-wrapper.so"))
  ;; (:unix "/home/niels/Dev/LearningCommonLisp/sinkhole/src/c-interop/malloc-wrapper.so")
  (:unix "/home/niels/common-lisp/sinkhole/src/c-interop/malloc-wrapper.so")
  ;; (:unix (namestring (truename (make-pathname :name "malloc-wrapper.so" :directory '(:relative "c-interop")))))
  (t (:default "malloc-wrapper")))

(use-foreign-library malloc-wrapper)

(defcfun "get_buffer" :pointer
  (size :unsigned-int))

(deftype ub8 () '(unsigned-byte 8))

(declaim (inline encode-lsb decode-lsb))

(defun get-lsb-byte (number byte)
  (declare (type integer number)
           (type (signed-byte 32) byte))
  (logand #xff (ash number (* byte -8))))

(defun decode-lsb (bytes)
  (let ((result 0))
    (declare (type integer result))
    (loop for b across bytes
          for ix from 0 do
            (setf result (logior result (ash b (* ix 8)))))
    result))

(defun encode-lsb (number bytes)
  (declare (type integer number))
  (let ((result (make-array (list bytes) :element-type 'ub8)))
    (loop for x from 0 below bytes
          do (setf (aref result x) (get-lsb-byte number x)))
    result))

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

(defun make-byte-stream (size &optional (data nil))
  (make-instance 'byte-stream :data (or data (get-buffer size)) :size size)
  ;; (make-instance 'byte-stream :data (or data (make-array size :element-type 'ub8)) :size size)
  )

(defmethod stream-write-byte ((stream byte-stream) byte)
  (if (= (slot-value stream 'position) (slot-value stream 'size))
      (progn
        ;; (setf (slot-value stream 'open-p) nil)
        (error 'end-of-file :stream stream))
      (progn
        (typecase (slot-value stream 'data)
          (simple-array
           (setf (aref (slot-value stream 'data) (slot-value stream 'position)) byte))
          (sb-sys:system-area-pointer
           (setf (mem-ref (slot-value stream 'data) :uchar (slot-value stream 'position)) byte)))
        (incf (slot-value stream 'position)))))

(defun write-short (stream value)
  (write-sequence (encode-lsb value 2) stream))
;; (defun write-int (stream value)
;;   (write-sequence (encode-lsb value 4) stream))
(defun write-long (stream value)
  (write-sequence (encode-lsb value 4) stream))
(defun write-long-long (stream value)
  (write-sequence (encode-lsb value 8) stream))

(declaim (inline write-short write-long write-long-long))

(defun space-left-bytes (stream)
  (- (slot-value stream 'size) (slot-value stream 'position)))

(defmethod #+sbcl sb-gray:stream-write-sequence
  #-sbcl stream-write-sequence ((stream byte-stream) sequence #+sbcl &optional start end #-sbcl &key)
  (loop for ix from start to (or end (1- (length sequence)))
        do (write-byte (aref sequence ix) stream)))

;; (defmethod stream-write-sequence ((stream byte-stream) sequence start end
;;                                   &key &allow-other-keys)
;;   (with-slots (data) stream
;;     (mapc #'(lambda (x) (write-byte x stream)) sequence)))

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
  (with-output-to-buffer (stream :size 10)
    (write-sequence (encode-lsb 63550 2) stream :end 1))
  ;; (let ((stream (make-byte-stream 10)))
  ;;   (write-byte 42 stream)
  ;;   (write-byte 42 stream)
  ;;   (write-byte 42 stream)
  ;;   stream)
  )
