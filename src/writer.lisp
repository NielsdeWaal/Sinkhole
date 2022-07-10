(defpackage :sinkhole/writer
  (:use :cl))
(in-package :sinkhole/writer)

;;;; Writer to fill buffer and handle overflows
;;;; Buffers which are full can be flushed to disk.
;;;;
;;;; TODOs
;;;; - Inserting into buffers (done)
;;;; - Giving back buffers which are full and can be flushed either to disk or some place in memory (done)
;;;; - Small cache for buffer which is currently being filled.
;;;; - Be able to set the backend to things other than simple array (i.e raw buffer) (done)
;;;; - tests

(defclass writer ()
  ((capacity
    :initarg :capacity
    :documentation "number of octets which can be stored per buffer"
    :accessor capacity)
   (current-buffer
    :initarg :current-buffer
    :accessor current-buffer)
   (writer-head
    :accessor writer-head
    :initform 0)))

(defun make-buffer (&key (capacity 512) (type :simple-array))
  (case type
    (:simple-array (make-array capacity :element-type 'unsigned-byte))
    (t (format t "Unsupported buffer type"))))

(defun make-writer (capacity)
  (make-instance 'writer :capacity capacity :current-buffer (make-buffer :capacity capacity)))

(defun reset-writer (writer)
  (setf (current-buffer writer) (make-array (capacity writer) :element-type 'unsigned-byte))
  (setf (writer-head writer) 0))

(defun write-to-buffer (writer bytes)
  (typecase bytes
    (list (if (> (length bytes) (- (capacity writer) (writer-head writer)))
              (prog1
                  (current-buffer writer)
                (reset-writer writer)
                (write-list-to-buffer writer bytes))
              (write-list-to-buffer writer bytes))) ;; check if room for length of list
    (number (if (= (capacity writer) (writer-head writer))
                (prog1
                    (current-buffer writer)
                  (reset-writer writer)
                  (write-byte-to-buffer writer bytes))
                (write-byte-to-buffer writer bytes))) ;; check if room for one more byte
    (t (format t "~A unsupported type"  bytes))))

(defun write-list-to-buffer (writer bytes)
  (loop for x in bytes
        for y from 0
        do
           (write-byte-to-buffer writer x)))

(defun write-byte-to-buffer (writer byte)
  (setf (aref (current-buffer writer) (writer-head writer)) byte)
  (incf (writer-head writer)))
