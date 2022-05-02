(in-package :cl-user)
(defpackage byte-stream-test
  (:use :cl
        :fiveam
        :sinkhole/byte-stream))
(in-package :byte-stream-test)

(defun same-elements (lst1 lst2)
  (and (= (length lst1) (length lst2))
       (loop for x in lst1
             for y in lst2
             always (equal x y))
       ;; (every #'(lambda (x) (member x lst2))
       ;;        lst1)
       ;; (every #'(lambda (x) (member x lst1))
       ;;        lst2)
       ))

(fiveam:def-suite* byte-stream-test)
(fiveam:test basic-byte-encoding
  (is (equalp (sinkhole/byte-stream::encode-lsb 63550 2)
              #(62 248)))
  (is (= (sinkhole/byte-stream::decode-lsb (make-array 2 :initial-contents '(62 248)))
         63550))
  (is (sinkhole/byte-stream::decode-lsb (sinkhole/byte-stream::encode-lsb 63550 2)))
  (loop repeat 10
        do (is (sinkhole/byte-stream::decode-lsb (sinkhole/byte-stream::encode-lsb (random 10000) 2)))))

(fiveam:test basic-stream
  (let ((stream (sinkhole/byte-stream::make-byte-stream 2)))
    (sinkhole/byte-stream::write-sequence (sinkhole/byte-stream::encode-lsb 63550 2) stream :end 1)
    (is (equalp (slot-value stream 'sinkhole/byte-stream::data)
                (make-array 2 :initial-contents '(62 248)))))
  (sinkhole/byte-stream::with-output-to-buffer (stream :size 2)
    (sinkhole/byte-stream::write-sequence (sinkhole/byte-stream::encode-lsb 63550 2) stream :end 1)
    (is (equalp (slot-value stream 'sinkhole/byte-stream::data)
                (make-array 2 :initial-contents '(62 248))))))
