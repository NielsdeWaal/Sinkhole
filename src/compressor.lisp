;; *Scratchpad*
;; compressor has the problem where we want to waste as little space as possible
;; This works fine for flat encoding as we simply fit KV pairs onto disk.
;; Delta encoding should allow for more efficient usage of storage,
;; however this leads to the problem where we can never be sure that we can fit another
;; pair into the memtable as we do not want to have to copy over values during flushing.
;;
;; This file could also be changed to cover a storage engine instead of just a compressor.
;; Doing so would skip some copying overhead but would be slightly in efficient for reading as
;; values would have to be decompressed before being send back to the reader.
;;
;; Would like to have a system where the compressor can sit in between the storage engine
;; and the memtable during writing. This could work as follows:
;; 1. Upon writing to the memtable, the storage engine is either
;;    started or gets a request for a new buffer
;; 2. When writing there should be a typecase/specialization to
;;    see which engine was used.
;; 3. The engine has to dictate whether it is full or not, this is needed for the fixed
;;    size buffer writes of the raw engine.
;; 4. If the buffer is full then it has to be flushed and a new memtab has to be created.
;;
;; There is a chicken-egg problem where the compressor might have some extra requirements
;; for writing. This is the case for the DoD compressor, here there is supposed to be a marker
;; in front of the compressed ts value.
;;
;; Another problem is where metadata might contain information about how much data
;; needs to be read. One solution here would be to have another generic function
;; dictating how much to read and in what way.

;; Using a standard file format
;; List of key value pairs.
;;
;; When using the flat compressor we simply write pairs of 64 bit integers.
;; DoD compressor also writes in pairs but captures the jitter
;; from the timestamps when writing. Lessening some of the overhead.

(in-package #:sinkhole/compressor)

(defclass storage-engine ()
  ((capacity
    :initarg :capacity)
   (compression-engine
    :initarg :compression-engine
    :accessor compression-engine
    :type compressor
    :documentation "Engine used for compression"))
  (:documentation "Superclass for the storagen engines."))

(defclass storage-engine-raw (storage-engine)
  ((buffer-size
    :initarg :buffer-size
    :documentation "Size of the raw buffer used.
Frequently set equal to a disk block size. This will be used with an SPDK backend."))
  (:documentation "Raw storage engine. This engine can be used to write to raw buffers
which flushed to disk in turn"))

(defclass storage-engine-file (storage-engine)
  ()
  (:documentation "File backed storage option."))

(defclass storage-engine-mem (storage-engine)
  ((storage
    :initform nil
    :accessor storage
    :documentation "Storage for the in-memory storage engine.")))

(defclass compressed-row ()
  ((timestamp
    :initarg :timestamp
    :accessor timestamp
    :documentation "Compressed timestamp")
   (value
    :initarg :value
    :accessor value
    :documentation "Compressed value")
   (ts-metadata-header
    :initarg :ts-metadata-header
    :accessor ts-metadata-header
    :documentation "Optional slot where possible metadata for the timestamp is stored. This metadata is meant
to be written in front of the timestamp.")
   (ts-metadata-footer
    :initarg :ts-metadata-footer
    :accessor ts-metadata-footer
    :documentation "Optional slot where possible metadata for the timestamp is stored. This metadata is meant
to be written after the timestamp.")))

(defclass compressor ()
  ())

(defclass flat-compressor (compressor)
  ())

;; NOTE when writing with this compressor - take into account the amount of bytes
;; required. (integer-with) is able to help with this. Otherwise could be wasting bytes.
;; Needs some sort of marker during writes to see how long the following value is.
(defclass delta-of-delta-compressor (compressor)
  ((prev-ts
    :initform 0
    :accessor prev-ts
    :documentation "Previously stored timestamp")
   (prev-delta
    :initform 0
    :accessor prev-delta
    :documentation "Previously calculated delta")))

(defun get-dod-ts (compressor timestamp)
  (let* ((ts-delta (- timestamp (prev-ts compressor)))
         (delta-of-delta (- ts-delta (prev-delta compressor))))
    (setf (prev-ts compressor) timestamp)
    (setf (prev-delta compressor) ts-delta)
    delta-of-delta)
  )

(defun convert-ts-to-unsigned (compressed-timestamp)
  (cond
    ((<= -63 compressed-timestamp 64) #x2)
    ((<= -8388607 compressed-timestamp 8388608) #x6)
    ((<= -2147483647 compressed-timestamp 2147483648) #xE)
    (t #xF)
    ))

(defgeneric storage-write (storage-engine timestamp value)
  (:documentation "Write to backing storage"))

(defmethod storage-write ((storage-engine storage-engine-mem) timestamp value)
  (let ((compressed (compress (compression-engine storage-engine) timestamp value)))
    (if (null (storage storage-engine))
        (setf (storage storage-engine) (list (timestamp compressed) (value compressed)))
        (setf (storage storage-engine)
              (append (storage storage-engine) (list (timestamp compressed) (value compressed)))))))

(defgeneric storage-fullp (engine)
  (:documentation "Check to see if storage backing engine is full or not."))

(defgeneric compress (compressor timestamp value)
  (:documentation "Return ts val pair after compressiong"))

(defmethod compress ((compressor flat-compressor) timestamp value)
  "Flat compressor so no need to include either compression or header/footer values."
  (make-instance 'compressed-row :timestamp timestamp :value value))

;; TODO if statement - true write raw ts value - false -> dod compress
(defmethod compress ((compressor delta-of-delta-compressor) timestamp value)
  (if (= 0 (prev-ts compressor))
      (progn
        (setf (prev-ts compressor) timestamp)
        (make-instance 'compressed-row :timestamp timestamp :value value :ts-metadata-header #xF))
      (let* ((ts (get-dod-ts compressor timestamp))
             (header (convert-ts-to-unsigned ts)))
        (make-instance 'compressed-row :timestamp ts :value value :ts-metadata-header header))))

(defgeneric decompress-prep (compressor compressed-timestamp)
  (:documentation "Possible prep work for decompressor. This might be needed for compressors where some
metadata is stored which needs to be processed first before decompression can begin.
This could be things like loading a table, or reconstructing values using metadata."))

(defmethod decompress-prep ((compressor flat-compressor) compressed-timestamp)
  compressed-timestamp)

(defgeneric decompress (compressor compressed-timestamp)
  (:documentation "Decompress timestamp and value from storage"))

(defmethod decompress ((compressor flat-compressor) compressed-timestamp)
  (list (timestamp compressed-timestamp) (value compressed-timestamp)))

(defmethod decompress ((compressor delta-of-delta-compressor) compressed-timestamp))

;; TODO decompress over iterator
;; TODO remove repeat safety protections
(defun leb128u-compress (value)
  (assert (plusp value))
  (loop repeat 10
        for byte = (logand value #x7F) then (logand i #x7F)
        for i = (ash value -7) then (ash i -7)
        while (/= i 0)
        collect (logior #x80 byte) into res
        finally (return (nconc res (list byte)))))

(defun leb128u-decompress (byte-array)
  (loop for byte in byte-array
        for i from 0
        sum (ash (logand byte #x7F) (* i 7))))

(defun leb128u-decompress-array (byte-array)
  (loop for byte in byte-array
        for i from 0
        sum (ash (logand byte #x7F) (* i 7)) into res
        when (= (logand byte #x80) 0)
          do (loop-finish)
        finally (return (values res (1+ i)))))

(defun leb128u-decompress-stream (stream)
  (loop for byte = (read-byte stream)
        for i from 0
        sum (ash (logand byte #x7F) (* i 7)) into res
        when (= (logand byte #x80) 0)
          do (loop-finish)
        finally (return (values res (1+ i)))))

(defun leb128i-compress (value)
  (loop repeat 10
        for byte = (logand value #x7F) then (logand i #x7F)
        for i = (ash value -7) then (ash i -7)
        until (or (and (zerop i) (zerop (logand byte #x40)))
                  (and (= i -1) (/= (logand byte #x40) 0)))
        collect (logior #x80 byte) into res
        finally (return (nconc res (list byte)))))

(defun leb128i-decompress (byte-array)
  (loop for byte in byte-array
        for i from 0
        sum (ash (logand byte #x7F) (* i 7)) into res
        finally (return (if (/= (logand byte #x40) 0)
                            (logior res (- (ash 1 (+ (* i 7) 7))))
                            res))))

(defun leb128i-decompress-array (byte-array)
  (loop for byte in byte-array
        for i from 0
        sum (ash (logand byte #x7F) (* i 7)) into res
        finally (return (if (/= (logand byte #x40) 0)
                            (logior res (- (ash 1 (+ (* i 7) 7))))
                            res))))

(defun leb128i-decompress-stream (stream)
  (loop for byte = (read-byte stream)
        for i from 0
        sum (ash (logand byte #x7F) (* i 7)) into res
        when (= (logand byte #x80) 0)
          do (loop-finish)
        finally (return (if (/= (logand byte #x40) 0)
                            (logior res (- (ash 1 (+ (* i 7) 7))))
                            res))))
