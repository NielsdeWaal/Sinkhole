(defpackage #:sinkhole/byte-stream
  (:use #:cl
        #:cffi
        #:trivial-gray-streams)
  (:export byte-stream
           make-byte-stream
           stream-read-byte
           with-output-to-buffer))

(defpackage #:sinkhole/ringbuffer
  (:use #:cl)
  (:export ringbuffer
           make-ringbuffer
           ringbuffer-push
           ringbuffer-push-po2
           ringbuffer-pop
           ringbuffer-pop-po2
           ringbuffer-collect-po2
           ringbuffer-count-stored))

(defpackage #:sinkhole/writer
  (:use #:cl)
  (:export writer
           make-writer
           write-to-buffer))

(defpackage #:sinkhole/qrb
  (:use #:cl)
  (:export QRB
           make-qrb
           insert)
  (:import-from #:sinkhole/ringbuffer
                make-ringbuffer
                ringbuffer-push-po2
                ringbuffer-collect-po2
                ringbuffer-count-stored))

(defpackage #:sinkhole/interval-tree
  (:use #:cl)
  (:export interval-tree
           make-interval-tree
           make-interval
           interval-tree-insert))

(defpackage #:sinkhole/compressor
  (:use #:cl)
  (:export leb128u-compress
           leb128u-decompress
           leb128u-decompress-array
           leb128u-decompress-stream
           leb128i-compress
           leb128i-decompress
           leb128i-decompress-array
           leb128i-decompress-stream))

(defpackage #:sinkhole/memtable
  (:use #:cl)
  (:export memtable
           make-memtable
           index
           prev-ts
           val-count
           memtable-insert
           memtable-fullp
           print-object
           memtable-flush-to-byte-stream
           memtable-load-from-byte-stream
           memtable-row
           make-memtable-row
           timestamp
           value)
  (:import-from #:sinkhole/byte-stream
                make-byte-stream
                write-sequence
                ;; data
                )
  (:import-from #:sinkhole/compressor
                leb128u-compress
                leb128u-decompress
                leb128u-decompress-stream
                leb128i-compress
                leb128i-decompress
                leb128i-decompress-stream))

(defpackage #:sinkhole
  (:use #:cl)
  (:export ;; sinkhole
           make-sinkhole)
  (:import-from #:sinkhole/qrb
                make-qrb)
  (:import-from #:sinkhole/memtable
                make-memtable
                memtable-insert
                memtable-fullp)
  (:import-from #:sinkhole/interval-tree
                make-interval-tree
                make-interval
                interval-tree-insert))
