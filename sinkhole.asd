;; (defsystem "sinkhole"
;;   :version "0.1.0"
;;   :author ""
;;   :license ""
;;   :depends-on ("cffi" "trivial-gray-streams")
;;   :components ((:module "src"
;;                 :components
;;                 ((:file "package")
;;                  (:file "main")
;;                  (:file "ringbuffer")
;;                  (:file "memtable")
;;                  (:file "interval-tree")
;;                  (:file "qrb")
;;                  (:file "byte-stream")
;;                  (:file "writer"))))
;;   :description ""
;;   :in-order-to ((test-op (test-op "sinkhole/tests"))))

(defsystem "sinkhole"
  :version "0.1.0"
  :depends-on ("cffi"
               "trivial-gray-streams")
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "byte-stream")
                 (:file "compressor")
                 (:file "memtable")
                 (:file "interval-tree")
                 (:file "ringbuffer")
                 (:file "qrb")
                 (:file "writer")
                 (:file "main")))))

;; (defsystem "sinkhole/tests"
;;   :author ""
;;   :license ""
;;   :depends-on ("sinkhole"
;;                "fiveam")
;;   :components ((:module "tests"
;;                 :components
;;                 ((:file "main")
;;                  (:file "ringbuffer")
;;                  (:file "memtable")
;;                  (:file "interval-tree")
;;                  (:file "qrb")
;;                  (:file "byte-stream-test"))))
;;   :description "Test system for sinkhole"
;;   :perform (test-op (op c) (symbol-call :fiveam :run c)))
