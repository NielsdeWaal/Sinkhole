(defsystem "sinkhole"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main")
                 (:file "ringbuffer")
                 (:file "memtable")
                 (:file "interval-tree")
                 (:file "qrb"))))
  :description ""
  :in-order-to ((test-op (test-op "sinkhole/tests"))))

(defsystem "sinkhole/tests"
  :author ""
  :license ""
  :depends-on ("sinkhole"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "ringbuffer")
                 (:file "memtable")
                 (:file "interval-tree")
                 (:file "qrb"))))
  :description "Test system for sinkhole"
  :perform (test-op (op c) (symbol-call :fiveam :run c)))
