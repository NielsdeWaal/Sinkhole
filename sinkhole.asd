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
                 (:file "interval-tree"))))
  :description ""
  :in-order-to ((test-op (test-op "sinkhole/tests"))))

(defsystem "sinkhole/tests"
  :author ""
  :license ""
  :depends-on ("sinkhole"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for sinkhole"
  :perform (test-op (op c) (symbol-call :rove :run c)))
