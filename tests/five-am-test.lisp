(in-package :cl-user)
(defpackage five-am-test
  (:use :cl
        :fiveam
        :sinkhole))
(in-package :five-am-test)

(defun same-elements (lst1 lst2)
  (and (= (length lst1) (length lst2))
       (every #'(lambda (x) (member x lst2 :test #'sinkhole::interval=))
              lst1)
       (every #'(lambda (x) (member x lst1 :test #'sinkhole::interval=))
              lst2)))

(fiveam:def-suite* interval-suite)
(fiveam:test interval-test
             (let ((tree (sinkhole::make-interval-tree)))
               (sinkhole::interval-tree-insert tree (sinkhole::make-interval 5 10))
               (sinkhole::interval-tree-insert tree (sinkhole::make-interval 15 25))
               (sinkhole::interval-tree-insert tree (sinkhole::make-interval 1 12))
               (sinkhole::interval-tree-insert tree (sinkhole::make-interval 8 16))
               (sinkhole::interval-tree-insert tree (sinkhole::make-interval 14 20))
               (sinkhole::interval-tree-insert tree (sinkhole::make-interval 18 21))
               (sinkhole::interval-tree-insert tree (sinkhole::make-interval 2 8))

               (is (same-elements (sinkhole::interval-tree-find-all tree (sinkhole::make-interval 8 10))
                                  (list (sinkhole::make-interval 5 10)
                                        (sinkhole::make-interval 1 12)
                                        (sinkhole::make-interval 2 8)
                                        (sinkhole::make-interval 8 16))))

               ;; (let ((res set-difference (sinkhole::interval-tree-find-all tree (sinkhole::make-interval 8 10))
               ;;            (list (sinkhole::make-interval 5 10)
               ;;                  (sinkhole::make-interval 1 12)
               ;;                  (sinkhole::make-interval 2 8)
               ;;                  (sinkhole::make-interval 8 16)))))
               ))
