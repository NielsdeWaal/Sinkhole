(require 'cffi)
(defpackage sinkhole
  (:use :cl :cffi))
(in-package :sinkhole)

(defclass sinkhole ()
  ())

(defun main ()
  (format t "Hello World"))
