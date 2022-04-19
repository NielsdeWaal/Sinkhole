(require 'cffi)

(defpackage :c-interop
  (:use :cl :cffi))

(in-package :c-interop)

(define-foreign-library malloc-wrapper
    (:unix "/home/niels/Dev/Sinkhole/src/c-interop/malloc-wrapper.so")
    (t (:default "malloc-wrapper")))

(use-foreign-library malloc-wrapper)

(defcfun "get_buffer" :pointer
  (size :unsigned-int))

(defcfun "print_buffer" :void
  (buf :pointer))

(defcfun "check_val" :int
  (buf :pointer))

(defun test-func (buf)
  (setf (mem-ref buf :char 0) (char-code #\b)))
