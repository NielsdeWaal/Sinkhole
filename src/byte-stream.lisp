(in-package :sinkhole)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline system-relative-namestring))
  (defun system-relative-namestring (system name)
    (namestring (asdf:system-relative-pathname system name))))

(cffi:define-foreign-library malloc-wrapper
  (:unix (merge-pathnames (uiop/os:getcwd) "c-interop/malloc-wrapper.so"))
  (t (:default "malloc-wrapper")))

(use-foreign-library malloc-wrapper)
