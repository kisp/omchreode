;;; library load file for OM
(in-package :om)

(require-library "ompw")

(load (compile-file (merge-pathnames #p"sources/bpf.lisp" *load-pathname*)))
(load (compile-file (merge-pathnames #p"sources/chreode.lisp" *load-pathname*)))
