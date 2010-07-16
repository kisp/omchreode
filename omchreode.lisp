;;; library load file for OM
(in-package :om)

(require-library "ompw")

(flet ((load-compile (path)
	 (if (member :om-deliver *features*)
	     (om::compile&load (make-pathname :type nil :defaults path))
	     (load (compile-file path)))))
  (load-compile (merge-pathnames #p"sources/bpf.lisp" *load-pathname*))
  (load-compile (merge-pathnames #p"sources/chreode.lisp" *load-pathname*)))
