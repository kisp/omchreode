(in-package :om)

(require-library "ompw")

(load (compile-file (merge-pathnames #p"sources/bpf.lisp" *load-pathname*)))
(load (compile-file (merge-pathnames #p"sources/chreode.lisp" *load-pathname*)))

(fill-library '(("generators" nil nil (get-value get-values
				       make-chreode make-tab make-tab-norm make-flip) nil)
		("dur-sampler" nil nil (make-dur-sampler get-durs) nil)
		("bpf-utils" nil nil (bpf-lookup) nil)
		("save-text" nil nil (save-text-simple save-text-coll) nil)))
