  
(in-package :om)
(om::compile&load (merge-pathnames #p"sources/bpf.lisp" *load-pathname*))
(om::compile&load (merge-pathnames #p"sources/chreode.lisp" *load-pathname*))

;--------------------------------------------------
; RC subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------
(fill-library '(("generators" nil nil (get-value get-values
                                       make-chreode make-tab make-tab-norm make-flip) nil)
                ("dur-sampler" nil nil (make-dur-sampler get-durs) nil)
                ("bpf-utils" nil nil (bpf-lookup) nil)
                ("save-text" nil nil (save-text-simple save-text-coll) nil)))
