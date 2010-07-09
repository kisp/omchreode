;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(asdf:defsystem omchreode
  :version "0.2.0"
  :description "Port of OMCHREODE library of Jean-Baptiste Barrière"
  :maintainer "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :author "Kilian Sprotte <kilian.sprotte@gmail.com>"
  :depends-on (ompw)
  :serial t
  :components ((:module "sources"
			:components ((:file "package")
				     ;; (:file "bpf")
				     (:file "chreode")))))
