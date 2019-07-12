;;;; cl-ygo.asd

(asdf:defsystem #:cl-ygo
  :description "Common Lisp version of yu-gi-oh! engine"
  :author "Xinlong Hu <along.emacs@gmail.com>"
  :license  "GNU General Public License v3.0"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "cl-ygo")
	       (:file "languages")))
