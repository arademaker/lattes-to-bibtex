;;;; lattes-to-bibtex.asd

(asdf:defsystem #:lattes-to-bibtex
  :serial t
  :depends-on (#:cxml
               #:cl-json
	       #:cxml-rng
               #:zip
               #:xuriella)
  :components ((:file "package")
               (:file "lattes-to-bibtex")
	       (:static-file "form.html")
	       (:static-file "sobre.html")
	       (:static-file "LICENSE")
	       (:static-file "README.md")
	       (:static-file "PENDING.org")
	       (:module "static" :components ((:static-file "lattes-to-bibtex.css")
					      (:static-file "lisplogo.png")))))




