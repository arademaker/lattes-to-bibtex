
;; TODO:
;; - validacao de arquivos zip
;; - JS para testar arquivo nao incluido
;; - jquery para submissao
;; - testar arquivo invalido
;; - tutorial sobre exportacao XML do sistema

(require :aserve)
(ql:quickload :cxml)
(ql:quickload :cl-json)
(ql:quickload :zip)
(ql:quickload :xuriella)

(defpackage :website
  (:use :common-lisp :excl :net.uri :net.aserve :net.html.generator :net.aserve.client :zip))

(in-package :website)

(defparameter *LATTES-MODS-XSLT* #P"/Users/arademaker/work/SLattes/lattes2mods.xsl")
(defparameter *DTD-LATTES* #P"/Users/arademaker/work/SLattes/LMPLCurriculo.DTD")

(defun try-to-fix-zip (buf)
  (let ((init-zip (search #(80 75 3 4) buf))
	(init-http (search #(72 84 84 80) buf)))
    (if (and (equal init-http 0) init-zip)
	(subseq buf init-zip)
	buf)))

(defun extract-lattes-from-zip (pathname tmp-template)
  (let ((names nil))
    (with-zipfile (zip pathname)
      (do-zipfile-entries (filename entry zip)
	(push (excl.osi:with-open-temp-file (ss tmp-template)
		(zipfile-entry-contents entry ss)) names)))
    names))

(defun lattes-valid-p (lattes-file)
  (let ((filename lattes-file))
    (if (stringp filename)
	(setf filename (pathname filename)))
    (handler-case 
	(let ((d (cxml:parse-file filename (cxml-dom:make-dom-builder)))
	      (x (cxml:parse-dtd-file *DTD-LATTES*)))
	  (not (dom:map-document (cxml:make-validator x #"CURRICULO-VITAE") d)))
      (cxml:well-formedness-violation () nil))))


(defun lattes-to-mods (lattes-file)
  " convert lattes to mods "
  (let ((filename lattes-file))
    (if (stringp lattes-file)
	(setf filename (pathname lattes-file)))
    (excl.osi:with-open-temp-file (ss "/tmp/mods-XXXXXX")
      (xuriella:apply-stylesheet *LATTES-MODS-XSLT* filename :output ss))))


(defun mods-to-bibtex (mods-file)
  " return a tuple of filenames: the first is the bibtex the second
    is the error "
  (let ((filenames nil))
    (excl.osi:with-open-temp-file (outfile "/tmp/bibtex-XXXXXX" :filename outfilename)
      (excl.osi:with-open-temp-file (errfile "/tmp/error-XXXXXX" :filename errfilename)
	(excl.osi:command-output (format nil "/opt/local/bin/xml2bib -b -w ~a" mods-file) 
				 :output-file outfile :error-output-file errfile)
	(append `(,outfilename ,errfilename) filenames)))
    (values-list filenames)))


(defun lattes-to-bibtex (lattes-file)
  (if (lattes-valid-p lattes-file)
      (multiple-value-bind (bibtex-file error-file) 
	  (mods-to-bibtex (lattes-to-mods lattes-file))
	(values bibtex-file error-file))
      (values nil nil)))
      

;; (defun mods-to-bibtex (mods-file)
;;   " return a tuple of filenames: the first is the bibtex the second is the error "
;;   (let ((outfile (excl.osi:mkstemp "/tmp/bib-XXXXXX"))
;; 	(errfile (excl.osi:mkstemp "/tmp/err-XXXXXX")))
;;     (progn 
;;       (excl.osi:command-output (format nil "xml2bib -b -w ~a" mods-file) :output-file outfile :error-output-file errfile)
;;       (close outfile)
;;       (close errfile)
;;       (list outfile errfile))))

(start :port 8000 :external-format (crlf-base-ef :utf-8))

(defparameter *known-form-items* '("fileup" "format"))

(defmacro my-header ()
  `(:head (:title "Conversor Lattes-BibTeX")
	  ((:meta :name "author" :content "Alexandre Rademaker"))
	  ((:link :href "/static/lattes-to-bibtex.css" :rel "stylesheet" :type "text/css"))
	  ((:link :href "http://fonts.googleapis.com/css?family=Averia+Gruesa+Libre&subset=latin,latin-ext" :rel "stylesheet" :type  "text/css"))
	  (:body (:h1 "Conversor Lattes-BibTeX"))))

(defmacro my-footer ()
  `((:div :class "footer")
    ((:div :class "contact")
     (:p ((:a :href "http://arademaker.github.com") "Alexandre Rademaker")
	 (:br)
	 "arademaker AT gmail DOT com"))
    ((:div :class "contact")
     (:p ((:a :href "http://github.com/arademaker/SLattes/" :target "_blank") "Semantic Lattes")
	 (:br)
	 ((:a :href "http://sourceforge.net/p/bibutils/home/Bibutils/" :target "_blank") "bibutils")
	 (:br)
	 ((:a :href "http://common-lisp.net/project/cxml/" :target "_blank") "CXML")
	 (:br)
	 ((:a :href "http://www.quicklisp.org/" :target "_blank") "QuickLisp")
	 (:br)
	 ((:a :href "http://allegroserve.sourceforge.net/" :target "_blank") "Allegro Serve")))
    ((:div :class "rss")
     ((:a :href "http://lispers.org/" :target "_blank")
      ((:img :src "/static/lisplogo.png" alt="lisp logo"))))))

(defmacro voltar ()
  `(:p "Clique " ((:a :href "/") "aqui") " para voltar"))


(defun generate-form (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html 
       (:html (my-header)
	      ((:form :method "post" :enctype "multipart/form-data" :action "/upload")
	       (:p "Submeta seu arquivo XML Lattes para conversão: ")
	       ((:input :name "fileup" :id "fileup" :type "file"))
	       ((:input :name "format" :value "json" :type "hidden"))
	       ((:input :type "submit" :value "Enviar")))
	      (my-footer))))))

(defun fetch-multipart-sequence (req &key (length nil) (format :binary))
  (if length
      (let ((buffer (make-array length :element-type (if (equal format :text)
							 'character
							 '(unsigned-byte 8))))
	    (start 0)
	    (end length))
	(do* ((bytes-read start index)
	      (index start (get-multipart-sequence req buffer :start index :end end)))
	     ((or (null index) (= index end)) (values buffer (or index bytes-read)))))
      (let ((buffer (get-all-multipart-data req :type format)))
	(values buffer (length buffer)))))

(defun read-file (path) 
  "Read file and returns a string with its contents" 
  (with-open-file (s path)
    (let* ((len (file-length s)) 
	   (data (make-string len)))
      (read-sequence data s) 
      (values data))))

(defun save-file (data)
  (excl.osi:with-open-temp-file (ss "/tmp/lattes-XXXXXX")
    ;; (format ss data)
    (write-sequence data ss)))

(defun process-form (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (html 
       (:html (my-header)
	      (do ((header (get-multipart-header req) (get-multipart-header req)))
		  (nil)
		 (multiple-value-bind (type item-name filename content-type)
		     (parse-multipart-header header)
		   (when (equal type :eof) 
		     (return t)) ;; no more headers
		   (when (member item-name *known-form-items* :test #'equal)
		     ;; it's a form item we know about, handle it
		     (case type
		       ((:file)
			(multiple-value-bind (buf len)
			    (fetch-multipart-sequence req :format :binary)
			  (let ((temp-lattes-file (save-file buf)))
			    (multiple-value-bind (bibtex-file error-file) 
				(lattes-to-bibtex temp-lattes-file)
			      (if bibtex-file
				  (html (:p "Obrigado por usar este serviço. Aqui está seu BibTex:")
					(voltar)
					((:div :class "bibsource")
					 (:pre 
					  (:princ-safe (read-file bibtex-file)))))
				  (html (:p "Este arquivo não é um XML/Lattes válido.")
					(voltar)))))))
		       ((:nofile)
			(html (:p "Você não anexou um arquivo.")
			      (voltar)))))))
	      (my-footer))))))


(defparameter *response-method-not-allowed* (net.aserve::make-resp 405 "Method Not Allowed"))

(push *response-method-not-allowed* net.aserve::*responses*)

(publish :path "/"
	 :content-type "text/html; charset=utf-8;"
	 :function #'generate-form)

(publish-directory :prefix "/static/" 
		   :destination (namestring #P"static/"))

(publish :path "/upload"
	 :content-type "text/html; charset=utf-8;"
	 :function #'process-form)


;;; test with json

(defun json-test (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (json:encode-json '((bibtex . "teste bib") (message . "teste msg")) (request-reply-stream req)))))

(publish :path "/json"
	 :content-type "application/json; charset=utf-8;"
	 :function #'json-test)

(publish-file :path "/json-form"
	      :content-type "text/html; charset=utf-8;"
	      :file "form-test.html")


;; (html-stream *standard-output* (html (:head (:title "Lattes2Bibtex Converter")
;;  						     ((:meta :name "author" :content "Alexandre Rademaker")))))

;; (json:with-object () (json:encode-object-member 'bibtex (read-file bibtex-file)) (json:encode-object-member 'message (read-file error-file)))