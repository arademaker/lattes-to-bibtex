
;; TODO:
;; - pacote Lisp
;; - tratar zip invalido que nao pode ser corrigido / corrompido
;; - JS para testar arquivo nao incluido
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


;; utilities functions

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
	(excl.osi:with-open-temp-file (ss tmp-template :filename entry-filename)
		(zipfile-entry-contents entry ss)
		(push entry-filename names))))
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
	(push errfilename filenames))
      (push outfilename filenames))
    (values-list filenames)))


(defun read-file (path) 
  "Read file and returns a string with its contents" 
  (with-open-file (s path)
    (let* ((len (file-length s)) 
	   (data (make-string len)))
      (read-sequence data s) 
      (values data))))

(defun save-to-temp (data template)
  (excl.osi:with-open-temp-file (ss template)
    ;; (format ss data)
    (write-sequence data ss)))


(defun lattes-to-bibtex (lattes-file)
  (if (lattes-valid-p lattes-file)
      (multiple-value-bind (bibtex-file error-file) 
	  (mods-to-bibtex (lattes-to-mods lattes-file))
	(values bibtex-file error-file))
      (values nil nil)))


(defun buffer-to-bibtex (buf filename)
  (let* ((filepath (pathname filename))
	 (filepath-type (pathname-type filepath :case :common)))
    (if (string-equal filepath-type  "ZIP")
	(let* ((zipfile (save-to-temp (try-to-fix-zip buf) "/tmp/zip-XXXXXX"))
	       (lattes-files (extract-lattes-from-zip zipfile "/tmp/lattes-XXXXXX")))
	  (lattes-to-bibtex (car lattes-files)))
	(let ((temp-lattes-file (save-to-temp buf "/tmp/lattes-XXXXXX")))
	  (lattes-to-bibtex temp-lattes-file)))))
      

;; website

(start :port 8000 :external-format (crlf-base-ef :utf-8))

(defparameter *known-form-items* '("fileup"))

(defmacro my-header ()
  `(:head (:title "Conversor Lattes-BibTeX")
	  ((:meta :name "author" :content "Alexandre Rademaker"))
	  ((:link :href "/static/lattes-to-bibtex.css" :rel "stylesheet" :type "text/css"))
	  ((:link :href "http://fonts.googleapis.com/css?family=Averia+Gruesa+Libre&subset=latin,latin-ext" 
		  :rel "stylesheet" :type  "text/css"))
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
			  (multiple-value-bind (bibtex-file error-file) 
			      (buffer-to-bibtex buf filename)
			    (if bibtex-file
				(html (:p "Obrigado por usar este serviço. Aqui está seu BibTex:")
				      (voltar)
				      ((:div :class "bibsource")
				       (:pre 
					(:princ-safe (read-file bibtex-file)))))
				(html (:p "Este arquivo não é um XML/Lattes válido.")
				      (voltar))))))
		       ((:nofile)
			(html (:p "Você não anexou um arquivo.")
			      (voltar)))))))
	      (my-footer))))))


(defparameter *response-method-not-allowed* (net.aserve::make-resp 405 "Method Not Allowed"))

(push *response-method-not-allowed* net.aserve::*responses*)

;; (publish :path "/"
;; 	 :content-type "text/html; charset=utf-8;"
;; 	 :function #'generate-form)

(publish-directory :prefix "/static/" 
		   :destination (namestring #P"static/"))

(publish :path "/upload"
	 :content-type "text/html; charset=utf-8;"
	 :function #'process-form)



;;; test with json

(defun process-json (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
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
		 (multiple-value-bind (bibtex-file error-file) 
		     (buffer-to-bibtex buf filename)
		   (if bibtex-file
		       (json:encode-json `((stdout . ,(read-file bibtex-file)) (stderr . ,(read-file error-file))
					   (message . ,"Obrigado por usar este serviço."))
					 (request-reply-stream req))
		       (json:encode-json `((stdout . ,"none") (stderr . ,"none")
					   (message . ,"Este arquivo não é um XML/Lattes válido."))
					 (request-reply-stream req))))))
	      ((:nofile)
	       (json:encode-json `((stdout . ,"none") (stderr . ,"none") 
				   (message . ,"Você não anexou nenhum arquivo")) 
				 (request-reply-stream req))))))))))


(publish :path "/json"
	 :content-type "application/json; charset=utf-8;"
	 :function #'process-json)

(publish-file :path "/"
	      :content-type "text/html; charset=utf-8;"
	      :file "form.html")


;; (html-stream *standard-output* (html (:head (:title "Lattes2Bibtex Converter")
;;  						     ((:meta :name "author" :content "Alexandre Rademaker")))))

;; (json:with-object () (json:encode-object-member 'bibtex (read-file bibtex-file)) (json:encode-object-member 'message (read-file error-file)))
