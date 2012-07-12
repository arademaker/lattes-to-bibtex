
;;;; lattes-to-bibtex.lisp

(in-package #:lattes-to-bibtex)

(defparameter *LATTES-MODS-XSLT* #P"~/work/SLattes/lattes2mods.xsl")
(defparameter *DTD-LATTES* #P"~/work/SLattes/LMPLCurriculo.DTD")
(defparameter *DATA-DIR* #P"/tmp/")

;; Zach suggested 
;; (asdf:system-source-directory :lattes-to-bibtex)
(defparameter *WORKING-PATH* (pathname-directory (asdf:system-definition-pathname 
						  (asdf:find-system :lattes-to-bibtex))))


;; utilities functions

(defun template (prefix)
  (namestring (make-pathname :directory (pathname-directory *DATA-DIR*)
			     :name (format nil "~a-XXXXXX" prefix))))

(defun is-fixable-zip-p (buf)
  (let ((init-zip (search #(80 75 3 4) buf))
	(init-http (search #(72 84 84 80) buf)))
    (if (and (equal init-http 0) init-zip)
	;; remove the header of the HTTP protocol
	(values t (subseq buf init-zip))
	;; I can't do anything
	nil)))
 
(defun extract-files-from-zip (pathname tmp-template)
  (let ((names nil))
    (with-zipfile (zip pathname)
      (do-zipfile-entries (filename entry zip)
	(excl.osi:with-open-temp-file (ss tmp-template :filename entry-filename)
	  (zipfile-entry-contents entry ss)
	  (push entry-filename names))))
    names))

(defun lattes-valid-p (lattes-file)
  " Test if a LATTES file is valid according the DTD "
  (let ((filename lattes-file))
    (if (stringp filename)
	(setf filename (pathname filename)))
    (handler-bind ((warning #'muffle-warning))
	(handler-case 
	    (let ((d (cxml:parse-file filename (cxml-dom:make-dom-builder)))
		  (x (cxml:parse-dtd-file *DTD-LATTES*)))
	      (not (dom:map-document (cxml:make-validator x #"CURRICULO-VITAE") d)))
	  (cxml:well-formedness-violation () nil)))))


(defun lattes-to-mods (lattes-file)
  " Convert a lattes XML file to a mods XML file using the XSLT "
  (let ((filename lattes-file))
    (if (stringp lattes-file)
	(setf filename (pathname lattes-file)))
    (excl.osi:with-open-temp-file (ss (template "mods"))
      (xuriella:apply-stylesheet *LATTES-MODS-XSLT* filename :output ss))))


(defun mods-to-bibtex (mods-file)
  " return a tuple of filenames: the first is the bibtex the second
    is the error "
  (let ((filenames nil))
    (excl.osi:with-open-temp-file (outfile (template "bibtex") :filename outfilename)
      (excl.osi:with-open-temp-file (errfile (template "error") :filename errfilename)
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
    (write-sequence data ss)))


(define-condition invalid-file (error)
  ((text :initarg :text :reader text)))

(define-condition invalid-lattes-file (invalid-file)
  ((text :initarg :text :reader text)))

(define-condition invalid-zip-file (invalid-file)
  ((text :initarg :text :reader text)))


(defun lattes-to-bibtex (lattes-file)
  (if (lattes-valid-p lattes-file)
      (multiple-value-bind (bibtex-file error-file) 
	  (mods-to-bibtex (lattes-to-mods lattes-file))
	(values bibtex-file error-file))
      (error 'invalid-lattes-file :text "Este arquivo não é um XML LATTES válido.")))


(defun extract-lattes-from-zip-buf (buf)
  (let* ((zip-file (save-to-temp buf (template "zip")))
	 (lattes-files (extract-files-from-zip zip-file (template "lattes"))))
    (assert (> (length lattes-files) 0))
    (car lattes-files)))


(defun buffer-to-bibtex (buf filename)
  (let* ((filepath (pathname filename))
	 (filepath-type (pathname-type filepath :case :common)))
    (if (string-equal filepath-type  "ZIP")
	(multiple-value-bind (test new-buf)
	    (is-fixable-zip-p buf)
	  (handler-case 
	      (if test
		  (lattes-to-bibtex (extract-lattes-from-zip-buf new-buf))
		  (lattes-to-bibtex (extract-lattes-from-zip-buf buf)))
	    (simple-error () 
	      (error 'invalid-zip-file :text "Este arquivo não é um ZIP válido."))))
	(lattes-to-bibtex (save-to-temp buf (template "lattes"))))))


;; (defun buffer-to-bibtex (buf filename)
;;   (let* ((filepath (pathname filename))
;; 	 (filepath-type (pathname-type filepath :case :common)))
;;     (if (string-equal filepath-type  "ZIP")
;; 	(handler-case 
;; 	    (lattes-to-bibtex (extract-lattes-from-zip-buf buf))
;; 	  (simple-error () 
;; 	    (multiple-value-bind (test new-buf)
;; 		(is-fixable-zip-p buf)
;; 	      (if test
;; 		  (handler-case 
;; 		      (lattes-to-bibtex (extract-lattes-from-zip-buf new-buf))
;; 		    (simple-error () 
;; 		      (error 'invalid-zip-file :text "Este arquivo não é um ZIP válido."))))
;; 	      (error 'invalid-zip-file :text "Este arquivo não é um ZIP válido.")))))))


;; WEBSITE

(defparameter *known-form-items* '("fileup"))

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

(defparameter *response-method-not-allowed* (net.aserve::make-resp 405 "Method Not Allowed"))

(push *response-method-not-allowed* net.aserve::*responses*)


(defun process-json (req ent)
  (with-http-response (req ent)
    (with-http-body (req ent)
      (do ((header (get-multipart-header req) (get-multipart-header req)))
	  (nil)
	(multiple-value-bind (type item-name filename content-type)
	    (parse-multipart-header header)
	  (when (equal type :eof) 
	    ;; no more headers
	    (return t)) 
	  (when (member item-name *known-form-items* :test #'equal)
	    ;; it's a form item we know about, handle it
	    (case type
	      ((:file)
	       (multiple-value-bind (buf len)
		   (fetch-multipart-sequence req :format :binary)
		 (handler-case 
		     (multiple-value-bind (bibtex-file error-file) 
			 (buffer-to-bibtex buf filename)
		       (json:encode-json `((stdout . ,(read-file bibtex-file)) 
					   (stderr . ,(read-file error-file))
					   (message . ,"Obrigado por usar este serviço."))
					 (request-reply-stream req)))
		   (invalid-file (err)
		     (json:encode-json `((stdout . ,nil) 
					 (stderr . ,nil)
					 (message . , (slot-value err 'text)))
				       (request-reply-stream req))))))
	      ((:nofile)
	       (json:encode-json `((stdout . ,nil) 
				   (stderr . ,nil) 
				   (message . ,"Você não anexou nenhum arquivo")) 
				 (request-reply-stream req))))))))))


(publish :path "/json"
	 :content-type "application/json; charset=utf-8;"
	 :function #'process-json)

(publish-directory :prefix "/static/" 
		   :destination (namestring (make-pathname :directory 
							   (append *WORKING-PATH* (list "static")))))

(publish-file :path "/"
	      :content-type "text/html; charset=utf-8;"
	      :file (namestring (make-pathname :name "form" :type "html" 
					       :directory *WORKING-PATH*)))

(publish-file :path "/sobre.html"
	      :content-type "text/html; charset=utf-8;"
	      :file (namestring (make-pathname :name "sobre" :type "html" 
					       :directory *WORKING-PATH*)))

(start :port 8000 :external-format (crlf-base-ef :utf-8))

