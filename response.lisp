(in-package :stupid-server)

(defun construct-content-type (type)
  "Construct header value (but here used to construct Content-Type)"
  (cond
    ((= (length type) 3)
     (format nil "~a; ~a=~a"
             (first type)
             (string-downcase (symbol-name (second type)))
             (third type)))
    ((= (length type) 1)
     (first type))
    (t (error "Something wrong"))))

(defun format-time (time)
  "Format time string"
  (multiple-value-bind (second minute hour date month year day-of-week)
      (decode-universal-time time)
    ;; Oh my!
    (format nil
            "~[Mon~;Tue~;Wed~;Thu~;Fri~;Sat~;Sun~], ~2,'0d ~[Jan~;Feb~;Mar~;Apr~;May~;Jun~;Jul~;Aug~;Sep~;Oct~;Nov~;Dec~] ~d ~2,'0d:~2,'0d:~2,'0d GMT"
            day-of-week date (1- month) year hour minute second)))

(defstruct response
  version
  code
  headers
  data
  (buffer #() :type simple-vector)

  (file-length 0 :type non-negative-fixnum)
  file-pathname)

(defun compose-response (response)
  "Translate response to simple-vector"
  (declare (optimize (speed 3)))
  (let (response-list)
    (let ((data (response-data response)))
      (when (or data (response-file-pathname response))
        (push data response-list)
        (push *newline-code* response-list)))
    (dolist (header (response-headers response))
      (push
       (string-to-octets-latin-1
        (format nil "~a: ~a~a"
                (car header) (cdr header)
                *newline-string*))
       response-list))
    (push
     (string-to-octets-latin-1
      (format nil "~a ~3,'0d ~a~a"
              (response-version response)
              (car (response-code response))
              (cdr (response-code response))
              *newline-string*))
     response-list)
    (setf (response-buffer response)
          (apply #'concatenate 'vector
                 response-list)))
  response)

;; Some response generators

(defun make-status-code-page-generator (code &optional (charset "utf-8"))
  "Default status code pages (like 404 not found)"
  (lambda (request)
    (let ((code-string (format nil "~d: ~a" (car code) (cdr code)))
          (response (make-response)))
      (setf (response-data response)
            (with-http-output (out)
              (write-line "<!DOCTYPE html>" out)
              (with-html-output (out)
                (htm
                 (:html
                  (terpri out)
                  (:head (:title (str code-string)))
                  (terpri out)
                  (:body
                   (terpri out)
                   (:h1 (str code-string))
                   (terpri out)
                   (:p (fmt "Error accessing ~a" (request-uri request)))))))
              (terpri out))
            (response-code response) code)
      (push (cons "Content-Type" (format nil "text/html;charset=~a" charset))
            (response-headers response))
      response)))

(defun get-status-code-page-generator (code)
  "Get status-code-page handler"
  (multiple-value-bind (gen found)
      (gethash (car code) *status-code-page-generators*)
    (if found gen
        (setf (gethash (car code) *status-code-page-generators*)
              (make-status-code-page-generator code)))))

(defun get-file-type (filename)
  "MIME types for different file extensions"
  (let ((type (pathname-type filename)))
    (cond
      ((or (string= type "htm")
           (string= type "html"))
       "text/html")
      ((or (string= type "jpg")
           (string= type "jpeg"))
       "image/jpeg")
      ((string= type "png")
       "image/png")
      ((string= type "txt")
       "text/plain")
      ((string= type "pdf")
       "application/pdf")
      (t "application/octet-stream"))))

(defun make-static-file-generator (filename &key (charset "utf-8"))
  "Static file handler (for example .html file / image on disk)"
  (lambda (request)
    (handler-case
        (let ((response (make-response))
              (pathname (truename filename)))
          (push
           (cons "Content-Type"
                 (let ((content-type (get-file-type pathname)))
                   (if (search "text" content-type)
                       (construct-content-type (list content-type :charset charset))
                       (construct-content-type (list content-type)))))
           (response-headers response))
          (setf (response-code response) *http-ok*
                (response-file-pathname response) pathname
                (response-file-length response)
                (with-open-file (in pathname :element-type '(unsigned-byte 8))
                  (file-length in)))
          response)
      (file-error ()
        (funcall (get-status-code-page-generator *http-not-found*) request)))))

(defun make-page-from-stream-generator (filler &key
                                                 (charset "utf-8")
                                                 (code *http-ok*))
  "Dynamic page handler. FILLER must be a function with 2 arguments:
STREAM and REQUEST. It must write to the stream content of html page"
  (lambda (request)
    (let ((response (make-response)))
      (setf (response-data response)
            (with-http-output (stream)
              (funcall filler stream request))
            (response-code response)
            code)
      (push (cons "Content-Type"
                  (construct-content-type (list "text/html" :charset charset)))
            (response-headers response))
      response)))

(defun make-post-request-handler (function &key location (code *http-found*))
  (let ((response (make-response)))
    (if location (push (cons "Location" location) (response-headers response)))
    (lambda (request)
      (let ((new-code (funcall function request)))
        (setf (response-code response)
              (if new-code new-code code)))
      response)))

(defun com-funcall (x y)
  (funcall y x))

(defun process-request (request)
  "Generate a response accepting a REQUEST"
  (let ((response
         (restart-case
             (funcall
              (cond
                ((or
                  (string= *method-get* (request-method request))
                  (string= *method-post* (request-method request))) ;We can understand only this now
                 (let ((generator (find (request-uri request) *dispatch-table*
                                        :test #'com-funcall :key #'car)))
                   (if generator (cdr generator)
                       (get-status-code-page-generator *http-not-found*))))
                (t
                 (get-status-code-page-generator *http-not-implemented*)))
              request)
           (server-send-error ()
             (funcall (get-status-code-page-generator *http-server-error*) request)))))

    (let ((length 0))
      (if (response-data response)
          (incf length (length (response-data response))))
      (if (response-file-length response)
          (incf length (response-file-length response)))
      (if (not (zerop length))
          (push (cons "Content-Length" length)
                (response-headers response))))
    (push '("Connection" . "close") (response-headers response))
    (setf (response-version response) (request-version request))
    response))

(defun set-uri-handler (uri handler &key (remove-old t))
  "Set URI handler possibly deleting the old one"
  (let ((dispatch-table
         (if (and (stringp uri) remove-old)
             (let ((entry (find uri *dispatch-table* :test #'com-funcall :key #'car)))
               (remove entry *dispatch-table*))
             *dispatch-table*)))

    (setq *dispatch-table*
          (cons
           (typecase uri
             (string
              (cons (lambda (str) (string= str uri)) handler))
             (function
              (cons uri handler))
             (t (error "URI must be of type string or function")))
           dispatch-table))))

(defmacro define-page-from-stream-handler ((uri stream) param-list &body body)
  "Create and set dynamic page handler. URI is an URI in the dispatch table.
Your handler must write to STREAM a content of page to be sent. PARAM-LIST
is a list of parameters used, in the form (NAME PNAME TYPE). NAME is a name of variable
which is bound with value of either HTTP header (if TYPE is :HEADER) or POST/GET parameter
(if TYPE is :PARAMTER) with name PNAME. Example of PARAM-LIST:
((ua \"User-Agent\" :header)
 (name \"name\" :parameter)).
In this case, if client accesses a web page http://server.name/url?name=value ,
variable UA will be bound with client's UA and NAME with \"value\""
  (let* ((request (gensym))
         (param-bindings (loop for param in param-list collect
                              (destructuring-bind (name pname type) param
                                (ecase type
                                  (:header
                                   `(,name (cdr (assoc ,pname (request-headers ,request) :test #'string=))))
                                  (:parameter
                                   `(,name (cdr (assoc ,pname (request-parameters ,request) :test #'string=)))))))))
    `(set-uri-handler ,uri
                      (make-page-from-stream-generator
                       (lambda (,stream ,request)
                         ,@(if param-bindings
                              `((let ,param-bindings
                                  ,@body))
                              (cons `(declare (ignore ,request))
                                    body)))))))


(defmacro define-post-request-handler (uri location param-list &body body)
  (let* ((request (gensym))
         (param-bindings (loop for param in param-list collect
                              (destructuring-bind (name pname type) param
                                (ecase type
                                  (:header
                                   `(,name (cdr (assoc ,pname (request-headers ,request) :test #'string=))))
                                  (:parameter
                                   `(,name (cdr (assoc ,pname (request-parameters ,request) :test #'string=)))))))))
    `(set-uri-handler ,uri
                      (make-post-request-handler
                       (lambda (,request)
                         ,@(if param-bindings
                               `((let ,param-bindings
                                   ,@body))
                               (cons `(declare (ignore ,request))
                                     body)))
                       :location ,location))))

;; Must be really slow, but since there is no sendfile(2) in Common Lisp :-(
(defun install-static-directory-handler (uri directory &key (charset "utf-8"))
  "Make and insert to *DISPATCH-TABLE* handler for files in directory.
Files in DIRECTORY will be available under their names prefixed with URI."
  (let ((directory-pathname (probe-file directory)))
    (if (or (not directory-pathname) (pathname-name directory-pathname))
        (error "There is no such directory in file system"))
    (set-uri-handler
     (lambda (string)
       (and (> (length string)
               (length uri))
            (string= uri (subseq string 0 (length uri)))))
     (lambda (request)
       (let ((filename
              (merge-pathnames
               (subseq (request-uri request)
                       (length uri))
               directory-pathname)))
         (funcall (make-static-file-generator filename :charset charset) request))))))
