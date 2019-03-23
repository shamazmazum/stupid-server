(in-package :stupid-server)

(defstruct request
  (method  "" :type string)
  (uri     "" :type string)
  (version "" :type string)
  (content-length 0 :type non-negative-fixnum)
  headers
  parameters
  content)

(defun parse-content-type (type)
  "Parse any HTTP header value (but used here to parse Content-Type)"
  (declare (optimize (speed 3)))
  (destructuring-bind (toplevel/subtype &optional parameter)
      (split-string type #\;)
    (if parameter
        (destructuring-bind (name value)
            (split-string parameter #\=)
          (list toplevel/subtype (intern (string-left-trim '(#\Space) (string-upcase name))
                                         (find-package :keyword)) value))
        (list toplevel/subtype))))

(defun parse-post-parameters (string)
  (declare (optimize (speed 3)))
  "Parse url-encoded (Content-Type: application/x-www-form-urlencoded) POST data"
  (let ((parameters (remove #\NewLine string)))
    (loop for parameter in (split-string parameters #\&) collect
              (destructuring-bind (name value)
                  (split-string parameter #\=)
                (cons name (url-decode value))))))

(defun parse-get-parameters (uri)
  (declare (optimize (speed 3)))
  "Extract get parameters from URI"
  (destructuring-bind (uri &optional get-data)
      (split-string uri #\?)
    (values
     uri
     (if get-data
         (loop
            for parameter in (split-string get-data #\&)
            for name-value = (split-string parameter #\=)
            collect (destructuring-bind (name value) name-value
                      (cons name value)))))))

(defun parse-request (string)
  "Parse HTTP request"
  (declare (optimize (speed 3))
           (type simple-string string))
  (let ((request-strings (split-string string #\NewLine))
        (request (make-request)))

    (let ((start (car request-strings)))
      (destructuring-bind (method uri version)
          (split-string start)
        (setf (request-method request) method
              (request-uri request) (url-decode uri)
              (request-version request) version)))

    (if (string= *method-get* (request-method request))
        (multiple-value-bind (uri parameters)
            (parse-get-parameters (request-uri request))
          (setf (request-uri request) uri
                (request-parameters request) parameters)))

    (setf (request-headers request)
          (loop
             for header in (cdr request-strings)
             until (string= "" (the string header))
             collect
               (let ((position (position #\: header)))
                 (cons (subseq header 0 position)
                       (subseq header (+ position 2))))))

    (let ((content-length (assoc "Content-Length"
                                 (request-headers request)
                                 :test #'string=)))
      (if content-length
          (setf (request-content-length request)
                (parse-integer (cdr content-length)))))
    request))

(defun parse-request-content (request)
  (declare (optimize (speed 3)))
  (let ((content-type (assoc "Content-Type" (request-headers request)
                             :test #'string=)))
    (if (and content-type (request-content request))
        ;; Content contains URL-encoded POST parameters
        (if (string= "application/x-www-form-urlencoded"
                     (the string (first (parse-content-type (cdr content-type)))))
            (setf (request-parameters request)
                  (parse-post-parameters
                   (octets-to-string-latin-1 (request-content request)))))))
  request)
