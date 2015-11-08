(in-package :stupid-server)

(defstruct request
  socket ; KLUDGE: for error handling
  (method  "" :type string)
  (uri     "" :type string)
  (version "" :type string)
  headers
  parameters
  content)

(defun parse-content-type (type)
  "Parse any HTTP header value (but used here to parse Content-Type)"
  (handler-case
      (destructuring-bind (toplevel/subtype &optional parameter)
          (split-string type #\;)
        (if parameter
            (destructuring-bind (name value)
                (split-string parameter #\=)
              (list toplevel/subtype (intern (string-left-trim '(#\Space) (string-upcase name))
                                             (find-package :keyword)) value))
            (list toplevel/subtype)))
    (error () (list type))))

(defun parse-post-parameters (string)
  "Parse url-encoded (Content-Type: application/x-www-form-urlencoded) POST data"
  (let (result)
    (loop
       for line in (split-string string #\NewLine)
       do
         (loop for parameter in (split-string line #\&) do
              (let ((name-value (split-string parameter #\=)))
                (handler-case
                    (destructuring-bind (name value) name-value
                      (push (cons name (url-decode value)) result))
                  (error () (cerror "Continue work" 'request-error
                                    :message "Cannot parse POST data"))))))
    result))

(defun parse-get-parameters (uri)
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

(defun parse-request (string &key socket)
  "Parse HTTP request"
  (let ((request-strings (split-string string #\NewLine))
        (request (make-request :socket socket)))

    (handler-case
        (let ((start (car request-strings)))
          (destructuring-bind (method uri version)
              (split-string start)
            (setf (request-method request) method
                  (request-uri request) (url-decode uri)
                  (request-version request) version)))
      (error () (error 'request-error
                       :message "fatal: cannot parse request"
                       :socket socket))) ; Resignal error as parse error

    (if (string= *method-get* (request-method request))
        (handler-case
            (multiple-value-bind (uri parameters)
                (parse-get-parameters (request-uri request))
              (setf (request-uri request) uri
                    (request-parameters request) parameters))
          (error () ())))

    (loop
       for header in (cdr request-strings)
       until (string= "" header)
       do
         (handler-case
             (let ((position (position #\: header)))
               (push
                (cons (subseq header 0 position)
                      (subseq header (+ position 2)))
                (request-headers request)))
           (error () (cerror "Continue work" 'request-error
                             :message "Cannot parse HTTP header"))))
    request))
