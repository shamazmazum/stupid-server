(in-package :stupid-server)

(defstruct request
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
                (if (= (length name-value) 2)
                    (destructuring-bind (name value) name-value
                      (push (cons name (url-decode value)) result))))))
    result))

(defun parse-get-parameters (uri)
  "Extract get parameters from URI"
  (destructuring-bind (uri &optional get-data &rest rest)
      (split-string uri #\?)
    (values
     uri
     (if (and get-data
              (not rest))
         (loop
            for parameter in (split-string get-data #\&)
            for name-value = (split-string parameter #\=)
            when (= (length name-value) 2)
            collect (destructuring-bind (name value) name-value
                      (cons name value)))))))

(defun parse-request (string)
  "Parse HTTP request"
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
             until (string= "" header) collect
               (let ((position (position #\: header)))
                 (cons (subseq header 0 position)
                       (subseq header (+ position 2))))))
    request))
