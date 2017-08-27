(in-package :stupid-server)

;; Methods
(defparameter *method-get* "GET")
(defparameter *method-post* "POST")
(defparameter *method-head* "HEAD")

;; Status codes
(defparameter *http-ok* '(200 . "OK"))
(defparameter *http-not-found* '(404 . "Not Found"))
(defparameter *http-found* '(302 . "Found"))
(defparameter *http-not-implemented* '(501 . "Not Implemented"))
(defparameter *http-not-modified* '(304 . "Not Modified"))

;; Versions
(defparameter *http/1.0* "HTTP/1.0") ; the only version supported

(defvar *event-base*)
(defvar *log-stream*)
(defvar *server-thread* nil)
(defvar *status-code-page-generators* (make-hash-table))
(defvar *dispatch-table* nil
  "Server URI-handler dispatch table")

(define-condition stupid-server::server-error (simple-error)
  ((socket :initarg :socket
           :reader stupid-server::error-socket))
  (:report (lambda (c s)
             (apply #'format s
                    (concatenate 'string "Server-error: " (simple-condition-format-control c))
                    (simple-condition-format-arguments c)))))

(defun stupid-server::server-error (socket format-string &rest format-arguments)
  (error 'server-error
         :socket socket
         :format-control format-string
         :format-arguments format-arguments))

(defun stupid-server::server-cerror (socket continue-string format-string &rest format-arguments)
  (cerror  continue-string
           'server-error
           :socket socket
           :format-control format-string
           :format-arguments format-arguments))
