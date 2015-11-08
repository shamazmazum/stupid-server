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

(define-condition server-error ()
  ((message :initarg :message
            :reader error-message)
   (socket :initarg :socket
           :reader error-socket))
  (:report (lambda (c s)
             (format s "Unhandeled server error: ~a"
                     (error-message c)))))
(define-condition request-error (server-error)
  ())
(define-condition timeout-error (server-error)
  ())
