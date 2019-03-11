(defpackage stupid-server
  (:use #:cl #:cl-who #:bt #:iomux #:sockets)
  (:shadow #:server-error #:error-message #:error-socket)
  (:export #:run-server
           #:stop-server
           #:*dispatch-table*
           #:set-uri-handler
           #:make-static-file-generator
           #:make-page-from-stream-generator
           #:make-post-request-handler
           #:install-static-directory-handler
           #:define-page-from-stream-handler
           #:define-post-request-handler))
