(defpackage stupid-server
  (:use #:cl #:cl-who #:bt #:iomux #:sockets)
  (:export #:run-server
           #:stop-server
           #:*dispatch-table*
           #:set-uri-handler
           #:make-static-file-generator
           #:make-page-from-stream-generator
           #:define-page-from-stream-handler))
