(in-package :stupid-server)

(defun disconnect-socket (socket)
  "Disconnect a socket and remove event handlers"
  (handler-bind
      ((warning #'(lambda (c) (declare (ignore c)) (invoke-restart 'muffle-warning))))
    (remove-fd-handlers *event-base*
                        (socket-os-fd socket)))
  (close socket))

(defmacro with-cleanup ((socket &optional fd) &body body)
  `(restart-case
       (progn ,@body)
     (server-cleanup ()
       (disconnect-socket ,socket)
       (if ,fd (sb-posix:close ,fd)))))

(defun send-response (socket response)
  "Send a response to client"
  (declare (optimize (speed 3))
           (type socket socket)
           (type response response))
  (let ((position 0)
        (sending-buffer t)
        (sending-stream nil)
        (file-fd (if (response-file-pathname response)
                     (sb-posix:open (response-file-pathname response)
                                    sb-posix:o-rdonly))))
    (declare (type non-negative-fixnum position))
    (labels ((send-handler (fd event error)
             (declare (ignore fd event))
             (with-cleanup (socket file-fd)
               (if (eql :timeout error)
                   (server-error socket "socket ~a timed out" socket))
               (when sending-buffer
                 (incf position (the fixnum
                                     (send-to socket (response-buffer response)
                                              :start position)))
                 (when (= position (length (response-buffer response)))
                   (setq sending-buffer nil)
                   (when file-fd
                     (setq sending-stream t
                           position 0))))

               (when sending-stream
                 (incf position (isys::sendfile (socket-os-fd socket)
                                                file-fd position
                                                (-
                                                 (response-file-length response)
                                                 position)))
                 (when (= position (response-file-length response))
                   (setq sending-stream nil)))

               (when (not (or sending-buffer sending-stream))
                 (finish-output socket)
                 (if file-fd (sb-posix:close file-fd))
                 (disconnect-socket socket)))))

      (set-io-handler *event-base*
                      (socket-os-fd socket)
                      :write #'send-handler
                      :timeout 10))))

(defun socket-receive (socket buffer &key start)
  (multiple-value-bind (buffer copied)
      (receive-from socket :buffer buffer :start start)
    (declare (ignore buffer))
    copied))

(defun read-request (socket)
  "Read request from client and send response"
  (let ((buffer (make-array 4096 :initial-element 0))
        (position 0)
        (content-start 0)
        (content-length 0)
        (reading-headers t)
        (reading-content nil)
        request)
    (flet ((request-handler (fd event error)
             (declare (ignore fd event))
             (with-cleanup (socket)
               (if (eql error :timeout)
                   (server-error socket "socket ~a timed out" socket))

               (when reading-headers
                 (incf position (socket-receive socket buffer :start position))
                 (let ((content-position (search #(13 10 13 10) buffer)))
                   (when content-position
                     (setq request (parse-request
                                    (octets-to-string-latin-1
                                     (subseq buffer 0 (+ content-position 4))))
                           reading-headers nil)
                     (when (string= (request-method request) *method-post*)
                       (let ((length (assoc "Content-Length" (request-headers request)
                                            :test #'string=)))
                         (if (not length)
                             (server-error socket "No Content-Length"))
                         (setq reading-content t
                               content-length (parse-integer (cdr length))
                               content-start (+ 4 content-position)))))))

               (when reading-content
                 (cond
                   ((= position (+ content-start content-length))
                    (setf (request-content request)
                          (subseq buffer content-start position)
                          reading-content nil))
                   (t (incf position (socket-receive socket buffer :start position))))))

               (when (not (or reading-headers reading-content))
                 (remove-fd-handlers *event-base*
                                     (socket-os-fd socket))
                 (if *log-stream*
                     (format *log-stream* "Requested ~a by '~a'~%"
                             (request-uri request)
                             (let ((ua (assoc "User-Agent" (request-headers request)
                                              :test #'string=)))
                               (if ua (cdr ua)))))
                 (if (and
                      (request-content request)
                      (let ((content-type (assoc "Content-Type" (request-headers request)
                                                 :test #'string=)))
                        (if content-type
                            (string= "application/x-www-form-urlencoded"
                                     (first (parse-content-type (cdr content-type)))))))
                     (setf (request-parameters request)
                           (parse-post-parameters
                            (octets-to-string-latin-1 (request-content request)))))

                 (send-response
                  socket
                  (compose-response
                   (process-request request))))))

      (set-io-handler *event-base*
                      (socket-os-fd socket)
                      :read #'request-handler
                      :timeout 10))))

;; Server operation

(defun log-error (c)
  "Log error if possible"
  (when *log-stream*
    (princ c *log-stream*)
    (terpri *log-stream*)))

(defun server-handle-error ()
  (if (find-restart 'server-send-error)
      (invoke-restart 'server-send-error))
  (if (find-restart 'server-cleanup)
      (invoke-restart 'server-cleanup)))

(defun run-server (&optional log-stream)
  "Run server. LOG-STREAM may be optional character output stream where log is stored"
  (if *server-thread*
      (error "Server is already running"))
  (setq *server-thread*
        (make-thread
         (lambda ()
           (let ((listener (make-socket :address-family :internet
                                        :type :stream
                                        :connect :passive
                                        :local-host #(127 0 0 1)
                                        :local-port 8080)))
             (flet ((listener-handler (fd event error)
                      (declare (ignore fd event))
                      (block nil
                        (if (eql :timeout error)
                            (return))
                        (let ((client (accept-connection listener)))
                          (when client
                            (if *log-stream*
                                (format *log-stream* "Accepted connection ~a~%" client))
                            (read-request client))))))
               (set-io-handler *event-base*
                               (socket-os-fd listener)
                               :read #'listener-handler
                               :timeout 15))
             (let ((*log-stream* log-stream))
               (handler-bind
                   ((error
                     (lambda (c)
                       (log-error c)
                       (server-handle-error))))
                 (event-dispatch *event-base*)
                 (disconnect-socket listener)))))
         :name "Event thread")))

(defun stop-server ()
  "Stop server"
  (let ((thread *server-thread*))
    (when thread
      (exit-event-loop *event-base*)
      (join-thread thread)
      (setq *server-thread* nil))))
