(in-package :stupid-server)

(defmacro with-http-output ((stream) &body body)
  (let ((sequence-stream (gensym)))
    `(flexi-streams:with-output-to-sequence (,sequence-stream)
       (let ((,stream (flexi-streams:make-flexi-stream
                       ,sequence-stream
                       :external-format '(:utf-8 :eol-style :crlf))))
         ,@body))))

(defun load-content (filename)
  "Returns simple-vector with content of a file"
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length in))))
      (read-sequence buffer in)
      buffer)))

(defun split-string (string &optional (split-char #\Space) result end)
  "Split STRING into list of strings. SPLIT-CHAR is the delimiter and
defaults to #\Space"
  (declare (optimize (speed 3))
           (type string string)
           (type character split-char))
  (let ((position (position split-char string :from-end t :end end)))
    (if position
        (split-string string
                      split-char
                      (push (subseq string (1+ position) end) result)
                      position)
        (push (subseq string 0 end) result))))

;; Ugly and sometimes slow
(defun url-decode (string)
  "Decode URL(percent)-encoded string"
  (let ((start 0)
        result)
    (setq string (substitute #\Space #\+ string))
    (block nil
      (tagbody :loop
         (let ((position (position #\% string :start start)))
           (cond
             (position
              (setq result (cons (subseq string start position) result)
                    start position))
             (t (return
                  (apply #'concatenate 'string
                         (reverse (cons (subseq string start) result)))))))
         (let ((encoded
                (loop
                   while (and
                          (/= start (length string))
                          (char= #\% (char string start)))
                   for code = (parse-integer
                               (subseq string (+ start 1) (+ start 3))
                               :radix 16)
                   do (incf start 3)
                   collect code)))
           (push (flexi-streams:octets-to-string encoded
                                                 :external-format '(:utf-8))
                 result))
         (go :loop)))))

(defun octets-to-string-latin-1 (buffer)
  "Fast function to translate simple-vector of char codes to string"
  (declare (optimize (speed 3))
           (type simple-vector buffer))
  (map 'string #'code-char
  #+(or linux unix)
  (remove 13 buffer)
  #-(or linux unix)
  buffer))

(defun string-to-octets-latin-1 (string)
  "Fast function to translate string to simple-vector of char codes"
  (declare (optimize (speed 3))
           (type simple-string string))
  (map 'vector #'char-code string))
