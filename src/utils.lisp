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

#+nil
(defun split-string (string &optional (split-char #\Space))
  "Split STRING into list of strings. SPLIT-CHAR is the delimiter and
defaults to #\Space"
  (declare (optimize (speed 3))
           (type string string)
           (type character split-char))
  (loop
     with old-pos = 0
     for pos = (position split-char string :test #'char= :start old-pos)
     collect (subseq string old-pos pos) into res
     while pos
     do (setq old-pos (1+ pos))
     finally (return res)))

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

(defun url-decode (string)
  (declare (optimize (speed 3))
           (type string string))
  (apply
   #'concatenate 'string
   (loop
      with old-pos = 0
      for pos = (position #\% string :start old-pos)
      collect (subseq string old-pos pos) into res
      if pos collect
        (flexi-streams:octets-to-string
         (loop
            while (and (< pos (length string)) (char= #\% (char string pos)))
            collect (parse-integer (subseq string (1+ pos) (+ pos 3)) :radix 16)
            do (incf pos 3))
         :external-format '(:utf-8))
      into res
      while pos
      do (setq old-pos pos)
      finally (return res))))

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
