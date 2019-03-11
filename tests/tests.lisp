(in-package :stupid-server-tests)

(defparameter *request-1*
  "GET / HTTP/1.1
Host: localhost:8080
Connection: keep-alive
Upgrade-Insecure-Requests: 1
User-Agent: Mozilla/5.0 (X11; FreeBSD amd64; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Iridium/2018.5 Safari/537.36 Chrome/67.0.3396.40
DNT: 1
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
Accept-Encoding: gzip, deflate, br
Accept-Language: ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7

")

(defparameter *request-2*
  "POST /postmsg HTTP/1.1
Host: localhost:8080
Connection: keep-alive
Content-Length: 21
Cache-Control: max-age=0
Origin: http://localhost:8080
Upgrade-Insecure-Requests: 1
DNT: 1
Content-Type: application/x-www-form-urlencoded
User-Agent: Mozilla/5.0 (X11; FreeBSD amd64; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Iridium/2018.5 Safari/537.36 Chrome/67.0.3396.40
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8
Referer: http://localhost:8080/
Accept-Encoding: gzip, deflate, br
Accept-Language: ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7

msg=test&captcha=1+15")

(defun run-tests ()
  "Run all tests"
  (run! 'request-tests)
  (run! 'response-tests))

(def-suite request-tests :description "Test request parsing functions")
(def-suite response-tests :description "Test response generation")

(in-suite request-tests)
(test check-content-type-parsing
  "Check parsing of content type"
  (let ((content-type
         (stupid-server::parse-content-type "text/plain")))
    (is (string= "text/plain" (car content-type))))
  (let ((content-type
         (stupid-server::parse-content-type "text/plain;charset=utf-8")))
    (is (string= "text/plain" (car content-type)))
    (is (string= (getf (cdr content-type) :charset) "utf-8")))
  ;; Charset is separated by space
  (let ((content-type
         (stupid-server::parse-content-type "text/plain; charset=utf-8")))
    (is (string= "text/plain" (car content-type)))
    (is (string= (getf (cdr content-type) :charset) "utf-8"))))

(test check-post-content-parsing
  "Check parsing of URL-encoded POST contnet"
  (let ((content (stupid-server::parse-post-parameters
                  "msg=test&captcha=3+%D0%A1%D0%B5%D1%80%D0%B3%D0%B5%D0%B5%D0%B2%D0%B8%D1%87")))
    (is (equalp content
                '(("msg" . "test") ("captcha" . "3 Сергеевич"))))))

(test check-get-parameters-parsing
  "Check parsing of URL-encoded GET parameters"
  (multiple-value-bind (url parameters)
      (stupid-server::parse-get-parameters
       "http://example.com?msg=test&captcha=3+%D0%A1%D0%B5%D1%80%D0%B3%D0%B5%D0%B5%D0%B2%D0%B8%D1%87")
    (is (string= "http://example.com" url)
        (equalp parameters
                '(("msg" . "test") ("captcha" . "3 Сергеевич"))))))

(test check-parse-request
  "Check parsing of entire HTTP request"
  ;; Check GET request
  (let ((request (stupid-server::parse-request *request-1*)))
    (is (string= "GET" (stupid-server::request-method request)))
    (is (string= "/" (stupid-server::request-uri request)))
    (is (string= "localhost:8080" (cdr (assoc "Host"
                                              (stupid-server::request-headers request)
                                              :test #'string=)))))

  ;; Check POST request
  (let ((request (stupid-server::parse-request *request-2*)))
    (is (string= "POST" (stupid-server::request-method request)))
    (is (string= "/postmsg" (stupid-server::request-uri request)))
    (setf (stupid-server::request-content request)
          (stupid-server::string-to-octets-latin-1
           (subseq *request-2* (- (length *request-2*)
                                  (stupid-server::request-content-length request)))))
    (stupid-server::parse-request-content request)
    (is (string= (cdr (assoc "msg" (stupid-server::request-parameters request) :test #'string=))
                 "test"))))

(in-suite response-tests)

(test check-content-type-generation
  "Check generation of content type"
  (is (string= (stupid-server::construct-content-type '("text/html"))
               "text/html"))
  (is (string= (stupid-server::construct-content-type '("text/html" :charset "utf-8"))
               "text/html; charset=utf-8")))

(test check-compose-response
  "Compose 404 Not Found response"
  (let ((response (stupid-server::compose-response
                   (funcall
                    (stupid-server::get-status-code-page-generator stupid-server::*http-ok*)
                    (stupid-server::make-request)))))
    (is (stupid-server::response-buffer response))))
