Stupid server
=========

Hi! This is an HTTP server written in Common Lisp.  It's very simple and stupid. It was meant as a joke, you should never use it. It lacks even basic functionality:

* It does not understand `keep-alive` connections.
* It does not process `multipart/form-data` POST requests. Only `application/x-www-form-urlencoded` is supported.
* Does not send `Last-Modified`.

Comparably to hunchentoot, it sucks. But if you serve small static home webpages with it, you can probably beat hunchentoot. Also it does not use threads (OK, it uses two, but it's not crucial) and does non-blocking IO with [iolib](http://github.com/sionescu/iolib).

You can run server with `(stupid-server:run-server)` and stop it with `(stupid-server:stop-server)`. The server listens port 8080 on localhost. There is no way you can change this behaviour but to edit the code. I know it's stupid.

You now must  setup URI handlers. This can be done so for static pages/files:

```
(stupid-server:set-uri-handler
 "/test.png" (stupid-server:make-static-file-generator "~/testserver/test.png"))
```

You also can generate pages on server side on the fly (a stupid idea of Edi Weitz)

```
(stupid-server:define-page-from-stream-handler
    ("/" out) ((ua "User-Agent" :header)
               (name "name" :parameter))
    (cl-who:with-html-output (out)
      (cl-who:htm
       (:html
        (:head (:title "Hello"))
        (:body
         (:p (cl-who:fmt "Hello world ~a(~a)" name ua)))))))
```

That's all folks!

*Stupid server™: powered by author's stupidity*
