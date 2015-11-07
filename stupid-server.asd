(asdf:defsystem :stupid-server
  :version "1.0"
  :depends-on (:cl-who :flexi-streams :iolib :bordeaux-threads)
  :components ((:file "package")
               (:file "definitions")
               (:file "utils")
               (:file "request")
               (:file "response")
               (:file "server")))
