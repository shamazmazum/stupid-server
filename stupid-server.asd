(asdf:defsystem :stupid-server
  :version "1.0"
  :depends-on (:cl-who :flexi-streams :iolib :bordeaux-threads)
  :components ((:file "src/package")
               (:file "src/definitions")
               (:file "src/utils")
               (:file "src/request")
               (:file "src/response")
               (:file "src/server"))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (asdf:load-system :stupid-server-tests)
                    (funcall
                     (intern "RUN-TESTS" (find-package "STUPID-SERVER-TESTS")))))
