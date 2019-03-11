(asdf:defsystem :stupid-server-tests
  :version "1.0"
  :depends-on (:stupid-server :fiveam)
  :serial t
  :components ((:file "tests/package")
               (:file "tests/tests")))
