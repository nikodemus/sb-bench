;;;; sb-bench.asd

(asdf:defsystem #:sb-bench
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "bench")))
