;;;; sb-bench.asd

(asdf:defsystem #:sb-bench
  :serial t
  :components ((:file "package")
               (:file "statistics")
               (:file "dataset")
               (:file "bench")))
