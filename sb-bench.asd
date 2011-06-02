;;;; sb-bench.asd

(asdf:defsystem #:sb-bench
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "dataset")
               (:file "statistics")
               (:file "bench")))
