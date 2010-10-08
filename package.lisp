;;;; package.lisp

(defpackage #:sb-bench
  (:use #:cl :sb-int :sb-ext)
  (:shadow #:format-universal-time)
  (:export
   #:run-benchmarks
   #:run-benchmark
   #:defbenchmark
   #:open-code
   #:report))
