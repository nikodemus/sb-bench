(require :sb-bench)

(sb-bench:defbenchmark generic-+ ((x y z) (0 0 0) :repeat 10000)
  (let ((r 42))
    (sb-bench:open-code (500)
      (setf r (+ (+ x x)
                 (+ x y)
                 (+ x z)
                 (+ y y)
                 (+ y z)
                 (+ z z))))
    r))

(print (sb-bench:run-benchmarks))
