;;;; sb-bench.lisp

(in-package #:sb-bench)

(defparameter *benchmarks* (make-hash-table))

(defmacro defbenchmark (name (lambda-list arguments &key (seconds 1.0))
                        &body body)
  (with-unique-names (iterations res)
    `(progn
       (defun ,name (,iterations ,@lambda-list)
         (declare (fixnum ,iterations))
         (let (,res)
           (loop repeat ,iterations
                 do (setf ,res (progn ,@body)))
           ,res))
       (setf (gethash ',name *benchmarks*)
             (list :seconds ,seconds :arguments (list ,@arguments))))))

(defun get-benchmark (name)
  (let ((info (or (gethash name *benchmarks*)
                  (error "Unknown benchmark: ~S" name))))
    (values (fdefinition name) info)))

(defmacro open-code ((n) &body body)
  `(progn
     ,@(loop repeat n
             collect `(progn ,@body))))

(defun estimate-iterations-for-run-time (fun arguments seconds)
  (declare (function fun))
  (let ((iterations 1))
    (declare (fixnum iterations))
    (loop
      (let ((trial-arguments (cons iterations arguments))
            (start (get-internal-run-time)))
        (declare (dynamic-extent trial-arguments))
        (apply fun trial-arguments)
        (let ((run-time (- (get-internal-run-time) start)))
          (cond ((> 10 run-time)
                 (setf iterations (* iterations 10)))
                (t
                 (return (* iterations (round (* internal-time-units-per-second seconds)
                                              run-time))))))))))

(defun mean (samples)
  (declare (type (simple-array fixnum (*)) samples))
  (/ (loop for elt across samples summing elt) (length samples)))

(defun variance (samples)
  (declare (type (simple-array fixnum (*)) samples))
  (let ((mean (mean samples)))
    (/ (reduce (lambda (a b)
                 (+ a (expt (- b mean) 2)))
               samples
               :initial-value 0)
       (length samples))))

(defun standard-deviation (samples)
  (sqrt (variance samples)))

(defun run-benchmark (name &key (arguments t) seconds runs iterations)
  (multiple-value-bind (fun info) (get-benchmark name)
    (declare (function fun))
    (let* ((arguments (if (eq t arguments) (getf info :arguments) arguments))
           (seconds (or seconds (getf info :seconds)))
           (runs (or runs 1))
           (iterations (or iterations
                           (estimate-iterations-for-run-time fun arguments seconds)))
           (run-arguments (cons iterations arguments))
           (samples (make-array runs :element-type 'fixnum))
           (consed (make-array runs :element-type 'fixnum))
           (run 0))
      (declare (fixnum run runs))
      (declare (sb-int:truly-dynamic-extent samples consed run-arguments))
      (flet ((time-run (&key
                        user-run-time-us
                        system-run-time-us
                        bytes-consed
                        &allow-other-keys)
               (declare (fixnum user-run-time-us system-run-time-us))
               (setf (aref samples run) (+ user-run-time-us system-run-time-us)
                     (aref consed run) bytes-consed)))
        (declare (dynamic-extent #'time-run))
        (gc :full t)
        (loop repeat runs
              do (apply #'call-with-timing #'time-run fun run-arguments)
                 (incf run)))
      (let* ((mean (mean samples))
             (std-deviation (standard-deviation samples)))
        (list name
              :runs runs
              :iterations/run iterations
              :mean-run-time (/ mean 1000000.0)
              :standard-deviation (/ std-deviation 1000000.0)
              :bytes-consed (round (mean consed))
              :quality (when (>= runs 10)
                         (round (- 100 (* 100.0 (/ std-deviation mean))))))))))

(defun run-benchmarks (&key (runs 10) seconds)
  (let (results)
    (maphash (lambda (name info)
               (declare (ignore info))
               (push (run-benchmark name :runs runs :seconds seconds) results))
             *benchmarks*)
    results))

