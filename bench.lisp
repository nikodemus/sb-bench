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
                 (return (max 1
                              (* iterations (round (* internal-time-units-per-second
                                                      seconds)
                                                   run-time)))))))))))

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
           (run-time (make-array runs :element-type 'fixnum))
           (gc-run-time (make-array runs :element-type 'fixnum))
           (real-time (make-array runs :element-type 'fixnum))
           (consed (make-array runs :element-type 'fixnum))
           (run 0))
      (declare (fixnum run runs))
      ;; TRULY-DYNAMIC-EXTENT because the compiler cannot prove that sample vectors will
      ;; fit on one page each.
      (declare (sb-int:truly-dynamic-extent
                run-time gc-run-time real-time consed run-arguments))
      (unless (typep iterations `(integer 1 ,most-positive-fixnum))
        (error "Number of iterations must be a positive fixnum."))
      (unless (typep runs `(integer 1 ,most-positive-fixnum))
        (error "Number of runs must be a positive fixnum."))
      (flet ((time-run (&key
                        user-run-time-us
                        system-run-time-us
                        gc-run-time-ms
                        real-time-ms
                        bytes-consed
                        &allow-other-keys)
               (declare (fixnum user-run-time-us system-run-time-us))
               (setf (aref run-time run) (+ user-run-time-us system-run-time-us)
                     (aref gc-run-time run) gc-run-time-ms
                     (aref real-time run) real-time-ms
                     (aref consed run) bytes-consed)))
        (declare (dynamic-extent #'time-run))
        (gc :full t)
        (loop repeat runs
              do (apply #'call-with-timing #'time-run fun run-arguments)
              (incf run)))
      (let ((run-time-mean (mean run-time))
            (run-time-sdev (standard-deviation run-time))
            (real-time-mean (mean real-time))
            (real-time-sdev (standard-deviation real-time)))
        (list name
             :runs runs
             :iterations/run iterations
             :run-time
             (list (/ run-time-mean (expt 10.0 6))
                   (/ run-time-sdev (expt 10.0 6)))
             :gc-run-time
             (list (/ (mean gc-run-time) 1000.0)
                   (/ (standard-deviation gc-run-time) 1000.0))
             :real-time
             (list (/ real-time-mean 1000.0)
                   (/ real-time-sdev 1000.0))
             :bytes-consed
             (list (round (mean consed))
                   (float (standard-deviation consed)))
             :quality (when (>= runs 10)
                        (if (or (zerop run-time-mean) (zerop real-time-mean))
                            0
                            (let ((run-time-q (/ run-time-sdev run-time-mean))
                                  (real-time-q (/ real-time-sdev real-time-mean)))
                              (max 0
                                  (round
                                   (- 100 (* 50.0 (+ run-time-q real-time-q)))))))))))))

(defun run-benchmarks (&key (runs 10) seconds)
  (let (results)
    (maphash (lambda (name info)
               (declare (ignore info))
               (push (run-benchmark name :runs runs :seconds seconds) results))
             *benchmarks*)
    results))

