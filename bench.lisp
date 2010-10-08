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

(defun standard-error (samples)
  (/ (standard-deviation samples)
     (sqrt (length samples))))

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
            (run-time-error (standard-error run-time))
            (real-time-mean (mean real-time))
            (real-time-error (standard-error real-time))
            (measurement-error (/ 1 (sqrt runs))))
        (list name
              :runs runs
              :iterations/run iterations
              :run-time
              (let ((sec (/ run-time-mean (expt 10.0 6))))
                (list sec
                      (/ run-time-error (expt 10.0 6))
                      (* sec measurement-error)))
              :real-time
              (let ((sec (/ real-time-mean 1000.0)))
                (list sec
                      (/ real-time-error 1000.0)
                      (* sec measurement-error)))
              :gc-run-time
              (let ((sec (/ (mean gc-run-time) 1000.0)))
                (list sec
                      (/ (standard-error gc-run-time) 1000.0)
                      (* sec measurement-error)))
              :bytes-consed
              (let ((bytes (round (mean consed))))
                (list bytes
                      (float (standard-error consed))
                      (* bytes measurement-error)))
              :quality
              (round
               (max 0
                    (min (- 100 (* 100 measurement-error))
                         (if (plusp run-time-mean)
                             (- 100 (* 100 (/ run-time-error run-time-mean)))
                             0)
                         (if (plusp real-time-mean)
                             (- 100 (* 100 (/ real-time-error real-time-mean)))
                             0)))))))))

(defun read-saved-result (pathname)
   (with-open-file (f pathname :external-format :utf-8)
     (with-standard-io-syntax
       (read f))))

(defun run-benchmarks (&key (runs 10) seconds save-as baseline)
  (let ((results nil)
        (pathname (when save-as
                    (ensure-directories-exist save-as))))
    (if baseline
        (dolist (spec (cdr (read-saved-result baseline)))
          (destructuring-bind (name &key runs iterations/run &allow-other-keys) spec
            (push (run-benchmark name :runs runs :iterations iterations/run) results)))
        (maphash (lambda (name info)
                   (declare (ignore info))
                   (push (run-benchmark name :runs runs :seconds seconds) results))
                 *benchmarks*))
    (when pathname
      (with-simple-restart (continue "Ignore the error and return the results.")
        (with-open-file (f pathname
                           :direction :output
                           :if-exists :supersede
                           :external-format :utf-8)
          (with-standard-io-syntax
            (prin1 (cons (namestring save-as) results) f)
            (terpri f)))))
    results))

(defun ensure-result-set (result-set)
  (if (consp result-set)
      result-set
      (read-saved-result result-set)))

(defun collate-results (result-sets &key (min-quality 90))
  (let ((results (make-hash-table)))
    (dolist (set (mapcar #'ensure-result-set result-sets))
      (let ((setname (pop set)))
        (dolist (benchmark set)
          (destructuring-bind
                (name &key runs iterations/run run-time gc-run-time real-time bytes-consed
                      quality)
              benchmark
            (when (and min-quality (< quality min-quality))
              (cerror "Let it pass." "Result from ~A below required minimum quality:~%  ~S"
                      setname benchmark))
            (let ((data (gethash name results))
                  (this (list setname
                              :run-time run-time :gc-run-time gc-run-time
                              :real-time real-time :bytes-consed bytes-consed
                              :quality quality)))
              (if data
                  (let ((spec (car data)))
                    (unless (and (eql (car spec) runs)
                                 (eql (cdr spec) iterations/run))
                      (error "Result set ~A is incompatible with others." setname))
                    (push this (cdr data)))
                  (setf (gethash name results)
                        (cons (cons runs iterations/run)
                              (list this)))))))))
    (let (summary)
      (maphash (lambda (name data)
                 (push (cons name (nreverse (cdr data))) summary))
               results)
      summary)))

(defun report (result-sets &key (min-quality 90)
               (stream *standard-output*))
  (let* ((result-sets (mapcar #'ensure-result-set result-sets))
         (summary (collate-results result-sets :min-quality min-quality))
         (colsize (+ 2 (reduce #'max (mapcar (lambda (set) (length (car set))) result-sets))))
         (namesize (reduce #'max (mapcar (lambda (benchmark)
                                           (length (prin1-to-string (car benchmark))))
                                         summary))))
    (format stream "~&~vT~{  ~A~^~}~%" namesize (mapcar #'car result-sets))
    (dolist (benchmark summary)
      (let ((name (pop benchmark)))
        (format stream "~vS~:{~v,2F~}~%"
                namesize
                name
                (mapcar (lambda (info)
                          (list colsize (car (getf (cdr info) :run-time))))
                        benchmark))))))