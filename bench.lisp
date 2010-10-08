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

;;; FIXME: Perhaps use geomeric mean instead?
(defun sample-statistics (samples)
  "Returns min, mean, max, and standard-error of SAMPLES as multiple
values."
  (declare (type (simple-array fixnum (*)) samples))
  ;; Written this way so that we can get by with just two traversals.
  (let* ((p 1)
         (n (length samples))
         (elt (aref samples 0))
         (min elt)
         (max elt)
         (sum elt))
    (loop while (< p n)
          do (setf elt (aref samples p))
             (cond ((< elt min)
                    (setf min elt))
                   ((> elt max)
                    (setf max elt)))
             (incf sum elt)
             (incf p))
    (setf p 0)
    (let ((mean (/ sum n))
          (var 0))
      (loop while (< p n)
            do (setf elt (aref samples p))
               (incf var (expt (- elt mean) 2))
               (incf p))
      (let* ((variance (/ var n))
             (standard-deviation (sqrt variance)))
        (values min mean max (/ standard-deviation (sqrt n)))))))

(defvar *benchmark-result-hook* 'process-benchmark-results)

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
      (funcall *benchmark-result-hook*
               name runs iterations
               :run-time-us run-time
               :real-time-ms real-time
               :gc-run-time-ms gc-run-time
               :bytes-consed consed))))

(defun process-benchmark-results (name runs iterations &key
                                  run-time-us real-time-ms gc-run-time-ms bytes-consed)
  (labels ((us-to-sec (us)
             (/ us (expt 10.0 6)))
           (ms-to-sec (ms)
             (/ ms (expt 10.0 3)))
           (process-samples (conv samples)
             (multiple-value-bind (min mean max error) (sample-statistics samples)
               (list (funcall conv mean)
                     :min (funcall conv min)
                     :max (funcall conv max)
                     :standard-error (funcall conv error)))))
    (list name
          :runs runs
          :iterations/run iterations
          :run-time (process-samples #'us-to-sec run-time-us)
          :real-time (process-samples #'ms-to-sec real-time-ms)
          :gc-run-time (process-samples #'ms-to-sec gc-run-time-ms)
          :bytes-consed (process-samples #'float bytes-consed))))

(defun read-saved-result (pathname)
   (with-open-file (f pathname :external-format :utf-8)
     (with-standard-io-syntax
       (read f))))

(defun ensure-result-set (result-set)
  (if (consp result-set)
      result-set
      (read-saved-result result-set)))

(defvar *run-counter* 0)

(defun gen-name ()
  (format nil "run-~A" (incf *run-counter*)))

(defun run-benchmarks (&key (runs 10) seconds save baseline (name (gen-name)))
  (let ((results nil)
        (pathname (when save
                    (ensure-directories-exist name))))
    (if baseline
        (dolist (spec (cdr (ensure-result-set baseline)))
          (destructuring-bind (name &key runs iterations/run &allow-other-keys) spec
            (push (run-benchmark name :runs runs :iterations iterations/run) results)))
        (maphash (lambda (name info)
                   (declare (ignore info))
                   (push (run-benchmark name :runs runs :seconds seconds) results))
                 *benchmarks*))
    (push (namestring name) results)
    (when pathname
      (with-simple-restart (continue "Ignore the error and return the results.")
        (with-open-file (f pathname
                           :direction :output
                           :if-exists :supersede
                           :external-format :utf-8)
          (with-standard-io-syntax
            (prin1 results f)
            (terpri f)))))
    results))

(defun collate-results (result-sets)
  (let ((results (make-hash-table)))
    (dolist (set (mapcar #'ensure-result-set result-sets))
      (let ((setname (pop set)))
        (dolist (benchmark set)
          (destructuring-bind
                (name &key runs iterations/run run-time gc-run-time real-time bytes-consed)
              benchmark
            (let ((data (gethash name results))
                  (this (list setname
                              :run-time run-time :gc-run-time gc-run-time
                              :real-time real-time :bytes-consed bytes-consed)))
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
                 (push (list* name (caar data) (nreverse (cdr data))) summary))
               results)
      summary)))

(defun benchmark-error-estimate (benchmark)
  (max
   ;; Measurement error
   (* (/ 1 (sqrt (pop benchmark)))
      (reduce #'max (mapcar (lambda (res)
                              (car (getf (cdr res) :run-time)))
                            (cdr benchmark))))
   ;; Standard error
   (reduce #'max (mapcar (lambda (res)
                           (getf (cdr (getf (cdr res) :run-time)) :standard-error))
                         (cdr benchmark)))))

(defun report (result-sets &key (stream *standard-output*))
  (let* ((result-sets (mapcar #'ensure-result-set result-sets))
         (summary (collate-results result-sets))
         (colsize (+ 2 (reduce #'max (mapcar (lambda (set) (length (car set))) result-sets))))
         (namesize (reduce #'max (mapcar (lambda (benchmark)
                                           (length (prin1-to-string (car benchmark))))
                                         summary))))
    (format stream "~&~vT~{  ~A~^~}~%" namesize (mapcar #'car result-sets))
    (dolist (benchmark summary)
      (let ((name (pop benchmark)))
        (format stream "~vS~:{~v,2F~}  error: ~,2F~%"
                namesize
                name
                (mapcar (lambda (info)
                          (list colsize (car (getf (cdr info) :run-time))))
                        (cdr benchmark))
                (benchmark-error-estimate benchmark))))))
