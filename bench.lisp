;;;; sb-bench.lisp

(in-package #:sb-bench)

(defparameter *benchmarks* (make-hash-table))

(defmacro defbenchmark (name (lambda-list arguments &key (repeat 1))
                        &body body)
  `(progn
     (defun ,name ,lambda-list ,@body)
     (setf (gethash ',name *benchmarks*)
           (list :repeat ,repeat :arguments (list ,@arguments)))))

(defun get-benchmark (name)
  (let ((info (or (gethash name *benchmarks*)
                  (error "Unknown benchmark: ~S" name))))
    (values (fdefinition name) info)))

(defmacro open-code ((n) &body body)
  `(progn
     ,@(loop repeat n
             collect `(progn ,@body))))

(defun run-benchmark (name &key (arguments nil argsp) (repeat nil repeatp))
  (multiple-value-bind (fun info) (get-benchmark name)
    (declare (function fun))
    (let* ((arguments (if argsp arguments (getf info :arguments)))
           (repeat (if repeatp repeat (getf info :repeat)))
           (samples (make-array repeat :element-type 'fixnum))
           (consed (make-array repeat :element-type 'fixnum))
           (run 0))
      (declare (fixnum run repeat))
      (declare (sb-int:truly-dynamic-extent samples consed))
      (flet ((time-run (&key
                        user-run-time-us
                        system-run-time-us
                        bytes-consed
                        aborted
                        &allow-other-keys)
               (declare (fixnum user-run-time-us system-run-time-us))
               (unless aborted
                 (setf (aref samples run)
                       (+ user-run-time-us system-run-time-us))
                 (setf (aref consed run) bytes-consed))))
        (declare (dynamic-extent #'time-run))
        (gc :full t)
        (loop repeat repeat
              do (apply #'call-with-timing #'time-run fun arguments)
              (incf run)))
      (if (> repeat 1)
          (let* ((mean (float (alexandria:mean samples)))
                 (std-deviation (float (alexandria:standard-deviation samples))))
            (list name
                  :run-time mean
                  :standard-deviation std-deviation
                  :bytes-consed (round (alexandria:mean consed))
                  :quality (round (- 100 (* 100.0 (/ std-deviation mean))))))
          (list name (aref samples 0))))))

(defun run-benchmarks ()
  (let (results)
    (maphash (lambda (name info)
               (declare (ignore info))
               (push (run-benchmark name) results))
             *benchmarks*)
    results))

