(in-package :sb-bench)

(defclass statistics ()
  ((mean :initarg :mean :reader mean-of)
   (min :initarg :min :reader min-of)
   (max :initarg :max :reader max-of)
   (standard-error :initarg :standard-error :reader standard-error-of)))

;;; FIXME: Perhaps use geomeric mean instead?
(defun sample-statistics (samples)
  (declare (sample-vector samples))
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
        (make-instance 'statistics
                       :min min
                       :mean mean
                       :max max
                       :standard-error (/ standard-deviation (sqrt n)))))))

(defun statistics (dataset type)
  (or (getf (cache dataset) type)
      (setf (getf (cache dataset) type)
            (sample-statistics
             (ecase type
               (:run-time-us (run-time-us-samples dataset))
               (:real-time-ms (real-time-ms-samples dataset))
               (:gc-run-time-ms (gc-run-time-ms-samples dataset))
               (:bytes-consed (bytes-consed-samples dataset)))))))
