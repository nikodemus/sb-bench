(in-package :sb-bench)

(defun format-universal-time ()
  (multiple-value-bind (sec min hour date month year) (get-decoded-time)
    (format nil "~D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year
            month
            date
            hour
            min
            sec)))

(deftype sample-vector ()
  `(simple-array (unsigned-byte 32) (*)))

(defun make-sample-vector (size)
  (declare (fixnum size))
  (make-array size :element-type '(unsigned-byte 32)))

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
