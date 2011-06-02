(in-package :sb-bench)

(deftype sample-vector ()
  `(simple-array (unsigned-byte 32) (*)))

(defun make-sample-vector (size)
  (declare (fixnum size))
  (make-array size :element-type '(unsigned-byte 32)))

(defclass dataset ()
  ((benchmark-name
    :initarg :benchmark-name
    :reader benchmark-name)
   (benchmark-runs
    :initarg :benchmark-runs
    :reader benchmark-runs)
   (benchmark-iterations/run
    :initarg :benchmark-iterations/run
    :reader benchmark-iterations/run)
   (comment
    :initarg :comment
    :initform (format-universal-time)
    :reader comment)
   (run-time-us-samples
    :initarg :run-time-us-samples
    :reader run-time-us-samples)
   (real-time-ms-samples
    :initarg :real-time-ms-samples
    :reader real-time-ms-samples)
   (gc-run-time-ms-samples
    :initarg :gc-run-time-ms-samples
    :reader gc-run-time-ms-samples)
   (bytes-consed-samples
    :initarg :bytes-consed-samples
    :reader bytes-consed-samples)
   (cache
    :initform nil
    :accessor cache)))

(defmethod print-object ((dataset dataset) stream)
  (print-unreadable-object (dataset stream)
    (format stream "Dataset ~S ~A [~/sb-impl::format-microseconds/]"
            (benchmark-name dataset)
            (comment dataset)
            (round (mean-of (statistics dataset :run-time-us))))))

(defun write-sample-vector (vector stream)
  (declare (type sample-vector vector))
  (write-byte (length vector) stream)
  (write-sequence vector stream))

(defun read-sample-vector (stream)
  (let ((vector (make-array (read-byte stream) :element-type '(unsigned-byte 32))))
    (read-sequence vector stream)
    vector))

(defun write-packed-string (string stream)
  (let* ((octets (string-to-octets string :external-format :utf-8))
         (n (length octets))
         (nullp nil))
    (dotimes (i (ceiling n 4))
      (let ((ub32 0))
        (dotimes (j 4)
          (setf (ldb (byte 8 (* 8 j)) ub32)
                (let ((k (+ j (* i 4))))
                  (if (< k n)
                      (aref octets k)
                      (setf nullp 0)))))
        (write-byte ub32 stream)))
    (unless nullp
      (write-byte 0 stream))))

(defun read-packed-string (stream)
  (let ((octets (make-array 128 :fill-pointer 0 :element-type '(unsigned-byte 8)))
        (end 0))
    (loop named unpack
          for ub32 = (read-byte stream)
          until (zerop ub32)
          do (dotimes (i 4)
               (let ((ub8 (ldb (byte 8 (* 8 i)) ub32)))
                 (if (zerop ub8)
                     (return-from unpack)
                     (vector-push-extend ub8 octets))
                 (incf end))))
    (octets-to-string octets :external-format :utf-8 :end end)))

(defun write-symbol (symbol stream)
  (write-packed-string (package-name (symbol-package symbol)) stream)
  (write-packed-string (symbol-name symbol) stream))

(defun read-symbol (stream)
  (let ((package (read-packed-string stream)))
    (or (find-package package) (make-package package))
    (intern (read-packed-string stream) package)))

(defparameter *dataset-file-header* (format nil ";; SB-BENCH dataset file v1~%"))

(defun save-dataset (dataset pathname)
  (with-open-file (f pathname
                     :element-type '(unsigned-byte 32)
                     :direction :output
                     :if-exists :supersede)
    (write-packed-string *dataset-file-header* f)
    (write-packed-string (comment dataset) f)
    (write-symbol (benchmark-name dataset) f)
    (write-byte (benchmark-runs dataset) f)
    (write-byte (benchmark-iterations/run dataset) f)
    (write-sample-vector (run-time-us-samples dataset) f)
    (write-sample-vector (real-time-ms-samples dataset) f)
    (write-sample-vector (gc-run-time-ms-samples dataset) f)
    (write-sample-vector (bytes-consed-samples dataset) f)
    dataset))

(defun load-dataset (pathname)
  (with-open-file (f pathname
                     :element-type '(unsigned-byte 32)
                     :direction :input)
    (unless (equal *dataset-file-header* (read-packed-string f))
      (error "~A is not an SB-BENCH dataset v1 file." pathname))
    (make-instance 'dataset
                   :comment (read-packed-string f)
                   :benchmark-name (read-symbol f)
                   :benchmark-runs (read-byte f)
                   :benchmark-iterations/run (read-byte f)
                   :run-time-us-samples (read-sample-vector f)
                   :real-time-ms-samples (read-sample-vector f)
                   :gc-run-time-ms-samples (read-sample-vector f)
                   :bytes-consed-samples (read-sample-vector f))))
