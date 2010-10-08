(in-package :sb-bench)

(deftype sample-vector ()
  `(simple-array (unsigned-byte 32) (*)))

(defun make-sample-vector (size)
  (declare (fixnum size))
  (make-array size :element-type '(unsigned-byte 32)))

(defclass result ()
  ((run-time-us :initar)))

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
                 (if (zerop ub32)
                     (return-from unpack)
                     (vector-push-extend ub8 octets))
                 (incf end))))
    (octets-to-string octets :external-format :utf-8 :end end)))

(defun write-symbol (symbol)
  (write-packed-string (package-name (symbol-package symbol)) f)
  (write-packed-string (symbol-name symbol) f))

(with-open-file (f "/tmp/foo.1"
                   :direction :output :if-exists :supersede
                   :element-type '(unsigned-byte 32))
  (write-packed-string "" f)
  #+nil
  (write-packed-string (format nil "This is a test!~%") f))

(with-open-file (f "/tmp/foo.1"
                   :element-type '(unsigned-byte 32))
  (read-packed-string f))

(pack-string "ooobar")

(defparameter *dataset-file-header* (format nil ";; SB-BENCH dataset file v1~%"))

(defun save-dataset (dataset pathname)
  (with-open-file (f pathname
                     :element-type '(unsigned-byte 32)
                     :direction :output
                     :if-exists :supersede)
    (write-packed-string *dataset-file-header* f)
    (write-packed-string (dataset-comment dataset) f)
    (write-symbol (dataset-benchmark-name dataset))
    (write-byte (dataset-benchmark-runs sample) f)
    (write-byte (dataset-benchmark-iterations/run sample) f)
    (write-sample-vector (sample-run-time-us sample) f)
    (write-sample-vector (sample-real-time-ms sample) f)
    (write-sample-vector (sample-gc-run-time-ms sample) f)
    (write-sample-vector (sample-bytes-consed sample) f)))

(defun load-dataset (pathname)
  (with-open-file (f pathname
                     :element-type '(unsigned-byte 32)
                     :direction :input)
    (unless (equal *dataset-file-header* (read-packaged-string f))
      (error "~A is not an SB-BENCH dataset file." pathname))
    (make-instance 'samples
                   :benchmark (let ((package (read-packaged-string f)))
                                (intern (read-packed-string f) package))
                   :iterations (read-byte f)
                   :run-time-us(read-sample-vector f)
                   :real-time-us (read-sample-vector f)
                   :gc-time-ms (read-sample-vector f)
                   :bytes-consed (read-sample-vector f))))

