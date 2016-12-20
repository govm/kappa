(in-package :cl-user)
(defpackage kappa-test.converter
  (:use :cl
        :kappa.converter
        :kappa.define
        :prove)
  (:import-from :flexi-streams
                :make-in-memory-input-stream)
  (:import-from :fast-io
                :with-fast-output))
(in-package :kappa-test.converter)

(defun vs (v)
  (make-in-memory-input-stream v))


(plan 5)

(let* ((v #(1 2 1 0 0 1 0 0))
       (s (vs v))
       (h (make-ofp_header-stream s)))
  (is (ofp_header-version h) 1)
  (is (ofp_header-type h) 2)
  (is (ofp_header-length h) #x0100)
  (is (ofp_header-xid h) #x00010000)
  (is (with-fast-output (buf) (dump-ofp_header h buf))
      v
      :test #'equalp))

(finalize)
