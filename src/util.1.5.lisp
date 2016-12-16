(in-package :cl-user)
(defpackage kappa.util.1.5
  (:use :cl
        :kappa.define.1.5
        :fast-io)
  (:export :get-versions))
(in-package :kappa.util.1.5)

(defun get-versions (ofp_hello)
  (let ((vs '())
        (elms (ofp_hello-elements ofp_hello)))
    (if elms
      (loop :for e :in elms
            :if (ofp_hello_elem_versionbitmap-p e)
            :do (let ((bms (ofp_hello_elem_versionbitmap-bitmaps e)))
                  (loop :for b :in bms
                        :for bi :from 0
                        :do (loop :for i :from 0 :to 31
                                  :do (if (logbitp i b)
                                        (push (+ (* bi 32) i) vs))))))
      (push (ofp_header-version (ofp_hello-header ofp_hello)) vs))
    vs))
