(in-package :cl-user)
(defpackage kappa-test.util
  (:use :cl
        :kappa.define
        :kappa.util
        :prove))
(in-package :kappa-test.util)

(plan nil)

(let* ((header (make-ofp_header :version 4
                                :type OFPT_HELLO
                                :length 16
                                :xid 0))
       (hello (make-ofp_hello :header header
                              :elements (list (make-ofp_hello_elem_versionbitmap
                                                :length 8
                                                :bitmaps (list #x12))))))
  (is (get-versions hello) '(4 1)))


(finalize)
