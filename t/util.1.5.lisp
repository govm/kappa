(in-package :cl-user)
(defpackage kappa-test.util.1.5
  (:use :cl
        :kappa.define.1.5
        :kappa.util.1.5
        :prove)
  (:import-from :kappa.define
                :make-ofp_header))
(in-package :kappa-test.util.1.5)

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
