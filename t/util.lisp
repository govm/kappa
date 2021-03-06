(in-package :cl-user)
(defpackage kappa-test.util
  (:use :cl
        :kappa.util
        :prove)
  (:import-from :kappa.define
                :make-ofp_header
                :ofp_header-version))
(in-package :kappa-test.util)

(plan 1)

(subtest "with-prefix"
  (let ((s (make-ofp_header :version 1)))
    (with-prefix (h ofp_header-)
      (is (h version s) 1))))

(finalize)
