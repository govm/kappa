(in-package :cl-user)
(defpackage kappa-test.define.1.0
  (:use :cl
        :kappa.define.1.0
        :prove))
(in-package :kappa-test.define.1.0)

(plan nil)

(is OFP_VERSION #x1)
(is OFPT_HELLO 0)
(is (get-constant-name "ofp_type" OFPT_HELLO) (symbol-name 'OFPT_HELLO))

(finalize)
