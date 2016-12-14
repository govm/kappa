(in-package :cl-user)
(defpackage kappa-test.define
  (:use :cl
        :kappa.define
        :prove))
(in-package :kappa-test)

(use-package :kappa.define)

(plan nil)

(is OFPT_HELLO 0)
(is (get-constant-name "ofp_type" OFPT_HELLO) (symbol-name 'OFPT_HELLO))

(finalize)
