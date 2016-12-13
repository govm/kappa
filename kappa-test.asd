#|
  This file is a part of kappa project.
  Copyright (c) 2016 Go Saito (gos@iij.ad.jp)
|#

(in-package :cl-user)
(defpackage kappa-test-asd
  (:use :cl :asdf))
(in-package :kappa-test-asd)

(defsystem kappa-test
  :author "Go Saito"
  :license "MIT"
  :depends-on (:kappa
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "kappa"))))
  :description "Test system for kappa"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
