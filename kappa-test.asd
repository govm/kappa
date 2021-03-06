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
               :fast-io
               :flexi-streams
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "kappa")
                 (:test-file "define.1.0")
                 (:test-file "util")
                 (:test-file "util.1.5")
                 (:test-file "converter")
                 (:test-file "converter.1.0")
                 (:test-file "converter.1.3"))))
  :description "Test system for kappa"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
