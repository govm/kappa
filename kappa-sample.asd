#|
  This file is a part of kappa project.
  Copyright (c) 2016 Go Saito (gos@iij.ad.jp)
|#

#|
  Author: Go Saito (gos@iij.ad.jp)
|#

(in-package :cl-user)
(defpackage kappa-sample-asd
  (:use :cl :asdf))
(in-package :kappa-sample-asd)

(defsystem kappa-sample
  :version "0.1"
  :author "Go Saito"
  :license "MIT"
  :depends-on (:kappa
               :cl-async
               :cl-annot
               :babel
               :fast-io)
  :components ((:module "sample"
                :components
                ((:file "sample"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op kappa-test))))
