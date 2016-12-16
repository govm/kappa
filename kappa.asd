#|
  This file is a part of kappa project.
  Copyright (c) 2016 Go Saito (gos@iij.ad.jp)
|#

#|
  Author: Go Saito (gos@iij.ad.jp)
|#

(in-package :cl-user)
(defpackage kappa-asd
  (:use :cl :asdf))
(in-package :kappa-asd)

(defsystem kappa
  :version "0.1"
  :author "Go Saito"
  :license "MIT"
  :depends-on (:cl-async
               :cl-libuv
               :cl-annot
               :babel
               :fast-io
               :alexandria
               :cffi
               :anaphora)
  :components ((:module "src"
                :components
                ((:file "kappa")
                 (:file "server" :depends-on ("define" "converter"
                                              "define.1.0" "define.1.5"))
                 (:file "define" :depends-on ("annot"))
                 (:file "util")
                 (:file "annot")
                 (:file "converter" :depends-on ("define"))
                 (:file "define.1.0" :depends-on ("annot"))
                 (:file "converter.1.0" :depends-on ("define.1.0"))
                 (:file "define.1.5" :depends-on ("annot"))
                 (:file "converter.1.5" :depends-on ("define.1.5"))
                 (:file "util.1.5" :depends-on ("define.1.5"))
                 )))
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
