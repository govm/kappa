(in-package :cl-user)
(defpackage kappa.annot
  (:use :cl
        :annot
        :annot.class
        :annot.util)
  (:import-from :alexandria
                :symbolicate)
  (:export :export-structure-p))
(in-package :kappa.annot)

(defmacro export-p (class-definition-form)
  (progn-form-replace-last
    (lambda (class-definition-form)
      `(progn
         (export ',(symbolicate (second class-definition-form) '-p))
         ,class-definition-form))
    class-definition-form))

(defmacro export-structure-p (class-definition-form)
  `(export-p
     (export-structure
       ,class-definition-form)))
