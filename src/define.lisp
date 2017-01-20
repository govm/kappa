(in-package :cl-user)
(defpackage kappa.define
  (:use :cl
        :annot)
  (:import-from :kappa.annot
                :export-structure-p))
(in-package :kappa.define)

(annot:enable-annot-syntax)

@export-structure-p
(defstruct ofp_header
  version
  type
  length
  (xid (random #xffffffff)))
