(in-package :cl-user)
(defpackage kappa.converter
  (:use :cl
        :fast-io
        :kappa.define)
  (:export :make-ofp_header-stream
           :dump-ofp_header))
(in-package :kappa.converter)

(defun make-ofp_header-stream (stream)
  (with-fast-input (buf nil stream)
    (let ((version (readu8-be buf))
          (type (readu8-be buf))
          (length (readu16-be buf))
          (xid (readu32-be buf)))
      (make-ofp_header :version version
                       :type type
                       :length length
                       :xid xid))))

(defun dump-ofp_header (header buf)
  (writeu8-be (ofp_header-version header) buf)
  (writeu8-be (ofp_header-type header) buf)
  (writeu16-be (ofp_header-length header) buf)
  (writeu32-be (ofp_header-xid header) buf))
