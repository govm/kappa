(in-package :cl-user)
(defpackage kappa-sample
  (:use :cl
        :kappa.define
        :fast-io)
  (:import-from :kappa.server
                :start-server
                :add-handler)
  (:import-from :kappa.util
                :get-versions)
  (:import-from :cl-async
                :write-socket-data)
  (:export :run))
(in-package :kappa-sample)

(defun hello-handler (socket header stream)
  (if (= (ofp_header-type header) OFPT_HELLO)
    (let ((body (make-ofp_hello-stream header stream)))
      (format t "HELLO ~A from ~A~&" body socket)
      (let* ((maxv (apply #'max (get-versions body)))
             (len (if (< maxv 4) 8 16))
             (header (make-ofp_header :version maxv
                                      :type OFPT_HELLO
                                      :length len
                                      :xid (ofp_header-xid header)))
             (hello (make-ofp_hello :header header
                                    :elements (if (= len 8)
                                                nil
                                                (list (make-ofp_hello_elem_versionbitmap
                                                        :length 8
                                                        :bitmaps (list (ash 1 maxv))))))))
        (write-socket-data socket (dump-ofp_hello hello)))
      t)))

(add-handler #'hello-handler)

(defun echo-handler (socket header stream)
  (if (= (ofp_header-type header) OFPT_ECHO_REQUEST)
    (let* ((body-len (- (ofp_header-length header) 8))
           (body (if (> body-len 0)
                   (with-fast-input (buf nil stream)
                     (let ((vec (make-octet-vector body-len)))
                       (fast-read-sequence vec buf 0 body-len)
                       vec))
                   nil))
           (data (with-fast-output (buf)
                   (writeu8-be (ofp_header-version header) buf)
                   (writeu8-be OFPT_ECHO_REPLY buf)
                   (writeu16-be (ofp_header-length header) buf)
                   (writeu32-be (ofp_header-xid header) buf)
                   (if body
                     (fast-write-sequence body buf)))))
      (format t "ECHO ~A from ~A~&" data socket)
      (write-socket-data socket data)
      t)
    nil))

(add-handler #'echo-handler)

(defun run ()
  (let ((kappa.server:*debug* t))
    (start-server)))
