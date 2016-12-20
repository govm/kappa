(in-package :cl-user)
(defpackage kappa.server
  (:use :cl
        :kappa.define
        :kappa.converter)
  (:import-from :cl-async
                :tcp-server
                :close-tcp-server
                :close-socket
                :signal-handler
                :start-event-loop
                :exit-event-loop)
  (:import-from :fast-io
                :with-fast-input
                :fast-read-sequence
                :make-octet-vector)
  (:export :start-server
           :add-handler
           :*debug*))
(in-package :kappa.server)

(defvar *handlers* '())

(defun add-handler (handler)
  "handler takes (socket header stream)"
  (push handler *handlers*)
  handler)

(defvar *debug* nil)

(add-handler (lambda (socket header stream)
               "catcher for message anyone handled"
               (if *debug*
                 (format *error-output*
                         "Unhandled message version:~A type:~A length:~A xid:~A from socket:~A~&~A~&"
                         (ofp_header-version header)
                         (let ((version (ofp_header-version header)))
                           (cond
                             ((= version kappa.define.1.0:OFP_VERSION)
                               (kappa.define.1.0:get-constant-name "ofp_type" (ofp_header-type header)))
                             ((= version kappa.define.1.5:OFP_VERSION)
                               (kappa.define.1.5:get-constant-name "ofp_type" (ofp_header-type header)))
                             (t
                               (format nil "unknown ~A" (ofp_header-type header)))))
                         (ofp_header-length header)
                         (ofp_header-xid header)
                         socket
                         (with-fast-input (buf nil stream)
                           (let* ((len (- (ofp_header-length header) 8))
                                  (vec (make-octet-vector len)))
                             (fast-read-sequence vec buf 0 len)
                             vec))))
               t))

(defun dispatcher (socket stream)
  (handler-case
      (let ((header (make-ofp_header-stream stream)))
        (loop :for h :in *handlers*
              :until (funcall h socket header stream)))
    (end-of-file () (progn
                      (format *error-output* "socket EOF~&")
                      (close-socket socket)))))

(defun kappa-server (host port)
  (let ((server (tcp-server host port #'dispatcher :stream t)))
    (signal-handler 2 (lambda (sig)
                        (declare (ignore sig))
                        (close-tcp-server server)
                        (exit-event-loop)))))

(defun start-server (&optional (host "0.0.0.0") (port 6653))
  (start-event-loop #'(lambda () (kappa-server host port))))
