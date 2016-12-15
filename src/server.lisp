(in-package :cl-user)
(defpackage kappa.server
  (:use :cl
        :kappa.define)
  (:import-from :cl-async
                :tcp-server
                :close-tcp-server
                :close-socket
                :write-socket-data
                :signal-handler
                :start-event-loop
                :exit-event-loop)
  (:export :start-server
           :add-handler
           :*debug*))
(in-package :kappa.server)

(defvar *handlers* '())

(defun add-handler (handler)
  "handler takes (socket header stream)"
  (push handler *handlers*))

(defvar *debug* t)

(add-handler (lambda (socket header stream)
               "catcher for message anyone handled"
               (if *debug*
                 (format *error-output*
                         "Unhandled message version:~A type:~A length:~A xid:~A~&"
                         (ofp_header-version header)
                         (get-constant-name "ofp_type" (ofp_header-type header))
                         (ofp_header-length header)
                         (ofp_header-xid header)))))

(defun dispatcher (socket stream)
  (handler-case
      (let ((header (make-ofp_header-stream stream)))
        (loop :for h :in *handlers*
              :until (not (funcall h socket header stream))))
    (end-of-file () (close-socket socket))))

(defun kappa-server (host port)
  (let ((server (tcp-server host port #'dispatcher :stream t)))
    (signal-handler 2 (lambda (sig)
                        (declare (ignore sig))
                        (close-tcp-server server)
                        (exit-event-loop)))))

(defun start-server (&optional (host "0.0.0.0") (port OFP_TCP_PORT))
  (start-event-loop #'(lambda () (kappa-server host port))))
