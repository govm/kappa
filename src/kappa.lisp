(in-package :cl-user)
(defpackage kappa
  (:use :cl)
  (:import-from :usocket
                :socket-server)
  (:export :start-server
           :test-server))
(in-package :kappa)

(defun start-server (host port function)
  (socket-server host port function nil
                :reuse-address t
                :multi-threading t))

(defun test-server (stream)
  (declare (type stream stream))
  (write-line "HELLO!" stream)
  (write-line (read-line stream) stream)
  (force-output stream))
