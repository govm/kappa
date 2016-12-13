(in-package :cl-user)
(defpackage kappa.server
  (:use :cl)
  (:import-from :usocket
                :socket-server)
  (:export :kappa-run
           :kappa-run-multi
           :kappa-spawn
           :kappa-spawn-multi
           :kappa-kill))
(in-package :kappa.server)


(defmacro kappa-run (host port function &optional args)
  `(socket-server ,host ,port ,function ,args
                  :reuse-address t))

(defmacro kappa-run-multi (host port function &optional args)
  `(socket-server ,host ,port ,function ,args
                  :reuse-address t
                  :multi-threading t))

(defmacro kappa-spawn (host port function &optional args)
  `(socket-server ,host ,port ,function ,args
                  :in-new-thread t
                  :reuse-address t))

(defmacro kappa-spawn-multi (host port function &optional args)
  `(socket-server ,host ,port ,function ,args
                  :in-new-thread t
                  :reuse-address t
                  :multi-threading t))

(defun kappa-kill (thread)
  (if (portable-threads:thread-alive-p thread)
    (portable-threads:kill-thread thread)))

(defun test-server (stream)
  (declare (type stream stream))
  (write-line "HELLO!" stream)
  (write-line (read-line stream) stream)
  (force-output stream))
