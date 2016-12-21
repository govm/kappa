(in-package :cl-user)
(defpackage kappa.util
  (:use :cl
        :fast-io)
  (:export :get-peername))
(in-package :kappa.util)

(defun %get-peername (socket)
  (let ((uvstream (as:socket-c socket)))
    (cffi:with-foreign-objects ((name :uint8 32) (namelen :int 1))
      (setf (cffi:mem-aref namelen :int 0) 32)
      (uv::uv-tcp-getpeername uvstream name namelen)
      (with-fast-output (buf)
        (loop :for i :from 0 :below (cffi:mem-aref namelen :int 0)
              :do (fast-write-byte (cffi:mem-aref name :uint8 i) buf))))))

(defun get-v4addr (buf)
  (let ((port (readu16-be buf))
        (addr (format nil "~A.~A.~A.~A"
                      (fast-read-byte buf)
                      (fast-read-byte buf)
                      (fast-read-byte buf)
                      (fast-read-byte buf))))
    (list addr port)))

(defun get-peername (socket)
  (let ((sockaddr (%get-peername socket)))
    (with-fast-input (buf sockaddr)
      (let ((family (readu16-le buf)))
        (if (= family 2) ; AF_INET
          (get-v4addr buf)
          sockaddr)))))

(defun adjust-length (data)
  (let ((len (length data)))
    (setf (aref data 2) (logand #x00ff (ash len -8)))
    (setf (aref data 3) (logand #x00ff len))))
