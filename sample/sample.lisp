(in-package :cl-user)
(defpackage kappa-sample
  (:use :cl
        :kappa.define.1.0
        :kappa.converter.1.0
        :fast-io)
  (:import-from :kappa.server
                :start-server
                :add-handler)
  (:import-from :kappa.util
                :get-peername)
  (:import-from :kappa.define
                :make-ofp_header
                :ofp_header-type
                :ofp_header-length
                :ofp_header-version
                :ofp_header-xid)
  (:import-from :kappa.converter
                :dump-ofp_header
                :make-ofp_header)
  (:import-from :cl-async
                :write-socket-data
                :socket-data)
  (:export :run))
(in-package :kappa-sample)

(annot:enable-annot-syntax)

@add-handler
(defun hello-handler (socket header stream)
  (if (= (ofp_header-type header) OFPT_HELLO)
    (progn
      (format t "HELLO ~A from ~A~&" header socket)
      (let* ((hello (make-ofp_header :version OFP_VERSION
                                     :type OFPT_HELLO
                                     :length 8
                                     :xid (ofp_header-xid header))))
        (write-socket-data socket (with-fast-output (buf) (dump-ofp_header hello buf)))
        (let ((data (with-fast-output (buf)
                      (dump-ofp_header (make-ofp_header :version OFP_VERSION
                                                        :type OFPT_FEATURES_REQUEST
                                                        :length 8
                                                        :xid (ofp_header-xid header))
                                       buf))))
          (write-socket-data socket data)))
      t)))

@add-handler
(defun features-reply-handler (socket header stream)
  (if (= (ofp_header-type header) OFPT_FEATURES_REPLY)
    (let* ((rep (make-ofp_switch_features-stream header stream)))
      (format t "FEATURES_REPLY ~A from ~A~&" rep socket)
      (setf (socket-data socket) rep)
      t)
    nil))

@add-handler
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
                   (setf (ofp_header-type header) OFPT_ECHO_REPLY)
                   (dump-ofp_header header buf)
                   (if body
                     (fast-write-sequence body buf)))))
      ;(format t "ECHO ~A from ~A~&" data socket)
      ;(format t "socket: ~A~&" (get-peername socket))
      (write-socket-data socket data)
      t)
    nil))

@add-handler
(defun port-status-handler (socket header stream)
  (if (= (ofp_header-type header) OFPT_PORT_STATUS)
    (let ((status (make-ofp_port_status-stream header stream)))
      (format t "PORT STATUS ~A~&" status)
      t)
    nil))

@add-handler
(defun packet-in-handler (socket header stream)
  (if (= (ofp_header-type header) OFPT_PACKET_IN)
    (let ((body (make-ofp_packet_in-stream header stream)))
      ;(format t "PACKET_IN ~A from ~A~&" body socket)
      (let* ((ports (ofp_switch_features-ports (socket-data socket)))
             (header (make-ofp_header :version OFP_VERSION
                                      :type OFPT_FLOW_MOD
                                      :length (+ 72 (* (length ports) 8))
                                      :xid (ofp_header-xid header)))
             (flow_mod (make-ofp_flow_mod :header header
                                          :match (make-ofp_match :wildcards (- OFPFW_ALL OFPFW_IN_PORT)
                                                                 :in_port (ofp_packet_in-in_port body))
                                          :cookie 0
                                          :command OFPFC_ADD
                                          :idle_timeout 10
                                          :hard_timeout 10
                                          :priority 1
                                          :buffer_id (ofp_packet_in-buffer_id body)
                                          :out_port 0
                                          :flags 0
                                          :actions (loop :for p :in ports
                                                         :collect (make-ofp_action_output :type OFPAT_OUTPUT
                                                                                          :len 8
                                                                                          :port (ofp_phy_port-port_no p)
                                                                                          :max_len 0))))
             (data (with-fast-output (buf) (dump-ofp_flow_mod flow_mod buf))))
        (write-socket-data socket data))
      t)
    nil))

(defun run ()
  (let ((kappa.server:*debug* t))
    ;(sb-profile:profile "KAPPA.SERVER" "KAPPA.DEFINE.1.0" "KAPPA.CONVERTER.1.0" "KAPPA-SAMPLE" "FAST-IO" "CL-ASYNC")
    (start-server)
    ;(sb-profile:report)
    ))
