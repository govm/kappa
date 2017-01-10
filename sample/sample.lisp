(in-package :cl-user)
(defpackage kappa-sample
  (:use :cl
        :kappa.define.1.0
        :kappa.converter.1.0
        :fast-io)
  (:import-from :kappa.server
                :start-server)
  (:import-from :kappa.util
                :get-peername
                :adjust-length
                :defhandler)
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
                :socket-data
                :with-delay)
  (:export :run))
(in-package :kappa-sample)

(defhandler hello-handler OFPT_HELLO (socket header stream)
  (format t "HELLO ~A from ~A~&" header socket)
  (write-socket-data socket
    (with-fast-output (buf)
      (dump-ofp_header (make-ofp_header :version OFP_VERSION
                                        :type OFPT_HELLO
                                        :length 8
                                        :xid (ofp_header-xid header))
                       buf)
      (dump-ofp_header (make-ofp_header :version OFP_VERSION
                                        :type OFPT_FEATURES_REQUEST
                                        :length 8
                                        :xid (ofp_header-xid header))
                       buf))))

(defhandler features-reply-handler OFPT_FEATURES_REPLY (socket header stream)
  (let* ((rep (make-ofp_switch_features-stream header stream)))
    (format t "FEATURES_REPLY ~A from ~A~&" rep socket)
    (setf (socket-data socket) rep)
    (write-socket-data socket
      (with-fast-output (buf)
        (dump-ofp_header (make-ofp_header :version OFP_VERSION
                                          :type OFPT_GET_CONFIG_REQUEST
                                          :length 8
                                          :xid (ofp_header-xid header))
                         buf))))
  (with-delay (30)
    (let ((h (make-ofp_header :version OFP_VERSION
                              :type OFPT_STATS_REQUEST
                              :length 0
                              :xid (ofp_header-xid header))))
      (let ((data (with-fast-output (buf)
                    (dump-ofp_stats_request (make-ofp_stats_request :header h
                                                                    :type OFPST_PORT
                                                                    :flags 1
                                                                    :body (make-ofp_port_stats_request :port_no 1))
                                            buf))))
        (adjust-length data)
        (write-socket-data socket data)))))

(defhandler get-config-reply-handler OFPT_GET_CONFIG_REPLY (socket header stream)
  (let ((rep (make-ofp_switch_config-stream header stream)))
    (format t "GET_CONFIG_REPLY ~A~&" rep)))

(defhandler stats-reply-handler OFPT_STATS_REPLY (socket header stream)
  (let ((rep (make-ofp_stats_reply-stream header stream)))
    (format t "STATS_REPLY ~A~&" rep)))

(defhandler echo-handler OFPT_ECHO_REQUEST (socket header stream)
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
    (write-socket-data socket data)))

(defhandler port-status-handler OFPT_PORT_STATUS (socket header stream)
  (let ((status (make-ofp_port_status-stream header stream)))
    (format t "PORT STATUS ~A~&" status)))

(defhandler error-msg-handler OFPT_ERROR (socket header stream)
  (let* ((e (make-ofp_error_msg-stream header stream))
         (type (ofp_error_msg-type e))
         (code (ofp_error_msg-code e)))
    (format t "ERROR: ~A" (get-constant-name "ofp_error_type" type))
    (cond ((= type OFPET_HELLO_FAILED)
           (format t ".~A~&" (get-constant-name "ofp_hello_failed_code" code)))
          ((= type OFPET_BAD_REQUEST)
           (format t ".~A~&" (get-constant-name "ofp_bad_request_code" code)))
          ((= type OFPET_BAD_ACTION)
           (format t ".~A~&" (get-constant-name "ofp_bad_action_code" code)))
          ((= type OFPET_FLOW_MOD_FAILED)
           (format t ".~A~&" (get-constant-name "ofp_flow_mod_failed_code" code)))
          ((= type OFPET_PORT_MOD_FAILED)
           (format t ".~A~&" (get-constant-name "ofp_port_mod_failed_code" code)))
          ((= type OFPET_QUEUE_OP_FAILED)
           (format t ".~A~&" (get-constant-name "ofp_queue_op_failed_code" code))))))

(defhandler flow-removed-handler OFPT_FLOW_REMOVED (socket header stream)
  (let ((body (make-ofp_flow_removed-stream header stream)))
    ;(format t "FLOW REMOVED: ~A~&" body)
    t))

(defhandler packet-in-handler OFPT_PACKET_IN (socket header stream)
  (let ((body (make-ofp_packet_in-stream header stream)))
    ;(format t "PACKET_IN ~A from ~A~&" body socket)
    (let* ((ports (ofp_switch_features-ports (socket-data socket)))
           (header (make-ofp_header :version OFP_VERSION
                                    :type OFPT_FLOW_MOD
                                    :length 0 ; set it later
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
      (adjust-length data)
      (write-socket-data socket data)
      (if (= (ofp_packet_in-buffer_id body) #xffffffff)
        (let* ((out (make-ofp_packet_out :header (make-ofp_header :version OFP_VERSION
                                                                  :type OFPT_PACKET_OUT
                                                                  :length 0
                                                                  :xid (ofp_header-xid header))
                                         :buffer_id (ofp_packet_in-buffer_id body)
                                         :in_port (ofp_packet_in-in_port body)
                                         :actions_len (* (length (ofp_flow_mod-actions flow_mod)) 8)
                                         :actions (ofp_flow_mod-actions flow_mod)
                                         :data (ofp_packet_in-data body)))
              (data_out (with-fast-output (buf) (dump-ofp_packet_out out buf))))
          (adjust-length data_out)
          (write-socket-data socket data_out))))))

(defun run ()
  (let ((kappa.server:*debug* t))
    ;(sb-profile:profile "KAPPA.SERVER" "KAPPA.DEFINE.1.0" "KAPPA.CONVERTER.1.0" "KAPPA-SAMPLE" "FAST-IO" "CL-ASYNC")
    (start-server)
    ;(sb-profile:report)
    ))
