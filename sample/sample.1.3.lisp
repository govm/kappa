(in-package :cl-user)
(defpackage kappa-sample.1.3
  (:use :cl
        :kappa.define.1.3
        :kappa.converter.1.3
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
(in-package :kappa-sample.1.3)

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
                                        :length 8)
                       buf))))

(defhandler features-reply-handler OFPT_FEATURES_REPLY (socket header stream)
  (let* ((rep (make-ofp_switch_features-stream header stream)))
    (format t "FEATURES_REPLY ~A from ~A~&" rep socket)
    (setf (socket-data socket) rep)
    (let* ((header (make-ofp_header :version OFP_VERSION
                                    :type OFPT_FLOW_MOD
                                    :length 0)) ; set it later
           (flow_mod (make-ofp_flow_mod :header header
                                        :cookie 0
                                        :cookie_mask 0
                                        :table_id 0
                                        :command OFPFC_ADD
                                        :idle_timeout 0
                                        :hard_timeout 0
                                        :priority 0
                                        :buffer_id OFP_NO_BUFFER
                                        :out_port OFPP_ANY
                                        :out_group OFPG_ANY
                                        :flags 0
                                        :match (make-ofp_match)
                                        :instructions (list (make-ofp_instruction_actions
                                                              :type OFPIT_APPLY_ACTIONS
                                                              :len 24
                                                              :actions (list (make-ofp_action_output
                                                                               :port OFPP_CONTROLLER
                                                                               :max_len OFPCML_NO_BUFFER))))))
           (data (with-fast-output (buf) (dump-ofp_flow_mod flow_mod buf))))
      (adjust-length data)
      (write-socket-data socket data))))

(defhandler get-config-reply-handler OFPT_GET_CONFIG_REPLY (socket header stream)
  (let ((rep (make-ofp_switch_config-stream header stream)))
    (format t "GET_CONFIG_REPLY ~A~&" rep)))

(defhandler stats-multipart-handler OFPT_MULTIPART_REPLY (socket header stream)
  (let ((rep (make-ofp_multipart_reply-stream header stream)))
    (format t "MULRIPART_REPLY ~A~&" rep)))

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
  (format t "ERROR HEADER: ~A~&" header)
  (let* ((e (make-ofp_error_msg-stream header stream))
         (type (ofp_error_msg-type e))
         (code (ofp_error_msg-code e)))
    (format t "ERROR: ~A.~A&"
            (get-constant-name "ofp_error_type" type)
            (get-constant-name (cond ((= type OFPET_HELLO_FAILED) "ofp_hello_failed_code")
                                     ((= type OFPET_BAD_REQUEST) "ofp_bad_request_code")
                                     ((= type OFPET_BAD_ACTION) "ofp_bad_action_code")
                                     ((= type OFPET_BAD_INSTRUCTION) "ofp_bad_instruction_code")
                                     ((= type OFPET_BAD_MATCH) "ofp_bad_match_code")
                                     ((= type OFPET_FLOW_MOD_FAILED) "ofp_flow_mod_failed_code")
                                     ((= type OFPET_GROUP_MOD_FAILED) "ofp_group_mod_failed_code")
                                     ((= type OFPET_PORT_MOD_FAILED) "ofp_port_mod_failed_code")
                                     ((= type OFPET_QUEUE_OP_FAILED) "ofp_queue_op_failed_code")
                                     ((= type OFPET_SWITCH_CONFIG_FAILED) "ofp_switch_config_failed_code")
                                     ((= type OFPET_ROLE_REQUEST_FAILED) "ofp_role_request_failed_code")
                                     ((= type OFPET_METER_MOD_FAILED) "ofp_meter_mod_failed_code")
                                     ((= type OFPET_TABLE_FEATURES_FAILED) "ofp_table_features_failed_code"))
                               code))))

(defhandler flow-removed-handler OFPT_FLOW_REMOVED (socket header stream)
  (let ((body (make-ofp_flow_removed-stream header stream)))
    ;(format t "FLOW REMOVED: ~A~&" body)
    t))

(defhandler packet-in-handler OFPT_PACKET_IN (socket header stream)
  (let ((body (make-ofp_packet_in-stream header stream)))
    ;(format t "PACKET_IN ~A from ~A~&" body socket)
    (let* ((header (make-ofp_header :version OFP_VERSION
                                    :type OFPT_FLOW_MOD
                                    :length 0)) ; set it later
           (flow_mod (make-ofp_flow_mod :header header
                                        :cookie 0
                                        :cookie_mask 0
                                        :table_id 0
                                        :command OFPFC_ADD
                                        :idle_timeout 10
                                        :hard_timeout 10
                                        :priority 1
                                        :buffer_id (ofp_packet_in-buffer_id body)
                                        :out_port OFPP_ANY
                                        :out_group OFPG_ANY
                                        :flags 0
                                        :match (make-ofp_match)
                                        :instructions (list (make-ofp_instruction_actions
                                                              :type OFPIT_APPLY_ACTIONS
                                                              :len 24
                                                              :actions (list (make-ofp_action_output
                                                                               :port OFPP_ALL
                                                                               :max_len 0))))))
           (data (with-fast-output (buf) (dump-ofp_flow_mod flow_mod buf))))
      (adjust-length data)
      (write-socket-data socket data)
      (if (= (ofp_packet_in-buffer_id body) OFP_NO_BUFFER)
        (let* ((out (make-ofp_packet_out :header (make-ofp_header :version OFP_VERSION
                                                                  :type OFPT_PACKET_OUT
                                                                  :length 0)
                                         :buffer_id (ofp_packet_in-buffer_id body)
                                         :in_port OFPP_CONTROLLER
                                         :actions_len (get-actions-length (ofp_instruction_actions-actions (car (ofp_flow_mod-instructions flow_mod))))
                                         :actions (ofp_instruction_actions-actions (car (ofp_flow_mod-instructions flow_mod)))
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
