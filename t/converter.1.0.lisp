(in-package :cl-user)
(defpackage kappa-test.converter.1.0
  (:use :cl
        :kappa.converter.1.0
        :kappa.define.1.0
        :prove)
  (:import-from :kappa.define
                :make-ofp_header)
  (:import-from :flexi-streams
                :make-in-memory-input-stream)
  (:import-from :fast-io
                :with-fast-output))
(in-package :kappa-test.converter.1.0)

(defun vs (v)
  (make-in-memory-input-stream v))


(plan 49)

(let* ((h (make-ofp_header :version 1 :type OFPT_FEATURES_REPLY :length 80 :xid 0))
       (v #(1 0 1 0 0 1 0 1; datapath_id
            0 0 0 1; n_buffers
            2; n_tables
            0 1 2; pad
            1 0 0 1; capabilities
            0 1 1 0; actions
            ; ofp_phy_port
            0 1; port_no
            1 2 3 4 5 6; hw_addr
            #.(char-code #\s)
            #.(char-code #\0)
            0 0 0 0 0 0 0 0 0 0 0 0 0 0; name
            0 1 0 1; config
            1 0 1 0; state
            0 1 0 1; curr
            1 0 1 0; advertised
            0 1 0 1; supported
            1 0 1 0; peer
            ))
       (s (vs v))
       (b (make-ofp_switch_features-stream h s)))
  (is (length v) (- 80 8))
  (is (ofp_switch_features-datapath_id b) #x0100010000010001)
  (is (ofp_switch_features-n_buffers b) 1)
  (is (ofp_switch_features-n_tables b) 2)
  (is (ofp_switch_features-capabilities b) #x01000001)
  (is (ofp_switch_features-actions b) #x00010100)
  (let ((ports (ofp_switch_features-ports b)))
    (is (length ports) 1)
    (let ((p (car ports)))
      (is (ofp_phy_port-port_no p) 1)
      (is (ofp_phy_port-hw_addr p) #(1 2 3 4 5 6) :test #'equalp)
      (is (ofp_phy_port-name p) "s0" :test #'string=)
      (is (ofp_phy_port-config p) #x00010001)
      (is (ofp_phy_port-state p) #x01000100)
      (is (ofp_phy_port-curr p) #x00010001)
      (is (ofp_phy_port-advertised p) #x01000100)
      (is (ofp_phy_port-supported p) #x00010001)
      (is (ofp_phy_port-peer p) #x01000100)))
  (is-error (read-byte s) 'end-of-file))

(let* ((h (make-ofp_header :version 1 :type OFPT_PACKET_IN :length 24 :xid 0))
       (v #(0 0 0 1; buffer_id
            1 0; total_len
            0 1; in_port
            2; reason
            3; pad
            1 2 3 4 5 6;data
            ))
       (s (vs v))
       (b (make-ofp_packet_in-stream h s)))
  (is (ofp_packet_in-buffer_id b) 1)
  (is (ofp_packet_in-total_len b) #x0100)
  (is (ofp_packet_in-in_port b) #x0001)
  (is (ofp_packet_in-reason b) 2)
  (is (ofp_packet_in-data b) #(1 2 3 4 5 6) :test #'equalp)
  (is-error (read-byte s) 'end-of-file))

(let* ((m (make-ofp_match :wildcards 1
                          :in_port 2
                          :dl_src #(1 2 3 4 5 6)
                          :dl_dst #(6 5 4 3 2 1)
                          :dl_vlan 3
                          :dl_vlan_pcp 4
                          :dl_type 5
                          :nw_tos 6
                          :nw_proto 7
                          :nw_src 8
                          :nw_dst 9
                          :tp_src 10
                          :tp_dst 11))
       (expect #(0 0 0 1
                 0 2
                 1 2 3 4 5 6
                 6 5 4 3 2 1
                 0 3
                 4
                 0
                 0 5
                 6
                 7
                 0 0
                 0 0 0 8
                 0 0 0 9
                 0 10
                 0 11))
       (dump (with-fast-output (buf) (dump-ofp_match m buf))))
  (is dump expect :test #'equalp))

(let* ((h (make-ofp_header :version 1 :type OFPT_FLOW_MOD :length 8 :xid 0))
       (m (make-ofp_match))
       (f (make-ofp_flow_mod :header h
                             :match m
                             :cookie #x01000001
                             :command 1
                             :idle_timeout 10
                             :hard_timeout 11
                             :priority 1
                             :buffer_id 2
                             :out_port 3
                             :flags 4
                             :actions (list (make-ofp_action_output :type OFPAT_OUTPUT
                                                                    :len 8
                                                                    :port 0
                                                                    :max_len 16)
                                            (make-ofp_action_enqueue :type OFPAT_ENQUEUE
                                                                     :len 16
                                                                     :port 1
                                                                     :queue_id 2)
                                            (make-ofp_action_vlan_vid :type OFPAT_SET_VLAN_VID
                                                                      :len 8
                                                                      :vlan_vid 1)
                                            (make-ofp_action_vlan_pcp :type OFPAT_SET_VLAN_PCP
                                                                      :len 8
                                                                      :vlan_pcp 1)
                                            (make-ofp_action_dl_addr :type OFPAT_SET_DL_SRC
                                                                     :len 16
                                                                     :dl_addr #(0 1 2 3 4 5))
                                            (make-ofp_action_nw_addr :type OFPAT_SET_NW_SRC
                                                                     :len 8
                                                                     :nw_addr 1)
                                            (make-ofp_action_nw_tos :type OFPAT_SET_NW_TOS
                                                                    :len 8
                                                                    :nw_tos 1)
                                            (make-ofp_action_tp_port :type OFPAT_SET_TP_SRC
                                                                     :len 8
                                                                     :tp_port 1)
                                            (make-ofp_action_vendor_header :type OFPAT_VENDOR
                                                                           :len 16
                                                                           :vendor 1
                                                                           :body #(0 1 2 3 4 5 6 7))
                                            )))
       (expect `#(1 #.OFPT_FLOW_MOD 0 8 0 0 0 0
                  ,@(loop :repeat 40 :collect 0)
                  0 0 0 0 1 0 0 1
                  0 1
                  0 10
                  0 11
                  0 1
                  0 0 0 2
                  0 3
                  0 4
                  0 #.OFPAT_OUTPUT
                  0 8
                  0 0
                  0 16
                  0 #.OFPAT_ENQUEUE
                  0 16
                  0 1
                  0 0 0 0 0 0
                  0 0 0 2
                  0 #.OFPAT_SET_VLAN_VID
                  0 8
                  0 1
                  0 0
                  0 #.OFPAT_SET_VLAN_PCP
                  0 8
                  1
                  0 0 0
                  0 #.OFPAT_SET_DL_SRC
                  0 16
                  0 1 2 3 4 5
                  0 0 0 0 0 0
                  0 #.OFPAT_SET_NW_SRC
                  0 8
                  0 0 0 1
                  0 #.OFPAT_SET_NW_TOS
                  0 8
                  1
                  0 0 0
                  0 #.OFPAT_SET_TP_SRC
                  0 8
                  0 1
                  0 0
                  255 255 ; OFPAT_VENDOR
                  0 16
                  0 0 0 1
                  0 1 2 3 4 5 6 7))
       (dump (with-fast-output (buf) (dump-ofp_flow_mod f buf))))
  (is dump expect :test #'equalp))

(let* ((h (make-ofp_header :version 1 :type OFPT_PORT_STATUS :length 64 :xid 0))
       (v `#(#.OFPPR_ADD
             0 0 0 0 0 0 0
             ,@(loop :repeat 48 :collect 0)
            ))
       (s (vs v))
       (b (make-ofp_port_status-stream h s)))
  (is (ofp_port_status-reason b) OFPPR_ADD)
  (is-type (ofp_port_status-desc b) 'ofp_phy_port)
  (is-error (read-byte s) 'end-of-file))

(let* ((h (make-ofp_header :version 1 :type OFPT_ERROR :length 16 :xid 0))
       (v `#(0 #.OFPET_HELLO_FAILED
             0 #.OFPHFC_INCOMPATIBLE
             1 2 3 4))
       (s (vs v))
       (b (make-ofp_error_msg-stream h s)))
  (is (ofp_error_msg-type b) OFPET_HELLO_FAILED)
  (is (ofp_error_msg-code b) OFPHFC_INCOMPATIBLE)
  (is (ofp_error_msg-data b) #(1 2 3 4) :test #'equalp)
  (is-error (read-byte s) 'end-of-file))

(let* ((h (make-ofp_header :version 1 :type OFPT_FLOW_REMOVED :length 88 :xid 0))
       (v `#(,@(loop :repeat 40 :collect 0)
             0 0 0 0 0 0 0 1
             0 1
             #.OFPRR_IDLE_TIMEOUT
             0
             1 2 3 4
             4 3 2 1
             0 1
             0 0
             0 0 0 0 0 0 0 1
             1 0 0 0 0 0 0 0))
       (s (vs v))
       (b (make-ofp_flow_removed-stream h s)))
  (is (ofp_flow_removed-cookie b) 1)
  (is (ofp_flow_removed-priority b) 1)
  (is (ofp_flow_removed-reason b) OFPRR_IDLE_TIMEOUT)
  (is (ofp_flow_removed-duration_sec b) #x01020304)
  (is (ofp_flow_removed-duration_nsec b) #x04030201)
  (is (ofp_flow_removed-idle_timeout b) 1)
  (is (ofp_flow_removed-packet_count b) 1)
  (is (ofp_flow_removed-byte_count b) #x0100000000000000)
  (is-error (read-byte s) 'end-of-file))

(let* ((h (make-ofp_header :version 1 :type OFPT_GET_CONFIG_REPLY :length 16 :xid 0))
       (v #(0 1 1 0))
       (s (vs v))
       (b (make-ofp_switch_config-stream h s)))
  (is (ofp_switch_config-flags b) 1)
  (is (ofp_switch_config-miss_send_len b) #x0100)
  (is-error (read-byte s) 'end-of-file))

(let* ((h (make-ofp_header :version 1 :type OFPT_SET_CONFIG :length 16 :xid 0))
       (c (make-ofp_switch_config :header h :flags 1 :miss_send_len #x0100))
       (expect #(1 #.OFPT_SET_CONFIG 0 16 0 0 0 0
                 0 1 1 0))
       (dump (with-fast-output (buf) (dump-ofp_switch_config c buf))))
  (is dump expect :test #'equalp))

(let* ((h (make-ofp_header :version 1 :type OFPT_PORT_MOD :length 32 :xid 0))
       (m (make-ofp_port_mod :header h
                             :port_no 1
                             :hw_addr #(0 1 2 3 4 5)
                             :config 1
                             :mask #xffffffff
                             :advertise 1))
       (expect #(1 #.OFPT_PORT_MOD 0 32 0 0 0 0
                 0 1
                 0 1 2 3 4 5
                 0 0 0 1
                 255 255 255 255
                 0 0 0 1
                 0 0 0 0))
       (dump (with-fast-output (buf) (dump-ofp_port_mod m buf))))
  (is dump expect :test #'equalp))

(subtest "ofp_stats"
  (let* ((h (make-ofp_header :version 1 :type OFPT_STATS_REQUEST :length 0 :xid 0)))
    (let* ((req (make-ofp_stats_request :header h
                                        :type OFPST_DESC
                                        :flags 1
                                        :body nil))
           (expect #(1 #.OFPT_STATS_REQUEST 0 0 0 0 0 0
                     0 #.OFPST_DESC 0 1))
           (dump (with-fast-output (buf) (dump-ofp_stats_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_stats_request :header h
                                        :type OFPST_FLOW
                                        :flags 1
                                        :body (make-ofp_flow_stats_request :match (make-ofp_match)
                                                                           :table_id 1
                                                                           :out_port 2)))
           (expect `#(1 #.OFPT_STATS_REQUEST 0 0 0 0 0 0
                      0 #.OFPST_FLOW 0 1
                      ,@(loop :repeat 40 :collect 0)
                      1
                      0
                      0 2))
           (dump (with-fast-output (buf) (dump-ofp_stats_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_stats_request :header h
                                        :type OFPST_AGGREGATE
                                        :flags 1
                                        :body (make-ofp_aggregate_stats_request :match (make-ofp_match)
                                                                                :table_id 1
                                                                                :out_port 2)))
           (expect `#(1 #.OFPT_STATS_REQUEST 0 0 0 0 0 0
                      0 #.OFPST_AGGREGATE 0 1
                      ,@(loop :repeat 40 :collect 0)
                      1
                      0
                      0 2))
           (dump (with-fast-output (buf) (dump-ofp_stats_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_stats_request :header h
                                        :type OFPST_TABLE
                                        :flags 1
                                        :body nil))
           (expect #(1 #.OFPT_STATS_REQUEST 0 0 0 0 0 0
                     0 #.OFPST_TABLE 0 1))
           (dump (with-fast-output (buf) (dump-ofp_stats_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_stats_request :header h
                                        :type OFPST_PORT
                                        :flags 1
                                        :body (make-ofp_port_stats_request :port_no 1)))
           (expect #(1 #.OFPT_STATS_REQUEST 0 0 0 0 0 0
                     0 #.OFPST_PORT 0 1
                     0 1
                     0 0 0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_stats_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_stats_request :header h
                                        :type OFPST_QUEUE
                                        :flags 1
                                        :body (make-ofp_queue_stats_request :port_no 1
                                                                            :queue_id 2)))
           (expect #(1 #.OFPT_STATS_REQUEST 0 0 0 0 0 0
                     0 #.OFPST_QUEUE 0 1
                     0 1
                     0 0
                     0 0 0 2))
           (dump (with-fast-output (buf) (dump-ofp_stats_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_stats_request :header h
                                        :type OFPST_VENDOR
                                        :flags 1
                                        :body #(0 1 2 3)))
           (expect #(1 #.OFPT_STATS_REQUEST 0 0 0 0 0 0
                     255 255 0 1
                     0 1 2 3))
           (dump (with-fast-output (buf) (dump-ofp_stats_request req buf))))
      (is dump expect :test #'equalp)))
  (let* ((h (make-ofp_header :version 1 :type OFPT_STATS_REPLY :length 0 :xid 0)))
    (let* ((v `#(0 #.OFPST_DESC 0 1
                 ,@(mapcar #'(lambda (c) (char-code c)) '(#\m #\f #\r))
                 ,@(loop :repeat (- DESC_STR_LEN 3) :collect 0)
                 ,@(mapcar #'(lambda (c) (char-code c)) '(#\h #\w))
                 ,@(loop :repeat (- DESC_STR_LEN 2) :collect 0)
                 ,@(mapcar #'(lambda (c) (char-code c)) '(#\s #\w))
                 ,@(loop :repeat (- DESC_STR_LEN 2) :collect 0)
                 ,@(mapcar #'(lambda (c) (char-code c)) '(#\s #\e #\r #\i #\a #\l))
                 ,@(loop :repeat (- SERIAL_NUM_LEN 6) :collect 0)
                 ,@(mapcar #'(lambda (c) (char-code c)) '(#\d #\p))
                 ,@(loop :repeat (- DESC_STR_LEN 2) :collect 0)))
           (s (vs v))
           (b (make-ofp_stats_reply-stream h s)))
      (is (ofp_stats_reply-type b) OFPST_DESC)
      (is (ofp_stats_reply-flags b) 1)
      (let ((bb (ofp_stats_reply-body b)))
        (ok (ofp_desc_stats-p bb))
        (is (ofp_desc_stats-mfr_desc bb) "mfr" :test #'string=)
        (is (ofp_desc_stats-hw_desc bb) "hw" :test #'string=)
        (is (ofp_desc_stats-sw_desc bb) "sw" :test #'string=)
        (is (ofp_desc_stats-serial_num bb) "serial" :test #'string=)
        (is (ofp_desc_stats-dp_desc bb) "dp" :test #'string=))
      (is-error (read-byte s) 'end-of-file))
    (let* ((v `#(0 #.OFPST_FLOW 0 1
                 0 112
                 1
                 0
                 ,@(loop :repeat 40 :collect 0)
                 0 0 0 1
                 0 0 1 0
                 0 1
                 0 2
                 0 3
                 0 0 0 0 0 0
                 0 0 0 0 0 0 0 1
                 0 0 0 0 0 0 1 0
                 0 0 0 0 0 1 0 0
                 0 #.OFPAT_OUTPUT 0 8 0 1 0 0
                 0 #.OFPAT_ENQUEUE 0 44 0 1 0 0 0 0 0 0 0 0 0 1))
           (s (vs v))
           (b (make-ofp_stats_reply-stream h s)))
      (is (ofp_stats_reply-type b) OFPST_FLOW)
      (let ((bb (ofp_stats_reply-body b)))
        (ok (ofp_flow_stats-p bb))
        (is (ofp_flow_stats-length bb) 112)
        (is (ofp_flow_stats-table_id bb) 1)
        (ok (ofp_match-p (ofp_flow_stats-match bb)))
        (is (ofp_flow_stats-duration_sec bb) 1)
        (is (ofp_flow_stats-duration_nsec bb) #x100)
        (is (ofp_flow_stats-priority bb) 1)
        (is (ofp_flow_stats-idle_timeout bb) 2)
        (is (ofp_flow_stats-hard_timeout bb) 3)
        (is (ofp_flow_stats-cookie bb) 1)
        (is (ofp_flow_stats-packet_count bb) #x100)
        (is (ofp_flow_stats-byte_count bb) #x10000)
        (ok (ofp_action_output-p (car (ofp_flow_stats-actions bb))))
        (ok (ofp_action_enqueue-p (cadr (ofp_flow_stats-actions bb)))))
      (is-error (read-byte s) 'end-of-file))
    (let* ((v #(0 #.OFPST_AGGREGATE 0 1
                0 0 0 0 0 0 0 1
                0 0 0 0 0 0 1 0
                0 0 0 1
                0 0 0 0))
           (s (vs v))
           (b (make-ofp_stats_reply-stream h s)))
      (is (ofp_stats_reply-type b) OFPST_AGGREGATE)
      (let ((bb (ofp_stats_reply-body b)))
        (ok (ofp_aggregate_stats_reply-p bb))
        (is (ofp_aggregate_stats_reply-packet_count bb) 1)
        (is (ofp_aggregate_stats_reply-byte_count bb) #x100)
        (is (ofp_aggregate_stats_reply-flow_count bb) 1))
      (is-error (read-byte s) 'end-of-file))
    (let* ((v `#(0 #.OFPST_TABLE 0 1
                 1
                 0 0 0
                 ,(char-code #\s) ,@(loop :repeat (- OFP_MAX_TABLE_NAME_LEN 1) :collect 0)
                 0 0 0 1
                 0 0 0 2
                 0 0 0 3
                 0 0 0 0 0 0 0 4
                 0 0 0 0 0 0 0 5))
           (s (vs v))
           (b (make-ofp_stats_reply-stream h s)))
      (is (ofp_stats_reply-type b) OFPST_TABLE)
      (let ((bb (ofp_stats_reply-body b)))
        (ok (ofp_table_stats-p bb))
        (is (ofp_table_stats-table_id bb) 1)
        (is (ofp_table_stats-name bb) "s" :test #'equalp)
        (is (ofp_table_stats-wildcards bb) 1)
        (is (ofp_table_stats-max_entries bb) 2)
        (is (ofp_table_stats-active_count bb) 3)
        (is (ofp_table_stats-lookup_count bb) 4)
        (is (ofp_table_stats-matched_count bb) 5))
      (is-error (read-byte s) 'end-of-file))
    (let* ((v #(0 #.OFPST_PORT 0 1
                0 1
                0 0 0 0 0 0
                0 0 0 0 0 0 0 1
                0 0 0 0 0 0 0 2
                0 0 0 0 0 0 0 3
                0 0 0 0 0 0 0 4
                0 0 0 0 0 0 0 5
                0 0 0 0 0 0 0 6
                0 0 0 0 0 0 0 7
                0 0 0 0 0 0 0 8
                0 0 0 0 0 0 0 9
                0 0 0 0 0 0 0 10
                0 0 0 0 0 0 0 11
                0 0 0 0 0 0 0 12))
           (s (vs v))
           (b (make-ofp_stats_reply-stream h s)))
      (is (ofp_stats_reply-type b) OFPST_PORT)
      (let ((bb (ofp_stats_reply-body b)))
        (ok (ofp_port_stats-p bb))
        (is (ofp_port_stats-port_no bb) 1)
        (is (ofp_port_stats-rx_packets bb) 1)
        (is (ofp_port_stats-tx_packets bb) 2)
        (is (ofp_port_stats-rx_bytes bb) 3)
        (is (ofp_port_stats-tx_bytes bb) 4)
        (is (ofp_port_stats-rx_dropped bb) 5)
        (is (ofp_port_stats-tx_dropped bb) 6)
        (is (ofp_port_stats-rx_errors bb) 7)
        (is (ofp_port_stats-tx_errors bb) 8)
        (is (ofp_port_stats-rx_frame_err bb) 9)
        (is (ofp_port_stats-rx_over_err bb) 10)
        (is (ofp_port_stats-rx_crc_err bb) 11)
        (is (ofp_port_stats-collisions bb) 12))
      (is-error (read-byte s) 'end-of-file))
    (let* ((v #(0 #.OFPST_QUEUE 0 1
                0 1
                0 0
                0 0 0 1
                0 0 0 0 0 0 0 1
                0 0 0 0 0 0 0 2
                0 0 0 0 0 0 0 3))
           (s (vs v))
           (b (make-ofp_stats_reply-stream h s)))
      (is (ofp_stats_reply-type b) OFPST_QUEUE)
      (let ((bb (ofp_stats_reply-body b)))
        (ok (ofp_queue_stats-p bb))
        (is (ofp_queue_stats-port_no bb) 1)
        (is (ofp_queue_stats-queue_id bb) 1)
        (is (ofp_queue_stats-tx_bytes bb) 1)
        (is (ofp_queue_stats-tx_packets bb) 2)
        (is (ofp_queue_stats-tx_errors bb) 3))
      (is-error (read-byte s) 'end-of-file))
    (let* ((v #(255 255 0 1
                0 1 2 3))
           (s (vs v))
           (h (make-ofp_header :version 1 :type OFPT_STATS_REPLY :length 16 :xid 0))
           (b (make-ofp_stats_reply-stream h s)))
      (is (ofp_stats_reply-type b) OFPST_VENDOR)
      (let ((bb (ofp_stats_reply-body b)))
        (ok (vectorp bb))
        (is bb #(0 1 2 3) :test #'equalp))
      (is-error (read-byte s) 'end-of-file))))

(subtest "ofp_packet_out"
  (let* ((h (make-ofp_header :version 1 :type OFPT_PACKET_OUT :length 32 :xid 0))
         (o (make-ofp_packet_out :header h
                                 :buffer_id 1
                                 :in_port 2
                                 :actions_len 3
                                 :actions (list (make-ofp_action_output :type OFPAT_OUTPUT
                                                                        :len 8
                                                                        :port 1
                                                                        :max_len 0))
                                 :data #(0 1 2 3 4 5 6 7)))
         (expect #(1 #.OFPT_PACKET_OUT 0 32 0 0 0 0
                   0 0 0 1
                   0 2
                   0 3
                   0 #.OFPAT_OUTPUT 0 8 0 1 0 0
                   0 1 2 3 4 5 6 7))
         (dump (with-fast-output (buf) (dump-ofp_packet_out o buf))))
    (is dump expect :test #'equalp)))

(subtest "ofp_vendor"
  (let* ((h (make-ofp_header :version 1 :type OFPT_VENDOR :length 16 :xid 0))
         (v (make-ofp_vendor_header :header h
                                    :vendor 1
                                    :data #(0 1 2 3)))
         (expect #(1 #.OFPT_VENDOR 0 16 0 0 0 0
                   0 0 0 1
                   0 1 2 3))
         (dump (with-fast-output (buf) (dump-ofp_vendor_header v buf))))
    (is dump expect :test #'equalp))
  (let* ((h (make-ofp_header :version 1 :type OFPT_VENDOR :length 16 :xid 0))
         (v #(0 0 0 1 0 1 2 3))
         (s (vs v))
         (b (make-ofp_vendor_header-stream h s)))
    (is (ofp_vendor_header-vendor b) 1)
    (is (ofp_vendor_header-data b) #(0 1 2 3) :test #'equalp)
    (is-error (read-byte s) 'end-of-file)))

(finalize)
