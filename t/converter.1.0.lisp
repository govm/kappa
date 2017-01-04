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


(plan 32)

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
            #.(char-int #\s)
            #.(char-int #\0)
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
                                                                    :max_len 16))))
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
                  0 16))
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

(finalize)
