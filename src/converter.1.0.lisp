(in-package :cl-user)
(defpackage kappa.converter.1.0
  (:use :cl
        :fast-io
        :anaphora
        :kappa.converter
        :kappa.define.1.0)
  (:import-from kappa.define
                :ofp_header-length)
  (:import-from :babel
                :octets-to-string))
(in-package :kappa.converter.1.0)

(annot:enable-annot-syntax)

(defun make-ofp_phy_port-buffer (buf)
  (make-ofp_phy_port :port_no (readu16-be buf)
                     :hw_addr (let ((vec (make-octet-vector OFP_ETH_ALEN)))
                                (fast-read-sequence vec buf 0 OFP_ETH_ALEN)
                                vec)
                     :name (let ((vec (make-octet-vector OFP_MAX_PORT_NAME_LEN)))
                             (fast-read-sequence vec buf 0 OFP_MAX_PORT_NAME_LEN)
                             (octets-to-string vec :end (position 0 vec)))
                     :config (readu32-be buf)
                     :state (readu32-be buf)
                     :curr (readu32-be buf)
                     :advertised (readu32-be buf)
                     :supported (readu32-be buf)
                     :peer (readu32-be buf))) ; 48

@export
(defun make-ofp_switch_features-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8)))
    (with-fast-input (buf nil stream)
      (make-ofp_switch_features :header header
                                :datapath_id (readu64-be buf)
                                :n_buffers (readu32-be buf)
                                :n_tables (readu8-be buf)
                                :capabilities (progn
                                                (readu16-be buf) ; pad
                                                (readu8-be buf) ; pad
                                                (readu32-be buf))
                                :actions (readu32-be buf)
                                :ports (loop :repeat (truncate (- rest 24) 48)
                                             :collect (make-ofp_phy_port-buffer buf))))))

@export
(defun make-ofp_switch_config-stream (header stream)
  (with-fast-input (buf nil stream)
    (make-ofp_switch_config :header header
                            :flags (readu16-be buf)
                            :miss_send_len (readu16-be buf))))

@export
(defun dump-ofp_switch_config (config buf)
  (dump-ofp_header (ofp_switch_config-header config) buf)
  (writeu16-be (ofp_switch_config-flags config) buf)
  (writeu16-be (ofp_switch_config-miss_send_len config) buf))

@export
(defun dump-ofp_port_mod (mod buf)
  (dump-ofp_header (ofp_port_mod-header mod) buf)
  (writeu16-be (ofp_port_mod-port_no mod) buf)
  (loop :for i :across (ofp_port_mod-hw_addr mod)
        :do (writeu8-be i buf))
  (writeu32-be (ofp_port_mod-config mod) buf)
  (writeu32-be (ofp_port_mod-mask mod) buf)
  (writeu32-be (ofp_port_mod-advertise mod) buf)
  (writeu32-be 0 buf)) ; pad

@export
(defun dump-ofp_stats_request (req buf)
  (dump-ofp_header (ofp_stats_request-header req) buf)
  (writeu16-be (ofp_stats_request-type req) buf)
  (writeu16-be (ofp_stats_request-flags req) buf)
  (let ((b (ofp_stats_request-body req)))
    (cond ((ofp_flow_stats_request-p b)
           (progn
             (dump-ofp_match (ofp_flow_stats_request-match b) buf)
             (writeu8-be (ofp_flow_stats_request-table_id b) buf)
             (writeu8-be 0 buf) ; pad
             (writeu16-be (ofp_flow_stats_request-out_port b) buf)))
          ((ofp_aggregate_stats_request-p b)
           (progn
             (dump-ofp_match (ofp_aggregate_stats_request-match b) buf)
             (writeu8-be (ofp_aggregate_stats_request-table_id b) buf)
             (writeu8-be 0 buf) ; pad
             (writeu16-be (ofp_aggregate_stats_request-out_port b) buf)))
          ((ofp_port_stats_request-p b)
           (progn
             (writeu16-be (ofp_port_stats_request-port_no b) buf)
             (loop :repeat 6 :do (writeu8-be 0 buf)))) ; pad
          ((ofp_queue_stats_request-p b)
           (progn
             (writeu16-be (ofp_queue_stats_request-port_no b) buf)
             (writeu16-be 0 buf) ; pad
             (writeu32-be (ofp_queue_stats_request-queue_id b) buf)))
          ((vectorp b)
           (loop :for i :across b :do (fast-write-byte i buf))))))

@export
(defun make-ofp_stats_reply-stream (header stream)
  (with-fast-input (buf nil stream)
    (let* ((rep (make-ofp_stats_reply :header header
                                      :type (readu16-be buf)
                                      :flags (readu16-be buf)
                                      :body nil))
           (type (ofp_stats_reply-type rep)))
      (setf (ofp_stats_reply-body rep)
            (cond ((= type OFPST_DESC)
                   (make-ofp_desc_stats :mfr_desc (let ((vec (make-octet-vector DESC_STR_LEN)))
                                                    (fast-read-sequence vec buf 0 DESC_STR_LEN)
                                                    (octets-to-string vec :end (position 0 vec)))
                                        :hw_desc (let ((vec (make-octet-vector DESC_STR_LEN)))
                                                   (fast-read-sequence vec buf 0 DESC_STR_LEN)
                                                   (octets-to-string vec :end (position 0 vec)))
                                        :sw_desc (let ((vec (make-octet-vector DESC_STR_LEN)))
                                                   (fast-read-sequence vec buf 0 DESC_STR_LEN)
                                                   (octets-to-string vec :end (position 0 vec)))
                                        :serial_num (let ((vec (make-octet-vector SERIAL_NUM_LEN)))
                                                      (fast-read-sequence vec buf 0 SERIAL_NUM_LEN)
                                                      (octets-to-string vec :end (position 0 vec)))
                                        :dp_desc (let ((vec (make-octet-vector DESC_STR_LEN)))
                                                   (fast-read-sequence vec buf 0 DESC_STR_LEN)
                                                   (octets-to-string vec :end (position 0 vec)))))
                  ((= type OFPST_FLOW)
                   (let ((flow (make-ofp_flow_stats :length (readu16-be buf)
                                                    :table_id (readu8-be buf)
                                                    :match (progn
                                                             (readu8-be buf) ; pad
                                                             (make-ofp_match-buffer buf))
                                                    :duration_sec (readu32-be buf)
                                                    :duration_nsec (readu32-be buf)
                                                    :priority (readu16-be buf)
                                                    :idle_timeout (readu16-be buf)
                                                    :hard_timeout (readu16-be buf)
                                                    :cookie (progn
                                                              (loop :repeat 6 :do (readu8-be buf)) ; pad
                                                              (readu64-be buf))
                                                    :packet_count (readu64-be buf)
                                                    :byte_count (readu64-be buf)
                                                    :actions nil)))
                     (setf (ofp_flow_stats-actions flow) (make-ofp_actions-buffer buf
                                                                                  (- (ofp_flow_stats-length flow) 88)))
                     flow))
                  ((= type OFPST_AGGREGATE)
                   (make-ofp_aggregate_stats_reply :packet_count (readu64-be buf)
                                                   :byte_count (readu64-be buf)
                                                   :flow_count (prog1
                                                                 (readu32-be buf)
                                                                 (readu32-be buf)))) ;pad
                  ((= type OFPST_TABLE)
                   (make-ofp_table_stats :table_id (readu8-be buf)
                                         :name (progn
                                                 (loop :repeat 3 :do (readu8-be buf)) ; pad
                                                 (let ((vec (make-octet-vector OFP_MAX_TABLE_NAME_LEN)))
                                                   (fast-read-sequence vec buf 0 OFP_MAX_TABLE_NAME_LEN)
                                                   (octets-to-string vec :end (position 0 vec))))
                                         :wildcards (readu32-be buf)
                                         :max_entries (readu32-be buf)
                                         :active_count (readu32-be buf)
                                         :lookup_count (readu64-be buf)
                                         :matched_count (readu64-be buf)))
                  ((= type OFPST_PORT)
                   (make-ofp_port_stats :port_no (readu16-be buf)
                                        :rx_packets (progn
                                                      (loop :repeat 6 :do (readu8-be buf)) ; pad
                                                      (readu64-be buf))
                                        :tx_packets (readu64-be buf)
                                        :rx_bytes (readu64-be buf)
                                        :tx_bytes (readu64-be buf)
                                        :rx_dropped (readu64-be buf)
                                        :tx_dropped (readu64-be buf)
                                        :rx_errors (readu64-be buf)
                                        :tx_errors (readu64-be buf)
                                        :rx_frame_err (readu64-be buf)
                                        :rx_over_err (readu64-be buf)
                                        :rx_crc_err (readu64-be buf)
                                        :collisions (readu64-be buf)))
                  ((= type OFPST_QUEUE)
                   (make-ofp_queue_stats :port_no (readu16-be buf)
                                         :queue_id (progn
                                                     (readu16-be buf) ; pad
                                                     (readu32-be buf))
                                         :tx_bytes (readu64-be buf)
                                         :tx_packets (readu64-be buf)
                                         :tx_errors (readu64-be buf)))
                  ((= type OFPST_VENDOR)
                   (let* ((blen (- (ofp_header-length header) 12))
                          (vec (make-octet-vector blen)))
                     (fast-read-sequence vec buf 0 blen)
                     vec))
                  (t nil)))
      rep)))

@export
(defun dump-ofp_packet_out (out buf)
  (dump-ofp_header (ofp_packet_out-header out) buf)
  (writeu32-be (ofp_packet_out-buffer_id out) buf)
  (writeu16-be (ofp_packet_out-in_port out) buf)
  (writeu16-be (ofp_packet_out-actions_len out) buf)
  (dump-ofp_actions (ofp_packet_out-actions out) buf)
  (loop :for i :across (ofp_packet_out-data out)
        :do (fast-write-byte i buf)))

@export
(defun make-ofp_packet_in-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8)))
    (with-fast-input (buf nil stream)
      (make-ofp_packet_in :header header
                          :buffer_id (readu32-be buf)
                          :total_len (readu16-be buf)
                          :in_port (readu16-be buf)
                          :reason (readu8-be buf)
                          :data (let ((vec (make-octet-vector (- rest 10))))
                                  (readu8-be buf) ; pad
                                  (fast-read-sequence vec buf 0 (- rest 10))
                                  vec)))))

@export
(defun dump-ofp_match (match buf)
  (aif (ofp_match-wildcards match)
    (writeu32-be it buf)
    (writeu32-be 0 buf))
  (aif (ofp_match-in_port match)
    (writeu16-be it buf)
    (writeu16-be 0 buf))
  (aif (ofp_match-dl_src match)
    (loop :for i :across it
          :do (fast-write-byte i buf))
    (loop :repeat OFP_ETH_ALEN
          :do (fast-write-byte 0 buf)))
  (aif (ofp_match-dl_dst match)
    (loop :for i :across it
          :do (fast-write-byte i buf))
    (loop :repeat OFP_ETH_ALEN
          :do (fast-write-byte 0 buf)))
  (aif (ofp_match-dl_vlan match)
    (writeu16-be it buf)
    (writeu16-be 0 buf))
  (aif (ofp_match-dl_vlan_pcp match)
    (writeu8-be it buf)
    (writeu8-be 0 buf))
  (writeu8-be 0 buf) ; pad
  (aif (ofp_match-dl_type match)
    (writeu16-be it buf)
    (writeu16-be 0 buf))
  (aif (ofp_match-nw_tos match)
    (writeu8-be it buf)
    (writeu8-be 0 buf))
  (aif (ofp_match-nw_proto match)
    (writeu8-be it buf)
    (writeu8-be 0 buf))
  (writeu16-be 0 buf) ; pad
  (aif (ofp_match-nw_src match)
    (writeu32-be it buf)
    (writeu32-be 0 buf))
  (aif (ofp_match-nw_dst match)
    (writeu32-be it buf)
    (writeu32-be 0 buf))
  (aif (ofp_match-tp_src match)
    (writeu16-be it buf)
    (writeu16-be 0 buf))
  (aif (ofp_match-tp_dst match)
    (writeu16-be it buf)
    (writeu16-be 0 buf)))

@export
(defun make-ofp_match-buffer (buf)
  (make-ofp_match :wildcards (readu32-be buf)
                  :in_port (readu16-be buf)
                  :dl_src (let ((vec (make-octet-vector OFP_ETH_ALEN)))
                            (fast-read-sequence vec buf 0 OFP_ETH_ALEN)
                            vec)
                  :dl_dst (let ((vec (make-octet-vector OFP_ETH_ALEN)))
                            (fast-read-sequence vec buf 0 OFP_ETH_ALEN)
                            vec)
                  :dl_vlan (readu16-be buf)
                  :dl_vlan_pcp (readu8-be buf)
                  :dl_type (progn
                             (readu8-be buf) ; pad
                             (readu16-be buf))
                  :nw_tos (readu8-be buf)
                  :nw_proto (readu8-be buf)
                  :nw_src (progn
                            (readu16-be buf) ; pad
                            (readu32-be buf))
                  :nw_dst (readu32-be buf)
                  :tp_src (readu16-be buf)
                  :tp_dst (readu16-be buf)))

@export
(defun dump-ofp_flow_mod (mod buf)
  (dump-ofp_header (ofp_flow_mod-header mod) buf)
  (dump-ofp_match (ofp_flow_mod-match mod) buf)
  (writeu64-be (ofp_flow_mod-cookie mod) buf)
  (writeu16-be (ofp_flow_mod-command mod) buf)
  (writeu16-be (ofp_flow_mod-idle_timeout mod) buf)
  (writeu16-be (ofp_flow_mod-hard_timeout mod) buf)
  (writeu16-be (ofp_flow_mod-priority mod) buf)
  (writeu32-be (ofp_flow_mod-buffer_id mod) buf)
  (writeu16-be (ofp_flow_mod-out_port mod) buf)
  (writeu16-be (ofp_flow_mod-flags mod) buf)
  (aif (ofp_flow_mod-actions mod)
    (dump-ofp_actions it buf)))

(defun dump-ofp_actions (actions buf)
  (loop :for a :in actions
        :do (cond ((ofp_action_output-p a)
                   (progn
                     (writeu16-be (ofp_action_output-type a) buf)
                     (writeu16-be (ofp_action_output-len a) buf)
                     (writeu16-be (ofp_action_output-port a) buf)
                     (writeu16-be (ofp_action_output-max_len a) buf)))
                   ((ofp_action_enqueue-p a)
                    (progn
                      (writeu16-be (ofp_action_enqueue-type a) buf)
                      (writeu16-be (ofp_action_enqueue-len a) buf)
                      (writeu16-be (ofp_action_enqueue-port a) buf)
                      (loop :repeat 6 :do (writeu8-be 0 buf)) ; pad
                      (writeu32-be (ofp_action_enqueue-queue_id a) buf)))
                   ((ofp_action_vlan_vid-p a)
                    (progn
                      (writeu16-be (ofp_action_vlan_vid-type a) buf)
                      (writeu16-be (ofp_action_vlan_vid-len a) buf)
                      (writeu16-be (ofp_action_vlan_vid-vlan_vid a) buf)
                      (writeu16-be 0 buf))) ; pad
                   ((ofp_action_vlan_pcp-p a)
                    (progn
                      (writeu16-be (ofp_action_vlan_pcp-type a) buf)
                      (writeu16-be (ofp_action_vlan_pcp-len a) buf)
                      (writeu8-be (ofp_action_vlan_pcp-vlan_pcp a) buf)
                      (loop :repeat 3 :do (writeu8-be 0 buf)))) ; pad
                   ((ofp_action_dl_addr-p a)
                    (progn
                      (writeu16-be (ofp_action_dl_addr-type a) buf)
                      (writeu16-be (ofp_action_dl_addr-len a) buf)
                      (loop :for i :across (ofp_action_dl_addr-dl_addr a)
                            :do (fast-write-byte i buf))
                      (loop :repeat 6 :do (writeu8-be 0 buf)))) ; pad
                   ((ofp_action_nw_addr-p a)
                    (progn
                      (writeu16-be (ofp_action_nw_addr-type a) buf)
                      (writeu16-be (ofp_action_nw_addr-len a) buf)
                      (writeu32-be (ofp_action_nw_addr-nw_addr a) buf)))
                   ((ofp_action_nw_tos-p a)
                    (progn
                      (writeu16-be (ofp_action_nw_tos-type a) buf)
                      (writeu16-be (ofp_action_nw_tos-len a) buf)
                      (writeu8-be (ofp_action_nw_tos-nw_tos a) buf)
                      (loop :repeat 3 :do (writeu8-be 0 buf)))) ; pad
                   ((ofp_action_tp_port-p a)
                    (progn
                      (writeu16-be (ofp_action_tp_port-type a) buf)
                      (writeu16-be (ofp_action_tp_port-len a) buf)
                      (writeu16-be (ofp_action_tp_port-tp_port a) buf)
                      (writeu16-be 0 buf))) ; pad
                   ((ofp_action_vendor_header-p a)
                    (progn
                      (writeu16-be (ofp_action_vendor_header-type a) buf)
                      (writeu16-be (ofp_action_vendor_header-len a) buf)
                      (writeu32-be (ofp_action_vendor_header-vendor a) buf)
                      (loop :for i :across (ofp_action_vendor_header-body a)
                            :do (fast-write-byte i buf))))
                  (t nil))))

@export
(defun make-ofp_actions-buffer (buf len)
  (loop :while (> len 0)
        :collect (let ((type (readu16-be buf))
                       (blen (readu16-be buf)))
                   (cond ((= type OFPAT_OUTPUT)
                          (progn
                            (setf len (- len 8))
                            (make-ofp_action_output :type type
                                                    :len blen
                                                    :port (readu16-be buf)
                                                    :max_len (readu16-be buf))))
                         ((= type OFPAT_ENQUEUE)
                          (progn
                            (setf len (- len 16))
                            (make-ofp_action_enqueue :type type
                                                     :len blen
                                                     :port (readu16-be buf)
                                                     :queue_id (progn
                                                                 (loop :repeat 6 :do (readu8-be buf)) ; pad
                                                                 (readu32-be buf)))))
                         ((= type OFPAT_SET_VLAN_VID)
                          (progn
                            (setf len (- len 8))
                            (make-ofp_action_vlan_vid :type type
                                                      :len blen
                                                      :vlan_vid (prog1
                                                                  (readu16-be buf)
                                                                  (readu16-be buf))))) ; pad
                         ((= type OFPAT_SET_VLAN_PCP)
                          (progn
                            (setf len (- len 8))
                            (make-ofp_action_vlan_pcp :type type
                                                      :len blen
                                                      :blan_pcp (prog1
                                                                  (readu8-be buf)
                                                                  (loop :repeat 3 :do (readu8-be buf)))))) ; pad
                         ((or (= type OFPAT_SET_DL_SRC) (= type OFPAT_SET_DL_DST))
                          (progn
                            (setf len (- len 16))
                            (make-ofp_action_dl_addr :type type
                                                     :len blen
                                                     :dl_addr (let ((vec (make-octet-vector OFP_ETH_ALEN)))
                                                                (fast-read-sequence vec buf 0 OFP_ETH_ALEN)
                                                                (loop :repeat 6 :do (readu8-be buf)) ; pad
                                                                vec))))
                         ((or (= type OFPAT_SET_NW_SRC) (= type OFPAT_SET_NW_DST))
                          (progn
                            (setf len (- len 8))
                            (make-ofp_action_nw_addr :type type
                                                     :len blen
                                                     :nw_addr (readu32-be buf))))
                         ((= type OFPAT_SET_NW_TOS)
                          (progn
                            (setf len (- len 8))
                            (make-ofp_action_nw_tos :type type
                                                    :len blen
                                                    :nw_tos (prog1
                                                              (readu8-be buf)
                                                              (loop :repeat 3 :do (readu8-be buf)))))) ; pad
                         ((or (= type OFPAT_SET_TP_SRC) (= type OFPAT_SET_TP_DST))
                          (progn
                            (setf len (- len 8))
                            (make-ofp_action_tp_port :type type
                                                     :len blen
                                                     :tp_port (prog1
                                                                (readu16-be buf)
                                                                (readu16-be buf))))) ; pad
                         ((= type OFPAT_VENDOR)
                          (progn
                            (setf len (- len blen))
                            (make-ofp_action_vendor_header :type type
                                                           :len blen
                                                           :vendor (readu32-be buf)
                                                           :body (let ((vec (make-octet-vector (- blen 8))))
                                                                   (fast-read-sequence vec buf 0 (- blen 8))
                                                                   vec))))))))

@export
(defun make-ofp_flow_removed-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8)))
    (with-fast-input (buf nil stream)
      (make-ofp_flow_removed :header header
                             :match (make-ofp_match-buffer buf)
                             :cookie (readu64-be buf)
                             :priority (readu16-be buf)
                             :reason (readu8-be buf)
                             :duration_sec (progn
                                             (readu8-be buf) ; pad
                                             (readu32-be buf))
                             :duration_nsec (readu32-be buf)
                             :idle_timeout (readu16-be buf)
                             :packet_count (progn
                                             (readu16-be buf) ; pad
                                             (readu64-be buf))
                             :byte_count (readu64-be buf)))))

@export
(defun make-ofp_port_status-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8)))
    (with-fast-input (buf nil stream)
      (make-ofp_port_status :header header
                            :reason (readu8-be buf)
                            :desc (progn
                                    (loop :repeat 7
                                          :do (readu8-be buf)) ; pad
                                    (make-ofp_phy_port-buffer buf))))))

@export
(defun make-ofp_error_msg-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8)))
    (with-fast-input (buf nil stream)
      (make-ofp_error_msg :header header
                          :type (readu16-be buf)
                          :code (readu16-be buf)
                          :data (let ((vec (make-octet-vector (- rest 4))))
                                  (fast-read-sequence vec buf 0 (- rest 4))
                                  vec)))))
