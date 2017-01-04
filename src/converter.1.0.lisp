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
                  (t nil))))

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
