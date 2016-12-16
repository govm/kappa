(in-package :cl-user)
(defpackage kappa.define.1.0
  (:use :cl
        :annot)
  (:import-from :kappa.annot
                :export-structure-p)
  (:export :get-constant-name))
(in-package :kappa.define.1.0)

(annot:enable-annot-syntax)

(let ((hash (make-hash-table :test #'equalp)))
  (defun defenumtype (type)
    (setf (gethash type hash) (make-hash-table :test #'equalp)))
  (defun %add-constant (type name num)
    (setf (gethash num (gethash type hash)) name))
  (defun get-constant-name (type num)
    (gethash num (gethash type hash))))

(defmacro add-constant (type sym num)
  `(progn
     (%add-constant ,type ,(symbol-name sym) ,num)
     @export
     (defconstant ,sym ,num)))

(defmacro defconstants (type &body body)
  `(progn
     (defenumtype ,type)
     ,@(mapcar #'(lambda (nv) `(add-constant ,type ,(car nv) ,(cadr nv))) body)))

(defconstants "global"
  (OFP_VERSION #x01)

  (OFP_MAX_TABLE_NAME_LEN 32)
  (OFP_MAX_PORT_NAME_LEN 16)

  (OFP_TCP_PORT 6653)
  (OFP_SSL_PORT 6653)

  (OFP_ETH_ALEN 6)

  (OFP_DEFAULT_MISS_SEND_LEN 128)

  (OFP_FLOW_PERMANENT 0)
  (OFP_DEFAULT_PRIORITY 0)

  (OFP_NO_BUFFER #xffffffff)

  (DESC_STR_LEN 256)
  (SERIAL_NUM_LEN 32)

  (OFPQ_ALL #xffffffff)
  (OFPQ_MIN_RATE_UNCFG #xffff)
  (OFPQ_MAX_RATE_UNCFG #xffff)

  (OFPCID_UNDEFINED 0))

(defconstants "ofp_type"
  (OFPT_HELLO 0)
  (OFPT_ERROR 1)
  (OFPT_ECHO_REQUEST 2)
  (OFPT_ECHO_REPLY 3)
  (OFPT_VENDOR 4)
  (OFPT_FEATURES_REQUEST 5)
  (OFPT_FEATURES_REPLY 6)
  (OFPT_GET_CONFIG_REQUEST 7)
  (OFPT_GET_CONFIG_REPLY 8)
  (OFPT_SET_CONFIG 9)
  (OFPT_PACKET_IN 10)
  (OFPT_FLOW_REMOVED 11)
  (OFPT_PORT_STATUS 12)
  (OFPT_PACKET_OUT 13)
  (OFPT_FLOW_MOD 14)
  (OFPT_PORT_MOD 15)
  (OFPT_STATS_REQUEST 16)
  (OFPT_STATS_REPLY 17)
  (OFPT_BARRIER_REQUEST 18)
  (OFPT_BARRIER_REPLY 19)
  (OFPT_QUEUE_GET_CONFIG_REQUEST 20)
  (OFPT_QUEUE_GET_CONFIG_REPLY 21))

@export-structure-p
(defstruct ofp_phy_port
  port_no
  hw_addr
  name
  config
  state
  curr
  advertised
  supported
  peer)

(defconstants "ofp_port_config"
  (OFPPC_PORT_DOWN (ash 1 0))
  (OFPPC_NO_STP (ash 1 1))
  (OFPPC_NO_RECV (ash 1 2))
  (OFPPC_NO_RECV_STP (ash 1 3))
  (OFPPC_NO_FLOOD (ash 1 4))
  (OFPPC_NO_FWD (ash 1 5))
  (OFPPC_NO_PACKET_IN (ash 1 6)))

(defconstants "ofp_port_state"
  (OFPPS_LINK_DOWN (ash 1 0))
  (OFPPS_STP_LISTEN (ash 0 8))
  (OFPPS_STP_LEARN (ash 1 8))
  (OFPPS_STP_FORWARD (ash 2 8))
  (OFPPS_STP_BLOCK (ash 3 8))
  (OFPPS_STP_MACK (ash 3 8)))

(defconstants "ofp_port"
  (OFPP_MAX #xff00)
  (OFPP_IN_PORT #xfff8)
  (OFPP_TABLE #xfff9)
  (OFPP_NORMAL #xfffa)
  (OFPP_FLOOD #xfffb)
  (OFPP_ALL #xfffc)
  (OFPP_CONTROLLER #xfffd)
  (OFPP_LOCAL #xfffe)
  (OFPP_NONE #xffff))

(defconstants "ofp_port_features"
  (OFPPF_10MB_HD (ash 1 0))
  (OFPPF_10MB_FD (ash 1 1))
  (OFPPF_100MB_HD (ash 1 2))
  (OFPPF_100MB_FD (ash 1 3))
  (OFPPF_1GB_HD (ash 1 4))
  (OFPPF_1GB_FD (ash 1 5))
  (OFPPF_10GB_FD (ash 1 6))
  (OFPPF_COPPER (ash 1 7))
  (OFPPF_FIBER (ash 1 8))
  (OFPPF_AUTONEG (ash 1 9))
  (OFPPF_PAUSE (ash 1 10))
  (OFPPF_PAUSE_ASYM (ash 1 11)))

@export-structure-p
(defstruct ofp_packet_queue
  queue_id
  len
  properties)

(defconstants "ofp_queue_properties"
  (OFPQT_NONE 0)
  (OFPQT_MIN_RATE 1))

@export-structure-p
(defstruct ofp_queue_prop_header
  property
  len)

@export-structure-p
(defstruct ofp_queue_prop_min_rate
  prop_header
  rate)

@export-structure-p
(defstruct ofp_match
  wildcards
  in_port
  dl_src
  dl_dst
  dl_vlan
  dl_vlan_pcp
  dl_type
  nw_tos
  nw_proto
  nw_src
  nw_dst
  tp_src
  tp_dst)

(defconstants "ofp_flow_wildcards"
  (OFPFW_IN_PORT (ash 1 0))
  (OFPFW_DL_VLAN (ash 1 1))
  (OFPFW_DL_SRC (ash 1 2))
  (OFPFW_DL_DST (ash 1 3))
  (OFPFW_DL_TYPE (ash 1 4))
  (OFPFW_NW_PROTO (ash 1 5))
  (OFPFW_TP_SRC (ash 1 6))
  (OFPFW_TP_DST (ash 1 7))
  (OFPFW_NW_SRC_SHIFT 8)
  (OFPFW_NW_SRC_BITS 6)
  (OFPFW_NW_SRC_MASK (ash (- (ash 1 OFPFW_NW_SRC_BITS) 1) OFPFW_NW_SRC_SHIFT))
  (OFPFW_NW_SRC_ALL (ash 32 OFPFW_NW_SRC_SHIFT))
  (OFPFW_NW_DST_SHIFT 14)
  (OFPFW_NW_DST_BITS 6)
  (OFPFW_NW_DST_MASK (ash (- (ash 1 OFPFW_NW_DST_BITS) 1) OFPFW_NW_DST_SHIFT))
  (OFPFW_NW_DST_ALL (ash 32 OFPFW_NW_DST_SHIFT))
  (OFPFW_DL_VLAN_PCP (ash 1 20))
  (OFPFW_NW_TOS (ash 1 21))
  (OFPFW_ALL (- (ash 1 22) 1)))

(defconstants "ofp_action_type"
  (OFPAT_OUTPUT 0)
  (OFPAT_SET_VLAN_VID 1)
  (OFPAT_SET_VLAN_PCP 2)
  (OFPAT_STRIP_VLAN 3)
  (OFPAT_SET_DL_SRC 4)
  (OFPAT_SET_DL_DST 5)
  (OFPAT_SET_NW_SRC 6)
  (OFPAT_SET_NW_DST 7)
  (OFPAT_SET_NW_TOS 8)
  (OFPAT_SET_TP_SRC 9)
  (OFPAT_SET_TP_DST 10)
  (OFPAT_ENQUEUE 11)
  (OFPAT_VENDOR #xffff))

@export-structure-p
(defstruct ofp_action_header
  type
  len)

@export-structure-p
(defstruct ofp_action_output
  (type OFPAT_OUTPUT)
  (len 8)
  port
  max_len)

@export-structure-p
(defstruct ofp_action_enqueue
  (type OFPAT_ENQUEUE)
  (len 16)
  port
  queue_id)

@export-structure-p
(defstruct ofp_action_vlan_vid
  (type OFPAT_SET_VLAN_VID)
  (len 8)
  vlan_vid)

@export-structure-p
(defstruct ofp_action_vlan_pcp
  (type OFPAT_SET_VLAN_PCP)
  (len 8)
  vlan_pcp)

@export-structure-p
(defstruct ofp_action_dl_addr
  type
  (len 16)
  dl_addr)

@export-structure-p
(defstruct ofp_action_nw_addr
  type
  (len 8)
  nw_addr)

@export-structure-p
(defstruct ofp_action_nw_tos
  type
  (len 8)
  nw_tos)

@export-structure-p
(defstruct ofp_action_tp_port
  type
  (len 8)
  tp_port)

@export-structure-p
(defstruct ofp_action_vendor_header
  (type OFPAT_VENDOR)
  len
  vendor)

@export-structure-p
(defstruct ofp_switch_features
  header
  datapath_id
  n_buffers
  n_tables
  capabilities
  actions
  ports)

(defconstants "ofp_capabilities"
  (OFPC_FLOW_STATS (ash 1 0))
  (OFPC_TABLE_STATS (ash 1 1))
  (OFPC_PORT_STATS (ash 1 2))
  (OFPC_STP (ash 1 3))
  (OFPC_RESERVED (ash 1 4))
  (OFPC_IP_REASM (ash 1 5))
  (OFPC_QUEUE_STATS (ash 1 6))
  (OFPC_ARP_MATCH_IP (ash 1 7)))

@export-structure-p
(defstruct ofp_switch_config
  header
  flags
  miss_send_len)

(defconstants "ofp_config_flags"
  (OFPC_FRAG_NORMAL 0)
  (OFPC_FRAG_DROP 1)
  (OFPC_FRAG_REASM 2)
  (OFPC_FRAG_MASK 3))

@export-structure-p
(defstruct ofp_flow_mod
  header
  match
  cookie
  command
  idle_timeout
  hard_timeout
  priority
  buffer_id
  out_port
  flags
  actions)

(defconstants "ofp_flow_mod_command"
  (OFPFC_ADD 0)
  (OFPFC_MODIFY 1)
  (OFPFC_MODIFY_STRICT 2)
  (OFPFC_DELETE 3)
  (OFPFC_DELETE_STRICT 4))

(defconstants "ofp_flow_mod_flags"
  (OFPFF_SEND_FLOW_REM (ash 1 0))
  (OFPFF_CHECK_OVERLAP (ash 1 1))
  (OFPFF_EMERG (ash 1 2)))

@export-structure-p
(defstruct ofp_port_mod
  header
  port_no
  hw_addr
  config
  mask
  advertise)

@export-structure-p
(defstruct ofp_queue_get_config_request
  header
  port)

@export-structure-p
(defstruct ofp_queue_get_config_reply
  header
  port
  queues)

@export-structure-p
(defstruct ofp_stats_request
  header
  type
  flags
  body)

@export-structure-p
(defstruct ofp_stats_reply
  header
  type
  flags
  body)

(defconstants "ofp_stats_types"
  (OFPST_DESC 0)
  (OFPST_FLOW 1)
  (OFPST_AGGREGATE 2)
  (OFPST_TABLE 3)
  (OFPST_PORT 4)
  (OFPST_QUEUE 5)
  (OFPST_VENDOR #xffff))

@export-structure-p
(defstruct ofp_desc_stats
  mfr_desc
  hw_desc
  sw_desc
  serial_num
  dp_desc)

@export-structure-p
(defstruct ofp_flow_stats_request
  match
  table_id
  out_port)

@export-structure-p
(defstruct ofp_flow_stats
  length
  table_id
  match
  duration_sec
  duration_nsec
  priority
  idle_timeout
  hard_timeout
  cookie
  packet_count
  byte_count
  actions)

@export-structure-p
(defstruct ofp_aggregate_stats_request
  match
  table_id
  out_port)

@export-structure-p
(defstruct ofp_aggregate_stats_reply
  packet_count
  byte_count
  flow_count)

@export-structure-p
(defstruct ofp_table_stats
  table_id
  name
  wildcards
  max_entries
  active_count
  lookup_count
  matched_count)

@export-structure-p
(defstruct ofp_port_stats_request
  port_no)

@export-structure-p
(defstruct ofp_port_stats
  port_no
  rx_packets
  tx_packets
  rx_bytes
  tx_bytes
  rx_dropped
  tx_dropped
  rx_errors
  tx_errors
  rx_frame_err
  rx_over_err
  rx_crc_err
  collisions)

@export-structure-p
(defstruct ofp_queue_stats_request
  port_no
  queue_id)

@export-structure-p
(defstruct ofp_queue_stats
  port_no
  queue_id
  tx_bytes
  tx_packets
  tx_errors)

@export-structure-p
(defstruct ofp_packet_out
  header
  buffer_id
  in_port
  actions_len
  actions
  data)

@export-structure-p
(defstruct ofp_packet_in
  header
  buffer_id
  total_len
  in_port
  reason
  data)

(defconstants "ofp_pakcet_in_reason"
  (OFPR_NO_MATCH 0)
  (OFPR_ACTION 1))

@export-structure-p
(defstruct ofp_flow_removed
  header
  match
  cookie
  priority
  reason
  duration_sec
  duration_nsec
  idle_timeout
  packet_count
  byte_count)

(defconstants "ofp_flow_removed_reason"
  (OFPRR_IDLE_TIMEOUT 0)
  (OFPRR_HARD_TIMEOUT 1)
  (OFPRR_DELETE 2))

@export-structure-p
(defstruct ofp_port_status
  header
  reason
  desc)

(defconstants "ofp_port_reason"
  (OFPPR_ADD 0)
  (OFPPR_DELETE 1)
  (OFPPR_MODIFY 2))

@export-structure-p
(defstruct ofp_error_msg
  header
  type
  code
  data)

(defconstants "ofp_error_type"
  (OFPET_HELLO_FAILED 0)
  (OFPET_BAD_REQUEST 1)
  (OFPET_BAD_ACTION 2)
  (OFPET_FLOW_MOD_FAILED 3)
  (OFPET_PORT_MOD_FAILED 4)
  (OFPET_QUEUE_OP_FAILED 5))

(defconstants "ofp_hello_failed_code"
  (OFPHFC_INCOMPATIBLE 0)
  (OFPHFC_EPERM 1))

(defconstants "ofp_bad_request_code"
  (OFPBRC_BAD_VERSION 0)
  (OFPBRC_BAD_TYPE 1)
  (OFPBRC_BAD_STAT 2)
  (OFPBRC_BAD_VENDOR 3)
  (OFPBRC_BAD_SUBTYPE 4)
  (OFPBRC_EPERM 5)
  (OFPBRC_BAD_LEN 6)
  (OFPBRC_BUFFER_EMPTY 7)
  (OFPBRC_BUFFER_UNKNOWN 8))

(defconstants "ofp_bad_action_code"
  (OFPBAC_BAD_TYPE 0)
  (OFPBAC_BAD_LEN 1)
  (OFPBAC_BAD_VENDOR 2)
  (OFPBAC_BAD_VENDOR_TYPE 3)
  (OFPBAC_BAD_OUT_PORT 4)
  (OFPBAC_BAD_ARGUMENT 5)
  (OFPBAC_EPERM 6)
  (OFPBAC_TOO_MANY 7)
  (OFPBAC_BAD_QUEUE 8))

(defconstants "ofp_flow_mod_failed_code"
  (OFPFMFC_ALL_TABLES_FULL 0)
  (OFPFMFC_OVERLAP 1)
  (OFPFMFC_EPERM 2)
  (OFPFMFC_BAD_EMERG_TIMEOUT 3)
  (OFPFMFC_BAD_COMMAND 4)
  (OFPFMFC_UNSUPPORTED 5))

(defconstants "ofp_port_mod_failed_code"
  (OFPPMFC_BAD_PORT 0)
  (OFPPMFC_BAD_HW_ADDR 1))

(defconstants "ofp_queue_op_failed_code"
  (OFPQOFC_BAD_PORT 0)
  (OFPQOFC_BAD_QUEUE 1)
  (OFPQOFC_EPERM 2))

@export-structure-p
(defstruct ofp_vendor_header
  header
  vendor
  data)
