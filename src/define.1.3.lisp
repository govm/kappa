(in-package :cl-user)
(defpackage kappa.define.1.3
  (:use :cl
        :annot)
  (:import-from :kappa.annot
                :export-structure-p)
  (:export :get-constant-name))
(in-package :kappa.define.1.3)

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
  (OFP_VERSION #x04)

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
  (OFPT_EXPERIMENTER 4)
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
  (OFPT_GROUP_MOD 15)
  (OFPT_PORT_MOD 16)
  (OFPT_TABLE_MOD 17)
  (OFPT_MULTIPART_REQUEST 18)
  (OFPT_MULTIPART_REPLY 19)
  (OFPT_BARRIER_REQUEST 20)
  (OFPT_BARRIER_REPLY 21)
  (OFPT_QUEUE_GET_CONFIG_REQUEST 22)
  (OFPT_QUEUE_GET_CONFIG_REPLY 23)
  (OFPT_ROLE_REQUEST 24)
  (OFPT_ROLE_REPLY 25)
  (OFPT_GET_ASYNC_REQUEST 26)
  (OFPT_GET_ASYNC_REPLY 27)
  (OFPT_SET_ASYNC 28)
  (OFPT_METER_MOD 29))

@export-structure-p
(defstruct ofp_port
  port_no
  hw_addr
  name
  config
  state
  curr
  advertised
  supported
  peer
  curr_speed
  max_speed)

(defconstants "ofp_port_config"
  (OFPPC_PORT_DOWN (ash 1 0))
  (OFPPC_NO_RECV (ash 1 2))
  (OFPPC_NO_FLOOD (ash 1 4))
  (OFPPC_NO_FWD (ash 1 5))
  (OFPPC_NO_PACKET_IN (ash 1 6)))

(defconstants "ofp_port_state"
  (OFPPS_LINK_DOWN (ash 1 0))
  (OFPPS_BLOCKED (ash 1 1))
  (OFPPS_LIVE (ash 1 2)))


(defconstants "ofp_port_no"
  (OFPP_MAX #xffffff00)
  (OFPP_IN_PORT #xfffffff8)
  (OFPP_TABLE #xfffffff9)
  (OFPP_NORMAL #xfffffffa)
  (OFPP_FLOOD #xfffffffb)
  (OFPP_ALL #xfffffffc)
  (OFPP_CONTROLLER #xfffffffd)
  (OFPP_LOCAL #xfffffffe)
  (OFPP_ANY #xffffffff))

(defconstants "ofp_port_features"
  (OFPPF_10MB_HD (ash 1 0))
  (OFPPF_10MB_FD (ash 1 1))
  (OFPPF_100MB_HD (ash 1 2))
  (OFPPF_100MB_FD (ash 1 3))
  (OFPPF_1GB_HD (ash 1 4))
  (OFPPF_1GB_FD (ash 1 5))
  (OFPPF_10GB_FD (ash 1 6))
  (OFPPF_40GB_FD (ash 1 7))
  (OFPPF_100GB_FD (ash 1 8))
  (OFPPF_1TB_FD (ash 1 9))
  (OFPPF_OTHER (ash 1 10))
  (OFPPF_COPPER (ash 1 11))
  (OFPPF_FIBER (ash 1 12))
  (OFPPF_AUTONEG (ash 1 13))
  (OFPPF_PAUSE (ash 1 14))
  (OFPPF_PAUSE_ASYM (ash 1 15)))

@export-structure-p
(defstruct ofp_packet_queue
  queue_id
  port
  len
  properties)

(defconstants "ofp_queue_properties"
  (OFPQT_MIN_RATE 1)
  (OFPQT_MAX_RATE 2)
  (OFPQT_EXPERIMENTER #xffff))

@export-structure-p
(defstruct ofp_queue_prop_header
  property
  len)

@export-structure-p
(defstruct ofp_queue_prop_min_rate
  prop_header
  rate)

@export-structure-p
(defstruct ofp_queue_prop_max_rate
  prop_header
  rate)

@export-structure-p
(defstruct ofp_queue_prop_experimenter
  prop_header
  experimenter
  data)

@export-structure-p
(defstruct ofp_match
  type
  length
  oxm_fields)

(defconstants "ofp_match_type"
  (OFPMT_STANDARD 0)
  (OFPMT_OXM 1))

(defconstants "ofp_oxm_class"
  (OFPXMC_NXM_0 #x0000)
  (OFPXMC_NXM_1 #x0001)
  (OFPXMC_OPENFLOW_BASIC #x8000)
  (OFPXMC_EXPERIMENTER #xffff))

(defconstants "oxm_ofb_match_fields"
  (OFPXMT_OFB_IN_PORT 0)
  (OFPXMT_OFB_IN_PHY_PORT 1)
  (OFPXMT_OFB_METADATA 2)
  (OFPXMT_OFB_ETH_DST 3)
  (OFPXMT_OFB_ETH_SRC 4)
  (OFPXMT_OFB_ETH_TYPE 5)
  (OFPXMT_OFB_VLAN_VID 6)
  (OFPXMT_OFB_VLAN_PCP 7)
  (OFPXMT_OFB_IP_DSCP 8)
  (OFPXMT_OFB_IP_ECN 9)
  (OFPXMT_OFB_IP_PROTO 10)
  (OFPXMT_OFB_IPV4_SRC 11)
  (OFPXMT_OFB_IPV4_DST 12)
  (OFPXMT_OFB_TCP_SRC 13)
  (OFPXMT_OFB_TCP_DST 14)
  (OFPXMT_OFB_UDP_SRC 15)
  (OFPXMT_OFB_UDP_DST 16)
  (OFPXMT_OFB_SCTP_SRC 17)
  (OFPXMT_OFB_SCTP_DST 18)
  (OFPXMT_OFB_ICMPV4_TYPE 19)
  (OFPXMT_OFB_ICMPV4_CODE 20)
  (OFPXMT_OFB_ARP_OP 21)
  (OFPXMT_OFB_ARP_SPA 22)
  (OFPXMT_OFB_ARP_TPA 23)
  (OFPXMT_OFB_ARP_SHA 24)
  (OFPXMT_OFB_ARP_THA 25)
  (OFPXMT_OFB_IPV6_SRC 26)
  (OFPXMT_OFB_IPV6_DST 27)
  (OFPXMT_OFB_IPV6_FLABEL 28)
  (OFPXMT_OFB_ICMPV6_TYPE 29)
  (OFPXMT_OFB_ICMPV6_CODE 30)
  (OFPXMT_OFB_IPV6_ND_TARGET 31)
  (OFPXMT_OFB_IPV6_ND_SLL 32)
  (OFPXMT_OFB_IPV6_ND_TLL 33)
  (OFPXMT_OFB_MPLS_LABEL 34)
  (OFPXMT_OFB_MPLS_TC 35)
  (OFPXMT_OFB_MPLS_BOS 36)
  (OFPXMT_OFB_PBB_ISID 37)
  (OFPXMT_OFB_TUNNEL_ID 38)
  (OFPXMT_OFB_IPV6_EXTHDR 39))

(defconstants "ofp_vlan_id"
  (OFPVID_PRESENT #x1000)
  (OFPVID_NONE #x0000))

(defconstants "ofp_ipv6exthdr_flags"
  (OFPIEH_NONEXT (ash 1 0))
  (OFPIEH_ESP (ash 1 1))
  (OFPIEH_AUTH (ash 1 2))
  (OFPIEH_DEST (ash 1 3))
  (OFPIEH_FRAG (ash 1 4))
  (OFPIEH_ROUTER (ash 1 5))
  (OFPIEH_HOP (ash 1 6))
  (OFPIEH_UNREP (ash 1 7))
  (OFPIEH_UNSEQ (ash 1 8)))

@export-structure-p
(defstruct ofp_oxm_experimenter_header
  oxm_header
  experimenter)

(defconstants "ofp_instruction_type"
  (OFPIT_GOTO_TABLE 1)
  (OFPIT_WRITE_METADATA 2)
  (OFPIT_WRITE_ACTIONS 3)
  (OFPIT_APPLY_ACTIONS 4)
  (OFPIT_CLEAR_ACTIONS 5)
  (OFPIT_METER 6)
  (OFPIT_EXPERIMENTER #xffff))

@export-structure-p
(defstruct ofp_instruction_goto_table
  (type OFPIT_GOTO_TABLE)
  (len 8)
  table_id)

@export-structure-p
(defstruct ofp_instruction_write_metadata
  (type OFPIT_WRITE_METADATA)
  (len 24)
  metadata
  metadata_mask)

@export-structure-p
(defstruct ofp_instruction_actions
  type
  len
  actions)

@export-structure-p
(defstruct ofp_instruction_meter
  (type OFPIT_METER)
  (len 8)
  meter_id)

(defconstants "ofp_action_type"
  (OFPAT_OUTPUT 0)
  (OFPAT_COPY_TTL_OUT 11)
  (OFPAT_COPY_TTL_IN 12)
  (OFPAT_SET_MPLS_TTL 15)
  (OFPAT_DEC_MPLS_TTL 16)
  (OFPAT_PUSH_VLAN 17)
  (OFPAT_POP_VLAN 18)
  (OFPAT_PUSH_MPLS 19)
  (OFPAT_POP_MPLS 20)
  (OFPAT_SET_QUEUE 21)
  (OFPAT_GROUP 22)
  (OFPAT_SET_NW_TTL 23)
  (OFPAT_DEC_NW_TTL 24)
  (OFPAT_SET_FIELD 25)
  (OFPAT_PUSH_PBB 26)
  (OFPAT_EXPERIMENTER #xffff))

@export-structure-p
(defstruct ofp_action_header
  type
  len)

@export-structure-p
(defstruct ofp_action_output
  (type OFPAT_OUTPUT)
  (len 16)
  port
  max_len)

(defconstants "ofp_controller_max_len"
  (OFPCML_MAX #xffe5)
  (OFPCML_NO_BUFFER #xffff))

@export-structure-p
(defstruct ofp_action_group
  (type OFPAT_GROUP)
  (len 8)
  group_id)

@export-structure-p
(defstruct ofp_action_set_queue
  (type OFPAT_SET_QUEUE)
  (len 8)
  queue_id)

@export-structure-p
(defstruct ofp_action_mpls_ttl
  (type OFPAT_SET_MPLS_TTL)
  (len 8)
  mpls_ttl)

@export-structure-p
(defstruct ofp_action_nw_ttl
  (type OFPAT_SET_NW_TTL)
  (len 8)
  nw_ttl)

@export-structure-p
(defstruct ofp_action_push
  type
  (len 8)
  ethertype)

@export-structure-p
(defstruct ofp_action_pop_mpls
  (type OFPAT_POP_MPLS)
  (len 8)
  ethertype)

@export-structure-p
(defstruct ofp_action_set_field
  (type OFPAT_SET_FIELD)
  len
  field)

@export-structure-p
(defstruct ofp_action_experimenter_header
  (type OFPAT_EXPERIMENTER)
  len
  experimenter
  data)

@export-structure-p
(defstruct ofp_switch_features
  header
  datapath_id
  n_buffers
  n_tables
  auxiliary_id
  capabilities
  reserved)

(defconstants "ofp_capabilities"
  (OFPC_FLOW_STATS (ash 1 0))
  (OFPC_TABLE_STATS (ash 1 1))
  (OFPC_PORT_STATS (ash 1 2))
  (OFPC_GROUP_STATS (ash 1 3))
  (OFPC_IP_REASM (ash 1 5))
  (OFPC_QUEUE_STATS (ash 1 6))
  (OFPC_PORT_BLOCKED (ash 1 8)))

@export-structure-p
(defstruct ofp_switch_config
  header
  flags
  miss_send_len)

(defconstants "ofp_config_flags"
  (OFPC_FRAG_NORMAL 0)
  (OFPC_FRAG_DROP (ash 1 0))
  (OFPC_FRAG_REASM (ash 1 1))
  (OFPC_FRAG_MASK 3))

(defconstants "ofp_table"
  (OFPTT_MAX #xfe)
  (OFPTT_ALL #xff))

@export-structure-p
(defstruct ofp_table_mod
  header
  table_id
  config)

@export-structure-p
(defstruct ofp_flow_mod
  header
  cookie
  cookie_mask
  table_id
  command
  idle_timeout
  hard_timeout
  priority
  buffer_id
  out_port
  out_group
  flags
  match
  instructions)

(defconstants "ofp_flow_mod_command"
  (OFPFC_ADD 0)
  (OFPFC_MODIFY 1)
  (OFPFC_MODIFY_STRICT 2)
  (OFPFC_DELETE 3)
  (OFPFC_DELETE_STRICT 4))

(defconstants "ofp_flow_mod_flags"
  (OFPFF_SEND_FLOW_REM (ash 1 0))
  (OFPFF_CHECK_OVERLAP (ash 1 1))
  (OFPFF_NO_PKT_COUNTS (ash 1 3))
  (OFPFF_NO_BYT_COUNTS (ash 1 4)))

@export-structure-p
(defstruct ofp_group_mod
  header
  command
  type
  group_id
  buckets)

(defconstants "ofp_group_mod_command"
  (OFPGC_ADD 0)
  (OFPGC_MODIFY 1)
  (OFPGC_DELETE 2))

(defconstants "ofp_group_type"
  (OFPGT_ALL 0)
  (OFPGT_SELECT 1)
  (OFPGT_INDIRECT 2)
  (OFPGT_FF 3))

@export-structure-p
(defstruct ofp_bucket
  len
  weight
  watch_port
  watch_group
  actions)

@export-structure-p
(defstruct ofp_port_mod
  header
  port_no
  hw_addr
  config
  mask
  advertise)

@export-structure-p
(defstruct ofp_meter_mod
  header
  command
  flags
  meter_id
  bands)

(defconstants "ofp_meter"
  (OFPM_MAX #xffff0000)
  (OFPM_SLOWPATH #xfffffffd)
  (OFPM_CONTROLLER #xfffffffe)
  (OFPM_ALL #xffffffff))

(defconstants "ofp_meter_mod_command"
  (OFPMC_ADD 0)
  (OFPMC_MODIFY 1)
  (OFPMC_DELETE 2))

(defconstants "ofp_meter_flags"
  (OFPMF_KBPS (ash 1 0))
  (OFPMF_PKTS (ash 1 1))
  (OFPMF_BURST (ash 1 2))
  (OFPMF_STATS (ash 1 3)))

@export-structure-p
(defstruct ofp_meter_band_header
  type
  len
  rate
  burst_size)

(defconstants "ofp_meter_band_type"
  (OFPMBT_DROP 1)
  (OFPMBT_DSCP_REMARK 2)
  (OFPMBT_EXPERIMENTER #xffff))

@export-structure-p
(defstruct ofp_meter_band_drop
  (type OFPMBT_DROP)
  (len 16)
  rate
  burst_size)

@export-structure-p
(defstruct ofp_meter_band_dscp_remark
  (type OFPMBT_DSCP_REMARK)
  (len 16)
  rate
  burst_size
  prec_level)

@export-structure-p
(defstruct ofp_meter_band_experimenter
  (type OFPMBT_EXPERIMENTER)
  len
  rate
  burst_size
  experimenter
  data)

@export-structure-p
(defstruct ofp_multipart_request
  header
  type
  flags
  body)

(defconstants "ofp_multipart_request_flags"
  (OFPMPF_REQ_MORE (ash 1 0)))

@export-structure-p
(defstruct ofp_multipart_reply
  header
  type
  flags
  body)

(defconstants "ofp_multipart_reply_flags"
  (OFPMPF_REPLY_MORE (ash 1 0)))

(defconstants "ofp_multipart_types"
  (OFPMP_DESC 0)
  (OFPMP_FLOW 1)
  (OFPMP_AGGREGATE 2)
  (OFPMP_TABLE 3)
  (OFPMP_PORT_STATS 4)
  (OFPMP_QUEUE 5)
  (OFPMP_GROUP 6)
  (OFPMP_GROUP_DESC 7)
  (OFPMP_GROUP_FEATURES 8)
  (OFPMP_METER 9)
  (OFPMP_METER_CONFIG 10)
  (OFPMP_METER_FEATURES 11)
  (OFPMP_TABLE_FEATURES 12)
  (OFPMP_PORT_DESC 13)
  (OFPMP_EXPERIMENTER #xffff))

@export-structure-p
(defstruct ofp_desc
  mfr_desc
  hw_desc
  sw_desc
  serial_num
  dp_desc)

@export-structure-p
(defstruct ofp_flow_stats_request
  table_id
  out_port
  out_group
  cookie
  cookie_mask
  match)

@export-structure-p
(defstruct ofp_flow_stats
  length
  table_id
  duration_sec
  duration_nsec
  priority
  idle_timeout
  hard_timeout
  flags
  cookie
  packet_count
  byte_count
  match
  instructions)

@export-structure-p
(defstruct ofp_aggregate_stats_request
  table_id
  out_port
  out_group
  cookie
  cookie_mask
  match)

@export-structure-p
(defstruct ofp_aggregate_stats_reply
  packet_count
  byte_count
  flow_count)

@export-structure-p
(defstruct ofp_table_stats
  table_id
  active_count
  lookup_count
  matched_count)

@export-structure-p
(defstruct ofp_table_features
  length
  table_id
  name
  metadata_match
  metadata_write
  config
  max_entries
  properties)

(defconstants "ofp_table_feature_prop_type"
  (OFPTFPT_INSTRUCTIONS 0)
  (OFPTFPT_INSTRUCTIONS_MISS 1)
  (OFPTFPT_NEXT_TABLES 2)
  (OFPTFPT_NEXT_TABLES_MISS 3)
  (OFPTFPT_WRITE_ACTIONS 4)
  (OFPTFPT_WRITE_ACTIONS_MISS 5)
  (OFPTFPT_APPLY_ACTIONS 6)
  (OFPTFPT_APPLY_ACTIONS_MISS 7)
  (OFPTFPT_MATCH 8)
  (OFPTFPT_WILDCARDS 10)
  (OFPTFPT_WRITE_SETFIELD 12)
  (OFPTFPT_WRITE_SETFIELD_MISS 13)
  (OFPTFPT_APPLY_SETFIELD 14)
  (OFPTFPT_APPLY_SETFIELD_MISS 15)
  (OFPTFPT_EXPERIMENTER #xfffe)
  (OFPTFPT_EXPERIMENTER_MISS #xffff))

@export-structure-p
(defstruct ofp_table_feature_prop_instructions
  type
  length
  instruction_ids)

@export-structure-p
(defstruct ofp_table_feature_prop_next_tables
  type
  length
  next_table_ids)

@export-structure-p
(defstruct ofp_table_feature_prop_actions
  type
  length
  action_ids)

@export-structure-p
(defstruct ofp_table_feature_prop_oxm
  type
  length
  oxm_ids)

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
  collisions
  duration_sec
  duration_nsec)

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
  tx_errors
  duration_sec
  duration_nsec)

@export-structure-p
(defstruct ofp_group_stats_request
  group_id)

@export-structure-p
(defstruct ofp_group_stats
  length
  group_id
  ref_count
  packet_count
  byte_count
  duration_sec
  duration_nsec
  bucket_stats)

@export-structure-p
(defstruct ofp_bucket_counter
  packet_count
  byte_count)

@export-structure-p
(defstruct ofp_group_desc_stats
  length
  type
  group_id
  buckets)

@export-structure-p
(defstruct ofp_group_features
  types
  capabilities
  max_groups
  actions)

(defconstants "ofp_group_capabilities"
  (OFPGFC_SELECT_WEIGHT (ash 1 0))
  (OFPGFC_SELECT_LIVENESS (ash 1 1))
  (OFPGFC_CHAINING (ash 1 2))
  (OFPGFC_CHAINING_CHECKS (ash 1 3)))

@export-structure-p
(defstruct ofp_meter_multipart_request
  meter_id)

@export-structure-p
(defstruct ofp_meter_stats
  meter_id
  len
  flow_count
  packet_in_count
  byte_in_count
  duration_sec
  duration_nsec
  band_stats)

@export-structure-p
(defstruct ofp_meter_band_stats
  packet_band_count
  byte_band_count)

@export-structure-p
(defstruct ofp_meter_config
  length
  flags
  meter_id
  bands)

@export-structure-p
(defstruct ofp_meter_features
  max_meter
  band_types
  capabilities
  max_bands
  max_color)

@export-structure-p
(defstruct ofp_experimenter_multipart_header
  experimenter
  exp_type
  data)

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
(defstruct ofp_packet_out
  header
  buffer_id
  in_port
  actions_len
  actions
  data)

@export-structure-p
(defstruct ofp_role_request
  header
  role
  generation_id)

(defconstants "ofp_controller_role"
  (OFPCR_ROLE_NOCHANGE 0)
  (OFPCR_ROLE_EQUAL 1)
  (OFPCR_ROLE_MASTER 2)
  (OFPCR_ROLE_SLAVE 3))

@export-structure-p
(defstruct ofp_async_config
  header
  packet_in_mask
  port_status_mask
  flow_removed_mask)

@export-structure-p
(defstruct ofp_packet_in
  header
  buffer_id
  total_len
  reason
  table_id
  cookie
  match
  data)

(defconstants "ofp_pakcet_in_reason"
  (OFPR_NO_MATCH 0)
  (OFPR_ACTION 1)
  (OFPR_INVALID_TTL 2))

@export-structure-p
(defstruct ofp_flow_removed
  header
  cookie
  priority
  reason
  table_id
  duration_sec
  duration_nsec
  idle_timeout
  hard_timeout
  packet_count
  byte_count
  match)

(defconstants "ofp_flow_removed_reason"
  (OFPRR_IDLE_TIMEOUT 0)
  (OFPRR_HARD_TIMEOUT 1)
  (OFPRR_DELETE 2)
  (OFPRR_GROUP_DELETE 3))

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
  (OFPET_BAD_INSTRUCTION 3)
  (OFPET_BAD_MATCH 4)
  (OFPET_FLOW_MOD_FAILED 5)
  (OFPET_GROUP_MOD_FAILED 6)
  (OFPET_PORT_MOD_FAILED 7)
  (OFPET_TABLE_MOD_FAILED 8)
  (OFPET_QUEUE_OP_FAILED 9)
  (OFPET_SWITCH_CONFIG_FAILED 10)
  (OFPET_ROLE_REQUEST_FAILED 11)
  (OFPET_METER_MOD_FAILED 12)
  (OFPET_TABLE_FEATURES_FAILED 13)
  (OFPET_EXPERIMENTER #xffff))

(defconstants "ofp_hello_failed_code"
  (OFPHFC_INCOMPATIBLE 0)
  (OFPHFC_EPERM 1))

(defconstants "ofp_bad_request_code"
  (OFPBRC_BAD_VERSION 0)
  (OFPBRC_BAD_TYPE 1)
  (OFPBRC_BAD_MULRIPART 2)
  (OFPBRC_BAD_EXPERIMENTER 3)
  (OFPBRC_BAD_EXP_RTPE 4)
  (OFPBRC_EPERM 5)
  (OFPBRC_BAD_LEN 6)
  (OFPBRC_BUFFER_EMPTY 7)
  (OFPBRC_BUFFER_UNKNOWN 8)
  (OFPBRC_BAD_TABLE_ID 9)
  (OFPBRC_IS_SLAVE 10)
  (OFPBRC_BAD_PORT 11)
  (OFPBRC_BAD_PACKET 12)
  (OFPBRC_MULTIPART_BUFFER_OVERFLOW 13))

(defconstants "ofp_bad_action_code"
  (OFPBAC_BAD_TYPE 0)
  (OFPBAC_BAD_LEN 1)
  (OFPBAC_BAD_EXPERIMENTER 2)
  (OFPBAC_BAD_EXP_TYPE 3)
  (OFPBAC_BAD_OUT_PORT 4)
  (OFPBAC_BAD_ARGUMENT 5)
  (OFPBAC_EPERM 6)
  (OFPBAC_TOO_MANY 7)
  (OFPBAC_BAD_QUEUE 8)
  (OFPBAC_BAD_OUT_GROUP 9)
  (OFPBAC_MATCH_INCONSISTENT 10)
  (OFPBAC_UNSUPPORTED_ORDER 11)
  (OFPBAC_BAD_TAG 12)
  (OFPBAC_BAD_SET_TYPE 13)
  (OFPBAC_BAD_SET_LEN 14)
  (OFPBAC_BAD_SET_ARGUMENT 15))

(defconstants "ofp_bad_instruction_code"
  (OFPBIC_UNKNOWN_INST 0)
  (OFPBIC_UNSUP_INST 1)
  (OFPBIC_BAD_TABLE_ID 2)
  (OFPBIC_UNSUP_METADATA 3)
  (OFPBIC_UNSUP_METADATA_MASK 4)
  (OFPBIC_BAD_EXPERIMENTER 5)
  (OFPBIC_BAD_EXP_TYPE 6)
  (OFPBIC_BAD_LEN 7)
  (OFPBIC_EPERM 8))

(defconstants "ofp_bad_match_code"
  (OFPBMC_BAD_TYPE 0)
  (OFPBMC_BAD_LEN 1)
  (OFPBMC_BAD_TAG 2)
  (OFPBMC_BAD_DL_ADDR_MASK 3)
  (OFPBMC_BAD_NW_ADDR_MASK 4)
  (OFPBMC_BAD_WILDCARDS 5)
  (OFPBMC_BAD_FIELD 6)
  (OFPBMC_BAD_VALUE 7)
  (OFPBMC_BAD_MASK 8)
  (OFPBMC_BAD_PREREQ 9)
  (OFPBMC_DUP_FIELD 10)
  (OFPBMC_EPERM 11))

(defconstants "ofp_flow_mod_failed_code"
  (OFPFMFC_UNKNOWN 0)
  (OFPFMFC_TABLE_FULL 1)
  (OFPFMFC_BAD_TABLE_ID 2)
  (OFPFMFC_OVERLAP 3)
  (OFPFMFC_EPERM 4)
  (OFPFMFC_BAD_TIMEOUT 5)
  (OFPFMFC_BAD_COMMAND 6)
  (OFPFMFC_BAD_FLAGS 7))

(defconstants "ofp_group_mod_failed_code"
  (OFPGMFC_GROUP_EXISTS 0)
  (OFPGMFC_INVALID_GROUP 1)
  (OFPGMFC_WEIGHT_UNSUPPORTED 2)
  (OFPGMFC_OUT_OF_GROUPS 3)
  (OFPGMFC_OUT_OF_BUCKETS 4)
  (OFPGMFC_CHAINING_UNSUPPORTED 5)
  (OFPGMFC_WATCH_UNSUPPORTED 6)
  (OFPGMFC_LOOP 7)
  (OFPGMFC_UNKNOWN_GROUP 8)
  (OFPGMFC_CHAINED_GROUP 9)
  (OFPGMFC_BAD_TYPE 10)
  (OFPGMFC_BAD_COMMAND 11)
  (OFPGMFC_BAD_BUCKET 12)
  (OFPGMFC_BAD_WATCH 13)
  (OFPGMFC_EPERM 14))

(defconstants "ofp_port_mod_failed_code"
  (OFPPMFC_BAD_PORT 0)
  (OFPPMFC_BAD_HW_ADDR 1)
  (OFPPMFC_BAD_CONFIG 2)
  (OFPPMFC_BAD_ADVERTISE 3)
  (OFPPMFC_EPERM 4))

(defconstants "ofp_table_mod_failed_code"
  (OFPTMFC_BAD_TABLE 0)
  (OFPTMFC_BAD_CONFIG 1)
  (OFPTMFC_EPERM 2))

(defconstants "ofp_queue_op_failed_code"
  (OFPQOFC_BAD_PORT 0)
  (OFPQOFC_BAD_QUEUE 1)
  (OFPQOFC_EPERM 2))

(defconstants "ofp_switch_config_failed_code"
  (OFPSCFC_BAD_FLAGS 0)
  (OFPSCFC_BAD_LEN 1)
  (OFPSCFC_EPERM 2))

(defconstants "ofp_role_request_failed_code"
  (OFPRRFC_STALE 0)
  (OFPRRFC_UNSUP 1)
  (OFPRRFC_BAD_ROLE 2))

(defconstants "ofp_meter_mod_failed_code"
  (OFPMMFC_UNKNOWN 0)
  (OFPMMFC_METER_EXISTS 1)
  (OFPMMFC_INVALID_METER 2)
  (OFPMMFC_UNKNOWN_METER 3)
  (OFPMMFC_BAD_COMMAND 4)
  (OFPMMFC_BAD_FLAGS 5)
  (OFPMMFC_BAD_RATE 6)
  (OFPMMFC_BAD_BURST 7)
  (OFPMMFC_BAD_BAND 8)
  (OFPMMFC_BAD_BAND_VALUE 9)
  (OFPMMFC_OUT_OF_METERS 10)
  (OFPMMFC_OUT_OF_BANDS11))

(defconstants "ofp_table_features_failed_code"
  (OFPTFFC_BAD_TABLE 0)
  (OFPTFFC_BAD_METADATA 1)
  (OFPTFFC_BAD_TYPE 2)
  (OFPTFFC_BAD_LEN 3)
  (OFPTFFC_BAD_ARGUMENT 4)
  (OFPTFFC_EPERM 5))

@export-structure-p
(defstruct ofp_error_experimenter_msg
  header
  (type OFPET_EXPERIMENTER)
  exp_type
  experimenter
  data)

@export-structure-p
(defstruct ofp_experimenter_header
  header
  experimenter
  exp_type
  data)
