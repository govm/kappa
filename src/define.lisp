(in-package :cl-user)
(defpackage kappa.define
  (:use :cl)
  (:import-from :usocket
                :socket-server)
  (:export :ofp_header))
(in-package :kappa.define)


(defmacro defconstants (doc &body body)
  (declare (ignore doc))
  `(progn
     ,@(mapcar #'(lambda (nv) `(export (defconstant ,(car nv) ,(cadr nv)))) body)))

(defstruct ofp_header
  version
  type
  length
  xid)

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
  (OFPT_ROLE_REQUEST 24)
  (OFPT_ROLE_REPLY 25)
  (OFPT_GET_ASYNC_REQUEST 26)
  (OFPT_GET_ASYNC_REPLY 27)
  (OFPT_SET_ASYNC 28)
  (OFPT_METER_MOD 29)
  (OFPT_ROLE_STATUS 30)
  (OFPT_TABLE_STATUS 31)
  (OFPT_REQUESTFORWARD 32)
  (OFPT_BUNDLE_CONTROL 33)
  (OFPT_BUNDLE_ADD_MESSAGE 34)
  (OFPT_CONTROLLER_STATUS 35))

(defconstants "ofp_port_no"
  (OFPP_MAX #xffffff00)
  (OFPP_UNSET #xfffffff7)
  (OFPP_IN_PORT #xfffffff8)
  (OFPP_TABLE #xfffffff9)
  (OFPP_NORMAL #xfffffffa)
  (OFPP_FLOOD #xfffffffb)
  (OFPP_ALL #xfffffffc)
  (OFPP_CONTROLLER #xfffffffd)
  (OFPP_LOCAL #xfffffffe)
  (OFPP_ANY #xffffffff))

(defstruct ofp_port
  port_no
  length
  hw_addr
  name
  config
  state
  properties)

(defconstants "ofp_port_config"
  (OFPPC_PORT_DOWN (ash 1 0))
  (OFPPC_NO_RECV (ash 1 2))
  (OFPPC_NO_FWD (ash 1 5))
  (OFPPC_NO_PACKET_IN (ash 1 6)))

(defconstants "ofp_port_state"
  (OFPPS_LINK_DOWN (ash 1 0))
  (OFPPS_BLOCKED (ash 1 1))
  (OFPPS_LIVE (ash 1 2)))

(defconstants "ofp_port_desc_prop_type"
  (OFPPDPT_ETHERNET 0)
  (OFPPDPT_OPTICAL 1)
  (OFPPDPT_PIPELINE_INPUT 2)
  (OFPPDPT_PIPELINE_OUTPUT 3)
  (OFPPDPT_RECIRCULATE 4)
  (OFPPDPT_EXPERIMENTER #xffff))

(defstruct ofp_port_desc_prop_header
  type
  length)

(defstruct ofp_port_desc_prop_ethernet
  type
  length
  curr
  advertised
  supported
  peer
  curr_speed
  max_speed)

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

(defstruct ofp_port_desc_prop_optical
  type
  length
  supported
  tx_min_freq_lmda
  tx_max_freq_lmda
  tx_grid_freq_lmda
  rx_min_freq_lmda
  rx_max_freq_lmda
  rx_grid_freq_lmda
  tx_pwr_min
  tx_pwr_max)

(defconstants "ofp_optical_port_features"
  (OFPOPF_RX_TUNE (ash 1 0))
  (OFPOPF_TX_TUNE (ash 1 1))
  (OFPOPF_TX_PWR (ash 1 2))
  (OFPOPF_USE_FREQ (ash 1 3)))

(defstruct ofp_port_desc_prop_oxm
  type
  length
  oxm_ids)

(defstruct ofp_port_desc_prop_recirculate
  type
  length
  port_nos)

(defstruct ofp_port_desc_prop_experimentar
  type
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_header_type
  namespace
  ns_type)

(defconstants "ofp_header_type_namespaces"
  (OFPHTN_ONF 0)
  (OFPHTN_ETHERTYPE 1)
  (OFPHTN_IP_PROTO 2)
  (OFPHTN_UDP_TCP_PORT 3)
  (OFPHTN_IPV4_OPTION 4))

(defconstants "ofp_header_type_onf"
  (OFPHTO_ETHERNET 0)
  (OFPHTO_NO_HEADER 1)
  (OFPHTO_OXM_EXPERIMENTER #xffff))

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
  (OFPXMC_PACKET_REGS #x8001)
  (OFPXMC_EXPERIMENTER #xffff))

(defconstants "oxm_ofb_match_fields"
  (OFPXMT_OFB_IN_PORT 0)
  (OFPXMT_OFB_IN_PHY_PORT 1)
  (OFPXMT_OFB_METADATA 2)
  (OFPXMT_OFB_ETH_DST 3)
  (OFPXMT_OFB_ETH_SRC 4)
  (OFPXMT_OFB_ETH_TYPE 5)
  (OFPXMT_OFB_VLAN_VID 6)
  (OFPXMT_VLAN_PCP 7)
  (OFPXMT_IP_DSCP 8)
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
  (OFPXMT_TUNNEL_ID 38)
  (OFPXMT_OFB_IPV6EXTHDR 39)
  (OFPXMT_OFB_PBB_UCA 41)
  (OFPXMT_OFB_TCP_FLAGS 42)
  (OFPXMT_OFB_ACTSET_OUTPUT 43)
  (OFPXMT_OFB_PACKET_TYPE 44))

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

(defstruct ofp_oxm_experimenter_header
  oxm_header
  experimenter)

(defstruct ofp_stats
  length
  oxs_fields)

(defconstants "ofp_oxs_class"
  (OFPXSC_OPENFLOW_BASIC #x8002)
  (OFPXSC_EXPERIMENTER #xffff))

(defconstants "oxs_ofb_stat_fields"
  (OFPXST_OFB_DURATION 0)
  (OFPXST_OFB_IDLE_TIME 1)
  (OFPXST_OFB_FLOW_COUNT 3)
  (OFPXST_ODB_PACKET_COUNT 4)
  (OFPXST_OFB_BYTE_COUNT 5))

(defstruct ofp_oxs_experimentar_header
  oxs_header
  experimenter)

(defconstants "ofp_instruction_type"
  (OFPIT_GOTO_TABLE 1)
  (OFPIT_WRITE_METADATA 2)
  (OFPIT_WRITE_ACTIONS 3)
  (OFPIT_APPLY_ACTIONS 4)
  (OFPIT_CLEAR_ACTIONS 5)
  (OFPIT_DEPRECATED 6)
  (OFPIT_STAT_TRIGGER 7)
  (OFPIT_EXPERIMENTER #xffff))

(defstruct ofp_instruction_header
  type
  len)

(defstruct ofp_instruction_goto_table
  (type OFPIT_GOTO_TABLE)
  (len 8)
  table_id)

(defstruct ofp_instruction_write_metadata
  (type OFPIT_WRITE_METADATA)
  (len 24)
  metadata
  metadata_mask)

(defstruct ofp_instruction_actions
  type
  len
  actions)

(defstruct ofp_instruction_stat_trigger
  (type OFPIT_STAT_TRIGGER)
  len
  flags
  thresholds)

(defconstants "ofp_stat_trigger_glags"
  (OFPSTF_PERIODIC (ash 1 0))
  (OFPSTF_ONLY_FIRST (ash 1 1)))

(defstruct ofp_instruction_experimenter_header
  (type OFPIT_EXPERIMENTER)
  len
  experimenter)

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
  (OFPAT_POP_PBB 27)
  (OFPAT_COPY_FIELD 28)
  (OFPAT_METER 29)
  (OFPAT_EXPERIMENTER #xffff))

(defstruct ofp_action_header
  type
  len)

(defstruct ofp_action_output
  (type OFPAT_OUTPUT)
  (len 16)
  port
  max_len)

(defconstants "ofp_conrtoller_max_len"
  (OFPCML_MAX #xffe5)
  (OFPCML_NO_BUFFER #xffff))

(defstruct ofp_action_group
  (type OFPAT_GROUP)
  (len 8)
  group_id)

(defstruct ofp_action_set_queue
  (type OFPAT_SET_QUEUE)
  (len 8)
  queue_id)

(defstruct ofp_action_meter
  (type OFPAT_METER)
  (len 8)
  meter_id)

(defstruct ofp_action_mpls_ttl
  (type OFPAT_SET_MPLS_TTL)
  (len 8)
  mpls_ttl)

(defstruct ofp_action_generic
  type
  (len 8))

(defstruct ofp_action_nw_ttl
  (type OFPAT_SET_NW_TTL)
  (len 8)
  nw_ttl)

(defstruct ofp_action_push
  type
  (len 8)
  ethertype)

(defstruct ofp_action_pop_mpls
  (type OFPAT_POP_MPLS)
  (len 8)
  ethertype)

(defstruct ofp_action_set_field
  (type OFPAT_SET_FIELD)
  len
  field)

(defstruct ofp_action_copy_field
  (type OFPAT_COPY_FIELD)
  len
  n_bits
  src_offset
  dst_offset
  oxm_ids)

(defstruct ofp_action_experimenter_header
  (type OFPAT_EXPERIMENTER)
  len
  experimenter)

(defstruct ofp_controller_status
  length
  short_id
  role
  readon
  channel_status
  properties)

(defconstants "ofp_controller_status_reason"
  (OFPCSR_REQUEST 0)
  (OFPCSR_CHANNEL_STATUS 1)
  (OFPCSR_ROLE 2)
  (OFPCSR_CONTROLLER_ADDED 3)
  (OFPCSR_CONTROLLER_REMOVED 4)
  (OFPCSR_SHORT_ID 5)
  (OFPCSR_EXPERIMENTER 6))

(defconstants "ofp_control_channel_status"
  (OFPCT_STATUS_UP 0)
  (OFPCT_STATUS_DOWN 1))

(defconstants "ofp_controller_status_prop_type"
  (OFPCSRT_URI 0)
  (OFPCSRT_EXPERIMENTER #xffff))

(defstruct ofp_controller_status_prop_uri
  (type OFPCSRT_URI)
  length
  uri)

(defstruct ofp_controller_status_prop_experimenter
  (type OFPCSRT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_experimenter_structure
  experimenter
  exp_type
  experimenter_data)

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
  (OFPC_PORT_BLOCKED (ash 1 8))
  (OFPC_BUNDLES (ash 1 9))
  (OFPC_FLOW_MONITORING (ash 1 10)))

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
  (OFPIT_MAX #xfe)
  (OIPIT_ALL #xff))

(defstruct ofp_table_mod
  header
  table_id
  config
  properties)

(defconstants "ofp_table_config"
  (OFPTC_DEPRECATED_MASK 3)
  (OFPTC_EVICTION (ash 1 2))
  (OFPTC_VACANCY_EVENT (ash 1 3)))

(defconstants "ofp_table_mod_prop_type"
  (OFPTMPT_EVICTION #x2)
  (OFPTMPT_VACANCY #x3)
  (OFPTMPT_EXPERIMENTER #xffff))

(defstruct ofp_table_mod_prop_header
  type
  length)

(defstruct ofp_table_mod_prop_eviction
  (type OFPTMPT_EVICTION)
  length
  flags)

(defconstants "ofp_table_mod_prop_eviction_flag"
  (OFPTMPEF_OTHER (ash 1 0))
  (OFPTMPEF_IMPORTANCE (ash 1 1))
  (OFPTMPEF_LIFETIME (ash 1 2)))

(defstruct ofp_table_mod_prop_vacancy
  (type OFPTMPT_VACANCY)
  length
  vacancy_down
  vacancy_up
  vacancy)

(defstruct ofp_table_mod_prop_experimenter
  (type OFPTMPT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

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
  importance
  match)

(defconstants "ofp_flow_mod_command"
  (OFPFC_ADD 0)
  (OFPFC_MODIFY 1)
  (OFPFC_MODIFY_STRICT 2)
  (OFPFC_DELETE 3)
  (OFPFC_DELETE_STRICT 4))

(defconstants "ofp_flow_mod_flags"
  (OFPFF_SEND_FLOW_REM (ash 1 0))
  (OFPFF_CHECK_OVERLAP (ash 1 1))
  (OFPFF_RESET_COUNTS (ash 1 2))
  (OFPFF_NO_PKT_COUNTS (ash 1 3))
  (OFPFF_NO_BYT_COUNTS (ash 1 4)))

(defstruct ofp_group_mod
  header
  command
  type
  group_id
  bucket_array_len
  command_bucket_id
  buckets)

(defconstants "ofp_group_mod_command"
  (OFPGC_ADD 0)
  (OFPGC_MODIFY 1)
  (OFPGC_DELETE 2)
  (OFPGC_INSERT_BUCKET 3)
  (OFPGC_REMOVE_BUCKET 5))

(defconstants "ofp_group_type"
  (OFPGT_ALL 0)
  (OFPGT_SELECT 1)
  (OFPGT_INDIRECT 2)
  (OFPGT_FF 3))

(defconstants "ofp_group"
  (OFPG_MAX #xffffff00)
  (OFPG_ALL #xfffffffc)
  (OFPG_ANY #xffffffff))

(defconstants "ofp_group_bucket"
  (OFPG_BUCKET_MAX #xffffff00)
  (OFPG_BUCKET_FIRST #xfffffffd)
  (OFPG_BUCKET_LAST #xfffffffe)
  (OFPG_BUCKET_ALL #xffffffff))

(defstruct ofp_bucket
  len
  action_array_len
  bucket_id
  actions)

(defconstants "ofp_group_bucket_prop_type"
  (OFPGBPT_WEIGHT 0)
  (OFPGBPT_WATCH_PORT 1)
  (OFPGBPT_WATCH_GROUP 2)
  (OFPGBPT_EXPERIMENTER #xffff))

(defstruct ofp_group_bucket_prop_header
  type
  length)

(defstruct ofp_group_bukcet_prop_weight
  (type OFPGBPT_WEIGHT)
  (length 8)
  weight)

(defstruct ofp_group_bucket_prop_watch
  type
  (length 8)
  watch)

(defstruct ofp_group_bucket_prop_experimenter
  (type OFPGBPT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

(defconstants "ofp_group_prop_type"
  (OFPGPT_EXPERIMENTER #xffff))

(defstruct ofp_group_prop_header
  type
  length)

(defstruct ofp_group_prop_experimenter
  (type OFPGPT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_port_mod
  header
  port_no
  hw_addr
  config
  mask
  properties)

(defconstants "ofp_port_mod_prop_type"
  (OFPPMPT_ETHERNET 0)
  (OFPPMPT_OPTICAL 1)
  (OFPPMPT_EXPERIMENTAL #xffff))

(defstruct ofp_port_mod_prop_header
  type
  length)

(defstruct ofp_port_mod_prop_ethernet
  (type OFPPMPT_ETHERNET)
  length
  advertise)

(defstruct ofp_port_mod_prop_optical
  (type OFPPMPT_OPTICAL)
  length
  configure
  freq_lmda
  fl_offset
  grid_span
  tx_pwr)

(defstruct ofp_port_mod_prop_experimenter
  (type OFPPMPT_EXPERIMENTAL)
  length
  experimenter
  exp_type
  experimenter_data)

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
  (OFPMF_PKTPS (ash 1 1))
  (OFPMF_BURST (ash 1 2))
  (OFPMS_STATS (ash 1 3)))

(defstruct ofp_meter_band_handler
  type
  length
  rate
  burst_size)

(defconstants "ofp_meter_band_type"
  (OFPMBT_DROP 1)
  (OFPMBT_DSCP_REMARK 2)
  (OFPMBT_EXPERIMENTER #xffff))

(defstruct ofp_meter_band_drop
  (type OFPMBT_DROP)
  (len 16)
  rate
  burst_size)

(defstruct ofp_meter_band_dscp_remark
  (type OFPMBT_DSCP_REMARK)
  (len 16)
  rate
  burst_size
  prec_level)

(defstruct ofp_meter_band_experimenter
  type
  len
  rate
  burst_size
  experimenter)

(defstruct ofp_multipart_request
  header
  type
  flags
  body)

(defstruct ofp_multipart_reply
  header
  type
  flags
  body)

(defconstants "ofp_multipart_request_flags"
  (OFPMPF_REQ_MORE (ash 1 0)))

(defconstants "ofp_multipart_reply_flags"
  (OFPMPF_REPLY_MORE (ash 1 0)))

(defconstants "ofp_multipart_type"
  (OFPMP_DESC 0)
  (OFPMP_FLOW_DESC 1)
  (OFPMP_AGGREGATE_STATS 2)
  (OFPMP_TABLE_STATS 3)
  (OFPMP_PORT_STATS 4)
  (OFPMP_QUEUE_STATS 5)
  (OFPMP_GROUP_STATS 6)
  (OFPMP_GROUP_DESC 7)
  (OFPMP_GROUP_FEATURES 8)
  (OFPMP_METER_STATS 9)
  (OFPMP_METER_DESC 10)
  (OFPMP_METER_FEATURES 11)
  (OFPMP_TABLE_FEATURES 12)
  (OFPMP_PORT_DESC 13)
  (OFPMP_TABLE_DESC 14)
  (OFPMP_QUEUE_DESC 15)
  (OFPMP_FLOW_MONITOR 16)
  (OFPMP_FLOW_STATS 17)
  (OFPMP_CONTROLLER_STATUS 18)
  (OFPMP_BUNDLE_FEATURES 19)
  (OFPMP_EXPERIMENTER #xffff))

(defstruct ofp_desc
  mfr_desc
  hw_desc
  sw_desc
  serial_num
  dp_desc)

(defstruct ofp_flow_stats_request
  table_id
  out_port
  out_group
  cookie
  cookie_mask
  match)

(defstruct ofp_flow_desc
  length
  table_id
  priority
  idle_timeout
  hard_timeout
  flags
  importance
  cookie
  match)

(defstruct ofp_flow_stats_request
  table_id
  out_port
  out_group
  cookie
  cookie_mask
  match)

(defstruct ofp_flow_stats
  length
  table_id
  reason
  priority
  match)

(defconstants "ofp_flow_stats_reason"
  (OFPFSR_STATS_REQUEST 0)
  (OFPFSR_STATS_TRIGGER 1))

(defstruct ofp_aggregate_stats_request
  table_id
  out_port
  out_group
  cookie
  cookie_mask
  match)

(defstruct ofp_aggregate_state_reply
  stats)

(defstruct ofp_port_multipart_request
  port_no)

(defstruct ofp_port_stats
  length
  port_no
  duration_sec
  duration_nsec
  rx_packets
  tx_packets
  rx_bytes
  tx_bytes
  rx_dropped
  tx_dropped
  rx_errors
  tx_errors
  properties)

(defconstants "ofp_port_stats_prop_type"
  (OFPPSPT_ETHERNET 0)
  (OFPPSPT_OPTICAL 1)
  (OFPPSPT_EXPERIMENTER #xffff))

(defstruct ofp_port_stats_prop_header
  type
  length)

(defstruct ofp_port_stats_prop_ethernet
  (type OFPPSPT_ETHERNET)
  length
  rx_frame_err
  rx_over_err
  rx_crc_err
  collisions)

(defstruct ofp_port_stats_prop_optical
  (type OFPPSPT_OPTICAL)
  length
  flags
  tx_freq_lmda
  tx_offset
  tx_grid_span
  rx_freq_lmda
  rx_offset
  rx_grid_span
  tx_pwr
  rx_pwr
  bias_current
  temperture)

(defconstants "ofp_port_stats_optical_flags"
  (OFPOSF_RX_TUNE (ash 1 0))
  (OFPOSF_TX_TUNE (ash 1 1))
  (OFPOSF_TX_PWR (ash 1 2))
  (OFPOSF_RX_PWR (ash 1 4))
  (OFPOSF_TX_BIAS (ash 1 5))
  (OFPOSF_TX_TEMP (ash 1 6)))

(defstruct ofp_port_stats_prop_experimenter
  (type OFPPSPT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_queue_multipart_request
  port_no
  queue_id)

(defstruct ofp_queue_stats
  length
  port_no
  queue_id
  tx_bytes
  tx_packets
  tx_errors
  duration_sec
  duration_nsec
  properties)

(defconstants "ofp_queue_stats_prop_type"
  (OFPQSPT_EXPERIMENTER #xffff))

(defstruct ofp_queue_stats_prop_header
  type
  length)

(defstruct ofp_queue_stats_prop_experimenter
  (type OFPQSPT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_queue_desc
  port_no
  queue_id
  len)

(defconstants "ofp_queue_desc_prop_type"
  (OFPQDPT_MIN_RATE 1)
  (OFPQDPT_MAX_RATE 2)
  (OFPQDPT_EXPERIMENTER #xffff))

(defstruct ofp_queue_desc_prop_handler
  type
  length)

(defstruct ofp_queue_desc_prop_min_rate
  (type OFPQDPT_MIN_RATE)
  (length 8)
  rate)

(defstruct ofp_queue_desc_prop_max_rate
  (type OFPQDPT_MAX_RATE)
  (length 8)
  rate)

(defstruct ofp_queue_desc_prop_experimenter
  (type OFPQDPT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_group_multipart_request
  group_id)

(defstruct ofp_group_state
  length
  group_id
  ref_count
  packet_count
  byte_count
  duration_sec
  duration_nsec
  bucket_stats)

(defstruct ofp_bucket_counter
  packet_count
  byte_count)

(defstruct ofp_group_desc
  length
  type
  group_id
  bucket_array_len
  buckets)

(defstruct ofp_group_features
  type
  capabilities
  max_groups
  actions)

(defconstants "ofp_group_capabilities"
  (OFPGFC_SELECT_WEIGHT (ash 1 0))
  (OFPGFC_SELECT_LIVENESS (ash 1 1))
  (OFPGFC_CHAINING (ash 1 2))
  (OFPGFC_CHAINING_CHECKS (ash 1 3)))

(defstruct ofp_meter_multipart_request
  meter_id)

(defstruct ofp_meter_stats
  meter_id
  len
  ref_count
  packet_in_count
  byte_in_count
  duration_sec
  duration_nsec
  band_stats)

(defstruct ofp_meter_band_stats
  packet_band_count
  byte_band_count)

(defstruct ofp_meter_desc
  length
  flags
  meter_id
  bands)

(defstruct ofp_meter_features
  max_meter
  band_types
  capabilities
  max_bands
  max_color
  features)

(defconstants "ofp_meter_feature_flags"
  (OFPMFF_ACTION_SET (ash 1 0))
  (OFPMFF_ANY_POSITION (ash 1 1))
  (OFPMFF_MULTI_LIST (ash 1 2)))

(defstruct ofp_table_stats
  table_id
  active_count
  lookup_count
  matched_count)

(defstruct ofp_table_desc
  length
  table_id
  config
  properties)

(defconstants "ofp_table_features_command"
  (OFPTFC_REPLACE 0)
  (OFPTFC_MODIFY 1)
  (OFPTFC_ENABLE 2)
  (OFPTFC_DISABLE 3))

(defstruct ofp_table_features
  length
  table_id
  command
  features
  name
  metadata_match
  metadata_write
  capabilities
  max_entries
  properties)

(defconstants "ofp_table_feature_flag"
  (OFPTFF_INGRESS_TABLE (ash 1 0))
  (OFPTFF_EGRESS_TABLE (ash 1 1))
  (OFPTFF_FIRST_EGRESS (ash 1 4)))

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
  (OFPTFPT_TABLE_SYNC_FROM 16)
  (OFPTFPT_WRITE_COPYFIELD 18)
  (OFPTFPT_WRITE_COPYFIELD_MISS 19)
  (OFPTFPT_APPLY_COPYFIELD 20)
  (OFPTFPT_APPLY_COPYFIELD_MISS 21)
  (OFPTFPT_PACKET_TYPES 22)
  (OFPTFPT_EXPERIMENTER #xfffe)
  (OFPTFPT_EXPERIMENTER_MISS #xffff))

(defstruct ofp_table_feature_prop_header
  type
  length)

(defstruct ofp_table_feature_prop_instructions
  type
  length
  instruction_ids)

(defstruct ofp_instruction_id
  type
  len
  exp_data)

(defstruct ofp_table_feature_prop_tables
  type
  length
  table_ids)

(defstruct ofp_table_feature_prop_actions
  type
  length
  action_ids)

(defstruct ofp_action_id
  type
  len
  exp_data)

(defstruct ofp_table_feature_prop_oxm
  type
  length
  oxm_ids)

(defstruct ofp_table_feature_prop_oxm_values
  (type OFPTFPT_PACKET_TYPES)
  length
  oxm_values)

(defstruct ofp_table_feature_prop_experimenter
  type
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_flow_monitor_request
  monitor_id
  out_port
  out_group
  flags
  table_id
  command
  match)

(defconstants "ofp_flow_monitor_command"
  (OFPFMC_ADD 0)
  (OFPFMC_MODIFY 1)
  (OFPFMC_DELETE 2))

(defconstants "ofp_flow_monitor_flags"
  (OFPFMF_INITIAL (ash 1 0))
  (OFPFMF_ADD (ash 1 1))
  (OFPFMF_REMOVED (ash 1 2))
  (OFPFMF_MODIFY (ash 1 3))
  (OFPFMF_INSTRUCTIONS (ash 1 4))
  (OFPFMF_NO_ABBREV (ash 1 5))
  (OFPFMF_ONLY_OWN (ash 1 6)))

(defstruct ofp_flow_update_header
  length
  event)

(defconstants "ofp_flow_update_event"
  (OFPFME_INITAL 0)
  (OFPFME_ADDED 1)
  (OFPFME_REMOVED 2)
  (OFPFME_MODIFIED 3)
  (OFPFME_ABBREV 4)
  (OFPFME_PAUSED 5)
  (OFPFME_RESUMED 6))

(defstruct ofp_flow_update_full
  length
  event
  table_id
  reason
  idle_timeout
  hard_timeout
  priority
  cookie
  match)

(defstruct ofp_flow_update_abbrev
  (length 8)
  (event OFPFME_ABBREV)
  xid)

(defstruct ofp_flow_update_paused
  (length 8)
  event)

(defstruct ofp_bundle_features_request
  feature_request_flags
  properties)

(defconstants "ofp_bundle_feature_flags"
  (OFPBF_TIMESTAMP (ash 1 0))
  (OFPBF_TIME_SET_SCHED (ash 1 1)))

(defstruct ofp_bundle_features
  capabilities
  properties)

(defconstants "ofp_bundle_flags"
  (OFPBF_ATOMIC (ash 1 0))
  (OFPBF_ORDERD (ash 1 1))
  (OFPBF_TIME (ash 1 2)))

(defstruct ofp_bundle_features_prop_header
  type
  length)

(defconstants "ofp_bundle_features_prop_type"
  (OFPTMPBF_TIME_CAPABILITY #x1)
  (OFPTMPBF_EXPERIMENTER #xffff))

(defstruct ofp_bundle_features_prop_time
  (type OFPTMPBF_TIME_CAPABILITY)
  length
  sched_accuracy
  sched_max_future
  sched_max_past
  timestamp)

(defstruct ofp_experimenter_multipart_header
  experimenter
  exp_type)

(defstruct ofp_packet_out
  header
  buffer_id
  actions_len
  match)

(defstruct ofp_role_request
  header
  role
  short_id
  generation_id)

(defconstants "ofp_controller_role"
  (OFPCR_ROLE_NOCHANGE 0)
  (OFPCR_ROLE_EQUAL 1)
  (OFPCR_ROLE_MASTER 2)
  (OFPCR_ROLE_SLAVE 3))

(defstruct ofp_bundle_ctrl_msg
  header
  bundle_id
  type
  flags
  properties)

(defconstants "ofp_bundle_ctrl_type"
  (OFPBCT_OPEN_REQUEST 0)
  (OFPBCT_OPEN_REPLY 1)
  (OFPBCT_CLOSE_REQUEST 2)
  (OFPBCT_CLOSE_REPLY 3)
  (OFPBCT_COMMIT_REQUEST 4)
  (OFPBCT_COMMIT_REPLY 5)
  (OFPBCT_DISCARD_REQUEST 6)
  (OFPBCT_DISCARD_REPLY 7))

(defstruct ofp_bundle_add_msg
  header
  bundle_id
  flags
  message)

(defconstants "ofp_bundle_prop_type"
  (OFPBPT_TIME 1)
  (OFPBPT_EXPERIMENTER #xffff))

(defstruct ofp_bundle_prop_header
  type
  length)

(defstruct ofp_bundle_prop_time
  (type OFPBPT_TIME)
  (length 24)
  scheduled_time)

(defstruct ofp_bundle_prop_experimenter
  (type OFPBPT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_time
  second
  nanosecond)

(defstruct ofp_async_config
  header
  properties)

(defconstants "ofp_async_config_prop_type"
  (OFPACPT_PACKET_IN_SLAVE 0)
  (OFPACPT_PACKET_IN_MASTER 1)
  (OFPACPT_PORT_STATUS_SLAVE 2)
  (OFPACPT_PORT_STATUS_MASTER 3)
  (OFPACPT_FLOW_REMOVED_SLAVE 4)
  (OFPACPT_FLOW_REMOVED_MASTER 5)
  (OFPACPT_ROLE_STATUS_SLAVE 6)
  (OFPACPT_ROLE_STATUS_MASTER 7)
  (OFPACPT_TABLE_STATUS_SLAVE 8)
  (OFPACPT_TABLE_STATUS_MASTER 9)
  (OFPACPT_REQUESTFORWARD_SLAVE 10)
  (OFPACPT_REQUESTFORWARD_MASTER 11)
  (OFPACPT_FLOW_STATS_SLAVE 12)
  (OFPACPT_FLOW_STATS_MASTER 13)
  (OFPACPT_CONT_STATUS_SLAVE 14)
  (OFPACPT_CONT_STATUS_MASTER 15)
  (OFPACPT_EXPERIMENTER_SLAVE #xfffe)
  (OFPACPT_EXPERIMENTER_MASTER #xffff))

(defstruct ofp_async_config_prop_header
  type
  length)

(defstruct ofp_async_config_prop_reasons
  type
  length
  mask)

(defstruct ofp_async_config_prop_experimenter
  type
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_packet_in
  header
  buffer_id
  total_len
  reason
  table_id
  cookie
  match)

(defconstants "ofp_packet_in_reason"
  (OFPR_TABLE_MISS 0)
  (OFPR_APPLY_ACTION 1)
  (OFPR_INVALID_TTL 2)
  (OFPR_ACTION_SET 3)
  (OFPR_GROUP 4)
  (OFPR_PACKET_OUT 5))

(defstruct ofp_flow_removed
  header
  table_id
  reason
  priority
  idle_timeout
  hard_timeout
  cookie
  match)

(defconstants "ofp_flow_removed_reason"
  (OFPRR_IDLE_TIMEOUT 0)
  (OFPRR_HARD_TIMEOUT 1)
  (OFPRR_DELTE 2)
  (OFPRR_GROUP_DELETE 3)
  (OFPRR_METER_DELETE 4)
  (OFPRR_EVICTION 5))

(defstruct ofp_port_status
  header
  reason
  desc)

(defconstants "ofp_port_reason"
  (OFPPR_ADD 0)
  (OFPPR_DELETE 1)
  (OFPPR_MODIFY 2))

(defstruct ofp_role_status
  header
  role
  reason
  generation_id
  properties)

(defconstants "ofp_contoller_role_reason"
  (OFPCRR_MASTER_REQUEST 0)
  (OFPCRR_CONFIG 1)
  (OFPCRR_EXPERIMENTER 2))

(defconstants "ofp_role_prop_type"
  (OFPRPT_EXPERIMENTER #xffff))

(defstruct ofp_role_prop_header
  type
  length)

(defstruct ofp_role_prop_experimenter
  (type OFPRPT_EXPERIMENTER)
  length
  experimenter
  exp_type
  experimenter_data)

(defstruct ofp_table_status
  header
  reason
  table)

(defconstants "ofp_table_reason"
  (OFPTR_VACANCY_DOWN 3)
  (OFPTR_VACANCY_UP 4))

(defstruct ofp_requestforward_header
  header
  request)

(defconstants "ofp_requestforward_reason"
  (OFPRFR_GROUP_MOD 0)
  (OFPRFR_METER_MOD 1))

(defstruct ofp_controller_status_header
  header
  status)

(defstruct ofp_hello
  header
  elements)

(defconstants "ofp_hello_elem_type"
  (OFPHET_VERSIONBITMAP 1))

(defstruct ofp_hello_elem_header
  type
  length)

(defstruct ofp_hello_elem_versionbitmap
  (type OFPHET_VERSIONBITMAP)
  length
  bitmaps)

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
  (OFPET_BAD_PROPERTY 14)
  (OFPET_ASYNC_CONFIG_FAILED 15)
  (OFPET_FLOW_MONITOR_FAILED 16)
  (OFPET_BUNDLE_FAILED 17)
  (OFPET_EXPERIMENTER #xffff))

(defconstants "ofp_hello_failed_code"
  (OFPHFC_INCOMPATIBLE 0)
  (OFPHFC_EPERM 1))

(defconstants "ofp_bad_request_code"
  (OFPBRC_BAD_VERSION 0)
  (OFPBRC_BAD_TYPE 1)
  (OFPBRC_BAD_MULTIPART 2)
  (OFPBRC_BAD_EXPERIMENTER 3)
  (OFPBRC_BAD_EXP_TYPE 4)
  (OFPBRC_EPERM 5)
  (OFPBRC_BAD_LEN 6)
  (OFPBRC_BUFFER_EMPTY 7)
  (OFPBRC_BUFFER_UNKNOWN 8)
  (OFPBRC_BAD_TABLE_ID 9)
  (OFPBRC_IS_SLAVE 10)
  (OFPBRC_BAD_PORT 11)
  (OFPBRC_BAD_PACKET 12)
  (OFPBRC_MULTIPART_BUFFER_OVERFLOW 13)
  (OFPBRC_MULRIPART_REQUEST_TIMEOUT 14)
  (OFPBRC_MULTIPART_REPLY_TIMEOUT 15)
  (OFPBRC_MU+TIPART_BAD_SCHED 16)
  (OFPBRC_PIPELINE_FIELDS_ONLY 17)
  (OFPBRC_UNKNOWN 18))

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
  (OFPBAC_BAD_SET_ARGUMENT 15)
  (OFPBAC_BAD_SET_MASK 16))

(defconstants "ofp_bad_instruction_code"
  (OFPBIC_UNKNOWN_INST 0)
  (OFPBIC_UNSUP_INST 1)
  (OFPBIC_BAD_TABLE_ID 2)
  (OFPBIC_UNSUP_METADATA 3)
  (OFPBIC_UNSUP_METADATA_MASK 4)
  (OFPBIC_BAD_EXPERIMENTER 5)
  (OFPBIC_BAD_EXP_TYPE 6)
  (OFPBIC_BAD_LEN 7)
  (OFPBID_EPERM 8)
  (OFPBIC_DUP_INST 9))

(defconstants "ofp_bad_match_code"
  (OFPBMC_BAD_TYPE 0)
  (OFPBMC_BAD_LEN 1)
  (OFPBMC_BAD_TAG 2)
  (OFPBMC_BAD_DL_ADDR_MASK 3)
  (OFPBMC_BAD_NW_ADDR_MASK 4)
  (OFPBMC_BAD_WILDCATDS 5)
  (OFPBMC_BAD_FIELD 6)
  (OFPBMC_BAD_VALUE 7)
  (OFPBMC_BAD_MASK 8)
  (OFPBMC_BAD_PREREQ 9)
  (OFPBMC_DUP_FIELD 10)
  (OFPBMC_EPERM 11))

; p185
