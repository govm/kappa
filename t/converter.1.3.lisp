(in-package :cl-user)
(defpackage kappa-test.converter.1.3
  (:use :cl
        :kappa.converter.1.3
        :kappa.define.1.3
        :prove)
  (:import-from :kappa.define
                :make-ofp_header)
  (:import-from :flexi-streams
                :make-in-memory-input-stream)
  (:import-from :fast-io
                :with-fast-output)
  (:import-from :kappa.util
                :with-prefix))
(in-package :kappa-test.converter.1.3)

(defun vs (v)
  (make-in-memory-input-stream v))


(plan 33)

(subtest "ofp_switch_features"
  (let* ((h (make-ofp_header :version 4 :type OFPT_FEATURES_REPLY :length 32 :xid 0))
         (v #(0 0 0 0 0 0 0 1
              0 0 0 2
              3
              4
              0 0
              0 0 0 5
              0 0 0 6))
         (s (vs v))
         (b (make-ofp_switch_features-stream h s)))
    (is (length v) (- 32 8))
    (with-prefix (sf ofp_switch_features-)
      (is (sf datapath_id b) 1)
      (is (sf n_buffers b) 2)
      (is (sf n_tables b) 3)
      (is (sf auxiliary_id b) 4)
      (is (sf capabilities b) 5)
      (is (sf reserved b) 6))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_switch_config"
  (let* ((h (make-ofp_header :version 4 :type OFPT_GET_CONFIG_REPLY :length 12 :xid 0))
         (v #(0 1 1 0))
         (s (vs v))
         (b (make-ofp_switch_config-stream h s)))
    (with-prefix (sc ofp_switch_config-)
      (is (sc flags b) 1)
      (is (sc miss_send_len b) #x0100))
    (is-error (read-byte s) 'end-of-file))

  (let* ((h (make-ofp_header :version 4 :type OFPT_SET_CONFIG :length 12 :xid 0))
         (c (make-ofp_switch_config :header h :flags 1 :miss_send_len #x0100))
         (expect #(4 #.OFPT_SET_CONFIG 0 12 0 0 0 0
                   0 1 1 0))
         (dump (with-fast-output (buf) (dump-ofp_switch_config c buf))))
    (is dump expect :test #'equalp)))

(subtest "ofp_table_mod"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 16 :xid 0))
         (v #(1
              0 0 0
              0 0 0 2))
         (s (vs v))
         (b (make-ofp_table_mod-stream h s)))
    (with-prefix (tm ofp_table_mod-)
      (is (tm table_id b) 1)
      (is (tm config b) 2))
    (is-error (read-byte s) 'end-of-file))
  (let* ((h (make-ofp_header :version 4 :type OFPT_TABLE_MOD :length 16 :xid 0))
         (m (make-ofp_table_mod :header h :table_id 1 :config 2))
         (expect #(4 #.OFPT_TABLE_MOD 0 16 0 0 0 0
                   1
                   0 0 0
                   0 0 0 2))
         (dump (with-fast-output (buf) (dump-ofp_table_mod m buf))))
    (is dump expect :test #'equalp)))

(subtest "ofp_flow_mod"
  (let* ((h (make-ofp_header :version 4 :type OFPT_FLOW_MOD :length 8 :xid 0))
         (f (make-ofp_flow_mod :header h
                               :cookie 1
                               :cookie_mask 2
                               :table_id 3
                               :command 4
                               :idle_timeout 5
                               :hard_timeout 6
                               :priority 7
                               :buffer_id 8
                               :out_port 9
                               :out_group 10
                               :flags 11
                               :match (make-ofp_match)
                               :instructions (with-prefix (mi make-ofp_instruction_)
                                               (list (mi goto_table :table_id 1)
                                                     (mi write_metadata :metadata 1
                                                                        :metadata_mask 2)
                                                     (mi actions :type OFPIT_WRITE_ACTIONS
                                                                 :len 8
                                                                 :actions (with-prefix (ma make-ofp_action_)
                                                                            (list (ma output :port 1
                                                                                             :max_len 2)
                                                                                  (ma group :group_id 1)
                                                                                  (ma set_queue :queue_id 1)
                                                                                  (ma mpls_ttl :mpls_ttl 1)
                                                                                  (ma nw_ttl :nw_ttl 1)
                                                                                  (ma push :type OFPAT_PUSH_VLAN
                                                                                           :len 8
                                                                                           :ethertype 1)
                                                                                  (ma pop_mpls :ethertype 1)
                                                                                  (ma set_field :len 8
                                                                                                :field #(0 1 2 3))
                                                                                  (ma experimenter_header
                                                                                      :len 12
                                                                                      :experimenter 1
                                                                                      :data #(0 1 2 3)))))
                                                     (mi meter :meter_id 1)))))
         (expect `#(4 #.OFPT_FLOW_MOD 0 8 0 0 0 0
                    0 0 0 0 0 0 0 1
                    0 0 0 0 0 0 0 2
                    3
                    4
                    0 5
                    0 6
                    0 7
                    0 0 0 8
                    0 0 0 9
                    0 0 0 10
                    0 11
                    0 0
                    0 1 0 4 0 0 0 0
                    0 #.OFPIT_GOTO_TABLE
                    0 8
                    1
                    0 0 0
                    0 #.OFPIT_WRITE_METADATA
                    0 24
                    0 0 0 0
                    0 0 0 0 0 0 0 1
                    0 0 0 0 0 0 0 2
                    0 #.OFPIT_WRITE_ACTIONS
                    0 8
                    0 0 0 0
                      0 #.OFPAT_OUTPUT
                      0 16
                      0 0 0 1
                      0 2
                      0 0 0 0 0 0
                      0 #.OFPAT_GROUP
                      0 8
                      0 0 0 1
                      0 #.OFPAT_SET_QUEUE
                      0 8
                      0 0 0 1
                      0 #.OFPAT_SET_MPLS_TTL
                      0 8
                      1
                      0 0 0
                      0 #.OFPAT_SET_NW_TTL
                      0 8
                      1
                      0 0 0
                      0 #.OFPAT_PUSH_VLAN
                      0 8
                      0 1
                      0 0
                      0 #.OFPAT_POP_MPLS
                      0 8
                      0 1
                      0 0
                      0 #.OFPAT_SET_FIELD
                      0 8
                      0 1 2 3
                      255 255 ; OFPAT_EXPERIMENTER
                      0 12
                      0 0 0 1
                      0 1 2 3
                    0 #.OFPIT_METER
                    0 8
                    0 0 0 1))
         (dump (with-fast-output (buf) (dump-ofp_flow_mod f buf))))
    (is dump expect :test #'equalp)))

(subtest "ofp_group_mod"
  (let* ((h (make-ofp_header :version 4 :type OFPT_GROUP_MOD :length 48 :xid 0))
         (g (make-ofp_group_mod :header h
                                :command 1
                                :type 2
                                :group_id 3
                                :buckets (list (make-ofp_bucket :len 32
                                                                :weight 1
                                                                :watch_port 2
                                                                :watch_group 3
                                                                :actions (list (make-ofp_action_output :port 1
                                                                                                       :max_len 2))))))
         (expect #(4 #.OFPT_GROUP_MOD 0 48 0 0 0 0
                   0 1
                   2
                   0
                   0 0 0 3
                     0 32
                     0 1
                     0 0 0 2
                     0 0 0 3
                     0 0 0 0
                       0 #.OFPAT_OUTPUT
                       0 16
                       0 0 0 1
                       0 2
                       0 0 0 0 0 0))
         (dump (with-fast-output (buf) (dump-ofp_group_mod g buf))))
    (is dump expect :test #'equalp)))

(subtest "ofp_port_mod"
  (let* ((h (make-ofp_header :version 4 :type OFPT_PORT_MOD :length 40 :xid 0))
         (m (make-ofp_port_mod :header h
                               :port_no 1
                               :hw_addr #(0 1 2 3 4 5)
                               :config 2
                               :mask 3
                               :advertise 4))
         (expect #(4 #.OFPT_PORT_MOD 0 40 0 0 0 0
                   0 0 0 1
                   0 0 0 0
                   0 1 2 3 4 5
                   0 0
                   0 0 0 2
                   0 0 0 3
                   0 0 0 4
                   0 0 0 0))
         (dump (with-fast-output (buf) (dump-ofp_port_mod m buf))))
    (is dump expect :test #'equalp)))

(subtest "ofp_meter_mod"
  (let* ((h (make-ofp_header :version 4 :type OFPT_METER_MOD :length 68 :xid 0))
         (m (make-ofp_meter_mod :header h
                                :command 1
                                :flags 2
                                :meter_id 3
                                :bands (with-prefix (mb make-ofp_meter_band_)
                                         (list (mb drop :rate 1
                                                        :burst_size 2)
                                               (mb dscp_remark :rate 1
                                                               :burst_size 2
                                                               :prec_level 3)
                                               (mb experimenter :len 20
                                                                :rate 1
                                                                :burst_size 2
                                                                :experimenter 3
                                                                :data #(0 1 2 3))))))
         (expect #(4 #.OFPT_METER_MOD 0 68 0 0 0 0
                   0 1
                   0 2
                   0 0 0 3
                     0 #.OFPMBT_DROP
                     0 16
                     0 0 0 1
                     0 0 0 2
                     0 0 0 0
                     0 #.OFPMBT_DSCP_REMARK
                     0 16
                     0 0 0 1
                     0 0 0 2
                     3
                     0 0 0
                     255 255 ; OFPMBT_EXPERIMENTER
                     0 20
                     0 0 0 1
                     0 0 0 2
                     0 0 0 3
                     0 1 2 3))
         (dump (with-fast-output (buf) (dump-ofp_meter_mod m buf))))
    (is dump expect :test #'equalp)))

(subtest "ofp_multipart_request"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REQUEST :length 0 :xid 0)))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_DESC
                                            :flags 0
                                            :body nil))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_DESC 0 0
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_FLOW
                                            :flags 0
                                            :body (make-ofp_flow_stats_request :table_id 1
                                                                               :out_port 2
                                                                               :out_group 3
                                                                               :cookie 4
                                                                               :cookie_mask 5
                                                                               :match (make-ofp_match))))
           (expect `#(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                      0 #.OFPMP_FLOW 0 0
                      0 0 0 0
                      1
                      0 0 0
                      0 0 0 2
                      0 0 0 3
                      0 0 0 0
                      0 0 0 0 0 0 0 4
                      0 0 0 0 0 0 0 5
                      0 1 0 4 0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_AGGREGATE
                                            :flags 0
                                            :body (make-ofp_aggregate_stats_request :table_id 1
                                                                                    :out_port 2
                                                                                    :out_group 3
                                                                                    :cookie 4
                                                                                    :cookie_mask 5
                                                                                    :match (make-ofp_match))))
           (expect `#(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                      0 #.OFPMP_AGGREGATE 0 0
                      0 0 0 0
                      1
                      0 0 0
                      0 0 0 2
                      0 0 0 3
                      0 0 0 0
                      0 0 0 0 0 0 0 4
                      0 0 0 0 0 0 0 5
                      0 1 0 4 0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_TABLE
                                            :flags 0
                                            :body nil))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_TABLE 0 0
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_TABLE_FEATURES
                                            :flags 0
                                            :body (make-ofp_table_features :length 0
                                                                           :table_id 1
                                                                           :name "t0"
                                                                           :metadata_match 2
                                                                           :metadata_write 3
                                                                           :config 4
                                                                           :max_entries 5
                                                                           :properties (with-prefix (mfp make-ofp_table_feature_prop_)
                                                                                         (list (mfp instructions
                                                                                                    :type OFPTFPT_INSTRUCTIONS
                                                                                                    :length 12
                                                                                                    :instruction_ids (list (make-ofp_instruction_goto_table :table_id 1)))
                                                                                               (mfp next_tables
                                                                                                    :type OFPTFPT_NEXT_TABLES
                                                                                                    :length 5
                                                                                                    :next_table_ids (list 1))
                                                                                               (mfp actions
                                                                                                    :type OFPTFPT_WRITE_ACTIONS
                                                                                                    :length 20
                                                                                                    :action_ids (list (make-ofp_action_output :port 1 :max_len 2)))
                                                                                               (mfp oxm
                                                                                                    :type OFPTFPT_MATCH
                                                                                                    :length 8
                                                                                                    :oxm_ids (list 1)))))))
           (expect `#(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                      0 #.OFPMP_TABLE_FEATURES 0 0
                      0 0 0 0
                      0 0 ; length
                      1
                      0 0 0 0 0
                      ,@(mapcar #'(lambda (c) (char-code c)) '(#\t #\0))
                      ,@(loop :repeat (- OFP_MAX_TABLE_NAME_LEN 2) :collect 0)
                      0 0 0 0 0 0 0 2
                      0 0 0 0 0 0 0 3
                      0 0 0 4
                      0 0 0 5
                        0 #.OFPTFPT_INSTRUCTIONS
                        0 12
                          0 #.OFPIT_GOTO_TABLE
                          0 8
                          1
                          0 0 0
                        0 0 0 0 ; pad
                        0 #.OFPTFPT_NEXT_TABLES
                        0 5
                          1
                        0 0 0 ; pad
                        0 #.OFPTFPT_WRITE_ACTIONS
                        0 20
                          0 #.OFPAT_OUTPUT
                          0 16
                          0 0 0 1
                          0 2
                          0 0 0 0 0 0
                        0 0 0 0 ; pad
                        0 #.OFPTFPT_MATCH
                        0 8
                          0 0 0 1))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_PORT_STATS
                                            :flags 0
                                            :body (make-ofp_port_stats_request :port_no 1)))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_PORT_STATS 0 0
                     0 0 0 0
                     0 0 0 1
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_PORT_DESC
                                            :flags 0
                                            :body nil))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_PORT_DESC 0 0
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_QUEUE
                                            :flags 0
                                            :body (make-ofp_queue_stats_request :port_no 1
                                                                                :queue_id 2)))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_QUEUE 0 0
                     0 0 0 0
                     0 0 0 1
                     0 0 0 2))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_GROUP
                                            :flags 0
                                            :body (make-ofp_group_stats_request :group_id 1)))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_GROUP 0 0
                     0 0 0 0
                     0 0 0 1
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_GROUP_DESC
                                            :flags 0
                                            :body nil))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_GROUP_DESC 0 0
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_GROUP_FEATURES
                                            :flags 0
                                            :body nil))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_GROUP_FEATURES 0 0
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_METER
                                            :flags 0
                                            :body (make-ofp_meter_multipart_request :meter_id 1)))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_METER 0 0
                     0 0 0 0
                     0 0 0 1
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_METER_FEATURES
                                            :flags 0
                                            :body nil))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     0 #.OFPMP_METER_FEATURES 0 0
                     0 0 0 0))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))
    (let* ((req (make-ofp_multipart_request :header h
                                            :type OFPMP_EXPERIMENTER
                                            :flags 0
                                            :body (make-ofp_experimenter_multipart_header
                                                    :experimenter 1
                                                    :exp_type 2
                                                    :data #(0 1 2 3))))
           (expect #(4 #.OFPT_MULTIPART_REQUEST 0 0 0 0 0 0
                     255 255 0 0
                     0 0 0 0
                     0 0 0 1
                     0 0 0 2
                     0 1 2 3))
           (dump (with-fast-output (buf) (dump-ofp_multipart_request req buf))))
      (is dump expect :test #'equalp))))
(subtest "ofp_multipart_reply: OFPMP_DESC"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v `#(0 #.OFPMP_DESC 0 0
               0 0 0 0
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
         (b (make-ofp_multipart_reply-stream h s)))
    (with-prefix (mr ofp_multipart_reply-)
      (is (mr type b) OFPMP_DESC)
      (is (mr flags b) 0))
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (desc ofp_desc-)
        (ok (desc p bb))
        (is (desc mfr_desc bb) "mfr" :test #'string=)
        (is (desc hw_desc bb) "hw" :test #'string=)
        (is (desc sw_desc bb) "sw" :test #'string=)
        (is (desc serial_num bb) "serial" :test #'string=)
        (is (desc dp_desc bb) "dp" :test #'string=)))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_FLOW"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_FLOW 0 0
              0 0 0 0
              0 64
              2
              0
              0 0 0 3
              0 0 0 4
              0 5
              0 6
              0 7
              0 8
              0 0 0 0
              0 0 0 0 0 0 0 9
              0 0 0 0 0 0 0 10
              0 0 0 0 0 0 0 11
              0 1 0 4 0 0 0 0
                0 #.OFPIT_GOTO_TABLE
                0 8
                1
                0 0 0))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_FLOW)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (fl ofp_flow_stats-)
        (ok (fl p bb))
        (is (fl length bb) 64)
        (is (fl table_id bb) 2)
        (is (fl duration_sec bb) 3)
        (is (fl duration_nsec bb) 4)
        (is (fl priority bb) 5)
        (is (fl idle_timeout bb) 6)
        (is (fl hard_timeout bb) 7)
        (is (fl flags bb) 8)
        (is (fl cookie bb) 9)
        (is (fl packet_count bb) 10)
        (is (fl byte_count bb) 11)
        (ok (ofp_match-p (fl match bb)))
        (ok (ofp_instruction_goto_table-p (car (fl instructions bb))))))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_AGGREGATE"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_AGGREGATE 0 0
              0 0 0 0
              0 0 0 0 0 0 0 1
              0 0 0 0 0 0 0 2
              0 0 0 3
              0 0 0 0))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_AGGREGATE)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (ag ofp_aggregate_stats_reply-)
        (ok (ag p bb))
        (is (ag packet_count bb) 1)
        (is (ag byte_count bb) 2)
        (is (ag flow_count bb) 3)))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_TABLE"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_TABLE 0 0
              0 0 0 0
              1
              0 0 0
              0 0 0 2
              0 0 0 0 0 0 0 3
              0 0 0 0 0 0 0 4))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_TABLE)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (tb ofp_table_stats-)
        (ok (tb p bb))
        (is (tb table_id bb) 1)
        (is (tb active_count bb) 2)
        (is (tb lookup_count bb) 3)
        (is (tb matched_count bb) 4)))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_TABLE_FEATURES"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v `#(0 #.OFPMP_TABLE_FEATURES 0 0
               0 0 0 0
               0 120 ; length
               1
               0 0 0 0 0
               ,(char-code #\t) ,@(loop :repeat (- OFP_MAX_TABLE_NAME_LEN 1) :collect 0)
               0 0 0 0 0 0 0 2
               0 0 0 0 0 0 0 3
               0 0 0 4
               0 0 0 5
                 0 #.OFPTFPT_INSTRUCTIONS
                 0 12
                   0 #.OFPIT_GOTO_TABLE
                   0 8
                   1
                   0 0 0
                 0 0 0 0
                 0 #.OFPTFPT_NEXT_TABLES
                 0 5
                   1
                 0 0 0
                 0 #.OFPTFPT_WRITE_ACTIONS
                 0 20
                   0 #.OFPAT_OUTPUT
                   0 16
                   0 0 0 1
                   0 2
                   0 0 0 0 0 0
                 0 0 0 0
                 0 #.OFPTFPT_MATCH
                 0 8
                   0 0 0 1))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_TABLE_FEATURES)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (tf ofp_table_features-)
        (ok (tf p bb))
        (is (tf length bb) 120)
        (is (tf table_id bb) 1)
        (is (tf name bb) "t" :test #'string=)
        (is (tf metadata_match bb) 2)
        (is (tf metadata_write bb) 3)
        (is (tf config bb) 4)
        (is (tf max_entries bb) 5)
        (let ((props (tf properties bb)))
          (ok (listp props))
          (with-prefix (pi ofp_table_feature_prop_instructions-)
            (ok (pi p (nth 0 props)))
            (ok (ofp_instruction_goto_table-p (car (pi instruction_ids (nth 0 props))))))
          (with-prefix (nt ofp_table_feature_prop_next_tables-)
            (ok (nt p (nth 1 props)))
            (ok (listp (nt next_table_ids (nth 1 props)))))
          (with-prefix (pa ofp_table_feature_prop_actions-)
            (ok (pa p (nth 2 props)))
            (ok (ofp_action_output-p (car (pa action_ids (nth 2 props))))))
          (with-prefix (oxm ofp_table_feature_prop_oxm-)
            (ok (oxm p (nth 3 props)))
            (ok (listp (oxm oxm_ids (nth 3 props))))))))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_PORT_STATS"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_PORT_STATS 0 0
              0 0 0 0
              0 0 0 1
              0 0 0 0
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
              0 0 0 0 0 0 0 12
              0 0 0 0 0 0 0 13
              0 0 0 14
              0 0 0 15))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_PORT_STATS)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (ps ofp_port_stats-)
        (ok (ps p bb))
        (is (ps port_no bb) 1)
        (is (ps rx_packets bb) 2)
        (is (ps tx_packets bb) 3)
        (is (ps rx_bytes bb) 4)
        (is (ps tx_bytes bb) 5)
        (is (ps rx_dropped bb) 6)
        (is (ps tx_dropped bb) 7)
        (is (ps rx_errors bb) 8)
        (is (ps tx_errors bb) 9)
        (is (ps rx_frame_err bb) 10)
        (is (ps rx_over_err bb) 11)
        (is (ps rx_crc_err bb) 12)
        (is (ps collisions bb) 13)
        (is (ps duration_sec bb) 14)
        (is (ps duration_nsec bb) 15)))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_PORT_DESC"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v `#(0 #.OFPMP_PORT_DESC 0 0
               0 0 0 0
               0 0 0 1
               0 0 0 0
               0 1 2 3 4 5
               0 0
               ,(char-code #\p) ,@(loop :repeat (- OFP_MAX_PORT_NAME_LEN 1) :collect 0)
               0 0 0 2
               0 0 0 3
               0 0 0 4
               0 0 0 5
               0 0 0 6
               0 0 0 7
               0 0 0 8
               0 0 0 9))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_PORT_DESC)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (po ofp_port-)
        (ok (po p bb))
        (is (po port_no bb) 1)
        (is (po hw_addr bb) #(0 1 2 3 4 5) :test #'equalp)
        (is (po name bb) "p" :test #'string=)
        (is (po config bb) 2)
        (is (po state bb) 3)
        (is (po curr bb) 4)
        (is (po advertised bb) 5)
        (is (po supported bb) 6)
        (is (po peer bb) 7)
        (is (po curr_speed bb) 8)
        (is (po max_speed bb) 9)))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_QUEUE"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_QUEUE 0 0
              0 0 0 0
              0 0 0 1
              0 0 0 2
              0 0 0 0 0 0 0 3
              0 0 0 0 0 0 0 4
              0 0 0 0 0 0 0 5
              0 0 0 6
              0 0 0 7))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_QUEUE)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (qs ofp_queue_stats-)
        (ok (qs p bb))
        (is (qs port_no bb) 1)
        (is (qs queue_id bb) 2)
        (is (qs tx_bytes bb) 3)
        (is (qs tx_packets bb) 4)
        (is (qs tx_errors bb) 5)
        (is (qs duration_sec bb) 6)
        (is (qs duration_nsec bb) 7)))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_GROUP"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_GROUP 0 0
              0 0 0 0
              0 72
              0 0
              0 0 0 1
              0 0 0 2
              0 0 0 0
              0 0 0 0 0 0 0 3
              0 0 0 0 0 0 0 4
              0 0 0 5
              0 0 0 6
                0 0 0 0 0 0 0 7
                0 0 0 0 0 0 0 8
                0 0 0 0 0 0 0 9
                0 0 0 0 0 0 0 10))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_GROUP)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (gs ofp_group_stats-)
        (ok (gs p bb))
        (is (gs length bb) 72)
        (is (gs group_id bb) 1)
        (is (gs ref_count bb) 2)
        (is (gs packet_count bb) 3)
        (is (gs byte_count bb) 4)
        (is (gs duration_sec bb) 5)
        (is (gs duration_nsec bb) 6)
        (ok (listp (gs bucket_stats bb)))
        (with-prefix (bc ofp_bucket_counter-)
          (let ((stats (gs bucket_stats bb)))
            (is (length stats) 2)
            (ok (bc p (car stats)))
            (is (bc packet_count (car stats)) 7)
            (is (bc byte_count (car stats)) 8)
            (ok (bc p (cadr stats)))
            (is (bc packet_count (cadr stats)) 9)
            (is (bc byte_count (cadr stats)) 10)))))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_GROUP_DESC"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_GROUP_DESC 0 0
              0 0 0 0
              0 40 ; length
              1
              0
              0 0 0 2
                0 32
                0 1
                0 0 0 2
                0 0 0 3
                0 0 0 0
                  0 #.OFPAT_OUTPUT
                  0 16
                  0 0 0 1
                  0 2
                  0 0 0 0 0 0))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_GROUP_DESC)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (gd ofp_group_desc_stats-)
        (ok (gd p bb))
        (is (gd length bb) 40)
        (is (gd type bb) 1)
        (is (gd group_id bb) 2)
        (let ((buckets (gd buckets bb)))
          (with-prefix (bu ofp_bucket-)
            (ok (listp buckets))
            (is (length buckets) 1)
            (ok (bu p (car buckets)))
            (is (bu len (car buckets)) 32)
            (is (bu weight (car buckets)) 1)
            (is (bu watch_port (car buckets)) 2)
            (is (bu watch_group (car buckets)) 3)
            (let ((acts (bu actions (car buckets))))
              (ok (listp acts))
              (is (length acts) 1)
              (ok (ofp_action_output-p (car acts))))))))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_GROUP_FEATURES"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_GROUP_FEATURES 0 0
              0 0 0 0
              0 0 0 1
              0 0 0 2
              0 0 0 3
              0 0 0 4
              0 0 0 5
              0 0 0 6
              0 0 0 7
              0 0 0 8
              0 0 0 9
              0 0 0 10))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_GROUP_FEATURES)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (gf ofp_group_features-)
        (ok (gf p bb))
        (is (gf types bb) 1)
        (is (gf capabilities bb) 2)
        (is (gf max_groups bb) (list 3 4 5 6) :test #'equalp)
        (is (gf actions bb) (list 7 8 9 10) :test #'equalp)))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_METER"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_METER 0 0
              0 0 0 0
              0 0 0 1
              0 56 ; len
              0 0 0 0 0 0
              0 0 0 2
              0 0 0 0 0 0 0 3
              0 0 0 0 0 0 0 4
              0 0 0 5
              0 0 0 6
                0 0 0 0 0 0 0 7
                0 0 0 0 0 0 0 8))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_METER)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (ms ofp_meter_stats-)
        (ok (ms p bb))
        (is (ms meter_id bb) 1)
        (is (ms len bb) 56)
        (is (ms flow_count bb) 2)
        (is (ms packet_in_count bb) 3)
        (is (ms byte_in_count bb) 4)
        (is (ms duration_sec bb) 5)
        (is (ms duration_nsec bb) 6)
        (with-prefix (bs ofp_meter_band_stats-)
          (let ((stats (ms band_stats bb)))
            (ok (listp stats))
            (ok (bs p (car stats)))
            (is (bs packet_band_count (car stats)) 7)
            (is (bs byte_band_count (car stats)) 8)))))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_METER_CONFIG"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_METER_CONFIG 0 0
              0 0 0 0
              0 60
              0 1
              0 0 0 2
                0 #.OFPMBT_DROP
                0 16
                0 0 0 1
                0 0 0 2
                0 0 0 0
                0 #.OFPMBT_DSCP_REMARK
                0 16
                0 0 0 1
                0 0 0 2
                3
                0 0 0
                255 255 ; OFPMBT_EXPERIMENTER
                0 20
                0 0 0 1
                0 0 0 2
                0 0 0 3
                0 1 2 3))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_METER_CONFIG)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (mc ofp_meter_config-)
        (ok (mc p bb))
        (is (mc length bb) 60)
        (is (mc flags bb) 1)
        (is (mc meter_id bb) 2)
        (let ((bands (mc bands bb)))
          (ok (listp bands))
          (ok (ofp_meter_band_drop-p (nth 0 bands)))
          (ok (ofp_meter_band_dscp_remark-p (nth 1 bands)))
          (ok (ofp_meter_band_experimenter-p (nth 2 bands))))))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_METER_FEATURES"
  (let* ((h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 0 :xid 0))
         (v #(0 #.OFPMP_METER_FEATURES 0 0
              0 0 0 0
              0 0 0 1
              0 0 0 2
              0 0 0 3
              4
              5
              0 0))
         (s (vs v))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_METER_FEATURES)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (mf ofp_meter_features-)
        (ok (mf p bb))
        (is (mf max_meter bb) 1)
        (is (mf band_types bb) 2)
        (is (mf capabilities bb) 3)
        (is (mf max_bands bb) 4)
        (is (mf max_color bb) 5)))
    (is-error (read-byte s) 'end-of-file)))
(subtest "ofp_multipart_reply: OFPMP_EXPERIMENTER"
  (let* ((v #(255 255 0 0
              0 0 0 0
              0 0 0 1
              0 0 0 2
              0 1 2 3))
         (s (vs v))
         (h (make-ofp_header :version 4 :type OFPT_MULTIPART_REPLY :length 28 :xid 0))
         (b (make-ofp_multipart_reply-stream h s)))
    (is (ofp_multipart_reply-type b) OFPMP_EXPERIMENTER)
    (let ((bb (ofp_multipart_reply-body b)))
      (with-prefix (em ofp_experimenter_multipart_header-)
        (ok (em p bb))
        (is (em experimenter bb) 1)
        (is (em exp_type bb) 2)
        (ok (vectorp (em data bb)))
        (is (em data bb) #(0 1 2 3) :test #'equalp)))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_queue"
  (let* ((h (make-ofp_header :version 4 :type OFPT_QUEUE_GET_CONFIG_REQUEST :length 16 :xid 0))
         (req (make-ofp_queue_get_config_request :header h
                                                 :port 1))
         (expect #(4 #.OFPT_QUEUE_GET_CONFIG_REQUEST 0 16 0 0 0 0
                   0 0 0 1
                   0 0 0 0))
         (dump (with-fast-output (buf) (dump-ofp_queue_get_config_request req buf))))
    (is dump expect :test #'equalp))
  (let* ((h (make-ofp_header :version 4 :type OFPT_QUEUE_GET_CONFIG_REPLY :length 84 :xid 0))
         (v #(0 0 0 1
              0 0 0 0
                0 0 0 2
                0 0 0 3
                0 68 ; len
                0 0 0 0 0 0
                  0 #.OFPQT_MIN_RATE
                  0 16
                  0 0 0 0
                  0 1
                  0 0 0 0 0 0
                  0 #.OFPQT_MAX_RATE
                  0 16
                  0 0 0 0
                  0 1
                  0 0 0 0 0 0
                  255 255
                  0 20
                  0 0 0 0
                  0 0 0 1
                  0 0 0 0
                  0 1 2 3))
         (s (vs v))
         (b (make-ofp_queue_get_config_reply-stream h s)))
    (with-prefix (cr ofp_queue_get_config_reply-)
      (ok (cr p b))
      (is (cr port b) 1)
      (let ((qs (cr queues b)))
        (with-prefix (pq ofp_packet_queue-)
          (ok (listp qs))
          (is (length qs) 1)
          (is (pq queue_id (car qs)) 2)
          (is (pq port (car qs)) 3)
          (is (pq len (car qs)) 68)
          (let ((props (pq properties (car qs))))
            (ok (listp props))
            (is (length props) 3)
            (ofp_queue_prop_min_rate-p (nth 0 props))
            (ofp_queue_prop_max_rate-p (nth 1 props))
            (ofp_queue_prop_experimenter-p (nth 2 props))))))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_packet_out"
  (let* ((h (make-ofp_header :version 4 :type OFPT_PACKET_OUT :length 46 :xid 0))
         (o (make-ofp_packet_out :header h
                                 :buffer_id 1
                                 :in_port 2
                                 :actions_len 3
                                 :actions (list (make-ofp_action_output :type OFPAT_OUTPUT
                                                                        :len 8
                                                                        :port 1
                                                                        :max_len 0))
                                 :data #(0 1 2 3 4 5 6 7)))
         (expect #(4 #.OFPT_PACKET_OUT 0 46 0 0 0 0
                   0 0 0 1
                   0 0 0 2
                   0 3
                   0 0 0 0 0 0
                   0 #.OFPAT_OUTPUT 0 8 0 0 0 1 0 0
                   0 0 0 0 0 0
                   0 1 2 3 4 5 6 7))
         (dump (with-fast-output (buf) (dump-ofp_packet_out o buf))))
    (is dump expect :test #'equalp)))

(subtest "ofp_role"
  (let* ((h (make-ofp_header :version 4 :type OFPT_ROLE_REQUEST :length 24 :xid 0))
         (req (make-ofp_role_request :header h
                                     :role 1
                                     :generation_id 2))
         (expect #(4 #.OFPT_ROLE_REQUEST 0 24 0 0 0 0
                   0 0 0 1
                   0 0 0 0
                   0 0 0 0 0 0 0 2))
         (dump (with-fast-output (buf) (dump-ofp_role_request req buf))))
    (is dump expect :test #'equalp))
  (let* ((h (make-ofp_header :version 4 :type OFPT_ROLE_REPLY :length 24 :xid 0))
         (v #(0 0 0 1
              0 0 0 0
              0 0 0 0 0 0 0 2))
         (s (vs v))
         (b (make-ofp_role_request-stream h s)))
    (with-prefix (rr ofp_role_request-)
      (ok (rr p b))
      (is (rr role b) 1)
      (is (rr generation_id b) 2))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_async_config"
  (let* ((h (make-ofp_header :version 4 :type OFPT_SET_ASYNC :length 32 :xid 0))
         (req (make-ofp_async_config :header h
                                     :packet_in_mask (list 0 1)
                                     :port_status_mask (list 2 3)
                                     :flow_removed_mask (list 4 5)))
         (expect #(4 #.OFPT_SET_ASYNC 0 32 0 0 0 0
                   0 0 0 0
                   0 0 0 1
                   0 0 0 2
                   0 0 0 3
                   0 0 0 4
                   0 0 0 5))
         (dump (with-fast-output (buf) (dump-ofp_async_config req buf))))
    (is dump expect :test #'equalp))
  (let* ((h (make-ofp_header :version 4 :type OFPT_GET_ASYNC_REPLY :length 32 :xid 0))
         (v #(0 0 0 1
              0 0 0 2
              0 0 0 3
              0 0 0 4
              0 0 0 5
              0 0 0 6))
         (s (vs v))
         (b (make-ofp_async_config-stream h s)))
    (with-prefix (ac ofp_async_config-)
      (ok (ac p b))
      (is (ac packet_in_mask b) (list 1 2) :test #'equalp)
      (is (ac port_status_mask b) (list 3 4) :test #'equalp)
      (is (ac flow_removed_mask b) (list 5 6) :test #'equalp))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_packet_in"
  (let* ((h (make-ofp_header :version 4 :type OFPT_PACKET_IN :length 38 :xid 0))
         (v #(0 0 0 1
              0 4
              2
              3
              0 0 0 0 0 0 0 4
              0 1 0 4 0 0 0 0
              0 0
              0 1 2 3))
         (s (vs v))
         (b (make-ofp_packet_in-stream h s)))
    (with-prefix (pi ofp_packet_in-)
      (ok (pi p b))
      (is (pi buffer_id b) 1)
      (is (pi total_len b) 4)
      (is (pi reason b) 2)
      (is (pi table_id b) 3)
      (ok (ofp_match-p (pi match b)))
      (is (pi data b) #(0 1 2 3) :test #'equalp))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_flow_removed"
  (let* ((h (make-ofp_header :version 4 :type OFPT_FLOW_REMOVED :length 56 :xid 0))
         (v #(0 0 0 0 0 0 0 1
              0 2
              3
              4
              0 0 0 5
              0 0 0 6
              0 7
              0 8
              0 0 0 0 0 0 0 9
              0 0 0 0 0 0 0 10
              0 1 0 4 0 0 0 0))
         (s (vs v))
         (b (make-ofp_flow_removed-stream h s)))
    (with-prefix (fr ofp_flow_removed-)
      (ok (fr p b))
      (is (fr cookie b) 1)
      (is (fr priority b) 2)
      (is (fr reason b) 3)
      (is (fr table_id b) 4)
      (is (fr duration_sec b) 5)
      (is (fr duration_nsec b) 6)
      (is (fr idle_timeout b) 7)
      (is (fr hard_timeout b) 8)
      (is (fr packet_count b) 9)
      (is (fr byte_count b) 10)
      (ok (ofp_match-p (fr match b))))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_port_status"
  (let* ((h (make-ofp_header :version 4 :type OFPT_PORT_STATUS :length 80 :xid 0))
         (v `#(1
               0 0 0 0 0 0 0
                 0 0 0 2
                 0 0 0 0
                 0 1 2 3 4 5
                 0 0
                 ,(char-code #\p) ,@(loop :repeat (- OFP_MAX_PORT_NAME_LEN 1) :collect 0)
                 0 0 0 3
                 0 0 0 4
                 0 0 0 5
                 0 0 0 6
                 0 0 0 7
                 0 0 0 8
                 0 0 0 9
                 0 0 0 10))
         (s (vs v))
         (b (make-ofp_port_status-stream h s)))
    (with-prefix (ps ofp_port_status-)
      (ok (ps p b))
      (is (ps reason b) 1)
      (with-prefix (po ofp_port-)
        (let ((desc (ps desc b)))
          (ok (po p desc))
          (is (po port_no desc) 2)
          (is (po hw_addr desc) #(0 1 2 3 4 5) :test #'equalp)
          (is (po name desc) "p" :test #'string=)
          (is (po config desc) 3)
          (is (po state desc) 4)
          (is (po curr desc) 5)
          (is (po advertised desc) 6)
          (is (po supported desc) 7)
          (is (po peer desc) 8)
          (is (po curr_speed desc) 9)
          (is (po max_speed desc) 10))))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_error_msg"
  (let* ((h (make-ofp_header :version 4 :type OFPT_ERROR :length 16 :xid 0))
         (v #(0 1
              0 2
              0 1 2 3))
         (s (vs v))
         (b (make-ofp_error_msg-stream h s)))
    (with-prefix (em ofp_error_msg-)
      (ok (em p b))
      (is (em type b) 1)
      (is (em code b) 2)
      (is (em data b) #(0 1 2 3) :test #'equalp))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_expermenter_header"
  (let* ((h (make-ofp_header :version 4 :type OFPT_EXPERIMENTER :length 20 :xid 0))
         (v (make-ofp_experimenter_header :header h
                                          :experimenter 1
                                          :exp_type 2
                                          :data #(0 1 2 3)))
         (expect #(4 #.OFPT_EXPERIMENTER 0 20 0 0 0 0
                   0 0 0 1
                   0 0 0 2
                   0 1 2 3))
         (dump (with-fast-output (buf) (dump-ofp_experimenter_header v buf))))
    (is dump expect :test #'equalp))
  (let* ((h (make-ofp_header :version 4 :type OFPT_EXPERIMENTER :length 20 :xid 0))
         (v #(0 0 0 1
              0 0 0 2
              0 1 2 3))
         (s (vs v))
         (b (make-ofp_experimenter_header-stream h s)))
    (with-prefix (ep ofp_experimenter_header-)
      (ok (ep p b))
      (is (ep experimenter b) 1)
      (is (ep exp_type b) 2)
      (is (ep data b) #(0 1 2 3) :test #'equalp))
    (is-error (read-byte s) 'end-of-file)))

(subtest "ofp_match"
  (let* ((m (make-ofp_match :type OFPMT_OXM
                            :length 24
                            :oxm_fields (list (make-oxm :class 0
                                                        :field 1
                                                        :hasmask 0
                                                        :length 4
                                                        :value #(0 1 2 3))
                                              (make-oxm :class 0
                                                        :field 1
                                                        :hasmask 1
                                                        :length 8
                                                        :value #(0 1 2 3)
                                                        :mask #(4 5 6 7)))))
         (expect #(0 #.OFPMT_OXM
                   0 24
                   0 0 2 4 0 1 2 3
                   0 0 3 8 0 1 2 3 4 5 6 7 ))
         (dump (with-fast-output (buf) (dump-ofp_match m buf))))
    (is dump expect :test #'equalp))
  (let* ((v #(0 #.OFPMT_OXM
              0 24
              0 0 2 4 0 1 2 3
              0 0 3 8 0 1 2 3 4 5 6 7))
         (s (vs v))
         (b (make-ofp_match-stream s)))
    (with-prefix (m ofp_match-)
      (ok (m p b))
      (is (m type b) OFPMT_OXM)
      (is (m length b) 24)
      (is (m oxm_fields b) (list (make-oxm :class 0
                                           :field 1
                                           :hasmask 0
                                           :length 4
                                           :value #(0 1 2 3))
                                 (make-oxm :class 0
                                           :field 1
                                           :hasmask 1
                                           :length 8
                                           :value #(0 1 2 3)
                                           :mask #(4 5 6 7)))
          :test #'equalp))
    (is-error (read-byte s) 'end-of-file)))

(finalize)
