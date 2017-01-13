(in-package :cl-user)
(defpackage kappa.converter.1.3
  (:use :cl
        :fast-io
        :anaphora
        :kappa.converter
        :kappa.define.1.3)
  (:import-from :kappa.define
                :ofp_header-length)
  (:import-from :kappa.util
                :with-prefix)
  (:import-from :babel
                :octets-to-string
                :string-to-octets))
(in-package :kappa.converter.1.3)

(annot:enable-annot-syntax)

@export
(defun make-ofp_switch_features-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8)))
    (with-fast-input (buf nil stream)
      (make-ofp_switch_features :header header
                                :datapath_id (readu64-be buf)
                                :n_buffers (readu32-be buf)
                                :n_tables (readu8-be buf)
                                :auxiliary_id (readu8-be buf)
                                :capabilities (progn
                                                (readu16-be buf) ; pad
                                                (readu32-be buf))
                                :reserved (readu32-be buf)))))

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
(defun dump-ofp_table_mod (mod buf)
  (with-prefix (tm ofp_table_mod-)
    (dump-ofp_header (tm header mod) buf)
    (writeu8-be (tm table_id mod) buf)
    (loop :repeat 3 :do (writeu8-be 0 buf)) ; pad
    (writeu32-be (tm config mod) buf)))

@export
(defun make-ofp_table_mod-stream (header stream)
  (with-fast-input (buf nil stream)
    (make-ofp_table_mod :header header
                        :table_id (readu8-be buf)
                        :config (progn
                                  (loop :repeat 3 :do (readu8-be buf)) ;pad
                                  (readu32-be buf)))))

@export
(defun dump-ofp_match (match buf)
  (with-prefix (ma ofp_match-)
    (if (and (not (ma type match)) (not (ma length match)))
      (progn
        (writeu16-be OFPMT_OXM buf)
        (writeu16-be 4 buf)
        (writeu32-be 0 buf))
      (let ((len (ma length match)))
        (writeu16-be (ma type match) buf)
        (writeu16-be len buf)
        (loop :for i :across (or (ma oxm_fields match) #())
              :do (writeu8-be i buf))
        (loop :repeat (padsize len)
              :do (writeu8-be 0 buf))))))

(defun read-vector (len buf)
  (let ((vec (make-octet-vector len)))
    (fast-read-sequence vec buf 0 len)
    vec))

@export
(defun make-ofp_match-buffer (buf)
  (let ((type (readu16-be buf))
        (len (readu16-be buf)))
    (prog1
      (make-ofp_match :type type
                      :length len
                      :oxm_fields (read-vector (- len 4) buf))
      (loop :repeat (padsize len)
            :do (readu8-be buf)))))

@export
(defun make-ofp_match-stream (stream)
  (with-fast-input (buf nil stream)
    (make-ofp_match-buffer buf)))

@export
(defun dump-ofp_flow_mod (mod buf)
  (with-prefix (fm ofp_flow_mod-)
    (dump-ofp_header (fm header mod) buf)
    (writeu64-be (fm cookie mod) buf)
    (writeu64-be (fm cookie_mask mod) buf)
    (writeu8-be (fm table_id mod) buf)
    (writeu8-be (fm command mod) buf)
    (writeu16-be (fm idle_timeout mod) buf)
    (writeu16-be (fm hard_timeout mod) buf)
    (writeu16-be (fm priority mod) buf)
    (writeu32-be (fm buffer_id mod) buf)
    (writeu32-be (fm out_port mod) buf)
    (writeu32-be (fm out_group mod) buf)
    (writeu16-be (fm flags mod) buf)
    (writeu16-be 0 buf) ; pad
    (dump-ofp_match (fm match mod) buf)
    (aif (fm instructions mod)
      (dump-ofp_instructions it buf))))

(defun dump-ofp_instructions (insts buf)
  (loop :for i :in insts
        :do (cond ((ofp_instruction_goto_table-p i)
                   (with-prefix (in ofp_instruction_goto_table-)
                     (writeu16-be (in type i) buf)
                     (writeu16-be (in len i) buf)
                     (writeu8-be (in table_id i) buf)
                     (loop :repeat 3 :do (writeu8-be 0 buf)))) ; pad
                  ((ofp_instruction_write_metadata-p i)
                   (with-prefix (in ofp_instruction_write_metadata-)
                     (writeu16-be (in type i) buf)
                     (writeu16-be (in len i) buf)
                     (writeu32-be 0 buf) ; pad
                     (writeu64-be (in metadata i) buf)
                     (writeu64-be (in metadata_mask i) buf)))
                  ((ofp_instruction_actions-p i)
                   (with-prefix (in ofp_instruction_actions-)
                     (writeu16-be (in type i) buf)
                     (writeu16-be (in len i) buf)
                     (writeu32-be 0 buf) ; pad
                     (aif (in actions i)
                       (dump-ofp_actions it buf))))
                  ((ofp_instruction_meter-p i)
                   (with-prefix (in ofp_instruction_meter-)
                     (writeu16-be (in type i) buf)
                     (writeu16-be (in len i) buf)
                     (writeu32-be (in meter_id i) buf))))))

(defun dump-ofp_actions (actions buf)
  (loop :for a :in actions
        :do (cond ((ofp_action_output-p a)
                   (with-prefix (act ofp_action_output-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (writeu32-be (act port a) buf)
                     (writeu16-be (act max_len a) buf)
                     (loop :repeat 6 :do (writeu8-be 0 buf)))) ; pad
                  ((ofp_action_group-p a)
                   (with-prefix (act ofp_action_group-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (writeu32-be (act group_id a) buf)))
                  ((ofp_action_set_queue-p a)
                   (with-prefix (act ofp_action_set_queue-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (writeu32-be (act queue_id a) buf)))
                  ((ofp_action_mpls_ttl-p a)
                   (with-prefix (act ofp_action_mpls_ttl-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (writeu8-be (act mpls_ttl a) buf)
                     (loop :repeat 3 :do (writeu8-be 0 buf)))) ; pad
                  ((ofp_action_nw_ttl-p a)
                   (with-prefix (act ofp_action_nw_ttl-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (writeu8-be (act nw_ttl a) buf)
                     (loop :repeat 3 :do (writeu8-be 0 buf)))) ; pad
                  ((ofp_action_push-p a)
                   (with-prefix (act ofp_action_push-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (writeu16-be (act ethertype a) buf)
                     (writeu16-be 0 buf))) ; pad
                  ((ofp_action_pop_mpls-p a)
                   (with-prefix (act ofp_action_pop_mpls-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (writeu16-be (act ethertype a) buf)
                     (writeu16-be 0 buf))) ; pad
                  ((ofp_action_set_field-p a)
                   (with-prefix (act ofp_action_set_field-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (loop :for i :across (act field a)
                           :do (writeu8-be i buf))
                     (loop :repeat (padsize (+ (length (act field a)) 4))
                           :do (writeu8-be 0 buf)))) ; pad
                  ((ofp_action_experimenter_header-p a)
                   (with-prefix (act ofp_action_experimenter_header-)
                     (writeu16-be (act type a) buf)
                     (writeu16-be (act len a) buf)
                     (writeu32-be (act experimenter a) buf)
                     (loop :for i :across (act data a)
                           :do (writeu8-be i buf))
                     (loop :repeat (- (act len a) 8 (length (act data a)))
                           :do (writeu8-be 0 buf)))))))

(defun make-ofp_instructions-buffer (buf len)
  (loop :while (> len 0)
        :collect (let ((type (readu16-be buf))
                       (blen (readu16-be buf)))
                   (setf len (- len blen))
                   (cond ((= type OFPIT_GOTO_TABLE)
                          (make-ofp_instruction_goto_table :type type
                                                           :len blen
                                                           :table_id (prog1
                                                                       (readu8-be buf)
                                                                       (loop :repeat 3 :do (readu8-be buf))))) ; pad
                         ((= type OFPIT_WRITE_METADATA)
                          (make-ofp_instruction_write_metadata :type type
                                                               :len blen
                                                               :metadata_match (progn
                                                                                 (readu32-be buf) ; pad
                                                                                 (readu64-be buf))
                                                               :metadata_mask (readu64-be buf)))
                         ((or (= type OFPIT_WRITE_ACTIONS) (= type OFPIT_APPLY_ACTIONS) (= type OFPIT_CLEAR_ACTIONS))
                          (make-ofp_instruction_actions :type type
                                                        :len blen
                                                        :actions (progn
                                                                   (readu32-be buf) ; pad
                                                                   (make-ofp_actions-buffer buf (- blen 8)))))
                         ((= type OFPIT_METER)
                          (make-ofp_instruction_meter :type type
                                                      :len blen
                                                      :meter_id (readu32-be buf)))))))

@export
(defun make-ofp_actions-buffer (buf len)
  (loop :while (> len 0)
        :collect (let ((type (readu16-be buf))
                       (blen (readu16-be buf)))
                   (setf len (- len blen))
                   (cond ((= type OFPAT_OUTPUT)
                          (make-ofp_action_output :type type
                                                  :len blen
                                                  :port (readu32-be buf)
                                                  :max_len (prog1
                                                             (readu16-be buf)
                                                             (loop :repeat 6 :do (readu8-be buf))))) ; pad
                         ((= type OFPAT_GROUP)
                          (make-ofp_action_group :type type
                                                 :len blen
                                                 :group_id (readu32-be buf)))
                         ((= type OFPAT_SET_QUEUE)
                          (make-ofp_action_set_queue :type type
                                                     :len blen
                                                     :queue_id (readu32-be buf)))
                         ((= type OFPAT_SET_MPLS_TTL)
                          (make-ofp_action_mpls_ttl :type type
                                                    :len blen
                                                    :mpls_ttl (prog1
                                                                (readu8-be buf)
                                                                (loop :repeat 3 :do (readu8-be buf))))) ; pad
                         ((= type OFPAT_SET_NW_TTL)
                          (make-ofp_action_nw_ttl :type type
                                                  :len blen
                                                  :nw_ttl (prog1
                                                            (readu8-be buf)
                                                            (loop :repeat 3 :do (readu8-be buf))))) ; pad
                         ((or (= type OFPAT_PUSH_VLAN) (= type OFPAT_PUSH_MPLS) (= type OFPAT_PUSH_PBB))
                          (make-ofp_action_puth :type type
                                                :len blen
                                                :ethertype (prog1
                                                             (readu16-be buf)
                                                             (readu16-be buf)))) ; pad
                         ((= type OFPAT_POP_MPLS)
                          (make-ofp_action_pop_mpls :type type
                                                    :len blen
                                                    :ethertype (prog1
                                                                 (readu16-be buf)
                                                                 (readu16-be buf)))) ; pad
                         ((= type OFPAT_SET_FIELD)
                          (make-ofp_action_set_field :type type
                                                     :len blen
                                                     :field (read-vector (- blen 4) buf))) ; include pad.
                         ((= type OFPAT_EXPERIMENTER)
                          (make-ofp_action_experimenter_header :type type
                                                               :len blen
                                                               :experimenter (readu32-be buf)
                                                               :data (read-vector (-blen 8) buf)))))))

@export
(defun get-actions-length (actions)
  (apply #'+ (loop :for a :in actions
                   :collect (cond ((ofp_action_output-p a) 16)
                                  ((ofp_action_group-p a) 8)
                                  ((ofp_action_set_queue-p a) 8)
                                  ((ofp_action_mpls_ttl-p a) 8)
                                  ((ofp_action_nw_ttl-p a) 8)
                                  ((ofp_action_push-p a) 8)
                                  ((ofp_action_pop_mpls-p a) 8)
                                  ((ofp_action_set_field-p a)
                                   (ofp_action_set_field-len a))
                                  ((ofp_action_experimenter_header-p a)
                                   (ofp_action_experimenter_header-len a))))))

@export
(defun get-instructions-length (insts)
  (apply #'+ (loop :for i :in insts
                   :collect (cond ((ofp_instruction_goto_table-p i) 8)
                                  ((ofp_instruction_write_metadata-p i) 24)
                                  ((ofp_instruction_actions-p i)
                                   (+ 8 (get-actions-length (ofp_instruction_actions-actions i))))
                                  ((ofp_instruction_meter-p i) 8)))))

@export
(defun dump-ofp_group_mod (mod buf)
  (with-prefix (gm ofp_group_mod-)
    (dump-ofp_header (gm header mod) buf)
    (writeu16-be (gm command mod) buf)
    (writeu8-be (gm type mod) buf)
    (writeu8-be 0 buf) ; pad
    (writeu32-be (gm group_id mod) buf)
    (aif (gm buckets mod)
      (dump-ofp_buckets it buf))))

(defun dump-ofp_buckets (bs buf)
  (with-prefix (bu ofp_bucket-)
    (loop :for b :in bs
          :do (progn
                (writeu16-be (bu len b) buf)
                (writeu16-be (bu weight b) buf)
                (writeu32-be (bu watch_port b) buf)
                (writeu32-be (bu watch_group b) buf)
                (writeu32-be 0 buf) ; pad
                (aif (bu actions b)
                  (dump-ofp_actions it buf))))))

@export
(defun dump-ofp_port_mod (mod buf)
  (with-prefix (pm ofp_port_mod-)
    (dump-ofp_header (pm header mod) buf)
    (writeu32-be (pm port_no mod) buf)
    (writeu32-be 0 buf) ; pad
    (loop :for i :across (pm hw_addr mod)
          :do (writeu8-be i buf))
    (writeu16-be 0 buf) ; pad
    (writeu32-be (pm config mod) buf)
    (writeu32-be (pm mask mod) buf)
    (writeu32-be (pm advertise mod) buf)
    (writeu32-be 0 buf))) ; pad

@export
(defun dump-ofp_meter_mod (mod buf)
  (with-prefix (mm ofp_meter_mod-)
    (dump-ofp_header (mm header mod) buf)
    (writeu16-be (mm command mod) buf)
    (writeu16-be (mm flags mod) buf)
    (writeu32-be (mm meter_id mod) buf)
    (aif (mm bands mod)
      (dump-ofp_meter_bands it buf))))

(defun dump-ofp_meter_bands (bands buf)
  (with-prefix (mb ofp_meter_band_)
    (loop :for b :in bands
          :do (cond ((mb drop-p b)
                     (with-prefix (m ofp_meter_band_drop-)
                       (writeu16-be (m type b) buf)
                       (writeu16-be (m len b) buf)
                       (writeu32-be (m rate b) buf)
                       (writeu32-be (m burst_size b) buf)
                       (writeu32-be 0 buf))) ; pad
                    ((mb dscp_remark-p b)
                     (with-prefix (m ofp_meter_band_dscp_remark-)
                       (writeu16-be (m type b) buf)
                       (writeu16-be (m len b) buf)
                       (writeu32-be (m rate b) buf)
                       (writeu32-be (m burst_size b) buf)
                       (writeu8-be (m prec_level b) buf)
                       (loop :repeat 3 :do (writeu8-be 0 buf)))) ; pad
                    ((mb experimenter-p b)
                     (with-prefix (m ofp_meter_band_experimenter-)
                       (writeu16-be (m type b) buf)
                       (writeu16-be (m len b) buf)
                       (writeu32-be (m rate b) buf)
                       (writeu32-be (m burst_size b) buf)
                       (writeu32-be (m experimenter b) buf)
                       (loop :for d :across (or (m data b) #())
                             :do (writeu8-be d buf))))))))

@export
(defun dump-ofp_multipart_request (req buf)
  (with-prefix (mp ofp_multipart_request-)
    (dump-ofp_header (mp header req) buf)
    (writeu16-be (mp type req) buf)
    (writeu16-be (mp flags req) buf)
    (writeu32-be 0 buf) ; pad
    (let ((b (mp body req)))
      (cond ((ofp_flow_stats_request-p b)
             (with-prefix (bo ofp_flow_stats_request-)
               (writeu8-be (bo table_id b) buf)
               (loop :repeat 3 :do (writeu8-be 0 buf)) ; pad
               (writeu32-be (bo out_port b) buf)
               (writeu32-be (bo out_group b) buf)
               (writeu32-be 0 buf) ; pad
               (writeu64-be (bo cookie b) buf)
               (writeu64-be (bo cookie_mask b) buf)
               (dump-ofp_match (bo match b) buf)))
            ((ofp_aggregate_stats_request-p b)
             (with-prefix (bo ofp_aggregate_stats_request-)
               (writeu8-be (bo table_id b) buf)
               (loop :repeat 3 :do (writeu8-be 0 buf)) ; pad
               (writeu32-be (bo out_port b) buf)
               (writeu32-be (bo out_group b) buf)
               (writeu32-be 0 buf) ; pad
               (writeu64-be (bo cookie b) buf)
               (writeu64-be (bo cookie_mask b) buf)
               (dump-ofp_match (bo match b) buf)))
            ((ofp_table_features-p b)
             (with-prefix (bo ofp_table_features-)
               (writeu16-be (bo length b) buf)
               (writeu8-be (bo table_id b) buf)
               (loop :repeat 5 :do (writeu8-be 0 buf)) ; pad
               (let ((vec (string-to-octets (bo name b))))
                 (fast-write-sequence vec buf)
                 (loop :repeat (- OFP_MAX_TABLE_NAME_LEN (length vec))
                       :do (writeu8-be 0 buf)))
               (writeu64-be (bo metadata_match b) buf)
               (writeu64-be (bo metadata_write b) buf)
               (writeu32-be (bo config b) buf)
               (writeu32-be (bo max_entries b) buf)
               (aif (bo properties b)
                 (dump-ofp_table_feature_props it buf))))
            ((ofp_port_stats_request-p b)
             (with-prefix (bo ofp_port_stats_request-)
               (writeu32-be (bo port_no b) buf)
               (writeu32-be 0 buf))) ; pad
            ((ofp_queue_stats_request-p b)
             (with-prefix (bo ofp_queue_stats_request-)
               (writeu32-be (bo port_no b) buf)
               (writeu32-be (bo queue_id b) buf)))
            ((ofp_group_stats_request-p b)
             (with-prefix (bo ofp_group_stats_request-)
               (writeu32-be (bo group_id b) buf)
               (writeu32-be 0 buf))) ; pad
            ((ofp_meter_multipart_request-p b)
             (with-prefix (bo ofp_meter_multipart_request-)
               (writeu32-be (bo meter_id b) buf)
               (writeu32-be 0 buf))) ; pad
            ((ofp_experimenter_multipart_header-p b)
             (with-prefix (bo ofp_experimenter_multipart_header-)
               (writeu32-be (bo experimenter b) buf)
               (writeu32-be (bo exp_type b) buf)
               (loop :for v :across (or (bo data b) #())
                     :do (writeu8-be v buf))))))))

(defun dump-ofp_table_feature_props (props buf)
  (loop :for prop :in props
        :do (cond ((ofp_table_feature_prop_instructions-p prop)
                   (with-prefix (pr ofp_table_feature_prop_instructions-)
                     (writeu16-be (pr type prop) buf)
                     (writeu16-be (pr length prop) buf)
                     (dump-ofp_instructions (pr instruction_ids prop) buf)
                     (loop :repeat (padsize (pr length prop))
                           :do (writeu8-be 0 buf)))) ; pad
                  ((ofp_table_feature_prop_next_tables-p prop)
                   (with-prefix (pr ofp_table_feature_prop_next_tables-)
                     (writeu16-be (pr type prop) buf)
                     (writeu16-be (pr length prop) buf)
                     (loop :for id :in (pr next_table_ids prop)
                           :do (writeu8-be id buf))
                     (loop :repeat (padsize (pr length prop))
                           :do (writeu8-be 0 buf)))) ; pad
                  ((ofp_table_feature_prop_actions-p prop)
                   (with-prefix (pr ofp_table_feature_prop_actions-)
                     (writeu16-be (pr type prop) buf)
                     (writeu16-be (pr length prop) buf)
                     (dump-ofp_actions (pr action_ids prop) buf)
                     (loop :repeat (padsize (pr length prop))
                           :do (writeu8-be 0 buf)))) ; pad
                  ((ofp_table_feature_prop_oxm-p prop)
                   (with-prefix (pr ofp_table_feature_prop_oxm-)
                     (writeu16-be (pr type prop) buf)
                     (writeu16-be (pr length prop) buf)
                     (loop :for oxm :in (pr oxm_ids prop)
                           :do (writeu32-be oxm buf))
                     (loop :repeat (padsize (pr length prop))
                           :do (writeu8-be 0 buf))))))) ; pad

@export
(defun make-ofp_multipart_reply-stream (header stream)
  (with-fast-input (buf nil stream)
    (let* ((rep (make-ofp_multipart_reply :header header
                                          :type (readu16-be buf)
                                          :flags (readu16-be buf)
                                          :body nil))
           (type (ofp_multipart_reply-type rep)))
      (readu32-be buf) ; pad
      (setf (ofp_multipart_reply-body rep)
            (cond ((= type OFPMP_DESC)
                   (make-ofp_desc :mfr_desc (let ((vec (make-octet-vector DESC_STR_LEN)))
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
                  ((= type OFPMP_FLOW)
                   (let* ((flow (make-ofp_flow_stats :length (readu16-be buf)
                                                     :table_id (prog1
                                                                 (readu8-be buf)
                                                                 (readu8-be buf)) ; pad
                                                     :duration_sec (readu32-be buf)
                                                     :duration_nsec (readu32-be buf)
                                                     :priority (readu16-be buf)
                                                     :idle_timeout (readu16-be buf)
                                                     :hard_timeout (readu16-be buf)
                                                     :flags (readu16-be buf)
                                                     :cookie (progn
                                                               (readu32-be buf) ; pad
                                                               (readu64-be buf))
                                                     :packet_count (readu64-be buf)
                                                     :byte_count (readu64-be buf)
                                                     :match (make-ofp_match-buffer buf)
                                                     :instructions nil))
                          (_mlen (ofp_match-length (ofp_flow_stats-match flow)))
                          (mlen (+ _mlen (padsize _mlen)))
                          (ilen (- (ofp_flow_stats-length flow) 48 mlen)))
                     (setf (ofp_flow_stats-instructions flow) (make-ofp_instructions-buffer buf ilen))
                     flow))
                  ((= type OFPMP_AGGREGATE)
                   (make-ofp_aggregate_stats_reply :packet_count (readu64-be buf)
                                                   :byte_count (readu64-be buf)
                                                   :flow_count (prog1
                                                                 (readu32-be buf)
                                                                 (readu32-be buf)))) ; pad
                  ((= type OFPMP_TABLE)
                   (make-ofp_table_stats :table_id (readu8-be buf)
                                         :active_count (progn
                                                         (loop :repeat 3 :do (readu8-be buf)) ; pad
                                                         (readu32-be buf))
                                         :lookup_count (readu64-be buf)
                                         :matched_count (readu64-be buf)))
                  ((= type OFPMP_TABLE_FEATURES)
                   (let ((len (readu16-be buf)))
                     (make-ofp_table_features :length len
                                              :table_id (prog1
                                                          (readu8-be buf)
                                                          (loop :repeat 5 :do (readu8-be buf))) ; pad
                                              :name (let ((vec (read-vector OFP_MAX_TABLE_NAME_LEN buf)))
                                                      (octets-to-string vec :end (position 0 vec)))
                                              :metadata_match (readu64-be buf)
                                              :metadata_write (readu64-be buf)
                                              :config (readu32-be buf)
                                              :max_entries (readu32-be buf)
                                              :properties (make-ofp_table_feature_props-buffer buf (- len 64)))))
                  ((= type OFPMP_PORT_STATS)
                   (make-ofp_port_stats :port_no (readu32-be buf)
                                        :rx_packets (progn
                                                      (readu32-be buf) ; pad
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
                                        :collisions (readu64-be buf)
                                        :duration_sec (readu32-be buf)
                                        :duration_nsec (readu32-be buf)))
                  ((= type OFPMP_PORT_DESC)
                   (make-ofp_port :port_no (readu32-be buf)
                                  :hw_addr (progn
                                             (readu32-be buf) ; pad
                                             (read-vector OFP_ETH_ALEN buf))
                                  :name (progn
                                          (readu16-be buf) ; pad
                                          (let ((vec (read-vector OFP_MAX_PORT_NAME_LEN buf)))
                                            (octets-to-string vec :end (position 0 vec))))
                                  :config (readu32-be buf)
                                  :state (readu32-be buf)
                                  :curr (readu32-be buf)
                                  :advertised (readu32-be buf)
                                  :supported (readu32-be buf)
                                  :peer (readu32-be buf)
                                  :curr_speed (readu32-be buf)
                                  :max_speed (readu32-be buf)))
                  ((= type OFPMP_QUEUE)
                   (make-ofp_queue_stats :port_no (readu32-be buf)
                                         :queue_id (readu32-be buf)
                                         :tx_bytes (readu64-be buf)
                                         :tx_packets (readu64-be buf)
                                         :tx_errors (readu64-be buf)
                                         :duration_sec (readu32-be buf)
                                         :duration_nsec (readu32-be buf)))
                  ((= type OFPMP_GROUP)
                   (let ((len (readu16-be buf)))
                     (make-ofp_group_stats :length len
                                           :group_id (progn
                                                       (readu16-be buf) ; pad
                                                       (readu32-be buf))
                                           :ref_count (readu32-be buf)
                                           :packet_count (progn
                                                           (readu32-be buf) ; pad
                                                           (readu64-be buf))
                                           :byte_count (readu64-be buf)
                                           :duration_sec (readu32-be buf)
                                           :duration_nsec (readu32-be buf)
                                           :bucket_stats (loop :repeat (truncate (- len 40) 16)
                                                               :collect (make-ofp_bucket_counter :packet_count (readu64-be buf)
                                                                                                 :byte_count (readu64-be buf))))))
                  ((= type OFPMP_GROUP_DESC)
                   (let ((len (readu16-be buf)))
                     (make-ofp_group_desc_stats :length len
                                                :type (readu8-be buf)
                                                :group_id (progn
                                                            (readu8-be buf) ; pad
                                                            (readu32-be buf))
                                                :buckets (make-ofp_bucket-buffer buf (- len 8)))))
                  ((= type OFPMP_GROUP_FEATURES)
                   (make-ofp_group_features :types (readu32-be buf)
                                            :capabilities (readu32-be buf)
                                            :max_groups (loop :repeat 4 :collect (readu32-be buf))
                                            :actions (loop :repeat 4 :collect (readu32-be buf))))
                  ((= type OFPMP_METER)
                   (let ((meter_id (readu32-be buf))
                         (len (readu16-be buf)))
                     (loop :repeat 6 :do (readu8-be buf)) ; pad
                     (make-ofp_meter_stats :meter_id meter_id
                                           :len len
                                           :flow_count (readu32-be buf)
                                           :packet_in_count (readu64-be buf)
                                           :byte_in_count (readu64-be buf)
                                           :duration_sec (readu32-be buf)
                                           :duration_nsec (readu32-be buf)
                                           :band_stats (loop :repeat (truncate (- len 40) 16)
                                                             :collect (make-ofp_meter_band_stats :packet_band_count (readu64-be buf)
                                                                                                 :byte_band_count (readu64-be buf))))))
                  ((= type OFPMP_METER_CONFIG)
                   (let ((len (readu16-be buf)))
                     (make-ofp_meter_config :length len
                                            :flags (readu16-be buf)
                                            :meter_id (readu32-be buf)
                                            :bands (make-ofp_meter_bands-buffer buf (- len 8)))))
                  ((= type OFPMP_METER_FEATURES)
                   (make-ofp_meter_features :max_meter (readu32-be buf)
                                            :band_types (readu32-be buf)
                                            :capabilities (readu32-be buf)
                                            :max_bands (readu8-be buf)
                                            :max_color (prog1
                                                         (readu8-be buf)
                                                         (readu16-be buf)))) ; pad
                  ((= type OFPMP_EXPERIMENTER)
                   (let ((blen (- (ofp_header-length header) 16)))
                     (make-ofp_experimenter_multipart_header :experimenter (readu32-be buf)
                                                             :exp_type (readu32-be buf)
                                                             :data (read-vector (- blen 8) buf))))
                  (t nil)))
      rep)))

(defun padsize (len)
  (- (* (truncate (+ len 7) 8) 8) len))

(defun make-ofp_table_feature_props-buffer (buf len)
  (loop :while (> len 0)
        :collect (let ((type (readu16-be buf))
                       (blen (readu16-be buf)))
                   (setf len (- len blen (padsize blen)))
                   (cond ((or (= type OFPTFPT_INSTRUCTIONS) (= type OFPTFPT_INSTRUCTIONS_MISS))
                          (make-ofp_table_feature_prop_instructions
                            :type type
                            :length blen
                            :instruction_ids (prog1
                                               (make-ofp_instructions-buffer buf (- blen 4))
                                               (loop :repeat (padsize blen) :do (readu8-be buf))))) ; pad
                         ((or (= type OFPTFPT_NEXT_TABLES) (= type OFPTFPT_NEXT_TABLES_MISS))
                          (make-ofp_table_feature_prop_next_tables
                            :type type
                            :length blen
                            :next_table_ids (prog1
                                              (loop :repeat (- blen 4) :collect (readu8-be buf))
                                              (loop :repeat (padsize blen) :do (readu8-be buf))))) ; pad
                         ((or (= type OFPTFPT_WRITE_ACTIONS) (= type OFPTFPT_WRITE_ACTIONS_MISS)
                              (= type OFPTFPT_APPLY_ACTIONS) (= type OFPTFPT_APPLY_ACTIONS_MISS))
                          (make-ofp_table_feature_prop_actions
                            :type type
                            :length blen
                            :action_ids (prog1
                                          (make-ofp_actions-buffer buf (- blen 4))
                                          (loop :repeat (padsize blen) :do (readu8-be buf))))) ; pad
                         ((or (= type OFPTFPT_MATCH) (= type OFPTFPT_WILDCARDS)
                              (= type OFPTFPT_WRITE_SETFIELD) (= type OFPTFPT_WRITE_SETFIELD_MISS)
                              (= type OFPTFPT_APPLY_SETFIELD) (= type OFPTFPT_APPLY_SETFIELD_MISS))
                          (make-ofp_table_feature_prop_oxm
                            :type type
                            :length blen
                            :oxm_ids (prog1
                                       (loop :repeat (truncate (- blen 4) 4) :collect (readu32-be buf))
                                       (loop :repeat (padsize blen) :do (readu8-be buf))))))))) ; pad

(defun make-ofp_meter_bands-buffer (buf len)
  (loop :while (> len 0)
        :collect (let ((type (readu16-be buf))
                       (blen (readu16-be buf)))
                   (setf len (- len blen))
                   (cond ((= type OFPMBT_DROP)
                          (make-ofp_meter_band_drop :type type
                                                    :len blen
                                                    :rate (readu32-be buf)
                                                    :burst_size (prog1
                                                                  (readu32-be buf)
                                                                  (readu32-be buf)))) ; pad
                         ((= type OFPMBT_DSCP_REMARK)
                          (make-ofp_meter_band_dscp_remark :type type
                                                           :len blen
                                                           :rate (readu32-be buf)
                                                           :burst_size (readu32-be buf)
                                                           :prec_level (prog1
                                                                         (readu8-be buf)
                                                                         (loop :repeat 3 :do (readu8-be buf))))) ; pad
                         ((= type OFPMBT_EXPERIMENTER)
                          (make-ofp_meter_band_experimenter :type type
                                                            :len blen
                                                            :rate (readu32-be buf)
                                                            :burst_size (readu32-be buf)
                                                            :experimenter (readu32-be buf)
                                                            :data (read-vector (- blen 16) buf)))))))

(defun make-ofp_bucket-buffer (buf len)
  (loop :while (> len 0)
        :collect (let ((blen (readu16-be buf)))
                   (setf len (- len blen))
                   (make-ofp_bucket :len blen
                                    :weight (readu16-be buf)
                                    :watch_port (readu32-be buf)
                                    :watch_group (readu32-be buf)
                                    :actions (progn
                                               (readu32-be buf) ; pad
                                               (make-ofp_actions-buffer buf (- blen 16)))))))

@export
(defun dump-ofp_queue_get_config_request (req buf)
  (with-prefix (qc ofp_queue_get_config_request-)
    (dump-ofp_header (qc header req) buf)
    (writeu32-be (qc port req) buf)
    (writeu32-be 0 buf))) ; pad

@export
(defun make-ofp_queue_get_config_reply-stream (header stream)
  (with-fast-input (buf nil stream)
    (make-ofp_queue_get_config_reply :header header
                                     :port (readu32-be buf)
                                     :queues (progn
                                               (readu32-be buf) ; pad
                                               (make-ofp_packet_queue-buffer buf
                                                                             (- (ofp_header-length header) 16))))))

(defun make-ofp_queue_prop_header-buffer (buf)
  (make-ofp_queue_prop_header :property (readu16-be buf)
                              :len (prog1
                                     (readu16-be buf)
                                     (readu32-be buf)))) ; pad

(defun make-ofp_packet_queue-buffer (buf len)
  (loop :while (> len 0)
        :collect (let ((queue_id (readu32-be buf))
                       (port (readu32-be buf))
                       (blen (readu16-be buf)))
                   (setf len (- len blen))
                   (loop :repeat 6 :do (readu8-be buf)) ; pad
                   (make-ofp_packet_queue :queue_id queue_id
                                          :port port
                                          :len blen
                                          :properties (make-ofp_queue_props-buffer buf (- blen 16))))))

(defun make-ofp_queue_props-buffer (buf len)
  (loop :while (> len 0)
        :collect (let* ((h (make-ofp_queue_prop_header-buffer buf))
                        (prop (ofp_queue_prop_header-property h))
                        (blen (ofp_queue_prop_header-len h)))
                   (setf len (- len blen))
                   (cond ((= prop OFPQT_MIN_RATE)
                          (make-ofp_queue_prop_min_rate :prop_header h
                                                        :rate (prog1
                                                                (readu16-be buf)
                                                                (loop :repeat 6 :do (readu8-be buf))))) ; pad
                         ((= prop OFPQT_MAX_RATE)
                          (make-ofp_queue_prop_max_rate :prop_header h
                                                        :rate (prog1
                                                                (readu16-be buf)
                                                                (loop :repeat 6 :do (readu8-be buf))))) ; pad
                         ((= prop OFPQT_EXPERIMENTER)
                          (make-ofp_queue_prop_experimenter :prop_header h
                                                            :experimenter (readu32-be buf)
                                                            :data (progn
                                                                    (readu32-be buf) ; pad
                                                                    (read-vector (- blen 16) buf))))))))

@export
(defun dump-ofp_packet_out (out buf)
  (with-prefix (po ofp_packet_out-)
    (dump-ofp_header (po header out) buf)
    (writeu32-be (po buffer_id out) buf)
    (writeu32-be (po in_port out) buf)
    (writeu16-be (po actions_len out) buf)
    (loop :repeat 6 :do (writeu8-be 0 buf)) ; pad
    (dump-ofp_actions (po actions out) buf)
    (loop :for i :across (po data out)
          :do (fast-write-byte i buf))))

@export
(defun dump-ofp_role_request (req buf)
  (with-prefix (ro ofp_role_request-)
    (dump-ofp_header (ro header req) buf)
    (writeu32-be (ro role req) buf)
    (writeu32-be 0 buf) ; pad
    (writeu64-be (ro generation_id req) buf)))

@export
(defun make-ofp_role_request-stream (header stream)
  (with-fast-input (buf nil stream)
    (make-ofp_role_request :header header
                           :role (readu32-be buf)
                           :generation_id (progn
                                            (readu32-be buf) ; pad
                                            (readu64-be buf)))))

@export
(defun dump-ofp_async_config (con buf)
  (with-prefix (ac ofp_async_config-)
    (dump-ofp_header (ac header con) buf)
    (loop :for i :in (ac packet_in_mask con)
          :do (writeu32-be i buf))
    (loop :for i :in (ac port_status_mask con)
          :do (writeu32-be i buf))
    (loop :for i :in (ac flow_removed_mask con)
          :do (writeu32-be i buf))))

@export
(defun make-ofp_async_config-stream (header stream)
  (with-fast-input (buf nil stream)
    (make-ofp_async_config :header header
                           :packet_in_mask (loop :repeat 2
                                                 :collect (readu32-be buf))
                           :port_status_mask (loop :repeat 2
                                                   :collect (readu32-be buf))
                           :flow_removed_mask (loop :repeat 2
                                                    :collect (readu32-be buf)))))

@export
(defun make-ofp_packet_in-stream (header stream)
  (with-fast-input (buf nil stream)
    (let* ((in (make-ofp_packet_in :header header
                                   :buffer_id (readu32-be buf)
                                   :total_len (readu16-be buf)
                                   :reason (readu8-be buf)
                                   :table_id (readu8-be buf)
                                   :cookie (readu64-be buf)
                                   :match (make-ofp_match-buffer buf)
                                   :data nil))
           (tlen (ofp_packet_in-total_len in)))
      (readu16-be buf) ; pad
      (setf (ofp_packet_in-data in) (read-vector tlen buf))
      in)))

@export
(defun make-ofp_flow_removed-stream (header stream)
  (with-fast-input (buf nil stream)
    (make-ofp_flow_removed :header header
                           :cookie (readu64-be buf)
                           :priority (readu16-be buf)
                           :reason (readu8-be buf)
                           :table_id (readu8-be buf)
                           :duration_sec (readu32-be buf)
                           :duration_nsec (readu32-be buf)
                           :idle_timeout (readu16-be buf)
                           :hard_timeout (readu16-be buf)
                           :packet_count (readu64-be buf)
                           :byte_count (readu64-be buf)
                           :match (make-ofp_match-buffer buf))))

@export
(defun make-ofp_port_status-stream (header stream)
  (with-fast-input (buf nil stream)
    (make-ofp_port_status :header header
                          :reason (readu8-be buf)
                          :desc (progn
                                  (loop :repeat 7
                                        :do (readu8-be buf)) ; pad
                                  (make-ofp_port-buffer buf)))))

(defun make-ofp_port-buffer (buf)
  (make-ofp_port :port_no (readu32-be buf)
                 :hw_addr (progn
                            (readu32-be buf) ; pad
                            (read-vector OFP_ETH_ALEN buf))
                 :name (progn
                         (readu16-be buf) ; pad
                         (let ((vec (read-vector OFP_MAX_PORT_NAME_LEN buf)))
                           (octets-to-string vec :end (position 0 vec))))
                 :config (readu32-be buf)
                 :state (readu32-be buf)
                 :curr (readu32-be buf)
                 :advertised (readu32-be buf)
                 :supported (readu32-be buf)
                 :peer (readu32-be buf)
                 :curr_speed (readu32-be buf)
                 :max_speed (readu32-be buf)))

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

@export
(defun dump-ofp_experimenter_header (v buf)
  (with-prefix (ex ofp_experimenter_header-)
    (dump-ofp_header (ex header v) buf)
    (writeu32-be (ex experimenter v) buf)
    (writeu32-be (ex exp_type v) buf)
    (loop :for i :across (ex data v)
          :do (fast-write-byte i buf))))

@export
(defun make-ofp_experimenter_header-stream (header stream)
  (with-fast-input (buf nil stream)
    (let ((len (ofp_header-length header)))
      (make-ofp_experimenter_header :header header
                                    :experimenter (readu32-be buf)
                                    :exp_type (readu32-be buf)
                                    :data (read-vector (- len 16) buf)))))
