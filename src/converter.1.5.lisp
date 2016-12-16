(in-package :cl-user)
(defpackage kappa.converter.1.5
  (:use :cl
        :annot
        :fast-io
        :kappa.define.1.5)
  (:import-from :kappa.define
                :ofp_header-length))
(in-package :kappa.converter.1.5)

(annot:enable-annot-syntax)

@export
(defun make-ofp_hello-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8))
        (elems '()))
    (if (> rest 0)
      (with-fast-input (buf nil stream)
        (loop :do (let ((type (readu16-be buf))
                        (length (readu16-be buf)))
                    (if (= type OFPHET_VERSIONBITMAP)
                      (let ((bitmaps '()))
                        (loop :repeat (/ (- length 4) 4)
                              :do (push (readu32-be buf) bitmaps))
                        (push (make-ofp_hello_elem_version :type type
                                                           :length length
                                                           :bitmaps (reverse bitmaps))
                              elems))
                      (loop :repeat (- length 4)
                            :do (readu8-be buf)))
                    (setf rest (- rest length)))
              :until (/= rest 0))))
    (make-ofp_hello :header header :elements (reverse elems))))

@export
(defun dump-ofp_hello (ofp_hello buf)
  (let ((header (ofp_hello-header ofp_hello)))
    (dump-ofp_header header buf)
    (let ((elm (ofp_hello-elements ofp_hello)))
      (loop :for e :in elm
            :do (if (ofp_hello_elm_versionbitmap-p e)
                  (progn
                    (writeu16-be (ofp_hello_elm_versionbitmap-type e) buf)
                    (writeu16-be (ofp_hello_elm_versionbitmap-length e) buf)
                    (loop :for b :in (ofp_hello_elm_versionbitmap-bitmaps e)
                          :do (writeu32-be b buf))))))))

@export
(defun make-ofp_switch_features-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8))
        (body (with-fast-input (buf nil stream)
                (make-ofp_switch_features :header header
                                          :datapath_id (readu64-be buf)
                                          :n_buffers (readu32-be buf)
                                          :n_tables (readu8-be buf)
                                          :auxiliary_id (readu8-be buf)
                                          :capabilities (progn
                                                          (readu16-be buf) ; pad
                                                          (readu32-be buf))
                                          :reserved (readu32-be buf)))))
    (with-fast-input (buf nil stream)
      (loop :repeat (- rest 24)
            :do (fast-read-byte buf))) ; drop
    body))

(defun make-ofp_match-buffer (buf)
  (let* ((type (readu16-be buf))
         (length (readu16-be buf))
         (flen (- length 4))
         (padlen (- (* (truncate (+ length 7) 8) 8) length))
         (fields (make-octet-vector flen)))
    (fast-read-sequence fields buf 0 flen)
    (loop :repeat padlen
          :do (readu8-be buf)) ; drop
    (values (make-ofp_match :type type
                            :length length
                            :oxm_fields fields)
            padlen)))

@export
(defun make-ofp_packet_in-stream (header stream)
  (let ((rest (- (ofp_header-length header) 8)))
    (with-fast-input (buf nil stream)
      (let ((buffer_id (readu32-be buf))
            (total_len (readu16-be buf))
            (reason (readu8-be buf))
            (table_id (readu8-be buf))
            (cookie (readu64-be buf)))
        (multiple-value-bind (match padlen)
            (make-ofp_match-buffer buf)
          (let* ((pad (readu16-be buf))
                 (datalen (- rest 16 (ofp_match-length match) padlen))
                 (data (make-octet-vector datalen)))
            (fast-read-sequence data buf 0 datalen)
            (make-ofp_packet_in :header header
                                :buffer_id buffer_id
                                :total_len total_len
                                :reason reason
                                :table_id table_id
                                :cookie cookie
                                :match match
                                :data data)))))))
