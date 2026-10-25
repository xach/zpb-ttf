;;;; font-loader-interface.lisp — Initialization and entry points for font-loader
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(in-package #:zpb-ttf)

(defun close-font-loader (font-loader)
  (when (input-stream font-loader)
    (close (input-stream font-loader))
    (setf (input-stream font-loader) nil))
  font-loader)

(defmacro with-font-loader ((loader thing &rest args) &body body)
  `(let ((,loader (open-font-loader ,thing ,@args)))
     (unwind-protect
          (progn ,@body)
       (close-font-loader ,loader))))

;;; Mac OS Roman code points for bytes #x80-#xFF.
(defparameter *mac-roman-high-codes*
  (coerce '(#x00C4 #x00C5 #x00C7 #x00C9 #x00D1 #x00D6 #x00DC #x00E1
            #x00E0 #x00E2 #x00E4 #x00E3 #x00E5 #x00E7 #x00E9 #x00E8
            #x00EA #x00EB #x00ED #x00EC #x00EE #x00EF #x00F1 #x00F3
            #x00F2 #x00F4 #x00F6 #x00F5 #x00FA #x00F9 #x00FB #x00FC
            #x2020 #x00B0 #x00A2 #x00A3 #x00A7 #x2022 #x00B6 #x00DF
            #x00AE #x00A9 #x2122 #x00B4 #x00A8 #x2260 #x00C6 #x00D8
            #x221E #x00B1 #x2264 #x2265 #x00A5 #x00B5 #x2202 #x2211
            #x220F #x03C0 #x222B #x00AA #x00BA #x03A9 #x00E6 #x00F8
            #x00BF #x00A1 #x00AC #x221A #x0192 #x2248 #x2206 #x00AB
            #x00BB #x2026 #x00A0 #x00C0 #x00C3 #x00D5 #x0152 #x0153
            #x2013 #x2014 #x201C #x201D #x2018 #x2019 #x00F7 #x25CA
            #x00FF #x0178 #x2044 #x20AC #x2039 #x203A #xFB01 #xFB02
            #x2021 #x00B7 #x201A #x201E #x2030 #x00C2 #x00CA #x00C1
            #x00CB #x00C8 #x00CD #x00CE #x00CF #x00CC #x00D3 #x00D4
            #xF8FF #x00D2 #x00DA #x00DB #x00D9 #x0131 #x02C6 #x02DC
            #x00AF #x02D8 #x02D9 #x02DA #x00B8 #x02DD #x02DB #x02C7)
          '(simple-array (unsigned-byte 16) (128))))

(defun decode-utf-16be-from-fd (fd offset length)
  "Decode LENGTH bytes of UTF-16BE; a trailing odd byte is ignored and
unpaired surrogates become U+FFFD."
  (let ((units (floor length 2))
        (out (make-string-output-stream))
        (i 0))
    (flet ((unit (k) (zpb-otf:font-data-read-u16 fd (+ offset (* k 2)))))
      (loop while (< i units)
            do (let ((u (unit i)))
                 (incf i)
                 (write-char
                  (cond
                    ((and (<= #xD800 u #xDBFF) (< i units) (<= #xDC00 (unit i) #xDFFF))
                     (prog1 (code-char (+ #x10000 (ash (- u #xD800) 10) (- (unit i) #xDC00)))
                       (incf i)))
                    ((<= #xD800 u #xDFFF) (code-char #xFFFD))
                    (t (code-char u)))
                  out))))
    (get-output-stream-string out)))

(defun decode-name-string-from-fd (fd offset length platform-id encoding-id)
  (cond
    ;; UTF-16BE: platform 0 (Unicode) or platform 3 (Windows) with encoding 1 or 10
    ((or (zerop platform-id)
         (and (= platform-id 3) (or (= encoding-id 1) (= encoding-id 10))))
     (decode-utf-16be-from-fd fd offset length))
    ;; Macintosh Roman
    ((and (= platform-id 1) (zerop encoding-id))
     (let ((str (make-string length)))
       (dotimes (i length str)
         (let ((byte (zpb-otf:font-data-read-u8 fd (+ offset i))))
           (setf (char str i)
                 (code-char (if (< byte #x80)
                                byte
                                (aref *mac-roman-high-codes* (- byte #x80)))))))))
    ;; Other single-byte encodings: Latin-1 as a best effort
    (t
     (let ((str (make-string length)))
       (dotimes (i length str)
         (setf (char str i) (code-char (zpb-otf:font-data-read-u8 fd (+ offset i)))))))))

(defun cmap-subtable-length (fd sub-off fmt table-end)
  "Length of the cmap subtable at SUB-OFF from its own header, clamped to TABLE-END."
  (let ((declared (if (>= fmt 8)
                      (zpb-otf:font-data-read-u32 fd (+ sub-off 4))
                      (zpb-otf:font-data-read-u16 fd (+ sub-off 2)))))
    (min declared (max 0 (- table-end sub-off)))))

(defun setup-cmap-lookup (loader cmap cmap-offset cmap-length)
  (let ((fd (font-data loader))
        (table-end (+ cmap-offset cmap-length))
        (best-fmt nil)
        (best-sub-off nil))
    ;; Look for Format 12 (preferred full Unicode) or Format 4 (BMP Unicode)
    (dotimes (i (zpb-otf:cmap-num-tables cmap))
      (let* ((erec (zpb-otf:cmap-encoding-records-ref cmap i))
             (plat (zpb-otf:encoding-record-platform-id erec))
             (enc (zpb-otf:encoding-record-encoding-id erec))
             (sub-off (+ cmap-offset (zpb-otf:encoding-record-subtable-offset erec)))
             (fmt (zpb-otf:font-data-read-u16 fd sub-off)))
        (cond
          ((and (= fmt 12) (or (= plat 0) (= plat 3)))
           (setf best-fmt 12 best-sub-off sub-off)
           (return))
          ((and (= fmt 4) (or (= plat 0) (and (= plat 3) (or (= enc 1) (= enc 10)))))
           ;; The first suitable format 4 subtable wins.
           (unless best-fmt
             (setf best-fmt 4 best-sub-off sub-off))))))

    (cond
      ((eql best-fmt 12)
       (let* ((sub-len (cmap-subtable-length fd best-sub-off 12 table-end))
              (cmap12 (zpb-otf:read-cmap12 fd best-sub-off sub-len))
              (num-groups (min (zpb-otf:cmap12-num-groups cmap12)
                               (max 0 (floor (- sub-len 16) 12)))))
         (setf (cmap-lookup-fn loader)
               (lambda (cp)
                 (let ((low 0)
                       (high (1- num-groups)))
                   (loop while (<= low high) do
                         (let* ((mid (floor (+ low high) 2))
                                (grp (zpb-otf:cmap12-groups-ref cmap12 mid))
                                (start (zpb-otf:sequential-map-group-start-char-code grp))
                                (end (zpb-otf:sequential-map-group-end-char-code grp)))
                           (cond
                             ((< cp start) (setf high (1- mid)))
                             ((> cp end) (setf low (1+ mid)))
                             (t (return
                                  (+ (zpb-otf:sequential-map-group-start-glyph-id grp)
                                     (- cp start))))))
                         finally (return 0))))
               (cmap-mappings-fn loader)
               (lambda (fn)
                 (dotimes (g num-groups)
                   (let* ((grp (zpb-otf:cmap12-groups-ref cmap12 g))
                          (start (zpb-otf:sequential-map-group-start-char-code grp))
                          (gid (zpb-otf:sequential-map-group-start-glyph-id grp)))
                     (loop for cp from start
                             to (min (zpb-otf:sequential-map-group-end-char-code grp) #x10FFFF)
                           do (funcall fn cp (+ gid (- cp start))))))))))

      ((eql best-fmt 4)
       (let* ((sub-len (cmap-subtable-length fd best-sub-off 4 table-end))
              (sub-end (+ best-sub-off sub-len))
              (cmap4 (zpb-otf:read-cmap4 fd best-sub-off sub-len))
              ;; Four u16 arrays of segCount entries follow the 16-byte header.
              (seg-len (min (floor (zpb-otf:cmap4-seg-count-x2 cmap4) 2)
                            (max 0 (floor (- sub-len 16) 8))))
              (ro-base (+ best-sub-off 16 (* 6 seg-len))))
         (labels ((segment-glyph (s cp)
                    (let ((start (zpb-otf:cmap4-start-code-ref cmap4 s))
                          (delta (zpb-otf:cmap4-id-delta-ref cmap4 s))
                          (ro-off (zpb-otf:cmap4-id-range-offsets-ref cmap4 s)))
                      (cond
                        ((< cp start) 0)
                        ((zerop ro-off) (logand #xFFFF (+ cp delta)))
                        (t
                         (let ((g-addr (+ ro-base (* 2 s) ro-off (* 2 (- cp start)))))
                           ;; A malformed idRangeOffset must not read past the subtable.
                           (if (> (+ g-addr 2) sub-end)
                               0
                               (let ((gid (zpb-otf:font-data-read-u16 fd g-addr)))
                                 (if (zerop gid) 0 (logand #xFFFF (+ gid delta))))))))))
                  (find-segment (cp)
                    ;; First segment whose endCode >= CP.
                    (let ((low 0) (high seg-len))
                      (loop while (< low high)
                            do (let ((mid (floor (+ low high) 2)))
                                 (if (< (zpb-otf:cmap4-end-code-ref cmap4 mid) cp)
                                     (setf low (1+ mid))
                                     (setf high mid))))
                      (when (< low seg-len) low))))
           (setf (cmap-lookup-fn loader)
                 (lambda (cp)
                   (let ((s (and (<= cp #xFFFF) (find-segment cp))))
                     (if s (segment-glyph s cp) 0)))
                 (cmap-mappings-fn loader)
                 (lambda (fn)
                   (dotimes (s seg-len)
                     (loop for cp from (zpb-otf:cmap4-start-code-ref cmap4 s)
                             to (min (zpb-otf:cmap4-end-code-ref cmap4 s) #xFFFE)
                           for gid = (segment-glyph s cp)
                           unless (zerop gid) do (funcall fn cp gid))))))))
      (t
       (setf (cmap-lookup-fn loader) (constantly 0))))))

(defun read-long-metrics (data offset size long-count glyph-count)
  "Read an hmtx/vmtx style table: LONG-COUNT (advance, bearing) pairs
followed by bare bearings for the remaining glyphs. Return a vector of
advances and a vector of GLYPH-COUNT bearings."
  (let* ((long-count (min long-count (floor size 4)))
         (advances (make-array long-count))
         (bearings (make-array (max glyph-count long-count) :initial-element 0))
         (trailing-end (+ offset size)))
    (dotimes (i long-count)
      (let ((pos (+ offset (* i 4))))
        (setf (aref advances i) (zpb-otf:font-data-read-u16 data pos)
              (aref bearings i) (zpb-otf:font-data-read-i16 data (+ pos 2)))))
    (loop for i from long-count below glyph-count
          for pos = (+ offset (* long-count 4) (* (- i long-count) 2))
          while (<= (+ pos 2) trailing-end)
          do (setf (aref bearings i) (zpb-otf:font-data-read-i16 data pos)))
    (values advances bearings)))

(defun read-kerning-format-0 (loader body kend)
  "Format 0: an ordered list of (left, right, value) pairs at BODY."
  (let* ((data (font-data loader))
         (n-pairs (min (zpb-otf:font-data-read-u16 data body)
                       (floor (max 0 (- kend body 8)) 6)))
         (pos (+ body 8)))
    (dotimes (p n-pairs)
      (let ((left (zpb-otf:font-data-read-u16 data pos))
            (right (zpb-otf:font-data-read-u16 data (+ pos 2)))
            (val (zpb-otf:font-data-read-i16 data (+ pos 4))))
        ;; Apple specifies a terminating #xFFFF/#xFFFF entry with value 0.
        (unless (and (= left #xFFFF) (= right #xFFFF) (zerop val))
          (setf (gethash (logior (ash left 16) right) (kerning-table loader)) val))
        (incf pos 6)))))

(defun read-kerning-class-table (data pos kend)
  "Return the first glyph and the vector of offsets of the class table at POS."
  (if (> (+ pos 4) kend)
      (values 0 (vector))
      (let* ((first (zpb-otf:font-data-read-u16 data pos))
             (count (min (zpb-otf:font-data-read-u16 data (+ pos 2))
                         (floor (- kend pos 4) 2)))
             (offsets (make-array count)))
        (dotimes (i count)
          (setf (aref offsets i) (zpb-otf:font-data-read-u16 data (+ pos 4 (* 2 i)))))
        (values first offsets))))

(defun read-kerning-format-2 (loader sub-off body kend)
  "Format 2: a class-based kerning array. The class-table offsets, and the
pre-multiplied row and column offsets they hold, are relative to the
start of the subtable; a pair's value is at SUB-OFF + left + right."
  (let ((data (font-data loader)))
    (when (<= (+ body 8) kend)
      (multiple-value-bind (left-first left-offsets)
          (read-kerning-class-table data (+ sub-off (zpb-otf:font-data-read-u16 data (+ body 2))) kend)
        (multiple-value-bind (right-first right-offsets)
            (read-kerning-class-table data (+ sub-off (zpb-otf:font-data-read-u16 data (+ body 4))) kend)
          (loop for lv across left-offsets
                for left from left-first
                do (loop for rv across right-offsets
                         for right from right-first
                         for pos = (+ sub-off lv rv)
                         when (<= (+ pos 2) kend)
                           do (let ((val (zpb-otf:font-data-read-i16 data pos)))
                                (unless (zerop val)
                                  (setf (gethash (logior (ash left 16) right) (kerning-table loader))
                                        val))))))))))

(defun read-kerning-subtable (loader format sub-off body kend)
  (case format
    (0 (read-kerning-format-0 loader body kend))
    (2 (read-kerning-format-2 loader sub-off body kend))
    ;; Format 1 (contextual state table) and others are ignored.
    ))

(defun load-kern-table (loader koff ksize)
  "Read the horizontal format 0 and format 2 subtables of the Microsoft
(version 0) or Apple (version 1.0) 'kern' table into the loader's
kerning table."
  (let* ((data (font-data loader))
         (kend (+ koff ksize))
         (version (zpb-otf:font-data-read-u16 data koff)))
    (case version
      (0
       ;; Subtable header: version u16, length u16, coverage u16 with the
       ;; format in the high byte and flags in the low byte.
       (let ((n-tables (zpb-otf:font-data-read-u16 data (+ koff 2)))
             (sub-off (+ koff 4)))
         (dotimes (t-idx n-tables)
           (when (> (+ sub-off 6) kend) (return))
           (let* ((sub-len (zpb-otf:font-data-read-u16 data (+ sub-off 2)))
                  (sub-cov (zpb-otf:font-data-read-u16 data (+ sub-off 4)))
                  (sub-end (min kend (+ sub-off (max sub-len 6)))))
             ;; Horizontal, not minimum values, not cross-stream.
             (when (= (logand sub-cov #b111) 1)
               (read-kerning-subtable loader (ldb (byte 8 8) sub-cov) sub-off (+ sub-off 6)
                                      ;; Fonts with many pairs overflow the 16-bit length.
                                      (if (= t-idx (1- n-tables)) kend sub-end)))
             (when (< sub-len 6) (return))
             (incf sub-off sub-len)))))
      (1
       ;; Subtable header: length u32, coverage u16 with flags in the high
       ;; byte and the format in the low byte, tupleIndex u16.
       (when (= (zpb-otf:font-data-read-u16 data (+ koff 2)) 0)
         (let ((n-tables (min (zpb-otf:font-data-read-u32 data (+ koff 4))
                              (floor (max 0 (- kend koff 8)) 8)))
               (sub-off (+ koff 8)))
           (dotimes (t-idx n-tables)
             (when (> (+ sub-off 8) kend) (return))
             (let* ((sub-len (zpb-otf:font-data-read-u32 data sub-off))
                    (sub-cov (zpb-otf:font-data-read-u16 data (+ sub-off 4))))
               ;; Not vertical, not cross-stream, not variation.
               (when (zerop (logand sub-cov #xE000))
                 (read-kerning-subtable loader (ldb (byte 8 0) sub-cov) sub-off (+ sub-off 8)
                                        (min kend (+ sub-off (max sub-len 8)))))
               (when (< sub-len 8) (return))
               (incf sub-off sub-len)))))))))

(defparameter *sfnt-versions*
  (list #x00010000 #x4F54544F #x74727565 #x74797031) ; 1.0, 'OTTO', 'true', 'typ1'
  "Valid sfntVersion values at the start of a font's table directory.")

(defun open-font-loader-from-data (data &key (collection-index 0) input-stream)
  (let* ((sfnt-magic (zpb-otf:font-data-read-u32 data 0))
         (font-offset 0)
         (coll-count nil)
         (coll-idx nil))

    ;; Handle Font Collection (ttcf)
    (when (= sfnt-magic #x74746366)
      (let ((num-fonts (zpb-otf:font-data-read-u32 data 8)))
        (setf coll-count num-fonts coll-idx collection-index)
        (when (>= collection-index num-fonts)
          (error 'unsupported-value
                 :description "Font index out of range in collection"
                 :actual-value collection-index
                 :expected-values (list num-fonts)))
        (setf font-offset (zpb-otf:font-data-read-u32 data (+ 12 (* collection-index 4))))))

    (let ((sfnt-version (zpb-otf:font-data-read-u32 data font-offset)))
      (unless (member sfnt-version *sfnt-versions*)
        (error 'bad-magic
               :location (format nil "table directory at offset ~D" font-offset)
               :actual-value sfnt-version
               :expected-values *sfnt-versions*)))

    (let* ((dir (zpb-otf:read-table-directory data font-offset))
           (num-tables (zpb-otf:table-directory-num-tables dir))
           (loader (make-instance 'font-loader
                                  :font-data data
                                  :input-stream input-stream
                                  :table-count num-tables
                                  :collection-font-count coll-count
                                  :collection-font-index coll-idx)))

      ;; Register table-info
      (dotimes (i num-tables)
        (let* ((rec (zpb-otf:table-directory-table-records-ref dir i))
               (tag (zpb-otf:table-record-tag rec))
               (t-off (zpb-otf:table-record-offset rec))
               (t-len (zpb-otf:table-record-length rec)))
          (setf (gethash tag (tables loader))
                (make-instance 'table-info :name tag :offset t-off :size t-len))))

      ;; 1. MAXP table
      (let ((info (table-info "maxp" loader)))
        (when info
          (let ((maxp (zpb-otf:read-maxp data (offset info) (size info))))
            (setf (glyph-count loader) (zpb-otf:maxp-num-glyphs maxp)))))

      ;; 2. HEAD table
      (let ((info (table-info "head" loader)))
        (when info
          (let ((head (zpb-otf:read-head data (offset info) (size info))))
            (setf (units/em loader) (zpb-otf:head-units-per-em head)
                  (bounding-box loader) (vector (zpb-otf:head-x-min head)
                                               (zpb-otf:head-y-min head)
                                               (zpb-otf:head-x-max head)
                                               (zpb-otf:head-y-max head))
                  (loca-offset-format loader) (if (zerop (zpb-otf:head-index-to-loc-format head))
                                                  :short
                                                  :long)))))

      ;; 3. HHEA & HMTX tables
      (let ((hhea-info (table-info "hhea" loader))
            (hmtx-info (table-info "hmtx" loader)))
        (when hhea-info
          (let* ((hhea (zpb-otf:read-hhea data (offset hhea-info) (size hhea-info)))
                 (num-h-metrics (zpb-otf:hhea-number-of-h-metrics hhea)))
            (setf (ascender loader) (zpb-otf:hhea-ascender hhea)
                  (descender loader) (zpb-otf:hhea-descender hhea)
                  (line-gap loader) (zpb-otf:hhea-line-gap hhea)
                  (max-width loader) (zpb-otf:hhea-advance-width-max hhea))
            (when hmtx-info
              (zpb-otf:read-hmtx data (offset hmtx-info) (size hmtx-info))
              (multiple-value-bind (widths lsbs)
                  (read-long-metrics data (offset hmtx-info) (size hmtx-info)
                                     num-h-metrics (glyph-count loader))
                (setf (advance-widths loader) widths
                      (left-side-bearings loader) lsbs))))))

      ;; 4. VHEA & VMTX tables (with horizontal metrics fallback)
      (let ((vhea-info (table-info "vhea" loader))
            (vmtx-info (table-info "vmtx" loader)))
        (cond
          ((and vhea-info vmtx-info)
           (let* ((vhea (zpb-otf:read-vhea data (offset vhea-info) (size vhea-info)))
                  (num-v-metrics (zpb-otf:vhea-number-of-long-ver-metrics vhea)))
             (zpb-otf:read-vmtx data (offset vmtx-info) (size vmtx-info))
             (setf (vascender loader) (zpb-otf:vhea-ascender vhea)
                   (vdescender loader) (zpb-otf:vhea-descender vhea))
             (multiple-value-bind (heights tsbs)
                 (read-long-metrics data (offset vmtx-info) (size vmtx-info)
                                    num-v-metrics (glyph-count loader))
               (setf (advance-heights loader) heights
                     (top-side-bearings loader) tsbs))))
          (t
           (setf (vhea-missing-p loader) t
                 (vmtx-missing-p loader) t)
           (let ((dx (/ (max-width loader) 2)))
             (setf (vascender loader) dx
                   (vdescender loader) (- dx)
                   (advance-heights loader) (vector (- (ascender loader) (descender loader))))))))

      ;; 5. POST table
      (let ((info (table-info "post" loader)))
        (when info
          (let ((post (zpb-otf:read-post data (offset info) (size info))))
            (setf (italic-angle loader) (zpb-otf:post-italic-angle post)
                  (fixed-pitch-p loader) (not (zerop (zpb-otf:post-is-fixed-pitch post)))
                  (underline-position loader) (zpb-otf:post-underline-position post)
                  (underline-thickness loader) (zpb-otf:post-underline-thickness post)))))

      ;; 6. NAME table
      (let ((info (table-info "name" loader)))
        (when info
          (let* ((name-tbl (zpb-otf:read-name data (offset info) (size info)))
                 (storage (+ (offset info) (zpb-otf:name-storage-offset name-tbl)))
                 (cnt (zpb-otf:name-count name-tbl))
                 (entries (make-array cnt)))
            (dotimes (i cnt)
              (let* ((nrec (zpb-otf:name-name-record-ref name-tbl i))
                     (plat (zpb-otf:name-record-platform-id nrec))
                     (enc (zpb-otf:name-record-encoding-id nrec))
                     (lang (zpb-otf:name-record-language-id nrec))
                     (nid (zpb-otf:name-record-name-id nrec))
                     (slen (zpb-otf:name-record-length nrec))
                     (soff (+ storage (zpb-otf:name-record-string-offset nrec)))
                     (val (decode-name-string-from-fd data soff slen plat enc)))
                (setf (aref entries i)
                      (make-instance 'name-entry
                                     :font-loader loader
                                     :platform-id plat
                                     :encoding-id enc
                                     :language-id lang
                                     :name-id nid
                                     :value val))))
            (setf (name-entries loader) entries))))

      ;; 7. CMAP table
      (let ((info (table-info "cmap" loader)))
        (when info
          (let ((cmap (zpb-otf:read-cmap data (offset info) (size info))))
            (setup-cmap-lookup loader cmap (offset info) (size info)))))

      ;; 8. KERN table
      (let ((info (table-info "kern" loader)))
        (when info
          (handler-case (load-kern-table loader (offset info) (size info))
            (zpb-otf:font-out-of-bounds-error () nil))))

      ;; Initialize glyph cache
      (setf (glyph-cache loader)
            (make-array (glyph-count loader) :initial-element nil))

      loader)))

(defun open-font-loader (thing &key (collection-index 0))
  (cond
    ((streamp thing)
     (let ((ok nil))
       (unwind-protect
            (let* ((len (file-length thing))
                   (buf (make-array len :element-type '(unsigned-byte 8))))
              (file-position thing 0)
              (read-sequence buf thing)
              (prog1 (open-font-loader-from-data (zpb-otf:make-font-data buf)
                                                 :collection-index collection-index
                                                 :input-stream thing)
                (setf ok t)))
         ;; The loader owns the stream; don't leak it when loading fails.
         (unless ok (close thing)))))
    (t
     (let ((data (zpb-otf:ensure-font-data thing)))
       (open-font-loader-from-data data :collection-index collection-index)))))
