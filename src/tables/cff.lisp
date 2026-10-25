;;;; cff.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+charset-range1-size+ +charset-range2-size+ +encoding-range1-size+
   +encoding-supplement-size+ +fd-select-range3-size+ +fd-select-range4-size+
   cff-header cff-header--padding-bytes cff-header--padding-count
   cff-header--padding-ref cff-header-hdr-size cff-header-major
   cff-header-minor cff-header-off-size cff-header-p
   cff-header-trailing-data-bytes cff-header-trailing-data-count
   cff-header-trailing-data-ref cff-index cff-index-count cff-index-data-bytes
   cff-index-data-count cff-index-data-ref cff-index-off-size
   cff-index-offsets-bytes cff-index-offsets-count cff-index-offsets-ref
   cff-index-p charset-format0 charset-format0-format-table
   charset-format0-glyph-count charset-format0-glyph-ref charset-format0-p
   charset-format1 charset-format1-format-table charset-format1-p
   charset-format1-ranges-count charset-format1-ranges-ref charset-format2
   charset-format2-format-table charset-format2-p charset-format2-ranges-count
   charset-format2-ranges-ref charset-range1 charset-range1-first
   charset-range1-n-left charset-range1-p charset-range2 charset-range2-first
   charset-range2-n-left charset-range2-p encoding-range1 encoding-range1-first
   encoding-range1-n-left encoding-range1-p encoding-supplement
   encoding-supplement-code encoding-supplement-glyph encoding-supplement-p
   fd-select-format0 fd-select-format0-fds-bytes fd-select-format0-fds-count
   fd-select-format0-fds-ref fd-select-format0-format-table fd-select-format0-p
   fd-select-format3 fd-select-format3-format-table fd-select-format3-n-ranges
   fd-select-format3-p fd-select-format3-ranges-count
   fd-select-format3-ranges-ref fd-select-format3-sentinel fd-select-format4
   fd-select-format4-format-table fd-select-format4-n-ranges
   fd-select-format4-p fd-select-format4-ranges-count
   fd-select-format4-ranges-ref fd-select-format4-sentinel fd-select-range3
   fd-select-range3-fd fd-select-range3-first fd-select-range3-p
   fd-select-range4 fd-select-range4-fd fd-select-range4-first
   fd-select-range4-p read-cff-header read-cff-index read-charset-format0
   read-charset-format1 read-charset-format2 read-custom-charset read-fd-select
   read-fd-select-format0 read-fd-select-format3 read-fd-select-format4
   sanitize-cff-header sanitize-cff-index sanitize-charset-format0
   sanitize-charset-format1 sanitize-charset-format2 sanitize-fd-select-format0
   sanitize-fd-select-format3 sanitize-fd-select-format4))

(defstruct
    (fd-select-range3 (:include record-ref)
     (:constructor %make-fd-select-range3 (%record-font-data %record-offset)))
  "Range struct for FdSelect format 3.")

(define-constant +fd-select-range3-size+ 3)

(declaim (inline fd-select-range3-first fd-select-range3-fd))

(defun fd-select-range3-first (rec)
  "First glyph index in range."
  (declare (type fd-select-range3 rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun fd-select-range3-fd (rec)
  "FD index for all glyphs in range."
  (declare (type fd-select-range3 rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (fd-select-range4 (:include record-ref)
     (:constructor %make-fd-select-range4 (%record-font-data %record-offset)))
  "Range struct for FdSelect format 4.")

(define-constant +fd-select-range4-size+ 6)

(declaim (inline fd-select-range4-first fd-select-range4-fd))

(defun fd-select-range4-first (rec)
  "First glyph index in range."
  (declare (type fd-select-range4 rec))
  (font-data-read-u32 (record-ref-font-data rec) (record-ref-offset rec)))

(defun fd-select-range4-fd (rec)
  "FD index for all glyphs in range."
  (declare (type fd-select-range4 rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (charset-range1 (:include record-ref)
     (:constructor %make-charset-range1 (%record-font-data %record-offset)))
  "Range struct for Charset format 1.")

(define-constant +charset-range1-size+ 3)

(declaim (inline charset-range1-first charset-range1-n-left))

(defun charset-range1-first (rec)
  "First glyph in range."
  (declare (type charset-range1 rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun charset-range1-n-left (rec)
  "Glyphs left in range (excluding first)."
  (declare (type charset-range1 rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (charset-range2 (:include record-ref)
     (:constructor %make-charset-range2 (%record-font-data %record-offset)))
  "Range struct for Charset format 2.")

(define-constant +charset-range2-size+ 4)

(declaim (inline charset-range2-first charset-range2-n-left))

(defun charset-range2-first (rec)
  "First glyph in range."
  (declare (type charset-range2 rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun charset-range2-n-left (rec)
  "Glyphs left in range (excluding first)."
  (declare (type charset-range2 rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (encoding-range1 (:include record-ref)
     (:constructor %make-encoding-range1 (%record-font-data %record-offset)))
  "Range struct for Encoding format 1.")

(define-constant +encoding-range1-size+ 2)

(declaim (inline encoding-range1-first encoding-range1-n-left))

(defun encoding-range1-first (rec)
  "First code in range."
  (declare (type encoding-range1 rec))
  (font-data-read-u8 (record-ref-font-data rec) (record-ref-offset rec)))

(defun encoding-range1-n-left (rec)
  "Codes left in range (excluding first)."
  (declare (type encoding-range1 rec))
  (font-data-read-u8 (record-ref-font-data rec) (+ (record-ref-offset rec) 1)))

(defstruct
    (encoding-supplement (:include record-ref)
     (:constructor %make-encoding-supplement
      (%record-font-data %record-offset)))
  "Supplemental encoding record.")

(define-constant +encoding-supplement-size+ 3)

(declaim (inline encoding-supplement-code encoding-supplement-glyph))

(defun encoding-supplement-code (rec)
  "Encoding."
  (declare (type encoding-supplement rec))
  (font-data-read-u8 (record-ref-font-data rec) (record-ref-offset rec)))

(defun encoding-supplement-glyph (rec)
  "Name."
  (declare (type encoding-supplement rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 1)))

(defstruct
    (cff-header (:include table-ref)
     (:constructor %make-cff-header
      (%table-font-data %table-offset %table-length)))
  "[Compact Font Format](https://learn.microsoft.com/en-us/typography/opentype/spec/cff) table header")

(declaim
 (inline cff-header-major cff-header-major-raw cff-header-minor
  cff-header-minor-raw cff-header-hdr-size cff-header-hdr-size-raw
  cff-header-off-size cff-header-off-size-raw cff-header--padding-count
  cff-header--padding-ref cff-header--padding-bytes
  cff-header-trailing-data-count cff-header-trailing-data-ref
  cff-header-trailing-data-bytes))

(defun cff-header-major (table)
  "Format major version (starting at 1)."
  (declare (type cff-header table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun cff-header-major-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun cff-header-minor (table)
  "Format minor version (starting at 0)."
  (declare (type cff-header table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 1)))

(defun cff-header-minor-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 1)))

(defun cff-header-hdr-size (table)
  "Header size (bytes)."
  (declare (type cff-header table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 2)))

(defun cff-header-hdr-size-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 2)))

(defun cff-header-off-size (table)
  "Absolute offset size."
  (declare (type cff-header table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 3)))

(defun cff-header-off-size-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 3)))

(defun cff-header--padding-count (table)
  "Padding bytes before the start of the Name INDEX."
  (declare (type cff-header table))
  (declare (ignorable table))
  (max 0 (- (cff-header-hdr-size table) 4)))

(defun cff-header--padding-ref (table index)
  "Padding bytes before the start of the Name INDEX."
  (declare (type cff-header table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff-header--padding-bytes (table)
  "Padding bytes before the start of the Name INDEX."
  (declare (type cff-header table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 4)
                   (cff-header--padding-count table)))

(defun cff-header-trailing-data-count (table)
  "Remaining table data."
  (declare (type cff-header table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 4
              (* (cff-header--padding-count table) 1)))
        1)))

(defun cff-header-trailing-data-ref (table index)
  "Remaining table data."
  (declare (type cff-header table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 4
             (* (cff-header--padding-count table) 1))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff-header-trailing-data-bytes (table)
  "Remaining table data."
  (declare (type cff-header table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 4
                      (* (cff-header--padding-count table) 1))
                   (cff-header-trailing-data-count table)))

(defun sanitize-cff-header (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  t)

(defun read-cff-header
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cff-header font-data offset length)
  (%make-cff-header font-data offset length))

(defstruct
    (cff-index (:include table-ref)
     (:constructor %make-cff-index
      (%table-font-data %table-offset %table-length)))
  "An array of variable-sized objects in a `CFF` table.")

(declaim
 (inline cff-index-count cff-index-count-raw cff-index-off-size
  cff-index-off-size-raw cff-index-offsets-count cff-index-offsets-ref
  cff-index-offsets-bytes cff-index-data-count cff-index-data-ref
  cff-index-data-bytes))

(defun cff-index-count (table)
  "Number of objects stored in INDEX."
  (declare (type cff-index table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun cff-index-count-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun cff-index-off-size (table)
  "Object array element size."
  (declare (type cff-index table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 2)))

(defun cff-index-off-size-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 2)))

(defun cff-index-offsets-count (table)
  "Bytes containing `count + 1` offsets each of `off_size`."
  (declare (type cff-index table))
  (declare (ignorable table))
  (* (+ (cff-index-count table) 1) (cff-index-off-size table)))

(defun cff-index-offsets-ref (table index)
  "Bytes containing `count + 1` offsets each of `off_size`."
  (declare (type cff-index table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 3) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff-index-offsets-bytes (table)
  "Bytes containing `count + 1` offsets each of `off_size`."
  (declare (type cff-index table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 3)
                   (cff-index-offsets-count table)))

(defun cff-index-data-count (table)
  "Array containing the object data."
  (declare (type cff-index table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 3
              (* (cff-index-offsets-count table) 1)))
        1)))

(defun cff-index-data-ref (table index)
  "Array containing the object data."
  (declare (type cff-index table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos
         (+
          (+ (table-ref-offset table) 3 (* (cff-index-offsets-count table) 1))
          (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun cff-index-data-bytes (table)
  "Array containing the object data."
  (declare (type cff-index table))
  (font-data-slice (table-ref-font-data table)
                   (+ (table-ref-offset table) 3
                      (* (cff-index-offsets-count table) 1))
                   (cff-index-data-count table)))

(defun sanitize-cff-index (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  t)

(defun read-cff-index
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-cff-index font-data offset length)
  (%make-cff-index font-data offset length))

(defun read-fd-select
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "Associates a glyph identifier with a Font DICT."
  (declare (ignorable length))
  (let ((fmt (font-data-read-u8 font-data (+ offset 0))))
    (case fmt
      (0 (read-fd-select-format0 font-data offset length))
      (3 (read-fd-select-format3 font-data offset length))
      (4 (read-fd-select-format4 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'fd-select)))))

(defstruct
    (fd-select-format0 (:include table-ref)
     (:constructor %make-fd-select-format0
      (%table-font-data %table-offset %table-length)))
  "FdSelect format 0.")

(declaim
 (inline fd-select-format0-format-table fd-select-format0-format-table-raw
  fd-select-format0-fds-count fd-select-format0-fds-ref
  fd-select-format0-fds-bytes))

(defun fd-select-format0-format-table (table)
  "Format = 0."
  (declare (type fd-select-format0 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun fd-select-format0-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun fd-select-format0-fds-count (table)
  "FD selector array (one entry for each glyph)."
  (declare (type fd-select-format0 table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 1))
        1)))

(defun fd-select-format0-fds-ref (table index)
  "FD selector array (one entry for each glyph)."
  (declare (type fd-select-format0 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 1) (* index 1))))
    (unless
        (<= (+ pos 1) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 1 :length
             (table-ref-length table)))
    (font-data-read-u8 (table-ref-font-data table) pos)))

(defun fd-select-format0-fds-bytes (table)
  "FD selector array (one entry for each glyph)."
  (declare (type fd-select-format0 table))
  (font-data-slice (table-ref-font-data table) (+ (table-ref-offset table) 1)
                   (fd-select-format0-fds-count table)))

(defun sanitize-fd-select-format0 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 1 length) (font-data-valid-range-p font-data offset 1))
    (error 'font-out-of-bounds-error :pos offset :size 1 :length
           (font-data-length font-data)))
  t)

(defun read-fd-select-format0
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-fd-select-format0 font-data offset length)
  (%make-fd-select-format0 font-data offset length))

(defstruct
    (fd-select-format3 (:include table-ref)
     (:constructor %make-fd-select-format3
      (%table-font-data %table-offset %table-length)))
  "FdSelect format 3.")

(declaim
 (inline fd-select-format3-format-table fd-select-format3-format-table-raw
  fd-select-format3-n-ranges fd-select-format3-n-ranges-raw
  fd-select-format3-ranges-count fd-select-format3-ranges-ref
  fd-select-format3-sentinel))

(defun fd-select-format3-format-table (table)
  "Format = 3."
  (declare (type fd-select-format3 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun fd-select-format3-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun fd-select-format3-n-ranges (table)
  "Number of ranges."
  (declare (type fd-select-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun fd-select-format3-n-ranges-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 1)))

(defun fd-select-format3-ranges-count (table)
  "Range3 array."
  (declare (type fd-select-format3 table))
  (declare (ignorable table))
  (fd-select-format3-n-ranges table))

(defun fd-select-format3-ranges-ref (table index)
  "Range3 array."
  (declare (type fd-select-format3 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 3) (* index 3))))
    (unless
        (<= (+ pos 3) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 3 :length
             (table-ref-length table)))
    (%make-fd-select-range3 (table-ref-font-data table) pos)))

(defun fd-select-format3-sentinel (table)
  "Sentinel GID. Set equal to the number of glyphs in the font."
  (declare (type fd-select-format3 table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 3
                         (* (fd-select-format3-ranges-count table) 3))))

(defun sanitize-fd-select-format3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  (let* ((cnt (fd-select-format3-n-ranges-raw font-data offset))
         (pos (+ 3 (* cnt 3))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-fd-select-format3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-fd-select-format3 font-data offset length)
  (%make-fd-select-format3 font-data offset length))

(defstruct
    (fd-select-format4 (:include table-ref)
     (:constructor %make-fd-select-format4
      (%table-font-data %table-offset %table-length)))
  "FdSelect format 4.")

(declaim
 (inline fd-select-format4-format-table fd-select-format4-format-table-raw
  fd-select-format4-n-ranges fd-select-format4-n-ranges-raw
  fd-select-format4-ranges-count fd-select-format4-ranges-ref
  fd-select-format4-sentinel))

(defun fd-select-format4-format-table (table)
  "Format = 4."
  (declare (type fd-select-format4 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun fd-select-format4-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun fd-select-format4-n-ranges (table)
  "Number of ranges."
  (declare (type fd-select-format4 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun fd-select-format4-n-ranges-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 1)))

(defun fd-select-format4-ranges-count (table)
  "Range4 array."
  (declare (type fd-select-format4 table))
  (declare (ignorable table))
  (fd-select-format4-n-ranges table))

(defun fd-select-format4-ranges-ref (table index)
  "Range4 array."
  (declare (type fd-select-format4 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 5) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-fd-select-range4 (table-ref-font-data table) pos)))

(defun fd-select-format4-sentinel (table)
  "Sentinel GID. Set equal to the number of glyphs in the font."
  (declare (type fd-select-format4 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 5
                         (* (fd-select-format4-ranges-count table) 6))))

(defun sanitize-fd-select-format4 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 5 length) (font-data-valid-range-p font-data offset 5))
    (error 'font-out-of-bounds-error :pos offset :size 5 :length
           (font-data-length font-data)))
  (let* ((cnt (fd-select-format4-n-ranges-raw font-data offset))
         (pos (+ 5 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-fd-select-format4
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-fd-select-format4 font-data offset length)
  (%make-fd-select-format4 font-data offset length))

(defun read-custom-charset
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "Charset with custom glyph id to string id mappings."
  (declare (ignorable length))
  (let ((fmt (font-data-read-u8 font-data (+ offset 0))))
    (case fmt
      (0 (read-charset-format0 font-data offset length))
      (1 (read-charset-format1 font-data offset length))
      (2 (read-charset-format2 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'custom-charset)))))

(defstruct
    (charset-format0 (:include table-ref)
     (:constructor %make-charset-format0
      (%table-font-data %table-offset %table-length)))
  "Charset format 0.")

(declaim
 (inline charset-format0-format-table charset-format0-format-table-raw
  charset-format0-glyph-count charset-format0-glyph-ref))

(defun charset-format0-format-table (table)
  "Format; =0"
  (declare (type charset-format0 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun charset-format0-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun charset-format0-glyph-count (table)
  "Glyph name array."
  (declare (type charset-format0 table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 1))
        2)))

(defun charset-format0-glyph-ref (table index)
  "Glyph name array."
  (declare (type charset-format0 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 1) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun sanitize-charset-format0 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 1 length) (font-data-valid-range-p font-data offset 1))
    (error 'font-out-of-bounds-error :pos offset :size 1 :length
           (font-data-length font-data)))
  t)

(defun read-charset-format0
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-charset-format0 font-data offset length)
  (%make-charset-format0 font-data offset length))

(defstruct
    (charset-format1 (:include table-ref)
     (:constructor %make-charset-format1
      (%table-font-data %table-offset %table-length)))
  "Charset format 1.")

(declaim
 (inline charset-format1-format-table charset-format1-format-table-raw
  charset-format1-ranges-count charset-format1-ranges-ref))

(defun charset-format1-format-table (table)
  "Format; =1"
  (declare (type charset-format1 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun charset-format1-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun charset-format1-ranges-count (table)
  "Range1 array."
  (declare (type charset-format1 table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 1))
        3)))

(defun charset-format1-ranges-ref (table index)
  "Range1 array."
  (declare (type charset-format1 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 1) (* index 3))))
    (unless
        (<= (+ pos 3) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 3 :length
             (table-ref-length table)))
    (%make-charset-range1 (table-ref-font-data table) pos)))

(defun sanitize-charset-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 1 length) (font-data-valid-range-p font-data offset 1))
    (error 'font-out-of-bounds-error :pos offset :size 1 :length
           (font-data-length font-data)))
  t)

(defun read-charset-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-charset-format1 font-data offset length)
  (%make-charset-format1 font-data offset length))

(defstruct
    (charset-format2 (:include table-ref)
     (:constructor %make-charset-format2
      (%table-font-data %table-offset %table-length)))
  "Charset format 2.")

(declaim
 (inline charset-format2-format-table charset-format2-format-table-raw
  charset-format2-ranges-count charset-format2-ranges-ref))

(defun charset-format2-format-table (table)
  "Format; =2"
  (declare (type charset-format2 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun charset-format2-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun charset-format2-ranges-count (table)
  "Range2 array."
  (declare (type charset-format2 table))
  (declare (ignorable table))
  (max 0
       (floor
        (- (+ (table-ref-offset table) (table-ref-length table))
           (+ (table-ref-offset table) 1))
        4)))

(defun charset-format2-ranges-ref (table index)
  "Range2 array."
  (declare (type charset-format2 table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 1) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (%make-charset-range2 (table-ref-font-data table) pos)))

(defun sanitize-charset-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 1 length) (font-data-valid-range-p font-data offset 1))
    (error 'font-out-of-bounds-error :pos offset :size 1 :length
           (font-data-length font-data)))
  t)

(defun read-charset-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-charset-format2 font-data offset length)
  (%make-charset-format2 font-data offset length))
