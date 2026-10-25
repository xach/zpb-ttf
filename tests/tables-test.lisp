;;;; tables-test.lisp — Comprehensive table reader tests for zpb-otf and zpb-ttf
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(defpackage #:zpb-otf-test
  (:use #:cl)
  (:export #:run-table-tests))

(in-package #:zpb-otf-test)

(defparameter *test-fonts-dir*
  (merge-pathnames "fonts/"
                   (make-pathname :directory (pathname-directory *load-truename*))))

(defun test-font-path (filename)
  (merge-pathnames filename *test-fonts-dir*))

(defun find-table (td tag)
  (dotimes (i (zpb-otf:table-directory-table-records-count td))
    (let ((rec (zpb-otf:table-directory-table-records-ref td i)))
      (when (string= (zpb-otf:table-record-tag rec) tag)
        (return (values (zpb-otf:table-record-offset rec)
                        (zpb-otf:table-record-length rec)))))))

(defvar *test-failures* 0)
(defvar *test-count* 0)

(defmacro test-assert (expr &optional desc)
  (let ((e (gensym)))
    `(progn
       (incf *test-count*)
       (handler-case
           (let ((,e ,expr))
             (if ,e
                 (format t "  [PASS] ~A~%" (or ,desc (prin1-to-string ',expr)))
                 (progn
                   (incf *test-failures*)
                   (format *error-output* "  [FAIL] ~A (evaluated to NIL)~%" (or ,desc (prin1-to-string ',expr))))))
         (error (err)
           (incf *test-failures*)
           (format *error-output* "  [ERROR] ~A (signaled: ~A)~%" (or ,desc (prin1-to-string ',expr)) err))))))

(defun test-font-data-primitives ()
  (format t "~%=== 1. Testing Font Data Primitives & Bounds Checking ===~%")
  (let* ((bytes (make-array 16 :element-type '(unsigned-byte 8)
                               :initial-contents '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
         (fd (zpb-otf:make-font-data bytes)))
    (test-assert (= (zpb-otf:font-data-length fd) 16) "font-data-length")
    (test-assert (= (zpb-otf:font-data-read-u8 fd 0) 0) "font-data-read-u8 at 0")
    (test-assert (= (zpb-otf:font-data-read-u8 fd 15) 15) "font-data-read-u8 at 15")
    (test-assert (= (zpb-otf:font-data-read-u16 fd 0) 1) "font-data-read-u16")
    (test-assert (= (zpb-otf:font-data-read-u24 fd 0) 258) "font-data-read-u24")
    (test-assert (= (zpb-otf:font-data-read-u32 fd 0) #x00010203) "font-data-read-u32")

    ;; Slice
    (let ((sl (zpb-otf:font-data-slice fd 4 8)))
      (test-assert (= (zpb-otf:font-data-length sl) 8) "font-data-slice length")
      (test-assert (= (zpb-otf:font-data-read-u8 sl 0) 4) "font-data-slice read-u8"))

    ;; Bounds checking signals font-out-of-bounds-error
    (test-assert
     (handler-case
         (progn (zpb-otf:font-data-read-u8 fd 16) nil)
       (zpb-otf:font-out-of-bounds-error () t))
     "Out-of-bounds read-u8 signals font-out-of-bounds-error")
    (test-assert
     (handler-case
         (progn (zpb-otf:font-data-read-u32 fd 13) nil)
       (zpb-otf:font-out-of-bounds-error () t))
     "Out-of-bounds read-u32 signals font-out-of-bounds-error")))

(defun test-ttc-collection ()
  (format t "~%=== 2. Testing TrueType Collection (TTC) Header ===~%")
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "TTC.ttc")))
         (ttc (zpb-otf:read-ttc-header fd 0)))
    (test-assert (string= (zpb-otf:ttc-header-ttc-tag ttc) "ttcf") "TTC tag is ttcf")
    (test-assert (= (zpb-otf:ttc-header-num-fonts ttc) 2) "TTC num-fonts is 2")
    (test-assert (= (zpb-otf:ttc-header-table-directory-offsets-count ttc) 2) "TTC directory offsets count is 2")
    ;; Test reading both font directories in the TTC
    (dotimes (i (zpb-otf:ttc-header-num-fonts ttc))
      (let* ((off (zpb-otf:ttc-header-table-directory-offsets-ref ttc i))
             (td (zpb-otf:read-table-directory fd off)))
        (test-assert (> (zpb-otf:table-directory-num-tables td) 0)
                     (format nil "TTC font ~D at offset ~D has ~D tables"
                             i off (zpb-otf:table-directory-num-tables td)))))))

(defun test-variable-tables ()
  (format t "~%=== 3. Testing Variable Font Tables (Cantarell-VF.otf) ===~%")
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "Cantarell-VF.otf")))
         (td (zpb-otf:read-table-directory fd 0)))
    ;; CFF2
    (multiple-value-bind (off len) (find-table td "CFF2")
      (test-assert off "Cantarell has CFF2 table")
      (let ((cff2 (zpb-otf:read-cff2-header fd off len)))
        (test-assert (= (zpb-otf:cff2-header-major-version cff2) 2) "CFF2 major version is 2")
        (test-assert (> (zpb-otf:cff2-header-header-size cff2) 0) "CFF2 header size > 0")))

    ;; fvar
    (multiple-value-bind (off len) (find-table td "fvar")
      (test-assert off "Cantarell has fvar table")
      (let ((fvar (zpb-otf:read-fvar fd off len)))
        (test-assert (> (zpb-otf:fvar-axis-count fvar) 0) "fvar axis count > 0")
        (test-assert (> (zpb-otf:fvar-instance-count fvar) 0) "fvar instance count > 0")))

    ;; STAT
    (multiple-value-bind (off len) (find-table td "STAT")
      (test-assert off "Cantarell has STAT table")
      (let ((stat (zpb-otf:read-stat fd off len)))
        (test-assert (> (zpb-otf:stat-design-axis-count stat) 0) "STAT design axis count > 0")))

    ;; HVAR
    (multiple-value-bind (off len) (find-table td "HVAR")
      (test-assert off "Cantarell has HVAR table")
      (let ((hvar (zpb-otf:read-hvar fd off len)))
        (test-assert (= (zpb-otf:hvar-version hvar) #x00010000) "HVAR version is 1.0")))

    ;; MVAR
    (multiple-value-bind (off len) (find-table td "MVAR")
      (test-assert off "Cantarell has MVAR table")
      (let ((mvar (zpb-otf:read-mvar fd off len)))
        (test-assert (= (zpb-otf:mvar-version mvar) #x00010000) "MVAR version is 1.0")
        (test-assert (> (zpb-otf:mvar-value-record-count mvar) 0) "MVAR value records > 0")))

    ;; avar
    (multiple-value-bind (off len) (find-table td "avar")
      (test-assert off "Cantarell has avar table")
      (let ((avar (zpb-otf:read-avar fd off len)))
        (test-assert (= (zpb-otf:avar-version avar) #x00010000) "avar version is 1.0")
        (test-assert (> (zpb-otf:avar-axis-count avar) 0) "avar axis count > 0")))))

(defun test-layout-tables ()
  (format t "~%=== 4. Testing OpenType Layout Tables (GDEF, GPOS, GSUB) ===~%")
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "Cantarell-VF.otf")))
         (td (zpb-otf:read-table-directory fd 0)))
    ;; GDEF
    (multiple-value-bind (off len) (find-table td "GDEF")
      (test-assert off "Cantarell has GDEF table")
      (let ((gdef (zpb-otf:read-gdef fd off len)))
        (test-assert (plusp (zpb-otf:gdef-version gdef)) "GDEF version is valid")))

    ;; GPOS
    (multiple-value-bind (off len) (find-table td "GPOS")
      (test-assert off "Cantarell has GPOS table")
      (let ((gpos (zpb-otf:read-gpos fd off len)))
        (test-assert (plusp (zpb-otf:gpos-version gpos)) "GPOS version is valid")))

    ;; GSUB
    (multiple-value-bind (off len) (find-table td "GSUB")
      (test-assert off "Cantarell has GSUB table")
      (let ((gsub (zpb-otf:read-gsub fd off len)))
        (test-assert (plusp (zpb-otf:gsub-version gsub)) "GSUB version is valid")))))

(defun test-truetype-tables ()
  (format t "~%=== 5. Testing TrueType Core Tables (LiberationSans-Regular.ttf) ===~%")
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "LiberationSans-Regular.ttf")))
         (td (zpb-otf:read-table-directory fd 0)))
    ;; head
    (multiple-value-bind (off len) (find-table td "head")
      (let ((head (zpb-otf:read-head fd off len)))
        (test-assert (= (zpb-otf:head-units-per-em head) 2048) "head units-per-em is 2048")
        (test-assert (= (zpb-otf:head-index-to-loc-format head) 1) "head index-to-loc-format is 1 (long)")))

    ;; hhea
    (multiple-value-bind (off len) (find-table td "hhea")
      (let ((hhea (zpb-otf:read-hhea fd off len)))
        (test-assert (= (zpb-otf:hhea-number-of-h-metrics hhea) 2620) "hhea number of h-metrics is 2620")
        (test-assert (> (zpb-otf:hhea-ascender hhea) 0) "hhea ascender is positive")))

    ;; maxp
    (multiple-value-bind (off len) (find-table td "maxp")
      (let ((maxp (zpb-otf:read-maxp fd off len)))
        (test-assert (= (zpb-otf:maxp-num-glyphs maxp) 2620) "maxp num-glyphs is 2620")))

    ;; OS/2
    (multiple-value-bind (off len) (find-table td "OS/2")
      (let ((os2 (zpb-otf:read-os2 fd off len)))
        (test-assert (= (zpb-otf:os2-us-weight-class os2) 400) "OS/2 weight class is 400 (Regular)")
        (test-assert (> (zpb-otf:os2-x-avg-char-width os2) 0) "OS/2 avg char width > 0")))

    ;; post
    (multiple-value-bind (off len) (find-table td "post")
      (let ((post (zpb-otf:read-post fd off len)))
        (test-assert (= (zpb-otf:post-italic-angle post) 0.0) "post italic angle is 0.0")))

    ;; name
    (multiple-value-bind (off len) (find-table td "name")
      (let ((name (zpb-otf:read-name fd off len)))
        (test-assert (> (zpb-otf:name-count name) 0) "name count > 0")))

    ;; cmap
    (multiple-value-bind (off len) (find-table td "cmap")
      (let ((cmap (zpb-otf:read-cmap fd off len)))
        (test-assert (> (zpb-otf:cmap-num-tables cmap) 0) "cmap num tables > 0")))

    ;; gasp
    (multiple-value-bind (off len) (find-table td "gasp")
      (let ((gasp (zpb-otf:read-gasp fd off len)))
        (test-assert (> (zpb-otf:gasp-num-ranges gasp) 0) "gasp num ranges > 0")))

    ;; kern
    (multiple-value-bind (off len) (find-table td "kern")
      (let ((kern (zpb-otf:read-ot-kern fd off len)))
        (test-assert (> (zpb-otf:ot-kern-n-tables kern) 0) "kern n-tables > 0")))))

(defun test-cff-and-baseline-tables ()
  (format t "~%=== 6. Testing CFF, BASE, VORG, DSIG, and Vertical Tables (base.ttf) ===~%")
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "base.ttf")))
         (td (zpb-otf:read-table-directory fd 0)))
    ;; CFF
    (multiple-value-bind (off len) (find-table td "CFF ")
      (let ((cff (zpb-otf:read-cff-header fd off len)))
        (test-assert (= (zpb-otf:cff-header-major cff) 1) "CFF major version is 1")
        (test-assert (= (zpb-otf:cff-header-minor cff) 0) "CFF minor version is 0")))

    ;; BASE
    (multiple-value-bind (off len) (find-table td "BASE")
      (let ((base (zpb-otf:read-base fd off len)))
        (test-assert (= (zpb-otf:base-version base) #x00010000) "BASE version is 1.0")))

    ;; DSIG
    (multiple-value-bind (off len) (find-table td "DSIG")
      (let ((dsig (zpb-otf:read-dsig fd off len)))
        (test-assert (= (zpb-otf:dsig-version dsig) 1) "DSIG version is 1")))

    ;; VORG
    (multiple-value-bind (off len) (find-table td "VORG")
      (let ((vorg (zpb-otf:read-vorg fd off len)))
        (test-assert (= (zpb-otf:vorg-version vorg) #x00010000) "VORG version is 1.0")
        (test-assert (= (zpb-otf:vorg-default-vert-origin-y vorg) 880) "VORG default vert origin Y is 880")))

    ;; vhea & vmtx
    (multiple-value-bind (off len) (find-table td "vhea")
      (let ((vhea (zpb-otf:read-vhea fd off len)))
        (test-assert (= (zpb-otf:vhea-number-of-long-ver-metrics vhea) 1) "vhea number of v-metrics is 1")))
    (multiple-value-bind (off len) (find-table td "vmtx")
      (let ((vmtx (zpb-otf:read-vmtx fd off len)))
        (test-assert (typep vmtx 'zpb-otf:vmtx) "vmtx parsed successfully")))))

(defun test-color-and-bitmap-tables ()
  (format t "~%=== 7. Testing Color, Bitmap, SBIX, and SVG Tables ===~%")
  ;; COLR & CPAL
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "chromacheck-colr.ttf")))
         (td (zpb-otf:read-table-directory fd 0)))
    (multiple-value-bind (off len) (find-table td "COLR")
      (let ((colr (zpb-otf:read-colr fd off len)))
        (test-assert (= (zpb-otf:colr-version colr) 0) "COLR version is 0")))
    (multiple-value-bind (off len) (find-table td "CPAL")
      (let ((cpal (zpb-otf:read-cpal fd off len)))
        (test-assert (= (zpb-otf:cpal-num-palettes cpal) 1) "CPAL num palettes is 1")
        (test-assert (= (zpb-otf:cpal-num-palette-entries cpal) 1) "CPAL num palette entries is 1"))))

  ;; sbix
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "chromacheck-sbix.ttf")))
         (td (zpb-otf:read-table-directory fd 0)))
    (multiple-value-bind (off len) (find-table td "sbix")
      (let ((sbix (zpb-otf:read-sbix fd off len)))
        (test-assert (= (zpb-otf:sbix-num-strikes sbix) 1) "sbix num strikes is 1"))))

  ;; SVG
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "chromacheck-svg.ttf")))
         (td (zpb-otf:read-table-directory fd 0)))
    (multiple-value-bind (off len) (find-table td "SVG ")
      (let ((svg (zpb-otf:read-svg fd off len)))
        (test-assert (= (zpb-otf:svg-version svg) 0) "SVG version is 0"))))

  ;; CBDT & CBLC
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "chromacheck-cbdt.ttf")))
         (td (zpb-otf:read-table-directory fd 0)))
    (multiple-value-bind (off len) (find-table td "CBDT")
      (let ((cbdt (zpb-otf:read-cbdt fd off len)))
        (test-assert (= (zpb-otf:cbdt-major-version cbdt) 2) "CBDT major version is 2")))
    (multiple-value-bind (off len) (find-table td "CBLC")
      (let ((cblc (zpb-otf:read-cblc fd off len)))
        (test-assert (= (zpb-otf:cblc-num-sizes cblc) 1) "CBLC num sizes is 1")))))

(defun test-math-table ()
  (format t "~%=== 8. Testing OpenType MATH Table (DejaVuMathTeXGyre.ttf) ===~%")
  (let* ((fd (zpb-otf:ensure-font-data (test-font-path "DejaVuMathTeXGyre.ttf")))
         (td (zpb-otf:read-table-directory fd 0)))
    (multiple-value-bind (off len) (find-table td "MATH")
      (test-assert off "DejaVuMathTeXGyre has MATH table")
      (let ((math (zpb-otf:read-math fd off len)))
        (test-assert (typep math 'zpb-otf:math) "MATH table parsed successfully")
        (test-assert (> (zpb-otf:math-math-constants-offset-offset math) 0) "MATH constants offset > 0")))))

(defun test-zpb-ttf-high-level ()
  (format t "~%=== 9. Testing High-Level zpb-ttf API Integration ===~%")
  ;; TrueType font with outlines
  (let ((loader (zpb-ttf:open-font-loader (test-font-path "LiberationSans-Regular.ttf"))))
    (test-assert (string= (zpb-ttf:family-name loader) "Liberation Sans") "family-name is Liberation Sans")
    (test-assert (string= (zpb-ttf:subfamily-name loader) "Regular") "subfamily-name is Regular")
    (test-assert (= (zpb-ttf:units/em loader) 2048) "units/em is 2048")
    (test-assert (= (zpb-ttf:glyph-count loader) 2620) "glyph-count is 2620")
    (test-assert (= (zpb-ttf:ascender loader) 1854) "ascender is 1854")
    (test-assert (= (zpb-ttf:descender loader) -434) "descender is -434")

    ;; Glyph A
    (let ((glyph (zpb-ttf:find-glyph #\A loader)))
      (test-assert glyph "find-glyph #\A returns glyph")
      (test-assert (= (zpb-ttf:font-index glyph) 36) "glyph #\A index is 36")
      (test-assert (= (zpb-ttf:advance-width glyph) 1366) "glyph #\A advance-width is 1366")
      (test-assert (= (zpb-ttf:left-side-bearing glyph) 4) "glyph #\A left-side-bearing is 4")
      (test-assert (= (zpb-ttf:contour-count glyph) 2) "glyph #\A contour count is 2")
      (let ((c0 (zpb-ttf:contour glyph 0)))
        (test-assert (= (length c0) 8) "glyph #\A contour 0 point count is 8")))

    ;; String bounding box
    (let ((bbox (zpb-ttf:string-bounding-box "Hello, World!" loader)))
      (test-assert (= (length bbox) 4) "string-bounding-box returns 4-element vector")
      (test-assert (> (aref bbox 2) (aref bbox 0)) "string-bounding-box width is positive")))

  ;; OpenType CFF2 font
  (let ((loader (zpb-ttf:open-font-loader (test-font-path "Cantarell-VF.otf"))))
    (test-assert (string= (zpb-ttf:family-name loader) "Cantarell") "family-name is Cantarell")
    (test-assert (= (zpb-ttf:glyph-count loader) 1307) "glyph-count is 1307")
    (let ((glyph (zpb-ttf:find-glyph #\A loader)))
      (test-assert glyph "find-glyph #\A in Cantarell returns glyph")
      (test-assert (= (zpb-ttf:advance-width glyph) 626) "glyph #\A advance width is 626"))))

;;; Regression tests on synthetic tables, for layouts the bundled fonts don't exercise.

(defun bytes->font-data (&rest items)
  "Build a FONT-DATA from ITEMS: integers are bytes; (:u16 n), (:u24 n) and (:u32 n) are big-endian."
  (let ((out '()))
    (dolist (it items)
      (if (integerp it)
          (push it out)
          (destructuring-bind (kind n) it
            (let ((width (ecase kind (:u16 2) (:u24 3) (:u32 4))))
              (loop for shift from (* 8 (1- width)) downto 0 by 8
                    do (push (ldb (byte 8 shift) n) out))))))
    (zpb-otf:make-font-data (coerce (nreverse out) '(simple-array (unsigned-byte 8) (*))))))

(defun test-synthetic-layouts ()
  (format t "~%--- Synthetic Layout Regressions ---~%")
  ;; Device vs VariationIndex is decided by deltaFormat at offset 4.
  (test-assert (typep (zpb-otf:read-device-or-variation-index
                       (bytes->font-data '(:u16 1) '(:u16 2) '(:u16 #x8000)) 0)
                      'zpb-otf:variation-index)
               "deltaFormat #x8000 reads as VariationIndex")
  (test-assert (typep (zpb-otf:read-device-or-variation-index
                       (bytes->font-data '(:u16 10) '(:u16 12) '(:u16 1) '(:u16 0)) 0)
                      'zpb-otf:device)
               "deltaFormat 1 reads as Device")
  ;; ValueRecord size and decoding follow the ValueFormat bits.
  (test-assert (= (zpb-otf:value-format-size #x0045) 6) "ValueFormat #x45 is 6 bytes")
  (let ((vr (zpb-otf:read-gpos-value-record (bytes->font-data '(:u16 #xFFF6) '(:u16 20)) 0 #x0005 0)))
    (test-assert (and (= (zpb-otf:gpos-value-record-x-placement vr) -10)
                      (= (zpb-otf:gpos-value-record-y-placement vr) 0)
                      (= (zpb-otf:gpos-value-record-x-advance vr) 20))
                 "ValueRecord with XPlacement|XAdvance decodes both fields"))
  ;; Lookup.markFilteringSet exists only with USE_MARK_FILTERING_SET.
  (test-assert (null (zpb-otf:lookup-mark-filtering-set
                      (zpb-otf:read-lookup (bytes->font-data '(:u16 1) '(:u16 0) '(:u16 1) '(:u16 8)) 0)))
               "markFilteringSet absent without the lookup flag")
  (test-assert (eql 7 (zpb-otf:lookup-mark-filtering-set
                       (zpb-otf:read-lookup (bytes->font-data '(:u16 1) '(:u16 #x10) '(:u16 1) '(:u16 8) '(:u16 7)) 0)))
               "markFilteringSet read after the subtable offsets when flagged")
  ;; CPAL v0 directly followed by its color records: no v1 fields.
  (let ((v0 (zpb-otf:read-cpal (bytes->font-data '(:u16 0) '(:u16 1) '(:u16 2) '(:u16 1) '(:u32 16)
                                                 '(:u16 0) '(:u16 0) '(:u32 #x11223344))
                               0)))
    (test-assert (null (zpb-otf:cpal-palette-types-array-offset-offset v0))
                 "CPAL v0 has no paletteTypesArrayOffset"))
  (let ((v1 (zpb-otf:read-cpal (bytes->font-data '(:u16 1) '(:u16 1) '(:u16 1) '(:u16 1) '(:u32 0)
                                                 '(:u16 0) '(:u32 0) '(:u32 0) '(:u32 0))
                               0)))
    (test-assert (eql 0 (zpb-otf:cpal-palette-types-array-offset-offset v1))
                 "CPAL v1 paletteTypesArrayOffset is read after colorRecordIndices"))
  ;; IFT flag-conditional fields.
  (flet ((patch-map (flags &rest tail)
           (apply #'bytes->font-data 2 0 0 0 flags
                  (append (make-list 16 :initial-element 0)
                          (list 1 '(:u24 0) '(:u32 0) '(:u32 0) '(:u16 1) #x41)
                          tail))))
    (let ((only-cff2 (zpb-otf:read-ift-patch-map (patch-map 2 '(:u32 99)) 0)))
      (test-assert (and (null (zpb-otf:ift-patch-map-cff-charstrings-offset only-cff2))
                        (eql 99 (zpb-otf:ift-patch-map-cff2-charstrings-offset only-cff2)))
                   "IFT cff2CharstringsOffset follows the URL template when cff is absent"))
    (let ((both (zpb-otf:read-ift-patch-map (patch-map 3 '(:u32 5) '(:u32 6)) 0)))
      (test-assert (and (eql 5 (zpb-otf:ift-patch-map-cff-charstrings-offset both))
                        (eql 6 (zpb-otf:ift-patch-map-cff2-charstrings-offset both)))
                   "IFT cff and cff2 charstring offsets with both flags")))
  (let ((plain (zpb-otf:read-entry-data (bytes->font-data 0 9) 0))
        (children (zpb-otf:read-entry-data (bytes->font-data 2 2 '(:u24 7) '(:u24 8)) 0)))
    (test-assert (and (null (zpb-otf:entry-data-feature-count plain))
                      (zerop (zpb-otf:entry-data-child-indices-count plain))
                      (= 1 (zpb-otf:entry-data-trailing-data-count plain)))
                 "IFT EntryData without flags has only trailing data")
    (test-assert (and (= 2 (zpb-otf:entry-data-child-indices-count children))
                      (= 8 (zpb-otf:entry-data-child-indices-ref children 1)))
                 "IFT EntryData child indices when CHILD_INDICES is set"))
  ;; Tuple sizes depend on TupleIndex flags and the caller's axis count.
  (let ((tvh (zpb-otf:read-tuple-variation-header
              (bytes->font-data '(:u16 0) '(:u16 #x8000) '(:u16 #x4000) '(:u16 #xC000)) 0)))
    (test-assert (and (= 2 (zpb-otf:tuple-variation-header-peak-tuple-count tvh 2))
                      (= 1.0 (zpb-otf:tuple-variation-header-peak-tuple-ref tvh 2 0))
                      (= -1.0 (zpb-otf:tuple-variation-header-peak-tuple-ref tvh 2 1)))
                 "TupleVariationHeader embedded peak tuple uses the axis count"))
  ;; 'kern' format 2: class tables and the value array are relative to the subtable.
  (let* ((fd (bytes->font-data '(:u16 0) '(:u16 1)                      ; kern v0, 1 subtable
                               '(:u16 0) '(:u16 38) '(:u16 #x0201)       ; format 2, horizontal
                               '(:u16 4) '(:u16 14) '(:u16 22) '(:u16 30) ; rowWidth, left, right, array
                               '(:u16 5) '(:u16 2) '(:u16 30) '(:u16 34)  ; left classes: glyphs 5, 6
                               '(:u16 7) '(:u16 2) '(:u16 0) '(:u16 2)    ; right classes: glyphs 7, 8
                               '(:u16 0) '(:u16 #xFFF6) '(:u16 20) '(:u16 0)))
         (loader (make-instance 'zpb-ttf::font-loader :font-data fd)))
    (zpb-ttf::load-kern-table loader 0 (zpb-otf:font-data-length fd))
    (let ((table (zpb-ttf::kerning-table loader)))
      (test-assert (and (= 2 (hash-table-count table))
                        (= -10 (gethash (logior (ash 5 16) 8) table))
                        (= 20 (gethash (logior (ash 6 16) 7) table)))
                   "kern format 2 class-based pairs are read; zero entries skipped")))
  ;; Range checks accept sizes beyond 32 bits.
  (test-assert (not (zpb-otf:font-data-valid-range-p (bytes->font-data 0 0) 1 (expt 2 33)))
               "valid-range-p rejects an oversized range"))

(defun run-table-tests ()
  (setf *test-failures* 0
        *test-count* 0)
  (format t "========================================================~%")
  (format t "Running Comprehensive OpenType Table Parser Test Suite~%")
  (format t "========================================================~%")

  (test-font-data-primitives)
  (test-ttc-collection)
  (test-variable-tables)
  (test-layout-tables)
  (test-truetype-tables)
  (test-cff-and-baseline-tables)
  (test-color-and-bitmap-tables)
  (test-math-table)
  (test-zpb-ttf-high-level)
  (test-synthetic-layouts)

  (format t "~%========================================================~%")
  (format t "Results: ~D tests run, ~D failures.~%" *test-count* *test-failures*)
  (format t "========================================================~%")
  (if (zerop *test-failures*)
      (progn
        (format t "ALL TABLE TESTS PASSED!~%~%")
        t)
      (error "Table test suite failed with ~D failure(s)" *test-failures*)))

#+sbcl
(when (member (pathname-name *load-truename*) '("tables-test") :test 'string-equal)
  (run-table-tests))
