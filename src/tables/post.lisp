;;;; post.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+post-tag+ post post-glyph-name-index-count post-glyph-name-index-ref
   post-is-fixed-pitch post-italic-angle post-max-mem-type1 post-max-mem-type42
   post-min-mem-type1 post-min-mem-type42 post-num-glyphs post-p
   post-string-data-start post-underline-position post-underline-thickness
   post-version read-post sanitize-post))

(defstruct
    (post (:include table-ref)
     (:constructor %make-post (%table-font-data %table-offset %table-length)))
  "[post (PostScript)](https://docs.microsoft.com/en-us/typography/opentype/spec/post#header) table")

(declaim
 (inline post-version post-version-raw post-italic-angle post-italic-angle-raw
  post-underline-position post-underline-position-raw post-underline-thickness
  post-underline-thickness-raw post-is-fixed-pitch post-is-fixed-pitch-raw
  post-min-mem-type42 post-min-mem-type42-raw post-max-mem-type42
  post-max-mem-type42-raw post-min-mem-type1 post-min-mem-type1-raw
  post-max-mem-type1 post-max-mem-type1-raw post-num-glyphs post-num-glyphs-raw
  post-glyph-name-index-count post-glyph-name-index-ref post-string-data-start))

(define-constant +post-tag+ "post")

(defun post-version (table)
  "0x00010000 for version 1.0 0x00020000 for version 2.0 0x00025000 for version 2.5 (deprecated) 0x00030000 for version 3.0"
  (declare (type post table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun post-version-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun post-italic-angle (table)
  "Italic angle in counter-clockwise degrees from the vertical. Zero for upright text, negative for text that leans to the right (forward)."
  (declare (type post table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun post-italic-angle-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 4)))

(defun post-underline-position (table)
  "This is the suggested distance of the top of the underline from the baseline (negative values indicate below baseline). The PostScript definition of this FontInfo dictionary key (the y coordinate of the center of the stroke) is not used for historical reasons. The value of the PostScript key may be calculated by subtracting half the underlineThickness from the value of this field."
  (declare (type post table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun post-underline-position-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun post-underline-thickness (table)
  "Suggested values for the underline thickness. In general, the underline thickness should match the thickness of the underscore character (U+005F LOW LINE), and should also match the strikeout thickness, which is specified in the OS/2 table."
  (declare (type post table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun post-underline-thickness-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun post-is-fixed-pitch (table)
  "Set to 0 if the font is proportionally spaced, non-zero if the font is not proportionally spaced (i.e. monospaced)."
  (declare (type post table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun post-is-fixed-pitch-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 12)))

(defun post-min-mem-type42 (table)
  "Minimum memory usage when an OpenType font is downloaded."
  (declare (type post table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun post-min-mem-type42-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 16)))

(defun post-max-mem-type42 (table)
  "Maximum memory usage when an OpenType font is downloaded."
  (declare (type post table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 20)))

(defun post-max-mem-type42-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 20)))

(defun post-min-mem-type1 (table)
  "Minimum memory usage when an OpenType font is downloaded as a Type 1 font."
  (declare (type post table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 24)))

(defun post-min-mem-type1-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 24)))

(defun post-max-mem-type1 (table)
  "Maximum memory usage when an OpenType font is downloaded as a Type 1 font."
  (declare (type post table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 28)))

(defun post-max-mem-type1-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 28)))

(defun post-num-glyphs (table)
  "Number of glyphs (this should be the same as numGlyphs in 'maxp' table)."
  (declare (type post table))
  (when
      (and (>= (post-version table) 131072)
           (<= (+ (+ (table-ref-offset table) 32) 2)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u16 (table-ref-font-data table)
                        (+ (table-ref-offset table) 32))))

(defun post-num-glyphs-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 32)))

(defun post-glyph-name-index-count (table)
  "Array of indices into the string data. See below for details."
  (declare (type post table))
  (declare (ignorable table))
  (if (and (>= (post-version table) 131072)
           (<= (+ (+ (table-ref-offset table) 34) 0)
               (+ (table-ref-offset table) (table-ref-length table))))
      (or (post-num-glyphs table) 0)
      0))

(defun post-glyph-name-index-ref (table index)
  "Array of indices into the string data. See below for details."
  (declare (type post table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 34) (* index 2))))
    (unless
        (<= (+ pos 2) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length
             (table-ref-length table)))
    (font-data-read-u16 (table-ref-font-data table) pos)))

(defun post-string-data-start (table)
  "Byte position of the string-data field; its elements are variable-length P-STRING data."
  (declare (type post table))
  (+ (table-ref-offset table) 34 (* (post-glyph-name-index-count table) 2)))

(defun sanitize-post (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 32 length) (font-data-valid-range-p font-data offset 32))
    (error 'font-out-of-bounds-error :pos offset :size 32 :length
           (font-data-length font-data)))
  (let* ((cnt (post-num-glyphs-raw font-data offset)) (pos (+ 34 (* cnt 2))))
    (when
        (and t
             (and (>= length 34)
                  (>= (post-version-raw font-data offset) 131072))
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-post
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-post font-data offset length)
  (%make-post font-data offset length))
