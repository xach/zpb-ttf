;;;; colr.lisp — Generated OpenType tables
;;;; Generated automatically from OpenType sexp schema
;;;; DO NOT EDIT DIRECTLY
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(export
 '(+base-glyph-paint-size+ +base-glyph-size+ +clip-size+ +color-index-size+
   +color-stop-size+ +colr-tag+ +composite-mode-clear+
   +composite-mode-color-burn+ +composite-mode-color-dodge+
   +composite-mode-darken+ +composite-mode-dest+ +composite-mode-dest-atop+
   +composite-mode-dest-in+ +composite-mode-dest-out+
   +composite-mode-dest-over+ +composite-mode-difference+
   +composite-mode-exclusion+ +composite-mode-hard-light+
   +composite-mode-hsl-color+ +composite-mode-hsl-hue+
   +composite-mode-hsl-luminosity+ +composite-mode-hsl-saturation+
   +composite-mode-lighten+ +composite-mode-multiply+ +composite-mode-overlay+
   +composite-mode-plus+ +composite-mode-screen+ +composite-mode-soft-light+
   +composite-mode-src+ +composite-mode-src-atop+ +composite-mode-src-in+
   +composite-mode-src-out+ +composite-mode-src-over+ +composite-mode-xor+
   +extend-pad+ +extend-reflect+ +extend-repeat+ +layer-size+
   +var-color-index-size+ +var-color-stop-size+ affine2x3 affine2x3-dx
   affine2x3-dy affine2x3-p affine2x3-xx affine2x3-xy affine2x3-yx affine2x3-yy
   base-glyph base-glyph-first-layer-index base-glyph-glyph-id base-glyph-list
   base-glyph-list-base-glyph-paint-records-count
   base-glyph-list-base-glyph-paint-records-ref
   base-glyph-list-num-base-glyph-paint-records base-glyph-list-p
   base-glyph-num-layers base-glyph-p base-glyph-paint
   base-glyph-paint-glyph-id base-glyph-paint-p base-glyph-paint-paint-offset
   clip clip-box-format1 clip-box-format1-format-table clip-box-format1-p
   clip-box-format1-x-max clip-box-format1-x-min clip-box-format1-y-max
   clip-box-format1-y-min clip-box-format2 clip-box-format2-format-table
   clip-box-format2-p clip-box-format2-var-index-base clip-box-format2-x-max
   clip-box-format2-x-min clip-box-format2-y-max clip-box-format2-y-min
   clip-clip-box-offset clip-end-glyph-id clip-list clip-list-clips-count
   clip-list-clips-ref clip-list-format-table clip-list-num-clips clip-list-p
   clip-p clip-start-glyph-id color-index color-index-alpha color-index-p
   color-index-palette-index color-line color-line-color-stops-count
   color-line-color-stops-ref color-line-extend color-line-num-stops
   color-line-p color-stop color-stop-alpha color-stop-p
   color-stop-palette-index color-stop-stop-offset colr
   colr-base-glyph-list-offset colr-base-glyph-list-offset-offset
   colr-base-glyph-records-offset colr-base-glyph-records-offset-offset
   colr-clip-list-offset colr-clip-list-offset-offset
   colr-item-variation-store-offset colr-item-variation-store-offset-offset
   colr-layer-list-offset colr-layer-list-offset-offset
   colr-layer-records-offset colr-layer-records-offset-offset
   colr-num-base-glyph-records colr-num-layer-records colr-p
   colr-var-index-map-offset colr-var-index-map-offset-offset colr-version
   layer layer-glyph-id layer-list layer-list-num-layers layer-list-p
   layer-list-paint-offsets-count layer-list-paint-offsets-ref layer-p
   layer-palette-index paint-colr-glyph paint-colr-glyph-format-table
   paint-colr-glyph-glyph-id paint-colr-glyph-p paint-colr-layers
   paint-colr-layers-first-layer-index paint-colr-layers-format-table
   paint-colr-layers-num-layers paint-colr-layers-p paint-composite
   paint-composite-backdrop-paint-offset
   paint-composite-backdrop-paint-offset-offset paint-composite-composite-mode
   paint-composite-format-table paint-composite-p
   paint-composite-source-paint-offset
   paint-composite-source-paint-offset-offset paint-glyph
   paint-glyph-format-table paint-glyph-glyph-id paint-glyph-p
   paint-glyph-paint-offset paint-glyph-paint-offset-offset paint-glyph2
   paint-glyph2-format-table paint-glyph2-glyph-id paint-glyph2-p
   paint-glyph2-paint-offset paint-glyph2-paint-offset-offset
   paint-linear-gradient paint-linear-gradient-color-line-offset
   paint-linear-gradient-color-line-offset-offset
   paint-linear-gradient-format-table paint-linear-gradient-p
   paint-linear-gradient-x0 paint-linear-gradient-x1 paint-linear-gradient-x2
   paint-linear-gradient-y0 paint-linear-gradient-y1 paint-linear-gradient-y2
   paint-radial-gradient paint-radial-gradient-color-line-offset
   paint-radial-gradient-color-line-offset-offset
   paint-radial-gradient-format-table paint-radial-gradient-p
   paint-radial-gradient-radius0 paint-radial-gradient-radius1
   paint-radial-gradient-x0 paint-radial-gradient-x1 paint-radial-gradient-y0
   paint-radial-gradient-y1 paint-rotate paint-rotate-angle
   paint-rotate-around-center paint-rotate-around-center-angle
   paint-rotate-around-center-center-x paint-rotate-around-center-center-y
   paint-rotate-around-center-format-table paint-rotate-around-center-p
   paint-rotate-around-center-paint-offset
   paint-rotate-around-center-paint-offset-offset paint-rotate-format-table
   paint-rotate-p paint-rotate-paint-offset paint-rotate-paint-offset-offset
   paint-scale paint-scale-around-center paint-scale-around-center-center-x
   paint-scale-around-center-center-y paint-scale-around-center-format-table
   paint-scale-around-center-p paint-scale-around-center-paint-offset
   paint-scale-around-center-paint-offset-offset
   paint-scale-around-center-scale-x paint-scale-around-center-scale-y
   paint-scale-format-table paint-scale-p paint-scale-paint-offset
   paint-scale-paint-offset-offset paint-scale-scale-x paint-scale-scale-y
   paint-scale-uniform paint-scale-uniform-around-center
   paint-scale-uniform-around-center-center-x
   paint-scale-uniform-around-center-center-y
   paint-scale-uniform-around-center-format-table
   paint-scale-uniform-around-center-p
   paint-scale-uniform-around-center-paint-offset
   paint-scale-uniform-around-center-paint-offset-offset
   paint-scale-uniform-around-center-scale paint-scale-uniform-format-table
   paint-scale-uniform-p paint-scale-uniform-paint-offset
   paint-scale-uniform-paint-offset-offset paint-scale-uniform-scale paint-skew
   paint-skew-around-center paint-skew-around-center-center-x
   paint-skew-around-center-center-y paint-skew-around-center-format-table
   paint-skew-around-center-p paint-skew-around-center-paint-offset
   paint-skew-around-center-paint-offset-offset
   paint-skew-around-center-x-skew-angle paint-skew-around-center-y-skew-angle
   paint-skew-format-table paint-skew-p paint-skew-paint-offset
   paint-skew-paint-offset-offset paint-skew-x-skew-angle
   paint-skew-y-skew-angle paint-solid paint-solid-alpha
   paint-solid-format-table paint-solid-p paint-solid-palette-index
   paint-sweep-gradient paint-sweep-gradient-center-x
   paint-sweep-gradient-center-y paint-sweep-gradient-color-line-offset
   paint-sweep-gradient-color-line-offset-offset paint-sweep-gradient-end-angle
   paint-sweep-gradient-format-table paint-sweep-gradient-p
   paint-sweep-gradient-start-angle paint-transform
   paint-transform-format-table paint-transform-p paint-transform-paint-offset
   paint-transform-paint-offset-offset paint-transform-transform-offset
   paint-transform-transform-offset-offset paint-translate paint-translate-dx
   paint-translate-dy paint-translate-format-table paint-translate-p
   paint-translate-paint-offset paint-translate-paint-offset-offset
   paint-var-linear-gradient paint-var-linear-gradient-color-line-offset
   paint-var-linear-gradient-color-line-offset-offset
   paint-var-linear-gradient-format-table paint-var-linear-gradient-p
   paint-var-linear-gradient-var-index-base paint-var-linear-gradient-x0
   paint-var-linear-gradient-x1 paint-var-linear-gradient-x2
   paint-var-linear-gradient-y0 paint-var-linear-gradient-y1
   paint-var-linear-gradient-y2 paint-var-radial-gradient
   paint-var-radial-gradient-color-line-offset
   paint-var-radial-gradient-color-line-offset-offset
   paint-var-radial-gradient-format-table paint-var-radial-gradient-p
   paint-var-radial-gradient-radius0 paint-var-radial-gradient-radius1
   paint-var-radial-gradient-var-index-base paint-var-radial-gradient-x0
   paint-var-radial-gradient-x1 paint-var-radial-gradient-y0
   paint-var-radial-gradient-y1 paint-var-rotate paint-var-rotate-angle
   paint-var-rotate-around-center paint-var-rotate-around-center-angle
   paint-var-rotate-around-center-center-x
   paint-var-rotate-around-center-center-y
   paint-var-rotate-around-center-format-table paint-var-rotate-around-center-p
   paint-var-rotate-around-center-paint-offset
   paint-var-rotate-around-center-paint-offset-offset
   paint-var-rotate-around-center-var-index-base paint-var-rotate-format-table
   paint-var-rotate-p paint-var-rotate-paint-offset
   paint-var-rotate-paint-offset-offset paint-var-rotate-var-index-base
   paint-var-scale paint-var-scale-around-center
   paint-var-scale-around-center-center-x
   paint-var-scale-around-center-center-y
   paint-var-scale-around-center-format-table paint-var-scale-around-center-p
   paint-var-scale-around-center-paint-offset
   paint-var-scale-around-center-paint-offset-offset
   paint-var-scale-around-center-scale-x paint-var-scale-around-center-scale-y
   paint-var-scale-around-center-var-index-base paint-var-scale-format-table
   paint-var-scale-p paint-var-scale-paint-offset
   paint-var-scale-paint-offset-offset paint-var-scale-scale-x
   paint-var-scale-scale-y paint-var-scale-uniform
   paint-var-scale-uniform-around-center
   paint-var-scale-uniform-around-center-center-x
   paint-var-scale-uniform-around-center-center-y
   paint-var-scale-uniform-around-center-format-table
   paint-var-scale-uniform-around-center-p
   paint-var-scale-uniform-around-center-paint-offset
   paint-var-scale-uniform-around-center-paint-offset-offset
   paint-var-scale-uniform-around-center-scale
   paint-var-scale-uniform-around-center-var-index-base
   paint-var-scale-uniform-format-table paint-var-scale-uniform-p
   paint-var-scale-uniform-paint-offset
   paint-var-scale-uniform-paint-offset-offset paint-var-scale-uniform-scale
   paint-var-scale-uniform-var-index-base paint-var-scale-var-index-base
   paint-var-skew paint-var-skew-around-center
   paint-var-skew-around-center-center-x paint-var-skew-around-center-center-y
   paint-var-skew-around-center-format-table paint-var-skew-around-center-p
   paint-var-skew-around-center-paint-offset
   paint-var-skew-around-center-paint-offset-offset
   paint-var-skew-around-center-var-index-base
   paint-var-skew-around-center-x-skew-angle
   paint-var-skew-around-center-y-skew-angle paint-var-skew-format-table
   paint-var-skew-p paint-var-skew-paint-offset
   paint-var-skew-paint-offset-offset paint-var-skew-var-index-base
   paint-var-skew-x-skew-angle paint-var-skew-y-skew-angle paint-var-solid
   paint-var-solid-alpha paint-var-solid-format-table paint-var-solid-p
   paint-var-solid-palette-index paint-var-solid-var-index-base
   paint-var-sweep-gradient paint-var-sweep-gradient-center-x
   paint-var-sweep-gradient-center-y paint-var-sweep-gradient-color-line-offset
   paint-var-sweep-gradient-color-line-offset-offset
   paint-var-sweep-gradient-end-angle paint-var-sweep-gradient-format-table
   paint-var-sweep-gradient-p paint-var-sweep-gradient-start-angle
   paint-var-sweep-gradient-var-index-base paint-var-transform
   paint-var-transform-format-table paint-var-transform-p
   paint-var-transform-paint-offset paint-var-transform-paint-offset-offset
   paint-var-transform-transform-offset
   paint-var-transform-transform-offset-offset paint-var-translate
   paint-var-translate-dx paint-var-translate-dy
   paint-var-translate-format-table paint-var-translate-p
   paint-var-translate-paint-offset paint-var-translate-paint-offset-offset
   paint-var-translate-var-index-base read-affine2x3 read-base-glyph-list
   read-clip-box read-clip-box-format1 read-clip-box-format2 read-clip-list
   read-color-line read-colr read-layer-list read-paint read-paint-colr-glyph
   read-paint-colr-layers read-paint-composite read-paint-glyph
   read-paint-glyph2 read-paint-linear-gradient read-paint-radial-gradient
   read-paint-rotate read-paint-rotate-around-center read-paint-scale
   read-paint-scale-around-center read-paint-scale-uniform
   read-paint-scale-uniform-around-center read-paint-skew
   read-paint-skew-around-center read-paint-solid read-paint-sweep-gradient
   read-paint-transform read-paint-translate read-paint-var-linear-gradient
   read-paint-var-radial-gradient read-paint-var-rotate
   read-paint-var-rotate-around-center read-paint-var-scale
   read-paint-var-scale-around-center read-paint-var-scale-uniform
   read-paint-var-scale-uniform-around-center read-paint-var-skew
   read-paint-var-skew-around-center read-paint-var-solid
   read-paint-var-sweep-gradient read-paint-var-transform
   read-paint-var-translate read-var-affine2x3 read-var-color-line
   sanitize-affine2x3 sanitize-base-glyph-list sanitize-clip-box-format1
   sanitize-clip-box-format2 sanitize-clip-list sanitize-color-line
   sanitize-colr sanitize-layer-list sanitize-paint-colr-glyph
   sanitize-paint-colr-layers sanitize-paint-composite sanitize-paint-glyph
   sanitize-paint-glyph2 sanitize-paint-linear-gradient
   sanitize-paint-radial-gradient sanitize-paint-rotate
   sanitize-paint-rotate-around-center sanitize-paint-scale
   sanitize-paint-scale-around-center sanitize-paint-scale-uniform
   sanitize-paint-scale-uniform-around-center sanitize-paint-skew
   sanitize-paint-skew-around-center sanitize-paint-solid
   sanitize-paint-sweep-gradient sanitize-paint-transform
   sanitize-paint-translate sanitize-paint-var-linear-gradient
   sanitize-paint-var-radial-gradient sanitize-paint-var-rotate
   sanitize-paint-var-rotate-around-center sanitize-paint-var-scale
   sanitize-paint-var-scale-around-center sanitize-paint-var-scale-uniform
   sanitize-paint-var-scale-uniform-around-center sanitize-paint-var-skew
   sanitize-paint-var-skew-around-center sanitize-paint-var-solid
   sanitize-paint-var-sweep-gradient sanitize-paint-var-transform
   sanitize-paint-var-translate sanitize-var-affine2x3 sanitize-var-color-line
   var-affine2x3 var-affine2x3-dx var-affine2x3-dy var-affine2x3-p
   var-affine2x3-var-index-base var-affine2x3-xx var-affine2x3-xy
   var-affine2x3-yx var-affine2x3-yy var-color-index var-color-index-alpha
   var-color-index-p var-color-index-palette-index
   var-color-index-var-index-base var-color-line
   var-color-line-color-stops-count var-color-line-color-stops-ref
   var-color-line-extend var-color-line-num-stops var-color-line-p
   var-color-stop var-color-stop-alpha var-color-stop-p
   var-color-stop-palette-index var-color-stop-stop-offset
   var-color-stop-var-index-base))

(define-constant +extend-pad+ 0
                 "[Extend](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) enumeration")

(define-constant +extend-repeat+ 1
                 "[Extend](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) enumeration")

(define-constant +extend-reflect+ 2
                 "[Extend](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) enumeration")

(define-constant +composite-mode-clear+ 0
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-src+ 1
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-dest+ 2
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-src-over+ 3
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-dest-over+ 4
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-src-in+ 5
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-dest-in+ 6
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-src-out+ 7
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-dest-out+ 8
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-src-atop+ 9
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-dest-atop+ 10
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-xor+ 11
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-plus+ 12
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-screen+ 13
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-overlay+ 14
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-darken+ 15
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-lighten+ 16
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-color-dodge+ 17
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-color-burn+ 18
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-hard-light+ 19
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-soft-light+ 20
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-difference+ 21
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-exclusion+ 22
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-multiply+ 23
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-hsl-hue+ 24
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-hsl-saturation+ 25
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-hsl-color+ 26
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(define-constant +composite-mode-hsl-luminosity+ 27
                 "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")

(defstruct
    (base-glyph (:include record-ref)
     (:constructor %make-base-glyph (%record-font-data %record-offset)))
  "[BaseGlyph](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyph-and-layer-records) record")

(define-constant +base-glyph-size+ 6)

(declaim
 (inline base-glyph-glyph-id base-glyph-first-layer-index
  base-glyph-num-layers))

(defun base-glyph-glyph-id (rec)
  "Glyph ID of the base glyph."
  (declare (type base-glyph rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun base-glyph-first-layer-index (rec)
  "Index (base 0) into the layerRecords array."
  (declare (type base-glyph rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun base-glyph-num-layers (rec)
  "Number of color layers associated with this glyph."
  (declare (type base-glyph rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (layer (:include record-ref)
     (:constructor %make-layer (%record-font-data %record-offset)))
  "[Layer](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyph-and-layer-records) record")

(define-constant +layer-size+ 4)

(declaim (inline layer-glyph-id layer-palette-index))

(defun layer-glyph-id (rec)
  "Glyph ID of the glyph used for a given layer."
  (declare (type layer rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun layer-palette-index (rec)
  "Index (base 0) for a palette entry in the CPAL table."
  (declare (type layer rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (base-glyph-paint (:include record-ref)
     (:constructor %make-base-glyph-paint (%record-font-data %record-offset)))
  "[BaseGlyphPaint](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) record")

(define-constant +base-glyph-paint-size+ 6)

(declaim (inline base-glyph-paint-glyph-id base-glyph-paint-paint-offset))

(defun base-glyph-paint-glyph-id (rec)
  "Glyph ID of the base glyph."
  (declare (type base-glyph-paint rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun base-glyph-paint-paint-offset (rec)
  "Offset to a Paint table, from the beginning of the [`BaseGlyphList`] table."
  (declare (type base-glyph-paint rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defstruct
    (clip (:include record-ref)
     (:constructor %make-clip (%record-font-data %record-offset)))
  "[Clip](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) record")

(define-constant +clip-size+ 7)

(declaim (inline clip-start-glyph-id clip-end-glyph-id clip-clip-box-offset))

(defun clip-start-glyph-id (rec)
  "First glyph ID in the range."
  (declare (type clip rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun clip-end-glyph-id (rec)
  "Last glyph ID in the range."
  (declare (type clip rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun clip-clip-box-offset (rec)
  "Offset to a ClipBox table, from the beginning of the [`ClipList`] table."
  (declare (type clip rec))
  (font-data-read-u24 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (color-index (:include record-ref)
     (:constructor %make-color-index (%record-font-data %record-offset)))
  "[ColorIndex](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) record")

(define-constant +color-index-size+ 4)

(declaim (inline color-index-palette-index color-index-alpha))

(defun color-index-palette-index (rec)
  "Index for a CPAL palette entry."
  (declare (type color-index rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun color-index-alpha (rec)
  "Alpha value."
  (declare (type color-index rec))
  (font-data-read-f2dot14 (record-ref-font-data rec)
                          (+ (record-ref-offset rec) 2)))

(defstruct
    (var-color-index (:include record-ref)
     (:constructor %make-var-color-index (%record-font-data %record-offset)))
  "[VarColorIndex](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) record")

(define-constant +var-color-index-size+ 8)

(declaim
 (inline var-color-index-palette-index var-color-index-alpha
  var-color-index-var-index-base))

(defun var-color-index-palette-index (rec)
  "Index for a CPAL palette entry."
  (declare (type var-color-index rec))
  (font-data-read-u16 (record-ref-font-data rec) (record-ref-offset rec)))

(defun var-color-index-alpha (rec)
  "Alpha value. For variation, use varIndexBase + 0."
  (declare (type var-color-index rec))
  (font-data-read-f2dot14 (record-ref-font-data rec)
                          (+ (record-ref-offset rec) 2)))

(defun var-color-index-var-index-base (rec)
  "Base index into DeltaSetIndexMap."
  (declare (type var-color-index rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 4)))

(defstruct
    (color-stop (:include record-ref)
     (:constructor %make-color-stop (%record-font-data %record-offset)))
  "[ColorStop](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) record")

(define-constant +color-stop-size+ 6)

(declaim
 (inline color-stop-stop-offset color-stop-palette-index color-stop-alpha))

(defun color-stop-stop-offset (rec)
  "Position on a color line."
  (declare (type color-stop rec))
  (font-data-read-f2dot14 (record-ref-font-data rec) (record-ref-offset rec)))

(defun color-stop-palette-index (rec)
  "Index for a CPAL palette entry."
  (declare (type color-stop rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun color-stop-alpha (rec)
  "Alpha value."
  (declare (type color-stop rec))
  (font-data-read-f2dot14 (record-ref-font-data rec)
                          (+ (record-ref-offset rec) 4)))

(defstruct
    (var-color-stop (:include record-ref)
     (:constructor %make-var-color-stop (%record-font-data %record-offset)))
  "[VarColorStop](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) record")

(define-constant +var-color-stop-size+ 10)

(declaim
 (inline var-color-stop-stop-offset var-color-stop-palette-index
  var-color-stop-alpha var-color-stop-var-index-base))

(defun var-color-stop-stop-offset (rec)
  "Position on a color line. For variation, use varIndexBase + 0."
  (declare (type var-color-stop rec))
  (font-data-read-f2dot14 (record-ref-font-data rec) (record-ref-offset rec)))

(defun var-color-stop-palette-index (rec)
  "Index for a CPAL palette entry."
  (declare (type var-color-stop rec))
  (font-data-read-u16 (record-ref-font-data rec) (+ (record-ref-offset rec) 2)))

(defun var-color-stop-alpha (rec)
  "Alpha value. For variation, use varIndexBase + 1."
  (declare (type var-color-stop rec))
  (font-data-read-f2dot14 (record-ref-font-data rec)
                          (+ (record-ref-offset rec) 4)))

(defun var-color-stop-var-index-base (rec)
  "Base index into DeltaSetIndexMap."
  (declare (type var-color-stop rec))
  (font-data-read-u32 (record-ref-font-data rec) (+ (record-ref-offset rec) 6)))

(defstruct
    (colr (:include table-ref)
     (:constructor %make-colr (%table-font-data %table-offset %table-length)))
  "[COLR (Color)](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#colr-header) table")

(declaim
 (inline colr-version colr-version-raw colr-num-base-glyph-records
  colr-num-base-glyph-records-raw colr-base-glyph-records-offset-offset
  colr-base-glyph-records-offset colr-layer-records-offset-offset
  colr-layer-records-offset colr-num-layer-records colr-num-layer-records-raw
  colr-base-glyph-list-offset-offset colr-base-glyph-list-offset
  colr-layer-list-offset-offset colr-layer-list-offset
  colr-clip-list-offset-offset colr-clip-list-offset
  colr-var-index-map-offset-offset colr-var-index-map-offset
  colr-item-variation-store-offset-offset colr-item-variation-store-offset))

(define-constant +colr-tag+ "COLR")

(defun colr-version (table)
  "Table version number - set to 0 or 1."
  (declare (type colr table))
  (font-data-read-u16 (table-ref-font-data table) (table-ref-offset table)))

(defun colr-version-raw (font-data offset)
  (font-data-read-u16 font-data offset))

(defun colr-num-base-glyph-records (table)
  "Number of BaseGlyph records; may be 0 in a version 1 table."
  (declare (type colr table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun colr-num-base-glyph-records-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 2)))

(defun colr-base-glyph-records-offset-offset (table)
  "Offset to baseGlyphRecords array (may be NULL)."
  (declare (type colr table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun colr-base-glyph-records-offset (table)
  "Offset to baseGlyphRecords array (may be NULL)."
  (declare (type colr table))
  (let ((off (colr-base-glyph-records-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun colr-layer-records-offset-offset (table)
  "Offset to layerRecords array (may be NULL)."
  (declare (type colr table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun colr-layer-records-offset (table)
  "Offset to layerRecords array (may be NULL)."
  (declare (type colr table))
  (let ((off (colr-layer-records-offset-offset table)))
    (when (and off (not (zerop off)))
      (font-data-slice (table-ref-font-data table)
                       (+ (table-ref-offset table) off)))))

(defun colr-num-layer-records (table)
  "Number of Layer records; may be 0 in a version 1 table."
  (declare (type colr table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun colr-num-layer-records-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 12)))

(defun colr-base-glyph-list-offset-offset (table)
  "Offset to BaseGlyphList table."
  (declare (type colr table))
  (when
      (and (>= (colr-version table) 1)
           (<= (+ (+ (table-ref-offset table) 14) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 14))))

(defun colr-base-glyph-list-offset (table)
  "Offset to BaseGlyphList table."
  (declare (type colr table))
  (let ((off (colr-base-glyph-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-base-glyph-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun colr-layer-list-offset-offset (table)
  "Offset to LayerList table (may be NULL)."
  (declare (type colr table))
  (when
      (and (>= (colr-version table) 1)
           (<= (+ (+ (table-ref-offset table) 18) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 18))))

(defun colr-layer-list-offset (table)
  "Offset to LayerList table (may be NULL)."
  (declare (type colr table))
  (let ((off (colr-layer-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-layer-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun colr-clip-list-offset-offset (table)
  "Offset to ClipList table (may be NULL)."
  (declare (type colr table))
  (when
      (and (>= (colr-version table) 1)
           (<= (+ (+ (table-ref-offset table) 22) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 22))))

(defun colr-clip-list-offset (table)
  "Offset to ClipList table (may be NULL)."
  (declare (type colr table))
  (let ((off (colr-clip-list-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-clip-list (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun colr-var-index-map-offset-offset (table)
  "Offset to DeltaSetIndexMap table (may be NULL)."
  (declare (type colr table))
  (when
      (and (>= (colr-version table) 1)
           (<= (+ (+ (table-ref-offset table) 26) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 26))))

(defun colr-var-index-map-offset (table)
  "Offset to DeltaSetIndexMap table (may be NULL)."
  (declare (type colr table))
  (let ((off (colr-var-index-map-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-delta-set-index-map (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun colr-item-variation-store-offset-offset (table)
  "Offset to ItemVariationStore (may be NULL)."
  (declare (type colr table))
  (when
      (and (>= (colr-version table) 1)
           (<= (+ (+ (table-ref-offset table) 30) 4)
               (+ (table-ref-offset table) (table-ref-length table))))
    (font-data-read-u32 (table-ref-font-data table)
                        (+ (table-ref-offset table) 30))))

(defun colr-item-variation-store-offset (table)
  "Offset to ItemVariationStore (may be NULL)."
  (declare (type colr table))
  (let ((off (colr-item-variation-store-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-item-variation-store (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-colr (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 14 length) (font-data-valid-range-p font-data offset 14))
    (error 'font-out-of-bounds-error :pos offset :size 14 :length
           (font-data-length font-data)))
  t)

(defun read-colr
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-colr font-data offset length)
  (%make-colr font-data offset length))

(defstruct
    (base-glyph-list (:include table-ref)
     (:constructor %make-base-glyph-list
      (%table-font-data %table-offset %table-length)))
  "[BaseGlyphList](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) table")

(declaim
 (inline base-glyph-list-num-base-glyph-paint-records
  base-glyph-list-num-base-glyph-paint-records-raw
  base-glyph-list-base-glyph-paint-records-count
  base-glyph-list-base-glyph-paint-records-ref))

(defun base-glyph-list-num-base-glyph-paint-records (table)
  (declare (type base-glyph-list table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun base-glyph-list-num-base-glyph-paint-records-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun base-glyph-list-base-glyph-paint-records-count (table)
  (declare (type base-glyph-list table))
  (declare (ignorable table))
  (base-glyph-list-num-base-glyph-paint-records table))

(defun base-glyph-list-base-glyph-paint-records-ref (table index)
  (declare (type base-glyph-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-base-glyph-paint (table-ref-font-data table) pos)))

(defun sanitize-base-glyph-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt
          (base-glyph-list-num-base-glyph-paint-records-raw font-data offset))
         (pos (+ 4 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-base-glyph-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-base-glyph-list font-data offset length)
  (%make-base-glyph-list font-data offset length))

(defstruct
    (layer-list (:include table-ref)
     (:constructor %make-layer-list
      (%table-font-data %table-offset %table-length)))
  "[LayerList](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) table")

(declaim
 (inline layer-list-num-layers layer-list-num-layers-raw
  layer-list-paint-offsets-count layer-list-paint-offsets-ref))

(defun layer-list-num-layers (table)
  (declare (type layer-list table))
  (font-data-read-u32 (table-ref-font-data table) (table-ref-offset table)))

(defun layer-list-num-layers-raw (font-data offset)
  (font-data-read-u32 font-data offset))

(defun layer-list-paint-offsets-count (table)
  "Offsets to Paint tables."
  (declare (type layer-list table))
  (declare (ignorable table))
  (layer-list-num-layers table))

(defun layer-list-paint-offsets-ref (table index)
  "Offsets to Paint tables."
  (declare (type layer-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 4) (* index 4))))
    (unless
        (<= (+ pos 4) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length
             (table-ref-length table)))
    (let ((off (font-data-read-u32 (table-ref-font-data table) pos)))
      (unless (zerop off)
        (read-paint (table-ref-font-data table)
         (+ (table-ref-offset table) off))))))

(defun sanitize-layer-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 4 length) (font-data-valid-range-p font-data offset 4))
    (error 'font-out-of-bounds-error :pos offset :size 4 :length
           (font-data-length font-data)))
  (let* ((cnt (layer-list-num-layers-raw font-data offset))
         (pos (+ 4 (* cnt 4))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-layer-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-layer-list font-data offset length)
  (%make-layer-list font-data offset length))

(defstruct
    (clip-list (:include table-ref)
     (:constructor %make-clip-list
      (%table-font-data %table-offset %table-length)))
  "[ClipList](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) table")

(declaim
 (inline clip-list-format-table clip-list-format-table-raw clip-list-num-clips
  clip-list-num-clips-raw clip-list-clips-count clip-list-clips-ref))

(defun clip-list-format-table (table)
  "Set to 1."
  (declare (type clip-list table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun clip-list-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun clip-list-num-clips (table)
  "Number of Clip records."
  (declare (type clip-list table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun clip-list-num-clips-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 1)))

(defun clip-list-clips-count (table)
  "Clip records. Sorted by startGlyphID."
  (declare (type clip-list table))
  (declare (ignorable table))
  (clip-list-num-clips table))

(defun clip-list-clips-ref (table index)
  "Clip records. Sorted by startGlyphID."
  (declare (type clip-list table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 5) (* index 7))))
    (unless
        (<= (+ pos 7) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 7 :length
             (table-ref-length table)))
    (%make-clip (table-ref-font-data table) pos)))

(defun sanitize-clip-list (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 5 length) (font-data-valid-range-p font-data offset 5))
    (error 'font-out-of-bounds-error :pos offset :size 5 :length
           (font-data-length font-data)))
  (let* ((cnt (clip-list-num-clips-raw font-data offset)) (pos (+ 5 (* cnt 7))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-clip-list
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-clip-list font-data offset length)
  (%make-clip-list font-data offset length))

(defun read-clip-box
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "[ClipBox](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) table"
  (declare (ignorable length))
  (let ((fmt (font-data-read-u8 font-data (+ offset 0))))
    (case fmt
      (1 (read-clip-box-format1 font-data offset length))
      (2 (read-clip-box-format2 font-data offset length))
      (otherwise
       (error 'unsupported-format-error :format fmt :table 'clip-box)))))

(defstruct
    (clip-box-format1 (:include table-ref)
     (:constructor %make-clip-box-format1
      (%table-font-data %table-offset %table-length)))
  "[ClipBoxFormat1](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) record")

(declaim
 (inline clip-box-format1-format-table clip-box-format1-format-table-raw
  clip-box-format1-x-min clip-box-format1-x-min-raw clip-box-format1-y-min
  clip-box-format1-y-min-raw clip-box-format1-x-max clip-box-format1-x-max-raw
  clip-box-format1-y-max clip-box-format1-y-max-raw))

(defun clip-box-format1-format-table (table)
  "Set to 1."
  (declare (type clip-box-format1 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun clip-box-format1-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun clip-box-format1-x-min (table)
  "Minimum x of clip box."
  (declare (type clip-box-format1 table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 1)))

(defun clip-box-format1-x-min-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 1)))

(defun clip-box-format1-y-min (table)
  "Minimum y of clip box."
  (declare (type clip-box-format1 table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 3)))

(defun clip-box-format1-y-min-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 3)))

(defun clip-box-format1-x-max (table)
  "Maximum x of clip box."
  (declare (type clip-box-format1 table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 5)))

(defun clip-box-format1-x-max-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 5)))

(defun clip-box-format1-y-max (table)
  "Maximum y of clip box."
  (declare (type clip-box-format1 table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 7)))

(defun clip-box-format1-y-max-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 7)))

(defun sanitize-clip-box-format1 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 9 length) (font-data-valid-range-p font-data offset 9))
    (error 'font-out-of-bounds-error :pos offset :size 9 :length
           (font-data-length font-data)))
  t)

(defun read-clip-box-format1
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-clip-box-format1 font-data offset length)
  (%make-clip-box-format1 font-data offset length))

(defstruct
    (clip-box-format2 (:include table-ref)
     (:constructor %make-clip-box-format2
      (%table-font-data %table-offset %table-length)))
  "[ClipBoxFormat2](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) record")

(declaim
 (inline clip-box-format2-format-table clip-box-format2-format-table-raw
  clip-box-format2-x-min clip-box-format2-x-min-raw clip-box-format2-y-min
  clip-box-format2-y-min-raw clip-box-format2-x-max clip-box-format2-x-max-raw
  clip-box-format2-y-max clip-box-format2-y-max-raw
  clip-box-format2-var-index-base clip-box-format2-var-index-base-raw))

(defun clip-box-format2-format-table (table)
  "Set to 2."
  (declare (type clip-box-format2 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun clip-box-format2-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun clip-box-format2-x-min (table)
  "Minimum x of clip box. For variation, use varIndexBase + 0."
  (declare (type clip-box-format2 table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 1)))

(defun clip-box-format2-x-min-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 1)))

(defun clip-box-format2-y-min (table)
  "Minimum y of clip box. For variation, use varIndexBase + 1."
  (declare (type clip-box-format2 table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 3)))

(defun clip-box-format2-y-min-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 3)))

(defun clip-box-format2-x-max (table)
  "Maximum x of clip box. For variation, use varIndexBase + 2."
  (declare (type clip-box-format2 table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 5)))

(defun clip-box-format2-x-max-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 5)))

(defun clip-box-format2-y-max (table)
  "Maximum y of clip box. For variation, use varIndexBase + 3."
  (declare (type clip-box-format2 table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 7)))

(defun clip-box-format2-y-max-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 7)))

(defun clip-box-format2-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type clip-box-format2 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 9)))

(defun clip-box-format2-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 9)))

(defun sanitize-clip-box-format2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 13 length) (font-data-valid-range-p font-data offset 13))
    (error 'font-out-of-bounds-error :pos offset :size 13 :length
           (font-data-length font-data)))
  t)

(defun read-clip-box-format2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-clip-box-format2 font-data offset length)
  (%make-clip-box-format2 font-data offset length))

(defstruct
    (color-line (:include table-ref)
     (:constructor %make-color-line
      (%table-font-data %table-offset %table-length)))
  "[ColorLine](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) table")

(declaim
 (inline color-line-extend color-line-extend-raw color-line-num-stops
  color-line-num-stops-raw color-line-color-stops-count
  color-line-color-stops-ref))

(defun color-line-extend (table)
  "An Extend enum value."
  (declare (type color-line table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun color-line-extend-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun color-line-num-stops (table)
  "Number of ColorStop records."
  (declare (type color-line table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun color-line-num-stops-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 1)))

(defun color-line-color-stops-count (table)
  (declare (type color-line table))
  (declare (ignorable table))
  (color-line-num-stops table))

(defun color-line-color-stops-ref (table index)
  (declare (type color-line table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 3) (* index 6))))
    (unless
        (<= (+ pos 6) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 6 :length
             (table-ref-length table)))
    (%make-color-stop (table-ref-font-data table) pos)))

(defun sanitize-color-line (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  (let* ((cnt (color-line-num-stops-raw font-data offset))
         (pos (+ 3 (* cnt 6))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-color-line
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-color-line font-data offset length)
  (%make-color-line font-data offset length))

(defstruct
    (var-color-line (:include table-ref)
     (:constructor %make-var-color-line
      (%table-font-data %table-offset %table-length)))
  "[VarColorLine](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) table")

(declaim
 (inline var-color-line-extend var-color-line-extend-raw
  var-color-line-num-stops var-color-line-num-stops-raw
  var-color-line-color-stops-count var-color-line-color-stops-ref))

(defun var-color-line-extend (table)
  "An Extend enum value."
  (declare (type var-color-line table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun var-color-line-extend-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun var-color-line-num-stops (table)
  "Number of ColorStop records."
  (declare (type var-color-line table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun var-color-line-num-stops-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 1)))

(defun var-color-line-color-stops-count (table)
  "Allows for variations."
  (declare (type var-color-line table))
  (declare (ignorable table))
  (var-color-line-num-stops table))

(defun var-color-line-color-stops-ref (table index)
  "Allows for variations."
  (declare (type var-color-line table)
           (ignorable index))
  (declare (type (unsigned-byte 32) index))
  (let ((pos (+ (+ (table-ref-offset table) 3) (* index 10))))
    (unless
        (<= (+ pos 10) (+ (table-ref-offset table) (table-ref-length table)))
      (error 'font-out-of-bounds-error :pos pos :size 10 :length
             (table-ref-length table)))
    (%make-var-color-stop (table-ref-font-data table) pos)))

(defun sanitize-var-color-line (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  (let* ((cnt (var-color-line-num-stops-raw font-data offset))
         (pos (+ 3 (* cnt 10))))
    (when
        (and t t
             (not
              (and (<= pos length)
                   (font-data-valid-range-p font-data offset pos))))
      (error 'font-out-of-bounds-error :pos offset :size pos :length
             (font-data-length font-data))))
  t)

(defun read-var-color-line
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-var-color-line font-data offset length)
  (%make-var-color-line font-data offset length))

(defun read-paint
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  "[Paint](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#paint-tables) tables"
  (declare (ignorable length))
  (let ((fmt (font-data-read-u8 font-data (+ offset 0))))
    (case fmt
      (1 (read-paint-colr-layers font-data offset length))
      (2 (read-paint-solid font-data offset length))
      (3 (read-paint-var-solid font-data offset length))
      (4 (read-paint-linear-gradient font-data offset length))
      (5 (read-paint-var-linear-gradient font-data offset length))
      (6 (read-paint-radial-gradient font-data offset length))
      (7 (read-paint-var-radial-gradient font-data offset length))
      (8 (read-paint-sweep-gradient font-data offset length))
      (9 (read-paint-var-sweep-gradient font-data offset length))
      (10 (read-paint-glyph font-data offset length))
      (11 (read-paint-colr-glyph font-data offset length))
      (12 (read-paint-transform font-data offset length))
      (13 (read-paint-var-transform font-data offset length))
      (14 (read-paint-translate font-data offset length))
      (15 (read-paint-var-translate font-data offset length))
      (16 (read-paint-scale font-data offset length))
      (17 (read-paint-var-scale font-data offset length))
      (18 (read-paint-scale-around-center font-data offset length))
      (19 (read-paint-var-scale-around-center font-data offset length))
      (20 (read-paint-scale-uniform font-data offset length))
      (21 (read-paint-var-scale-uniform font-data offset length))
      (22 (read-paint-scale-uniform-around-center font-data offset length))
      (23 (read-paint-var-scale-uniform-around-center font-data offset length))
      (24 (read-paint-rotate font-data offset length))
      (25 (read-paint-var-rotate font-data offset length))
      (26 (read-paint-rotate-around-center font-data offset length))
      (27 (read-paint-var-rotate-around-center font-data offset length))
      (28 (read-paint-skew font-data offset length))
      (29 (read-paint-var-skew font-data offset length))
      (30 (read-paint-skew-around-center font-data offset length))
      (31 (read-paint-var-skew-around-center font-data offset length))
      (32 (read-paint-composite font-data offset length))
      (33 (read-paint-glyph2 font-data offset length))
      (otherwise (error 'unsupported-format-error :format fmt :table 'paint)))))

(defstruct
    (paint-colr-layers (:include table-ref)
     (:constructor %make-paint-colr-layers
      (%table-font-data %table-offset %table-length)))
  "[PaintColrLayers](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-1-paintcolrlayers) table")

(declaim
 (inline paint-colr-layers-format-table paint-colr-layers-format-table-raw
  paint-colr-layers-num-layers paint-colr-layers-num-layers-raw
  paint-colr-layers-first-layer-index paint-colr-layers-first-layer-index-raw))

(defun paint-colr-layers-format-table (table)
  "Set to 1."
  (declare (type paint-colr-layers table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-colr-layers-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-colr-layers-num-layers (table)
  "Number of offsets to paint tables to read from LayerList."
  (declare (type paint-colr-layers table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 1)))

(defun paint-colr-layers-num-layers-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 1)))

(defun paint-colr-layers-first-layer-index (table)
  "Index (base 0) into the LayerList."
  (declare (type paint-colr-layers table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 2)))

(defun paint-colr-layers-first-layer-index-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 2)))

(defun sanitize-paint-colr-layers (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-paint-colr-layers
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-colr-layers font-data offset length)
  (%make-paint-colr-layers font-data offset length))

(defstruct
    (paint-solid (:include table-ref)
     (:constructor %make-paint-solid
      (%table-font-data %table-offset %table-length)))
  "[PaintSolid](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-2-and-3-paintsolid-paintvarsolid) table")

(declaim
 (inline paint-solid-format-table paint-solid-format-table-raw
  paint-solid-palette-index paint-solid-palette-index-raw paint-solid-alpha
  paint-solid-alpha-raw))

(defun paint-solid-format-table (table)
  "Set to 2."
  (declare (type paint-solid table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-solid-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-solid-palette-index (table)
  "Index for a CPAL palette entry."
  (declare (type paint-solid table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-solid-palette-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 1)))

(defun paint-solid-alpha (table)
  "Alpha value."
  (declare (type paint-solid table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 3)))

(defun paint-solid-alpha-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 3)))

(defun sanitize-paint-solid (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 5 length) (font-data-valid-range-p font-data offset 5))
    (error 'font-out-of-bounds-error :pos offset :size 5 :length
           (font-data-length font-data)))
  t)

(defun read-paint-solid
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-solid font-data offset length)
  (%make-paint-solid font-data offset length))

(defstruct
    (paint-var-solid (:include table-ref)
     (:constructor %make-paint-var-solid
      (%table-font-data %table-offset %table-length)))
  "[PaintVarSolid](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-2-and-3-paintsolid-paintvarsolid) table")

(declaim
 (inline paint-var-solid-format-table paint-var-solid-format-table-raw
  paint-var-solid-palette-index paint-var-solid-palette-index-raw
  paint-var-solid-alpha paint-var-solid-alpha-raw
  paint-var-solid-var-index-base paint-var-solid-var-index-base-raw))

(defun paint-var-solid-format-table (table)
  "Set to 3."
  (declare (type paint-var-solid table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-solid-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-solid-palette-index (table)
  "Index for a CPAL palette entry."
  (declare (type paint-var-solid table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-solid-palette-index-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 1)))

(defun paint-var-solid-alpha (table)
  "Alpha value. For variation, use varIndexBase + 0."
  (declare (type paint-var-solid table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 3)))

(defun paint-var-solid-alpha-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 3)))

(defun paint-var-solid-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-solid table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 5)))

(defun paint-var-solid-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 5)))

(defun sanitize-paint-var-solid (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 9 length) (font-data-valid-range-p font-data offset 9))
    (error 'font-out-of-bounds-error :pos offset :size 9 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-solid
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-solid font-data offset length)
  (%make-paint-var-solid font-data offset length))

(defstruct
    (paint-linear-gradient (:include table-ref)
     (:constructor %make-paint-linear-gradient
      (%table-font-data %table-offset %table-length)))
  "[PaintLinearGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-4-and-5-paintlineargradient-paintvarlineargradient) table")

(declaim
 (inline paint-linear-gradient-format-table
  paint-linear-gradient-format-table-raw
  paint-linear-gradient-color-line-offset-offset
  paint-linear-gradient-color-line-offset paint-linear-gradient-x0
  paint-linear-gradient-x0-raw paint-linear-gradient-y0
  paint-linear-gradient-y0-raw paint-linear-gradient-x1
  paint-linear-gradient-x1-raw paint-linear-gradient-y1
  paint-linear-gradient-y1-raw paint-linear-gradient-x2
  paint-linear-gradient-x2-raw paint-linear-gradient-y2
  paint-linear-gradient-y2-raw))

(defun paint-linear-gradient-format-table (table)
  "Set to 4."
  (declare (type paint-linear-gradient table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-linear-gradient-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-linear-gradient-color-line-offset-offset (table)
  "Offset to ColorLine table."
  (declare (type paint-linear-gradient table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-linear-gradient-color-line-offset (table)
  "Offset to ColorLine table."
  (declare (type paint-linear-gradient table))
  (let ((off (paint-linear-gradient-color-line-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-color-line (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-linear-gradient-x0 (table)
  "Start point (p₀) x coordinate."
  (declare (type paint-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun paint-linear-gradient-x0-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun paint-linear-gradient-y0 (table)
  "Start point (p₀) y coordinate."
  (declare (type paint-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-linear-gradient-y0-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-linear-gradient-x1 (table)
  "End point (p₁) x coordinate."
  (declare (type paint-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-linear-gradient-x1-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun paint-linear-gradient-y1 (table)
  "End point (p₁) y coordinate."
  (declare (type paint-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun paint-linear-gradient-y1-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun paint-linear-gradient-x2 (table)
  "Rotation point (p₂) x coordinate."
  (declare (type paint-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun paint-linear-gradient-x2-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 12)))

(defun paint-linear-gradient-y2 (table)
  "Rotation point (p₂) y coordinate."
  (declare (type paint-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 14)))

(defun paint-linear-gradient-y2-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 14)))

(defun sanitize-paint-linear-gradient (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-paint-linear-gradient
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-linear-gradient font-data offset length)
  (%make-paint-linear-gradient font-data offset length))

(defstruct
    (paint-var-linear-gradient (:include table-ref)
     (:constructor %make-paint-var-linear-gradient
      (%table-font-data %table-offset %table-length)))
  "[PaintVarLinearGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-4-and-5-paintlineargradient-paintvarlineargradient) table")

(declaim
 (inline paint-var-linear-gradient-format-table
  paint-var-linear-gradient-format-table-raw
  paint-var-linear-gradient-color-line-offset-offset
  paint-var-linear-gradient-color-line-offset paint-var-linear-gradient-x0
  paint-var-linear-gradient-x0-raw paint-var-linear-gradient-y0
  paint-var-linear-gradient-y0-raw paint-var-linear-gradient-x1
  paint-var-linear-gradient-x1-raw paint-var-linear-gradient-y1
  paint-var-linear-gradient-y1-raw paint-var-linear-gradient-x2
  paint-var-linear-gradient-x2-raw paint-var-linear-gradient-y2
  paint-var-linear-gradient-y2-raw paint-var-linear-gradient-var-index-base
  paint-var-linear-gradient-var-index-base-raw))

(defun paint-var-linear-gradient-format-table (table)
  "Set to 5."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-linear-gradient-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-linear-gradient-color-line-offset-offset (table)
  "Offset to VarColorLine table."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-linear-gradient-color-line-offset (table)
  "Offset to VarColorLine table."
  (declare (type paint-var-linear-gradient table))
  (let ((off (paint-var-linear-gradient-color-line-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-var-color-line (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-linear-gradient-x0 (table)
  "Start point (p₀) x coordinate. For variation, use varIndexBase + 0."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun paint-var-linear-gradient-x0-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun paint-var-linear-gradient-y0 (table)
  "Start point (p₀) y coordinate. For variation, use varIndexBase + 1."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-var-linear-gradient-y0-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-var-linear-gradient-x1 (table)
  "End point (p₁) x coordinate. For variation, use varIndexBase + 2."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-var-linear-gradient-x1-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun paint-var-linear-gradient-y1 (table)
  "End point (p₁) y coordinate. For variation, use varIndexBase + 3."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun paint-var-linear-gradient-y1-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun paint-var-linear-gradient-x2 (table)
  "Rotation point (p₂) x coordinate. For variation, use varIndexBase + 4."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun paint-var-linear-gradient-x2-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 12)))

(defun paint-var-linear-gradient-y2 (table)
  "Rotation point (p₂) y coordinate. For variation, use varIndexBase + 5."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 14)))

(defun paint-var-linear-gradient-y2-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 14)))

(defun paint-var-linear-gradient-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-linear-gradient table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun paint-var-linear-gradient-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 16)))

(defun sanitize-paint-var-linear-gradient (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-linear-gradient
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-linear-gradient font-data offset length)
  (%make-paint-var-linear-gradient font-data offset length))

(defstruct
    (paint-radial-gradient (:include table-ref)
     (:constructor %make-paint-radial-gradient
      (%table-font-data %table-offset %table-length)))
  "[PaintRadialGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-6-and-7-paintradialgradient-paintvarradialgradient) table")

(declaim
 (inline paint-radial-gradient-format-table
  paint-radial-gradient-format-table-raw
  paint-radial-gradient-color-line-offset-offset
  paint-radial-gradient-color-line-offset paint-radial-gradient-x0
  paint-radial-gradient-x0-raw paint-radial-gradient-y0
  paint-radial-gradient-y0-raw paint-radial-gradient-radius0
  paint-radial-gradient-radius0-raw paint-radial-gradient-x1
  paint-radial-gradient-x1-raw paint-radial-gradient-y1
  paint-radial-gradient-y1-raw paint-radial-gradient-radius1
  paint-radial-gradient-radius1-raw))

(defun paint-radial-gradient-format-table (table)
  "Set to 6."
  (declare (type paint-radial-gradient table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-radial-gradient-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-radial-gradient-color-line-offset-offset (table)
  "Offset to ColorLine table."
  (declare (type paint-radial-gradient table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-radial-gradient-color-line-offset (table)
  "Offset to ColorLine table."
  (declare (type paint-radial-gradient table))
  (let ((off (paint-radial-gradient-color-line-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-color-line (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-radial-gradient-x0 (table)
  "Start circle center x coordinate."
  (declare (type paint-radial-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun paint-radial-gradient-x0-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun paint-radial-gradient-y0 (table)
  "Start circle center y coordinate."
  (declare (type paint-radial-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-radial-gradient-y0-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-radial-gradient-radius0 (table)
  "Start circle radius."
  (declare (type paint-radial-gradient table))
  (font-data-read-ufword (table-ref-font-data table)
                         (+ (table-ref-offset table) 8)))

(defun paint-radial-gradient-radius0-raw (font-data offset)
  (font-data-read-ufword font-data (+ offset 8)))

(defun paint-radial-gradient-x1 (table)
  "End circle center x coordinate."
  (declare (type paint-radial-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun paint-radial-gradient-x1-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun paint-radial-gradient-y1 (table)
  "End circle center y coordinate."
  (declare (type paint-radial-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun paint-radial-gradient-y1-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 12)))

(defun paint-radial-gradient-radius1 (table)
  "End circle radius."
  (declare (type paint-radial-gradient table))
  (font-data-read-ufword (table-ref-font-data table)
                         (+ (table-ref-offset table) 14)))

(defun paint-radial-gradient-radius1-raw (font-data offset)
  (font-data-read-ufword font-data (+ offset 14)))

(defun sanitize-paint-radial-gradient (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-paint-radial-gradient
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-radial-gradient font-data offset length)
  (%make-paint-radial-gradient font-data offset length))

(defstruct
    (paint-var-radial-gradient (:include table-ref)
     (:constructor %make-paint-var-radial-gradient
      (%table-font-data %table-offset %table-length)))
  "[PaintVarRadialGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-6-and-7-paintradialgradient-paintvarradialgradient) table")

(declaim
 (inline paint-var-radial-gradient-format-table
  paint-var-radial-gradient-format-table-raw
  paint-var-radial-gradient-color-line-offset-offset
  paint-var-radial-gradient-color-line-offset paint-var-radial-gradient-x0
  paint-var-radial-gradient-x0-raw paint-var-radial-gradient-y0
  paint-var-radial-gradient-y0-raw paint-var-radial-gradient-radius0
  paint-var-radial-gradient-radius0-raw paint-var-radial-gradient-x1
  paint-var-radial-gradient-x1-raw paint-var-radial-gradient-y1
  paint-var-radial-gradient-y1-raw paint-var-radial-gradient-radius1
  paint-var-radial-gradient-radius1-raw
  paint-var-radial-gradient-var-index-base
  paint-var-radial-gradient-var-index-base-raw))

(defun paint-var-radial-gradient-format-table (table)
  "Set to 7."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-radial-gradient-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-radial-gradient-color-line-offset-offset (table)
  "Offset to VarColorLine table."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-radial-gradient-color-line-offset (table)
  "Offset to VarColorLine table."
  (declare (type paint-var-radial-gradient table))
  (let ((off (paint-var-radial-gradient-color-line-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-var-color-line (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-radial-gradient-x0 (table)
  "Start circle center x coordinate. For variation, use varIndexBase + 0."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun paint-var-radial-gradient-x0-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun paint-var-radial-gradient-y0 (table)
  "Start circle center y coordinate. For variation, use varIndexBase + 1."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-var-radial-gradient-y0-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-var-radial-gradient-radius0 (table)
  "Start circle radius. For variation, use varIndexBase + 2."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-ufword (table-ref-font-data table)
                         (+ (table-ref-offset table) 8)))

(defun paint-var-radial-gradient-radius0-raw (font-data offset)
  (font-data-read-ufword font-data (+ offset 8)))

(defun paint-var-radial-gradient-x1 (table)
  "End circle center x coordinate. For variation, use varIndexBase + 3."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun paint-var-radial-gradient-x1-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun paint-var-radial-gradient-y1 (table)
  "End circle center y coordinate. For variation, use varIndexBase + 4."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun paint-var-radial-gradient-y1-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 12)))

(defun paint-var-radial-gradient-radius1 (table)
  "End circle radius. For variation, use varIndexBase + 5."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-ufword (table-ref-font-data table)
                         (+ (table-ref-offset table) 14)))

(defun paint-var-radial-gradient-radius1-raw (font-data offset)
  (font-data-read-ufword font-data (+ offset 14)))

(defun paint-var-radial-gradient-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-radial-gradient table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 16)))

(defun paint-var-radial-gradient-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 16)))

(defun sanitize-paint-var-radial-gradient (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 20 length) (font-data-valid-range-p font-data offset 20))
    (error 'font-out-of-bounds-error :pos offset :size 20 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-radial-gradient
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-radial-gradient font-data offset length)
  (%make-paint-var-radial-gradient font-data offset length))

(defstruct
    (paint-sweep-gradient (:include table-ref)
     (:constructor %make-paint-sweep-gradient
      (%table-font-data %table-offset %table-length)))
  "[PaintSweepGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-8-and-9-paintsweepgradient-paintvarsweepgradient) table")

(declaim
 (inline paint-sweep-gradient-format-table
  paint-sweep-gradient-format-table-raw
  paint-sweep-gradient-color-line-offset-offset
  paint-sweep-gradient-color-line-offset paint-sweep-gradient-center-x
  paint-sweep-gradient-center-x-raw paint-sweep-gradient-center-y
  paint-sweep-gradient-center-y-raw paint-sweep-gradient-start-angle
  paint-sweep-gradient-start-angle-raw paint-sweep-gradient-end-angle
  paint-sweep-gradient-end-angle-raw))

(defun paint-sweep-gradient-format-table (table)
  "Set to 8."
  (declare (type paint-sweep-gradient table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-sweep-gradient-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-sweep-gradient-color-line-offset-offset (table)
  "Offset to ColorLine table."
  (declare (type paint-sweep-gradient table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-sweep-gradient-color-line-offset (table)
  "Offset to ColorLine table."
  (declare (type paint-sweep-gradient table))
  (let ((off (paint-sweep-gradient-color-line-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-color-line (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-sweep-gradient-center-x (table)
  "Center x coordinate."
  (declare (type paint-sweep-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun paint-sweep-gradient-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun paint-sweep-gradient-center-y (table)
  "Center y coordinate."
  (declare (type paint-sweep-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-sweep-gradient-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-sweep-gradient-start-angle (table)
  "Start of the angular range of the gradient, 180° in counter-clockwise degrees per 1.0 of value."
  (declare (type paint-sweep-gradient table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 8)))

(defun paint-sweep-gradient-start-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 8)))

(defun paint-sweep-gradient-end-angle (table)
  "End of the angular range of the gradient, 180° in counter-clockwise degrees per 1.0 of value."
  (declare (type paint-sweep-gradient table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 10)))

(defun paint-sweep-gradient-end-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 10)))

(defun sanitize-paint-sweep-gradient (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-paint-sweep-gradient
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-sweep-gradient font-data offset length)
  (%make-paint-sweep-gradient font-data offset length))

(defstruct
    (paint-var-sweep-gradient (:include table-ref)
     (:constructor %make-paint-var-sweep-gradient
      (%table-font-data %table-offset %table-length)))
  "[PaintVarSweepGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-8-and-9-paintsweepgradient-paintvarsweepgradient) table")

(declaim
 (inline paint-var-sweep-gradient-format-table
  paint-var-sweep-gradient-format-table-raw
  paint-var-sweep-gradient-color-line-offset-offset
  paint-var-sweep-gradient-color-line-offset paint-var-sweep-gradient-center-x
  paint-var-sweep-gradient-center-x-raw paint-var-sweep-gradient-center-y
  paint-var-sweep-gradient-center-y-raw paint-var-sweep-gradient-start-angle
  paint-var-sweep-gradient-start-angle-raw paint-var-sweep-gradient-end-angle
  paint-var-sweep-gradient-end-angle-raw
  paint-var-sweep-gradient-var-index-base
  paint-var-sweep-gradient-var-index-base-raw))

(defun paint-var-sweep-gradient-format-table (table)
  "Set to 9."
  (declare (type paint-var-sweep-gradient table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-sweep-gradient-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-sweep-gradient-color-line-offset-offset (table)
  "Offset to VarColorLine table."
  (declare (type paint-var-sweep-gradient table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-sweep-gradient-color-line-offset (table)
  "Offset to VarColorLine table."
  (declare (type paint-var-sweep-gradient table))
  (let ((off (paint-var-sweep-gradient-color-line-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-var-color-line (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-sweep-gradient-center-x (table)
  "Center x coordinate. For variation, use varIndexBase + 0."
  (declare (type paint-var-sweep-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun paint-var-sweep-gradient-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun paint-var-sweep-gradient-center-y (table)
  "Center y coordinate. For variation, use varIndexBase + 1."
  (declare (type paint-var-sweep-gradient table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-var-sweep-gradient-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-var-sweep-gradient-start-angle (table)
  "Start of the angular range of the gradient, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 2."
  (declare (type paint-var-sweep-gradient table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 8)))

(defun paint-var-sweep-gradient-start-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 8)))

(defun paint-var-sweep-gradient-end-angle (table)
  "End of the angular range of the gradient, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 3."
  (declare (type paint-var-sweep-gradient table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 10)))

(defun paint-var-sweep-gradient-end-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 10)))

(defun paint-var-sweep-gradient-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-sweep-gradient table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun paint-var-sweep-gradient-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 12)))

(defun sanitize-paint-var-sweep-gradient (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-sweep-gradient
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-sweep-gradient font-data offset length)
  (%make-paint-var-sweep-gradient font-data offset length))

(defstruct
    (paint-glyph (:include table-ref)
     (:constructor %make-paint-glyph
      (%table-font-data %table-offset %table-length)))
  "[PaintGlyph](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-10-paintglyph) table")

(declaim
 (inline paint-glyph-format-table paint-glyph-format-table-raw
  paint-glyph-paint-offset-offset paint-glyph-paint-offset paint-glyph-glyph-id
  paint-glyph-glyph-id-raw))

(defun paint-glyph-format-table (table)
  "Set to 10."
  (declare (type paint-glyph table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-glyph-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-glyph-paint-offset-offset (table)
  "Offset to a Paint table."
  (declare (type paint-glyph table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-glyph-paint-offset (table)
  "Offset to a Paint table."
  (declare (type paint-glyph table))
  (let ((off (paint-glyph-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-glyph-glyph-id (table)
  "Glyph ID for the source outline."
  (declare (type paint-glyph table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun paint-glyph-glyph-id-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 4)))

(defun sanitize-paint-glyph (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-paint-glyph
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-glyph font-data offset length)
  (%make-paint-glyph font-data offset length))

(defstruct
    (paint-glyph2 (:include table-ref)
     (:constructor %make-paint-glyph2
      (%table-font-data %table-offset %table-length)))
  "PaintGlyph2, format 33 (ISO/IEC 14496-22:2026).")

(declaim
 (inline paint-glyph2-format-table paint-glyph2-format-table-raw
  paint-glyph2-paint-offset-offset paint-glyph2-paint-offset
  paint-glyph2-glyph-id paint-glyph2-glyph-id-raw))

(defun paint-glyph2-format-table (table)
  "Set to 33."
  (declare (type paint-glyph2 table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-glyph2-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-glyph2-paint-offset-offset (table)
  "Offset to a Paint table."
  (declare (type paint-glyph2 table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-glyph2-paint-offset (table)
  "Offset to a Paint table."
  (declare (type paint-glyph2 table))
  (let ((off (paint-glyph2-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-glyph2-glyph-id (table)
  "Glyph ID for the source outline."
  (declare (type paint-glyph2 table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun paint-glyph2-glyph-id-raw (font-data offset)
  (font-data-read-u24 font-data (+ offset 4)))

(defun sanitize-paint-glyph2 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 7 length) (font-data-valid-range-p font-data offset 7))
    (error 'font-out-of-bounds-error :pos offset :size 7 :length
           (font-data-length font-data)))
  t)

(defun read-paint-glyph2
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-glyph2 font-data offset length)
  (%make-paint-glyph2 font-data offset length))

(defstruct
    (paint-colr-glyph (:include table-ref)
     (:constructor %make-paint-colr-glyph
      (%table-font-data %table-offset %table-length)))
  "[PaintColrGlyph](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-11-paintcolrglyph) table")

(declaim
 (inline paint-colr-glyph-format-table paint-colr-glyph-format-table-raw
  paint-colr-glyph-glyph-id paint-colr-glyph-glyph-id-raw))

(defun paint-colr-glyph-format-table (table)
  "Set to 11."
  (declare (type paint-colr-glyph table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-colr-glyph-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-colr-glyph-glyph-id (table)
  "Glyph ID for a BaseGlyphList base glyph."
  (declare (type paint-colr-glyph table))
  (font-data-read-u16 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-colr-glyph-glyph-id-raw (font-data offset)
  (font-data-read-u16 font-data (+ offset 1)))

(defun sanitize-paint-colr-glyph (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 3 length) (font-data-valid-range-p font-data offset 3))
    (error 'font-out-of-bounds-error :pos offset :size 3 :length
           (font-data-length font-data)))
  t)

(defun read-paint-colr-glyph
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-colr-glyph font-data offset length)
  (%make-paint-colr-glyph font-data offset length))

(defstruct
    (paint-transform (:include table-ref)
     (:constructor %make-paint-transform
      (%table-font-data %table-offset %table-length)))
  "[PaintTransform](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-12-and-13-painttransform-paintvartransform) table")

(declaim
 (inline paint-transform-format-table paint-transform-format-table-raw
  paint-transform-paint-offset-offset paint-transform-paint-offset
  paint-transform-transform-offset-offset paint-transform-transform-offset))

(defun paint-transform-format-table (table)
  "Set to 12."
  (declare (type paint-transform table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-transform-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-transform-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-transform table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-transform-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-transform table))
  (let ((off (paint-transform-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-transform-transform-offset-offset (table)
  "Offset to an Affine2x3 table."
  (declare (type paint-transform table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun paint-transform-transform-offset (table)
  "Offset to an Affine2x3 table."
  (declare (type paint-transform table))
  (let ((off (paint-transform-transform-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-affine2x3 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-paint-transform (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 7 length) (font-data-valid-range-p font-data offset 7))
    (error 'font-out-of-bounds-error :pos offset :size 7 :length
           (font-data-length font-data)))
  t)

(defun read-paint-transform
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-transform font-data offset length)
  (%make-paint-transform font-data offset length))

(defstruct
    (paint-var-transform (:include table-ref)
     (:constructor %make-paint-var-transform
      (%table-font-data %table-offset %table-length)))
  "[PaintVarTransform](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-12-and-13-painttransform-paintvartransform) table")

(declaim
 (inline paint-var-transform-format-table paint-var-transform-format-table-raw
  paint-var-transform-paint-offset-offset paint-var-transform-paint-offset
  paint-var-transform-transform-offset-offset
  paint-var-transform-transform-offset))

(defun paint-var-transform-format-table (table)
  "Set to 13."
  (declare (type paint-var-transform table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-transform-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-transform-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-transform table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-transform-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-transform table))
  (let ((off (paint-var-transform-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-transform-transform-offset-offset (table)
  "Offset to a VarAffine2x3 table."
  (declare (type paint-var-transform table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 4)))

(defun paint-var-transform-transform-offset (table)
  "Offset to a VarAffine2x3 table."
  (declare (type paint-var-transform table))
  (let ((off (paint-var-transform-transform-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-var-affine2x3 (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-paint-var-transform (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 7 length) (font-data-valid-range-p font-data offset 7))
    (error 'font-out-of-bounds-error :pos offset :size 7 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-transform
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-transform font-data offset length)
  (%make-paint-var-transform font-data offset length))

(defstruct
    (affine2x3 (:include table-ref)
     (:constructor %make-affine2x3
      (%table-font-data %table-offset %table-length)))
  "[Affine2x3](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-12-and-13-painttransform-paintvartransform) record")

(declaim
 (inline affine2x3-xx affine2x3-xx-raw affine2x3-yx affine2x3-yx-raw
  affine2x3-xy affine2x3-xy-raw affine2x3-yy affine2x3-yy-raw affine2x3-dx
  affine2x3-dx-raw affine2x3-dy affine2x3-dy-raw))

(defun affine2x3-xx (table)
  "x-component of transformed x-basis vector."
  (declare (type affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table) (table-ref-offset table)))

(defun affine2x3-xx-raw (font-data offset)
  (font-data-read-fixed font-data offset))

(defun affine2x3-yx (table)
  "y-component of transformed x-basis vector."
  (declare (type affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun affine2x3-yx-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 4)))

(defun affine2x3-xy (table)
  "x-component of transformed y-basis vector."
  (declare (type affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun affine2x3-xy-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 8)))

(defun affine2x3-yy (table)
  "y-component of transformed y-basis vector."
  (declare (type affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun affine2x3-yy-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 12)))

(defun affine2x3-dx (table)
  "Translation in x direction."
  (declare (type affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 16)))

(defun affine2x3-dx-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 16)))

(defun affine2x3-dy (table)
  "Translation in y direction."
  (declare (type affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 20)))

(defun affine2x3-dy-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 20)))

(defun sanitize-affine2x3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 24 length) (font-data-valid-range-p font-data offset 24))
    (error 'font-out-of-bounds-error :pos offset :size 24 :length
           (font-data-length font-data)))
  t)

(defun read-affine2x3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-affine2x3 font-data offset length)
  (%make-affine2x3 font-data offset length))

(defstruct
    (var-affine2x3 (:include table-ref)
     (:constructor %make-var-affine2x3
      (%table-font-data %table-offset %table-length)))
  "[VarAffine2x3](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-12-and-13-painttransform-paintvartransform) record")

(declaim
 (inline var-affine2x3-xx var-affine2x3-xx-raw var-affine2x3-yx
  var-affine2x3-yx-raw var-affine2x3-xy var-affine2x3-xy-raw var-affine2x3-yy
  var-affine2x3-yy-raw var-affine2x3-dx var-affine2x3-dx-raw var-affine2x3-dy
  var-affine2x3-dy-raw var-affine2x3-var-index-base
  var-affine2x3-var-index-base-raw))

(defun var-affine2x3-xx (table)
  "x-component of transformed x-basis vector. For variation, use varIndexBase + 0."
  (declare (type var-affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table) (table-ref-offset table)))

(defun var-affine2x3-xx-raw (font-data offset)
  (font-data-read-fixed font-data offset))

(defun var-affine2x3-yx (table)
  "y-component of transformed x-basis vector. For variation, use varIndexBase + 1."
  (declare (type var-affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun var-affine2x3-yx-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 4)))

(defun var-affine2x3-xy (table)
  "x-component of transformed y-basis vector. For variation, use varIndexBase + 2."
  (declare (type var-affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun var-affine2x3-xy-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 8)))

(defun var-affine2x3-yy (table)
  "y-component of transformed y-basis vector. For variation, use varIndexBase + 3."
  (declare (type var-affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 12)))

(defun var-affine2x3-yy-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 12)))

(defun var-affine2x3-dx (table)
  "Translation in x direction. For variation, use varIndexBase + 4."
  (declare (type var-affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 16)))

(defun var-affine2x3-dx-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 16)))

(defun var-affine2x3-dy (table)
  "Translation in y direction. For variation, use varIndexBase + 5."
  (declare (type var-affine2x3 table))
  (font-data-read-fixed (table-ref-font-data table)
                        (+ (table-ref-offset table) 20)))

(defun var-affine2x3-dy-raw (font-data offset)
  (font-data-read-fixed font-data (+ offset 20)))

(defun var-affine2x3-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type var-affine2x3 table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 24)))

(defun var-affine2x3-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 24)))

(defun sanitize-var-affine2x3 (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 28 length) (font-data-valid-range-p font-data offset 28))
    (error 'font-out-of-bounds-error :pos offset :size 28 :length
           (font-data-length font-data)))
  t)

(defun read-var-affine2x3
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-var-affine2x3 font-data offset length)
  (%make-var-affine2x3 font-data offset length))

(defstruct
    (paint-translate (:include table-ref)
     (:constructor %make-paint-translate
      (%table-font-data %table-offset %table-length)))
  "[PaintTranslate](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-14-and-15-painttranslate-paintvartranslate) table")

(declaim
 (inline paint-translate-format-table paint-translate-format-table-raw
  paint-translate-paint-offset-offset paint-translate-paint-offset
  paint-translate-dx paint-translate-dx-raw paint-translate-dy
  paint-translate-dy-raw))

(defun paint-translate-format-table (table)
  "Set to 14."
  (declare (type paint-translate table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-translate-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-translate-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-translate table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-translate-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-translate table))
  (let ((off (paint-translate-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-translate-dx (table)
  "Translation in x direction."
  (declare (type paint-translate table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun paint-translate-dx-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun paint-translate-dy (table)
  "Translation in y direction."
  (declare (type paint-translate table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-translate-dy-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun sanitize-paint-translate (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-paint-translate
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-translate font-data offset length)
  (%make-paint-translate font-data offset length))

(defstruct
    (paint-var-translate (:include table-ref)
     (:constructor %make-paint-var-translate
      (%table-font-data %table-offset %table-length)))
  "[PaintVarTranslate](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-14-and-15-painttranslate-paintvartranslate) table")

(declaim
 (inline paint-var-translate-format-table paint-var-translate-format-table-raw
  paint-var-translate-paint-offset-offset paint-var-translate-paint-offset
  paint-var-translate-dx paint-var-translate-dx-raw paint-var-translate-dy
  paint-var-translate-dy-raw paint-var-translate-var-index-base
  paint-var-translate-var-index-base-raw))

(defun paint-var-translate-format-table (table)
  "Set to 15."
  (declare (type paint-var-translate table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-translate-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-translate-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-translate table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-translate-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-translate table))
  (let ((off (paint-var-translate-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-translate-dx (table)
  "Translation in x direction. For variation, use varIndexBase + 0."
  (declare (type paint-var-translate table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 4)))

(defun paint-var-translate-dx-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 4)))

(defun paint-var-translate-dy (table)
  "Translation in y direction. For variation, use varIndexBase + 1."
  (declare (type paint-var-translate table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-var-translate-dy-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-var-translate-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-translate table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun paint-var-translate-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun sanitize-paint-var-translate (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-translate
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-translate font-data offset length)
  (%make-paint-var-translate font-data offset length))

(defstruct
    (paint-scale (:include table-ref)
     (:constructor %make-paint-scale
      (%table-font-data %table-offset %table-length)))
  "[PaintScale](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")

(declaim
 (inline paint-scale-format-table paint-scale-format-table-raw
  paint-scale-paint-offset-offset paint-scale-paint-offset paint-scale-scale-x
  paint-scale-scale-x-raw paint-scale-scale-y paint-scale-scale-y-raw))

(defun paint-scale-format-table (table)
  "Set to 16."
  (declare (type paint-scale table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-scale-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-scale-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-scale table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-scale-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-scale table))
  (let ((off (paint-scale-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-scale-scale-x (table)
  "Scale factor in x direction."
  (declare (type paint-scale table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-scale-scale-x-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-scale-scale-y (table)
  "Scale factor in y direction."
  (declare (type paint-scale table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun paint-scale-scale-y-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun sanitize-paint-scale (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-paint-scale
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-scale font-data offset length)
  (%make-paint-scale font-data offset length))

(defstruct
    (paint-var-scale (:include table-ref)
     (:constructor %make-paint-var-scale
      (%table-font-data %table-offset %table-length)))
  "[PaintVarScale](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")

(declaim
 (inline paint-var-scale-format-table paint-var-scale-format-table-raw
  paint-var-scale-paint-offset-offset paint-var-scale-paint-offset
  paint-var-scale-scale-x paint-var-scale-scale-x-raw paint-var-scale-scale-y
  paint-var-scale-scale-y-raw paint-var-scale-var-index-base
  paint-var-scale-var-index-base-raw))

(defun paint-var-scale-format-table (table)
  "Set to 17."
  (declare (type paint-var-scale table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-scale-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-scale-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-scale table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-scale-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-scale table))
  (let ((off (paint-var-scale-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-scale-scale-x (table)
  "Scale factor in x direction. For variation, use varIndexBase + 0."
  (declare (type paint-var-scale table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-var-scale-scale-x-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-var-scale-scale-y (table)
  "Scale factor in y direction. For variation, use varIndexBase + 1."
  (declare (type paint-var-scale table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun paint-var-scale-scale-y-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun paint-var-scale-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-scale table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun paint-var-scale-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun sanitize-paint-var-scale (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-scale
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-scale font-data offset length)
  (%make-paint-var-scale font-data offset length))

(defstruct
    (paint-scale-around-center (:include table-ref)
     (:constructor %make-paint-scale-around-center
      (%table-font-data %table-offset %table-length)))
  "[PaintScaleAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")

(declaim
 (inline paint-scale-around-center-format-table
  paint-scale-around-center-format-table-raw
  paint-scale-around-center-paint-offset-offset
  paint-scale-around-center-paint-offset paint-scale-around-center-scale-x
  paint-scale-around-center-scale-x-raw paint-scale-around-center-scale-y
  paint-scale-around-center-scale-y-raw paint-scale-around-center-center-x
  paint-scale-around-center-center-x-raw paint-scale-around-center-center-y
  paint-scale-around-center-center-y-raw))

(defun paint-scale-around-center-format-table (table)
  "Set to 18."
  (declare (type paint-scale-around-center table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-scale-around-center-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-scale-around-center-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-scale-around-center table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-scale-around-center-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-scale-around-center table))
  (let ((off (paint-scale-around-center-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-scale-around-center-scale-x (table)
  "Scale factor in x direction."
  (declare (type paint-scale-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-scale-around-center-scale-x-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-scale-around-center-scale-y (table)
  "Scale factor in y direction."
  (declare (type paint-scale-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun paint-scale-around-center-scale-y-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun paint-scale-around-center-center-x (table)
  "x coordinate for the center of scaling."
  (declare (type paint-scale-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-scale-around-center-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun paint-scale-around-center-center-y (table)
  "y coordinate for the center of scaling."
  (declare (type paint-scale-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun paint-scale-around-center-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun sanitize-paint-scale-around-center (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-paint-scale-around-center
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-scale-around-center font-data offset length)
  (%make-paint-scale-around-center font-data offset length))

(defstruct
    (paint-var-scale-around-center (:include table-ref)
     (:constructor %make-paint-var-scale-around-center
      (%table-font-data %table-offset %table-length)))
  "[PaintVarScaleAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")

(declaim
 (inline paint-var-scale-around-center-format-table
  paint-var-scale-around-center-format-table-raw
  paint-var-scale-around-center-paint-offset-offset
  paint-var-scale-around-center-paint-offset
  paint-var-scale-around-center-scale-x
  paint-var-scale-around-center-scale-x-raw
  paint-var-scale-around-center-scale-y
  paint-var-scale-around-center-scale-y-raw
  paint-var-scale-around-center-center-x
  paint-var-scale-around-center-center-x-raw
  paint-var-scale-around-center-center-y
  paint-var-scale-around-center-center-y-raw
  paint-var-scale-around-center-var-index-base
  paint-var-scale-around-center-var-index-base-raw))

(defun paint-var-scale-around-center-format-table (table)
  "Set to 19."
  (declare (type paint-var-scale-around-center table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-scale-around-center-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-scale-around-center-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-scale-around-center table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-scale-around-center-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-scale-around-center table))
  (let ((off (paint-var-scale-around-center-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-scale-around-center-scale-x (table)
  "Scale factor in x direction. For variation, use varIndexBase + 0."
  (declare (type paint-var-scale-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-var-scale-around-center-scale-x-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-var-scale-around-center-scale-y (table)
  "Scale factor in y direction. For variation, use varIndexBase + 1."
  (declare (type paint-var-scale-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun paint-var-scale-around-center-scale-y-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun paint-var-scale-around-center-center-x (table)
  "x coordinate for the center of scaling. For variation, use varIndexBase + 2."
  (declare (type paint-var-scale-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-var-scale-around-center-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun paint-var-scale-around-center-center-y (table)
  "y coordinate for the center of scaling. For variation, use varIndexBase + 3."
  (declare (type paint-var-scale-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun paint-var-scale-around-center-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun paint-var-scale-around-center-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-scale-around-center table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun paint-var-scale-around-center-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 12)))

(defun sanitize-paint-var-scale-around-center (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-scale-around-center
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-scale-around-center font-data offset length)
  (%make-paint-var-scale-around-center font-data offset length))

(defstruct
    (paint-scale-uniform (:include table-ref)
     (:constructor %make-paint-scale-uniform
      (%table-font-data %table-offset %table-length)))
  "[PaintScaleUniform](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")

(declaim
 (inline paint-scale-uniform-format-table paint-scale-uniform-format-table-raw
  paint-scale-uniform-paint-offset-offset paint-scale-uniform-paint-offset
  paint-scale-uniform-scale paint-scale-uniform-scale-raw))

(defun paint-scale-uniform-format-table (table)
  "Set to 20."
  (declare (type paint-scale-uniform table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-scale-uniform-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-scale-uniform-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-scale-uniform table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-scale-uniform-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-scale-uniform table))
  (let ((off (paint-scale-uniform-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-scale-uniform-scale (table)
  "Scale factor in x and y directions."
  (declare (type paint-scale-uniform table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-scale-uniform-scale-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun sanitize-paint-scale-uniform (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-paint-scale-uniform
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-scale-uniform font-data offset length)
  (%make-paint-scale-uniform font-data offset length))

(defstruct
    (paint-var-scale-uniform (:include table-ref)
     (:constructor %make-paint-var-scale-uniform
      (%table-font-data %table-offset %table-length)))
  "[PaintVarScaleUniform](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")

(declaim
 (inline paint-var-scale-uniform-format-table
  paint-var-scale-uniform-format-table-raw
  paint-var-scale-uniform-paint-offset-offset
  paint-var-scale-uniform-paint-offset paint-var-scale-uniform-scale
  paint-var-scale-uniform-scale-raw paint-var-scale-uniform-var-index-base
  paint-var-scale-uniform-var-index-base-raw))

(defun paint-var-scale-uniform-format-table (table)
  "Set to 21."
  (declare (type paint-var-scale-uniform table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-scale-uniform-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-scale-uniform-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-scale-uniform table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-scale-uniform-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-scale-uniform table))
  (let ((off (paint-var-scale-uniform-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-scale-uniform-scale (table)
  "Scale factor in x and y directions. For variation, use varIndexBase + 0."
  (declare (type paint-var-scale-uniform table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-var-scale-uniform-scale-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-var-scale-uniform-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-scale-uniform table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun paint-var-scale-uniform-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 6)))

(defun sanitize-paint-var-scale-uniform (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-scale-uniform
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-scale-uniform font-data offset length)
  (%make-paint-var-scale-uniform font-data offset length))

(defstruct
    (paint-scale-uniform-around-center (:include table-ref)
     (:constructor %make-paint-scale-uniform-around-center
      (%table-font-data %table-offset %table-length)))
  "[PaintScaleUniformAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")

(declaim
 (inline paint-scale-uniform-around-center-format-table
  paint-scale-uniform-around-center-format-table-raw
  paint-scale-uniform-around-center-paint-offset-offset
  paint-scale-uniform-around-center-paint-offset
  paint-scale-uniform-around-center-scale
  paint-scale-uniform-around-center-scale-raw
  paint-scale-uniform-around-center-center-x
  paint-scale-uniform-around-center-center-x-raw
  paint-scale-uniform-around-center-center-y
  paint-scale-uniform-around-center-center-y-raw))

(defun paint-scale-uniform-around-center-format-table (table)
  "Set to 22."
  (declare (type paint-scale-uniform-around-center table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-scale-uniform-around-center-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-scale-uniform-around-center-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-scale-uniform-around-center table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-scale-uniform-around-center-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-scale-uniform-around-center table))
  (let ((off (paint-scale-uniform-around-center-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-scale-uniform-around-center-scale (table)
  "Scale factor in x and y directions."
  (declare (type paint-scale-uniform-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-scale-uniform-around-center-scale-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-scale-uniform-around-center-center-x (table)
  "x coordinate for the center of scaling."
  (declare (type paint-scale-uniform-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-scale-uniform-around-center-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-scale-uniform-around-center-center-y (table)
  "y coordinate for the center of scaling."
  (declare (type paint-scale-uniform-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-scale-uniform-around-center-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun sanitize-paint-scale-uniform-around-center (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  t)

(defun read-paint-scale-uniform-around-center
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-scale-uniform-around-center font-data offset length)
  (%make-paint-scale-uniform-around-center font-data offset length))

(defstruct
    (paint-var-scale-uniform-around-center (:include table-ref)
     (:constructor %make-paint-var-scale-uniform-around-center
      (%table-font-data %table-offset %table-length)))
  "[PaintVarScaleUniformAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")

(declaim
 (inline paint-var-scale-uniform-around-center-format-table
  paint-var-scale-uniform-around-center-format-table-raw
  paint-var-scale-uniform-around-center-paint-offset-offset
  paint-var-scale-uniform-around-center-paint-offset
  paint-var-scale-uniform-around-center-scale
  paint-var-scale-uniform-around-center-scale-raw
  paint-var-scale-uniform-around-center-center-x
  paint-var-scale-uniform-around-center-center-x-raw
  paint-var-scale-uniform-around-center-center-y
  paint-var-scale-uniform-around-center-center-y-raw
  paint-var-scale-uniform-around-center-var-index-base
  paint-var-scale-uniform-around-center-var-index-base-raw))

(defun paint-var-scale-uniform-around-center-format-table (table)
  "Set to 23."
  (declare (type paint-var-scale-uniform-around-center table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-scale-uniform-around-center-format-table-raw
       (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-scale-uniform-around-center-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-scale-uniform-around-center table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-scale-uniform-around-center-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-scale-uniform-around-center table))
  (let ((off (paint-var-scale-uniform-around-center-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-scale-uniform-around-center-scale (table)
  "Scale factor in x and y directions. For variation, use varIndexBase + 0."
  (declare (type paint-var-scale-uniform-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-var-scale-uniform-around-center-scale-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-var-scale-uniform-around-center-center-x (table)
  "x coordinate for the center of scaling. For variation, use varIndexBase + 1."
  (declare (type paint-var-scale-uniform-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-var-scale-uniform-around-center-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-var-scale-uniform-around-center-center-y (table)
  "y coordinate for the center of scaling. For variation, use varIndexBase + 2."
  (declare (type paint-var-scale-uniform-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-var-scale-uniform-around-center-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun paint-var-scale-uniform-around-center-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-scale-uniform-around-center table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun paint-var-scale-uniform-around-center-var-index-base-raw
       (font-data offset)
  (font-data-read-u32 font-data (+ offset 10)))

(defun sanitize-paint-var-scale-uniform-around-center (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 14 length) (font-data-valid-range-p font-data offset 14))
    (error 'font-out-of-bounds-error :pos offset :size 14 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-scale-uniform-around-center
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-scale-uniform-around-center font-data offset length)
  (%make-paint-var-scale-uniform-around-center font-data offset length))

(defstruct
    (paint-rotate (:include table-ref)
     (:constructor %make-paint-rotate
      (%table-font-data %table-offset %table-length)))
  "[PaintRotate](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-24-to-27-paintrotate-paintvarrotate-paintrotatearoundcenter-paintvarrotatearoundcenter) table")

(declaim
 (inline paint-rotate-format-table paint-rotate-format-table-raw
  paint-rotate-paint-offset-offset paint-rotate-paint-offset paint-rotate-angle
  paint-rotate-angle-raw))

(defun paint-rotate-format-table (table)
  "Set to 24."
  (declare (type paint-rotate table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-rotate-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-rotate-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-rotate table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-rotate-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-rotate table))
  (let ((off (paint-rotate-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-rotate-angle (table)
  "Rotation angle, 180° in counter-clockwise degrees per 1.0 of value."
  (declare (type paint-rotate table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-rotate-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun sanitize-paint-rotate (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 6 length) (font-data-valid-range-p font-data offset 6))
    (error 'font-out-of-bounds-error :pos offset :size 6 :length
           (font-data-length font-data)))
  t)

(defun read-paint-rotate
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-rotate font-data offset length)
  (%make-paint-rotate font-data offset length))

(defstruct
    (paint-var-rotate (:include table-ref)
     (:constructor %make-paint-var-rotate
      (%table-font-data %table-offset %table-length)))
  "[PaintVarRotate](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-24-to-27-paintrotate-paintvarrotate-paintrotatearoundcenter-paintvarrotatearoundcenter) table")

(declaim
 (inline paint-var-rotate-format-table paint-var-rotate-format-table-raw
  paint-var-rotate-paint-offset-offset paint-var-rotate-paint-offset
  paint-var-rotate-angle paint-var-rotate-angle-raw
  paint-var-rotate-var-index-base paint-var-rotate-var-index-base-raw))

(defun paint-var-rotate-format-table (table)
  "Set to 25."
  (declare (type paint-var-rotate table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-rotate-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-rotate-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-rotate table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-rotate-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-rotate table))
  (let ((off (paint-var-rotate-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-rotate-angle (table)
  "Rotation angle, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 0."
  (declare (type paint-var-rotate table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-var-rotate-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-var-rotate-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-rotate table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 6)))

(defun paint-var-rotate-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 6)))

(defun sanitize-paint-var-rotate (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-rotate
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-rotate font-data offset length)
  (%make-paint-var-rotate font-data offset length))

(defstruct
    (paint-rotate-around-center (:include table-ref)
     (:constructor %make-paint-rotate-around-center
      (%table-font-data %table-offset %table-length)))
  "[PaintRotateAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-24-to-27-paintrotate-paintvarrotate-paintrotatearoundcenter-paintvarrotatearoundcenter) table")

(declaim
 (inline paint-rotate-around-center-format-table
  paint-rotate-around-center-format-table-raw
  paint-rotate-around-center-paint-offset-offset
  paint-rotate-around-center-paint-offset paint-rotate-around-center-angle
  paint-rotate-around-center-angle-raw paint-rotate-around-center-center-x
  paint-rotate-around-center-center-x-raw paint-rotate-around-center-center-y
  paint-rotate-around-center-center-y-raw))

(defun paint-rotate-around-center-format-table (table)
  "Set to 26."
  (declare (type paint-rotate-around-center table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-rotate-around-center-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-rotate-around-center-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-rotate-around-center table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-rotate-around-center-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-rotate-around-center table))
  (let ((off (paint-rotate-around-center-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-rotate-around-center-angle (table)
  "Rotation angle, 180° in counter-clockwise degrees per 1.0 of value."
  (declare (type paint-rotate-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-rotate-around-center-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-rotate-around-center-center-x (table)
  "x coordinate for the center of rotation."
  (declare (type paint-rotate-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-rotate-around-center-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-rotate-around-center-center-y (table)
  "y coordinate for the center of rotation."
  (declare (type paint-rotate-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-rotate-around-center-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun sanitize-paint-rotate-around-center (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 10 length) (font-data-valid-range-p font-data offset 10))
    (error 'font-out-of-bounds-error :pos offset :size 10 :length
           (font-data-length font-data)))
  t)

(defun read-paint-rotate-around-center
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-rotate-around-center font-data offset length)
  (%make-paint-rotate-around-center font-data offset length))

(defstruct
    (paint-var-rotate-around-center (:include table-ref)
     (:constructor %make-paint-var-rotate-around-center
      (%table-font-data %table-offset %table-length)))
  "[PaintVarRotateAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-24-to-27-paintrotate-paintvarrotate-paintrotatearoundcenter-paintvarrotatearoundcenter) table")

(declaim
 (inline paint-var-rotate-around-center-format-table
  paint-var-rotate-around-center-format-table-raw
  paint-var-rotate-around-center-paint-offset-offset
  paint-var-rotate-around-center-paint-offset
  paint-var-rotate-around-center-angle paint-var-rotate-around-center-angle-raw
  paint-var-rotate-around-center-center-x
  paint-var-rotate-around-center-center-x-raw
  paint-var-rotate-around-center-center-y
  paint-var-rotate-around-center-center-y-raw
  paint-var-rotate-around-center-var-index-base
  paint-var-rotate-around-center-var-index-base-raw))

(defun paint-var-rotate-around-center-format-table (table)
  "Set to 27."
  (declare (type paint-var-rotate-around-center table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-rotate-around-center-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-rotate-around-center-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-rotate-around-center table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-rotate-around-center-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-rotate-around-center table))
  (let ((off (paint-var-rotate-around-center-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-rotate-around-center-angle (table)
  "Rotation angle, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 0."
  (declare (type paint-var-rotate-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-var-rotate-around-center-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-var-rotate-around-center-center-x (table)
  "x coordinate for the center of rotation. For variation, use varIndexBase + 1."
  (declare (type paint-var-rotate-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 6)))

(defun paint-var-rotate-around-center-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 6)))

(defun paint-var-rotate-around-center-center-y (table)
  "y coordinate for the center of rotation. For variation, use varIndexBase + 2."
  (declare (type paint-var-rotate-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-var-rotate-around-center-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun paint-var-rotate-around-center-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-rotate-around-center table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 10)))

(defun paint-var-rotate-around-center-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 10)))

(defun sanitize-paint-var-rotate-around-center (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 14 length) (font-data-valid-range-p font-data offset 14))
    (error 'font-out-of-bounds-error :pos offset :size 14 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-rotate-around-center
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-rotate-around-center font-data offset length)
  (%make-paint-var-rotate-around-center font-data offset length))

(defstruct
    (paint-skew (:include table-ref)
     (:constructor %make-paint-skew
      (%table-font-data %table-offset %table-length)))
  "[PaintSkew](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-28-to-31-paintskew-paintvarskew-paintskewaroundcenter-paintvarskewaroundcenter) table")

(declaim
 (inline paint-skew-format-table paint-skew-format-table-raw
  paint-skew-paint-offset-offset paint-skew-paint-offset
  paint-skew-x-skew-angle paint-skew-x-skew-angle-raw paint-skew-y-skew-angle
  paint-skew-y-skew-angle-raw))

(defun paint-skew-format-table (table)
  "Set to 28."
  (declare (type paint-skew table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-skew-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-skew-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-skew table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-skew-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-skew table))
  (let ((off (paint-skew-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-skew-x-skew-angle (table)
  "Angle of skew in the direction of the x-axis, 180° in counter-clockwise degrees per 1.0 of value."
  (declare (type paint-skew table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-skew-x-skew-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-skew-y-skew-angle (table)
  "Angle of skew in the direction of the y-axis, 180° in counter-clockwise degrees per 1.0 of value."
  (declare (type paint-skew table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun paint-skew-y-skew-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun sanitize-paint-skew (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-paint-skew
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-skew font-data offset length)
  (%make-paint-skew font-data offset length))

(defstruct
    (paint-var-skew (:include table-ref)
     (:constructor %make-paint-var-skew
      (%table-font-data %table-offset %table-length)))
  "[PaintVarSkew](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-28-to-31-paintskew-paintvarskew-paintskewaroundcenter-paintvarskewaroundcenter) table")

(declaim
 (inline paint-var-skew-format-table paint-var-skew-format-table-raw
  paint-var-skew-paint-offset-offset paint-var-skew-paint-offset
  paint-var-skew-x-skew-angle paint-var-skew-x-skew-angle-raw
  paint-var-skew-y-skew-angle paint-var-skew-y-skew-angle-raw
  paint-var-skew-var-index-base paint-var-skew-var-index-base-raw))

(defun paint-var-skew-format-table (table)
  "Set to 29."
  (declare (type paint-var-skew table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-skew-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-skew-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-skew table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-skew-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-skew table))
  (let ((off (paint-var-skew-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-skew-x-skew-angle (table)
  "Angle of skew in the direction of the x-axis, 180° ┬░ in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 0."
  (declare (type paint-var-skew table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-var-skew-x-skew-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-var-skew-y-skew-angle (table)
  "Angle of skew in the direction of the y-axis, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 1."
  (declare (type paint-var-skew table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun paint-var-skew-y-skew-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun paint-var-skew-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-skew table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 8)))

(defun paint-var-skew-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 8)))

(defun sanitize-paint-var-skew (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-skew
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-skew font-data offset length)
  (%make-paint-var-skew font-data offset length))

(defstruct
    (paint-skew-around-center (:include table-ref)
     (:constructor %make-paint-skew-around-center
      (%table-font-data %table-offset %table-length)))
  "[PaintSkewAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-28-to-31-paintskew-paintvarskew-paintskewaroundcenter-paintvarskewaroundcenter) table")

(declaim
 (inline paint-skew-around-center-format-table
  paint-skew-around-center-format-table-raw
  paint-skew-around-center-paint-offset-offset
  paint-skew-around-center-paint-offset paint-skew-around-center-x-skew-angle
  paint-skew-around-center-x-skew-angle-raw
  paint-skew-around-center-y-skew-angle
  paint-skew-around-center-y-skew-angle-raw paint-skew-around-center-center-x
  paint-skew-around-center-center-x-raw paint-skew-around-center-center-y
  paint-skew-around-center-center-y-raw))

(defun paint-skew-around-center-format-table (table)
  "Set to 30."
  (declare (type paint-skew-around-center table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-skew-around-center-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-skew-around-center-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-skew-around-center table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-skew-around-center-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-skew-around-center table))
  (let ((off (paint-skew-around-center-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-skew-around-center-x-skew-angle (table)
  "Angle of skew in the direction of the x-axis, 180° in counter-clockwise degrees per 1.0 of value."
  (declare (type paint-skew-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-skew-around-center-x-skew-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-skew-around-center-y-skew-angle (table)
  "Angle of skew in the direction of the y-axis, 180° in counter-clockwise degrees per 1.0 of value."
  (declare (type paint-skew-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun paint-skew-around-center-y-skew-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun paint-skew-around-center-center-x (table)
  "x coordinate for the center of rotation."
  (declare (type paint-skew-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-skew-around-center-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun paint-skew-around-center-center-y (table)
  "y coordinate for the center of rotation."
  (declare (type paint-skew-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun paint-skew-around-center-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun sanitize-paint-skew-around-center (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 12 length) (font-data-valid-range-p font-data offset 12))
    (error 'font-out-of-bounds-error :pos offset :size 12 :length
           (font-data-length font-data)))
  t)

(defun read-paint-skew-around-center
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-skew-around-center font-data offset length)
  (%make-paint-skew-around-center font-data offset length))

(defstruct
    (paint-var-skew-around-center (:include table-ref)
     (:constructor %make-paint-var-skew-around-center
      (%table-font-data %table-offset %table-length)))
  "[PaintVarSkewAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-28-to-31-paintskew-paintvarskew-paintskewaroundcenter-paintvarskewaroundcenter) table")

(declaim
 (inline paint-var-skew-around-center-format-table
  paint-var-skew-around-center-format-table-raw
  paint-var-skew-around-center-paint-offset-offset
  paint-var-skew-around-center-paint-offset
  paint-var-skew-around-center-x-skew-angle
  paint-var-skew-around-center-x-skew-angle-raw
  paint-var-skew-around-center-y-skew-angle
  paint-var-skew-around-center-y-skew-angle-raw
  paint-var-skew-around-center-center-x
  paint-var-skew-around-center-center-x-raw
  paint-var-skew-around-center-center-y
  paint-var-skew-around-center-center-y-raw
  paint-var-skew-around-center-var-index-base
  paint-var-skew-around-center-var-index-base-raw))

(defun paint-var-skew-around-center-format-table (table)
  "Set to 31."
  (declare (type paint-var-skew-around-center table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-var-skew-around-center-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-var-skew-around-center-paint-offset-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-skew-around-center table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-var-skew-around-center-paint-offset (table)
  "Offset to a Paint subtable."
  (declare (type paint-var-skew-around-center table))
  (let ((off (paint-var-skew-around-center-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-var-skew-around-center-x-skew-angle (table)
  "Angle of skew in the direction of the x-axis, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 0."
  (declare (type paint-var-skew-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 4)))

(defun paint-var-skew-around-center-x-skew-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 4)))

(defun paint-var-skew-around-center-y-skew-angle (table)
  "Angle of skew in the direction of the y-axis, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 1."
  (declare (type paint-var-skew-around-center table))
  (font-data-read-f2dot14 (table-ref-font-data table)
                          (+ (table-ref-offset table) 6)))

(defun paint-var-skew-around-center-y-skew-angle-raw (font-data offset)
  (font-data-read-f2dot14 font-data (+ offset 6)))

(defun paint-var-skew-around-center-center-x (table)
  "x coordinate for the center of rotation. For variation, use varIndexBase + 2."
  (declare (type paint-var-skew-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 8)))

(defun paint-var-skew-around-center-center-x-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 8)))

(defun paint-var-skew-around-center-center-y (table)
  "y coordinate for the center of rotation. For variation, use varIndexBase + 3."
  (declare (type paint-var-skew-around-center table))
  (font-data-read-fword (table-ref-font-data table)
                        (+ (table-ref-offset table) 10)))

(defun paint-var-skew-around-center-center-y-raw (font-data offset)
  (font-data-read-fword font-data (+ offset 10)))

(defun paint-var-skew-around-center-var-index-base (table)
  "Base index into DeltaSetIndexMap."
  (declare (type paint-var-skew-around-center table))
  (font-data-read-u32 (table-ref-font-data table)
                      (+ (table-ref-offset table) 12)))

(defun paint-var-skew-around-center-var-index-base-raw (font-data offset)
  (font-data-read-u32 font-data (+ offset 12)))

(defun sanitize-paint-var-skew-around-center (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 16 length) (font-data-valid-range-p font-data offset 16))
    (error 'font-out-of-bounds-error :pos offset :size 16 :length
           (font-data-length font-data)))
  t)

(defun read-paint-var-skew-around-center
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-var-skew-around-center font-data offset length)
  (%make-paint-var-skew-around-center font-data offset length))

(defstruct
    (paint-composite (:include table-ref)
     (:constructor %make-paint-composite
      (%table-font-data %table-offset %table-length)))
  "[PaintComposite](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) table")

(declaim
 (inline paint-composite-format-table paint-composite-format-table-raw
  paint-composite-source-paint-offset-offset
  paint-composite-source-paint-offset paint-composite-composite-mode
  paint-composite-composite-mode-raw
  paint-composite-backdrop-paint-offset-offset
  paint-composite-backdrop-paint-offset))

(defun paint-composite-format-table (table)
  "Set to 32."
  (declare (type paint-composite table))
  (font-data-read-u8 (table-ref-font-data table) (table-ref-offset table)))

(defun paint-composite-format-table-raw (font-data offset)
  (font-data-read-u8 font-data offset))

(defun paint-composite-source-paint-offset-offset (table)
  "Offset to a source Paint table."
  (declare (type paint-composite table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 1)))

(defun paint-composite-source-paint-offset (table)
  "Offset to a source Paint table."
  (declare (type paint-composite table))
  (let ((off (paint-composite-source-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun paint-composite-composite-mode (table)
  "A CompositeMode enumeration value."
  (declare (type paint-composite table))
  (font-data-read-u8 (table-ref-font-data table)
                     (+ (table-ref-offset table) 4)))

(defun paint-composite-composite-mode-raw (font-data offset)
  (font-data-read-u8 font-data (+ offset 4)))

(defun paint-composite-backdrop-paint-offset-offset (table)
  "Offset to a backdrop Paint table."
  (declare (type paint-composite table))
  (font-data-read-u24 (table-ref-font-data table)
                      (+ (table-ref-offset table) 5)))

(defun paint-composite-backdrop-paint-offset (table)
  "Offset to a backdrop Paint table."
  (declare (type paint-composite table))
  (let ((off (paint-composite-backdrop-paint-offset-offset table)))
    (when (and off (not (zerop off)))
      (read-paint (table-ref-font-data table)
       (+ (table-ref-offset table) off)))))

(defun sanitize-paint-composite (font-data offset length)
  (declare (ignorable length)
           (type font-data font-data)
           (type (unsigned-byte 32) offset length))
  (unless (and (<= 8 length) (font-data-valid-range-p font-data offset 8))
    (error 'font-out-of-bounds-error :pos offset :size 8 :length
           (font-data-length font-data)))
  t)

(defun read-paint-composite
       (font-data offset
        &optional (length (- (font-data-length font-data) offset)))
  (sanitize-paint-composite font-data offset length)
  (%make-paint-composite font-data offset length))
