;;;; glyf.sexp — OpenType table schema
;;;; Generated from Fontations glyf.rs spec


(table glyf (:tag "glyf")
 (:doc
  "The [glyf (Glyph Data)](https://docs.microsoft.com/en-us/typography/opentype/spec/glyf) table"))


(table simple-glyph
 (:doc
  "The [Glyph Header](https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#glyph-headers)")
 (field number-of-contours i16 :doc
  "If the number of contours is greater than or equal to zero, this is a simple glyph. If negative, this is a composite glyph — the value -1 should be used for composite glyphs.")
 (field x-min i16 :doc "Minimum x for coordinate data.")
 (field y-min i16 :doc "Minimum y for coordinate data.")
 (field x-max i16 :doc "Maximum x for coordinate data.")
 (field y-max i16 :doc "Maximum y for coordinate data.")
 (field end-pts-of-contours (array u16) :count number-of-contours :doc
  "Array of point indices for the last point of each contour, in increasing numeric order")
 (field instruction-length u16 :doc
  "Total number of bytes for instructions. If instructionLength is zero, no instructions are present for this glyph, and this field is followed directly by the flags field.")
 (field instructions (array u8) :count instruction-length :doc
  "Array of instruction byte code for the glyph.")
 (field glyph-data (array u8) :count :remainder :doc
  "the raw data for flags & x/y coordinates"))


(flags u8 simple-glyph-flags (:doc "Flags used in [SimpleGlyph]")
 (on-curve-point 1) (x-short-vector 2) (y-short-vector 4) (repeat-flag 8)
 (x-is-same-or-positive-x-short-vector 16)
 (y-is-same-or-positive-y-short-vector 32) (overlap-simple 64) (cubic 128))


(table composite-glyph
 (:doc
  "[CompositeGlyph](https://docs.microsoft.com/en-us/typography/opentype/spec/glyf#glyph-headers)")
 (field number-of-contours i16 :doc
  "If the number of contours is greater than or equal to zero, this is a simple glyph. If negative, this is a composite glyph — the value -1 should be used for composite glyphs.")
 (field x-min i16 :doc "Minimum x for coordinate data.")
 (field y-min i16 :doc "Minimum y for coordinate data.")
 (field x-max i16 :doc "Maximum x for coordinate data.")
 (field y-max i16 :doc "Maximum y for coordinate data.")
 (field component-data (array u8) :count :remainder :doc
  "component flag glyph index of component"))


(flags u16 composite-glyph-flags (:doc "Flags used in [CompositeGlyph]")
 (arg-1-and-2-are-words 1) (args-are-xy-values 2) (round-xy-to-grid 4)
 (we-have-a-scale 8) (more-components 32) (we-have-an-x-and-y-scale 64)
 (we-have-a-two-by-two 128) (we-have-instructions 256) (use-my-metrics 512)
 (overlap-compound 1024) (scaled-component-offset 2048)
 (unscaled-component-offset 4096) (gid-is-24-bit 8192))


(format-table i16 glyph (:doc "Simple or composite glyph.")
 (variant simple simple-glyph) (variant composite composite-glyph))

