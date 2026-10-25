;;;; ift.sexp — OpenType table schema
;;;; Generated from Fontations ift.rs spec


(flags u8 patch-map-field-presence-flags (cff-charstrings-offset 1)
 (cff2-charstrings-offset 2))


(table ift-patch-map
 (:doc
  "[Patch Map Table](https://w3c.github.io/IFT/Overview.html#patch-map-table)")
 (field format-table u8 :doc "Format identifier: format = 2")
 (field -reserved-0 u8) (field -reserved-1 u8) (field -reserved-2 u8)
 (field field-flags patch-map-field-presence-flags)
 (field compatibility-id compatibility-id :doc
  "Unique ID that identifies compatible patches.")
 (field default-patch-format u8 :doc
  "Patch format number for patches referenced by this mapping.")
 (field entry-count uint24) (field entries-offset (offset32 mapping-entries))
 (field entry-id-string-data-offset (offset32 id-string-data))
 (field url-template-length u16)
 (field url-template (array u8) :count url-template-length)
 (field cff-charstrings-offset u32 :when-flag (field-flags cff-charstrings-offset))
 (field cff2-charstrings-offset u32 :when-flag (field-flags cff2-charstrings-offset)))


(table mapping-entries (field entry-data (array u8) :count :remainder))


(table entry-data (field format-flags entry-format-flags)
 (field feature-count u8 :when-flag (format-flags features-and-design-space))
 (field feature-tags (array tag) :count feature-count
  :when-flag (format-flags features-and-design-space))
 (field design-space-count u16 :when-flag (format-flags features-and-design-space))
 (field design-space-segments (array design-space-segment) :count
  design-space-count :when-flag (format-flags features-and-design-space))
 (field match-mode-and-count match-mode-and-count
  :when-flag (format-flags child-indices))
 (field child-indices (array uint24) :count (try_into match-mode-and-count)
  :when-flag (format-flags child-indices))
 (field trailing-data (array u8) :count :remainder))


(flags u8 entry-format-flags (features-and-design-space 1) (child-indices 2)
 (entry-id-delta 4) (patch-format 8) (codepoints-bit-1 16)
 (codepoints-bit-2 32) (ignored 64) (reserved 128))


(record design-space-segment (field axis-tag tag) (field start fixed)
 (field end fixed))


(table id-string-data (field id-data (array u8) :count :remainder))


(table table-keyed-patch
 (:doc
  "[Table Keyed Patch](https://w3c.github.io/IFT/Overview.html#table-keyed)")
 (field format-table tag) (field -reserved u32)
 (field compatibility-id compatibility-id :doc
  "Unique ID that identifies compatible patches.")
 (field patches-count u16)
 (field patch-offsets (array (offset32 table-patch)) :count
  (add patches-count 1)))


(table table-patch
 (:doc "[TablePatch](https://w3c.github.io/IFT/Overview.html#tablepatch)")
 (field tag tag) (field flags table-patch-flags)
 (field max-uncompressed-length u32)
 (field brotli-stream (array u8) :count :remainder))


(flags u8 table-patch-flags (replace-table 1) (drop-table 2))


(table glyph-keyed-patch
 (:doc
  "[Glyph Keyed Patch](https://w3c.github.io/IFT/Overview.html#glyph-keyed)")
 (field format-table tag) (field -reserved u32) (field flags glyph-keyed-flags)
 (field compatibility-id compatibility-id) (field max-uncompressed-length u32)
 (field brotli-stream (array u8) :count :remainder))


(flags u8 glyph-keyed-flags (none 0) (wide-glyph-ids 1))


(table glyph-patches
 (:doc "[GlyphPatches](https://w3c.github.io/IFT/Overview.html#glyphpatches)")
 (field glyph-count u32) (field table-count u8)
 (field glyph-ids computed-array<u16-or24>)
 ;; tables and glyphDataOffsets follow glyphIds, whose element size depends on
 ;; the patch format flags; their position must be computed at runtime.
 )


(table ift-glyph-data (field data (array u8) :count :remainder))



(record compatibility-id
 (:doc "[Compatibility ID](https://w3c.github.io/IFT/Overview.html): uint32[4].")
 (field id0 u32) (field id1 u32) (field id2 u32) (field id3 u32))
