;;;; varc.sexp — OpenType table schema
;;;; Generated from Fontations varc.rs spec


(table varc (:tag "VARC")
 (:doc
  "[VARC](https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md) (Variable Composites / Components Table)  [FontTools VARC](https://github.com/fonttools/fonttools/blob/5e6b12d12fa08abafbeb7570f47707fbedf69a45/Lib/fontTools/ttLib/tables/otData.py#L3459-L3476)")
 (field version major-minor :doc "Major/minor version number. Set to 1.0.")
 (field coverage-offset (offset32 coverage-table))
 (field multi-var-store-offset (offset32 multi-item-variation-store))
 (field condition-list-offset (offset32 condition-list))
 (field axis-indices-list-offset (offset32 index2))
 (field var-composite-glyphs-offset (offset32 index2)))


(table multi-item-variation-store
 (:doc
  "* <https://github.com/fonttools/fonttools/blob/5e6b12d12fa08abafbeb7570f47707fbedf69a45/Lib/fontTools/ttLib/tables/otData.py#L3451-L3457> * <https://github.com/harfbuzz/harfbuzz/blob/7be12b33e3f07067c159d8f516eb31df58c75876/src/hb-ot-layout-common.hh#L3517-L3520C3>")
 (field format-table u16 :format 1)
 (field region-list-offset (offset32 sparse-variation-region-list))
 (field variation-data-count u16)
 (field variation-data-offsets (array (offset32 multi-item-variation-data))
  :count variation-data-count))


(table sparse-variation-region-list (field region-count u32)
 (field region-offsets (array (offset32 sparse-variation-region)) :count
  region-count))


(table sparse-variation-region (field region-axis-count u16)
 (field axis-coordinate-offsets
  (array (offset32 sparse-region-axis-coordinates)) :count region-axis-count))


(table sparse-region-axis-coordinates (field axis-index u16)
 (field start f2-dot14) (field peak f2-dot14) (field end f2-dot14))


(table multi-item-variation-data (field format-table u8 :format 1)
 (field region-index-count u16)
 (field region-indices (array u16) :count region-index-count)
 (field delta-sets-offset (offset32 index2)))


(table condition-list (field condition-count u32)
 (field condition-offsets (array (offset32 condition)) :count condition-count))


(flags u32 varc-flags
 (:doc
  "Flags used in the [VarcComponent] byte stream  <https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md#variable-component-flags>")
 (reset-unspecified-axes 1) (have-axes 2) (axis-values-have-variation 4)
 (transform-has-variation 8) (have-translate-x 16) (have-translate-y 32)
 (have-rotation 64) (have-condition 128) (have-scale-x 256) (have-scale-y 512)
 (have-tcenter-x 1024) (have-tcenter-y 2048) (gid-is-24-bit 4096)
 (have-skew-x 8192) (have-skew-y 16384) (reserved-mask 4294934528))

