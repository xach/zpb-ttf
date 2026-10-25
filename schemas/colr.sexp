;;;; colr.sexp — OpenType table schema
;;;; Generated from Fontations colr.rs spec


(table colr (:tag "COLR")
 (:doc
  "[COLR (Color)](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#colr-header) table")
 (field version u16 :doc "Table version number - set to 0 or 1.")
 (field num-base-glyph-records u16 :doc
  "Number of BaseGlyph records; may be 0 in a version 1 table.")
 (field base-glyph-records-offset (offset32 [base-glyph]) :doc
  "Offset to baseGlyphRecords array (may be NULL).")
 (field layer-records-offset (offset32 [layer]) :doc
  "Offset to layerRecords array (may be NULL).")
 (field num-layer-records u16 :doc
  "Number of Layer records; may be 0 in a version 1 table.")
 (field base-glyph-list-offset (offset32 base-glyph-list) :since-version "1"
  :doc "Offset to BaseGlyphList table.")
 (field layer-list-offset (offset32 layer-list) :since-version "1" :doc
  "Offset to LayerList table (may be NULL).")
 (field clip-list-offset (offset32 clip-list) :since-version "1" :doc
  "Offset to ClipList table (may be NULL).")
 (field var-index-map-offset (offset32 delta-set-index-map) :since-version "1"
  :doc "Offset to DeltaSetIndexMap table (may be NULL).")
 (field item-variation-store-offset (offset32 item-variation-store)
  :since-version "1" :doc "Offset to ItemVariationStore (may be NULL)."))


(record base-glyph
 (:doc
  "[BaseGlyph](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyph-and-layer-records) record")
 (field glyph-id glyph-id16 :doc "Glyph ID of the base glyph.")
 (field first-layer-index u16 :doc
  "Index (base 0) into the layerRecords array.")
 (field num-layers u16 :doc
  "Number of color layers associated with this glyph."))


(record layer
 (:doc
  "[Layer](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyph-and-layer-records) record")
 (field glyph-id glyph-id16 :doc
  "Glyph ID of the glyph used for a given layer.")
 (field palette-index u16 :doc
  "Index (base 0) for a palette entry in the CPAL table."))


(table base-glyph-list
 (:doc
  "[BaseGlyphList](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) table")
 (field num-base-glyph-paint-records u32)
 (field base-glyph-paint-records (array base-glyph-paint) :count
  num-base-glyph-paint-records))


(record base-glyph-paint
 (:doc
  "[BaseGlyphPaint](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) record")
 (field glyph-id glyph-id16 :doc "Glyph ID of the base glyph.")
 (field paint-offset (offset32 paint) :doc
  "Offset to a Paint table, from the beginning of the [`BaseGlyphList`] table."))


(table layer-list
 (:doc
  "[LayerList](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) table")
 (field num-layers u32)
 (field paint-offsets (array (offset32 paint)) :count num-layers :doc
  "Offsets to Paint tables."))


(table clip-list
 (:doc
  "[ClipList](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) table")
 (field format-table u8 :doc "Set to 1.")
 (field num-clips u32 :doc "Number of Clip records.")
 (field clips (array clip) :count num-clips :doc
  "Clip records. Sorted by startGlyphID."))


(record clip
 (:doc
  "[Clip](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) record")
 (field start-glyph-id glyph-id16 :doc "First glyph ID in the range.")
 (field end-glyph-id glyph-id16 :doc "Last glyph ID in the range.")
 (field clip-box-offset (offset24 clip-box) :doc
  "Offset to a ClipBox table, from the beginning of the [`ClipList`] table."))


(format-table u8 clip-box
 (:doc
  "[ClipBox](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) table")
 (variant format1 clip-box-format1) (variant format2 clip-box-format2))


(table clip-box-format1
 (:doc
  "[ClipBoxFormat1](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) record")
 (field format-table u8 :format 1 :doc "Set to 1.")
 (field x-min f-word :doc "Minimum x of clip box.")
 (field y-min f-word :doc "Minimum y of clip box.")
 (field x-max f-word :doc "Maximum x of clip box.")
 (field y-max f-word :doc "Maximum y of clip box."))


(table clip-box-format2
 (:doc
  "[ClipBoxFormat2](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#baseglyphlist-layerlist-and-cliplist) record")
 (field format-table u8 :format 2 :doc "Set to 2.")
 (field x-min f-word :doc
  "Minimum x of clip box. For variation, use varIndexBase + 0.")
 (field y-min f-word :doc
  "Minimum y of clip box. For variation, use varIndexBase + 1.")
 (field x-max f-word :doc
  "Maximum x of clip box. For variation, use varIndexBase + 2.")
 (field y-max f-word :doc
  "Maximum y of clip box. For variation, use varIndexBase + 3.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(record color-index
 (:doc
  "[ColorIndex](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) record")
 (field palette-index u16 :doc "Index for a CPAL palette entry.")
 (field alpha f2-dot14 :doc "Alpha value."))


(record var-color-index
 (:doc
  "[VarColorIndex](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) record")
 (field palette-index u16 :doc "Index for a CPAL palette entry.")
 (field alpha f2-dot14 :doc
  "Alpha value. For variation, use varIndexBase + 0.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(record color-stop
 (:doc
  "[ColorStop](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) record")
 (field stop-offset f2-dot14 :doc "Position on a color line.")
 (field palette-index u16 :doc "Index for a CPAL palette entry.")
 (field alpha f2-dot14 :doc "Alpha value."))


(record var-color-stop
 (:doc
  "[VarColorStop](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) record")
 (field stop-offset f2-dot14 :doc
  "Position on a color line. For variation, use varIndexBase + 0.")
 (field palette-index u16 :doc "Index for a CPAL palette entry.")
 (field alpha f2-dot14 :doc
  "Alpha value. For variation, use varIndexBase + 1.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table color-line
 (:doc
  "[ColorLine](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) table")
 (field extend extend :doc "An Extend enum value.")
 (field num-stops u16 :doc "Number of ColorStop records.")
 (field color-stops (array color-stop) :count num-stops))


(table var-color-line
 (:doc
  "[VarColorLine](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) table")
 (field extend extend :doc "An Extend enum value.")
 (field num-stops u16 :doc "Number of ColorStop records.")
 (field color-stops (array var-color-stop) :count num-stops :doc
  "Allows for variations."))


(enum u8 extend
 (:doc
  "[Extend](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-references-colorstop-and-colorline) enumeration")
 (pad 0) (repeat 1) (reflect 2))


(format-table u8 paint
 (:doc
  "[Paint](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#paint-tables) tables")
 (variant colr-layers paint-colr-layers) (variant solid paint-solid)
 (variant var-solid paint-var-solid)
 (variant linear-gradient paint-linear-gradient)
 (variant var-linear-gradient paint-var-linear-gradient)
 (variant radial-gradient paint-radial-gradient)
 (variant var-radial-gradient paint-var-radial-gradient)
 (variant sweep-gradient paint-sweep-gradient)
 (variant var-sweep-gradient paint-var-sweep-gradient)
 (variant glyph paint-glyph) (variant colr-glyph paint-colr-glyph)
 (variant transform paint-transform)
 (variant var-transform paint-var-transform)
 (variant translate paint-translate)
 (variant var-translate paint-var-translate) (variant scale paint-scale)
 (variant var-scale paint-var-scale)
 (variant scale-around-center paint-scale-around-center)
 (variant var-scale-around-center paint-var-scale-around-center)
 (variant scale-uniform paint-scale-uniform)
 (variant var-scale-uniform paint-var-scale-uniform)
 (variant scale-uniform-around-center paint-scale-uniform-around-center)
 (variant var-scale-uniform-around-center
  paint-var-scale-uniform-around-center)
 (variant rotate paint-rotate) (variant var-rotate paint-var-rotate)
 (variant rotate-around-center paint-rotate-around-center)
 (variant var-rotate-around-center paint-var-rotate-around-center)
 (variant skew paint-skew) (variant var-skew paint-var-skew)
 (variant skew-around-center paint-skew-around-center)
 (variant var-skew-around-center paint-var-skew-around-center)
 (variant composite paint-composite) (variant glyph2 paint-glyph2))


(table paint-colr-layers
 (:doc
  "[PaintColrLayers](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-1-paintcolrlayers) table")
 (field format-table u8 :format 1 :doc "Set to 1.")
 (field num-layers u8 :doc
  "Number of offsets to paint tables to read from LayerList.")
 (field first-layer-index u32 :doc "Index (base 0) into the LayerList."))


(table paint-solid
 (:doc
  "[PaintSolid](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-2-and-3-paintsolid-paintvarsolid) table")
 (field format-table u8 :format 2 :doc "Set to 2.")
 (field palette-index u16 :doc "Index for a CPAL palette entry.")
 (field alpha f2-dot14 :doc "Alpha value."))


(table paint-var-solid
 (:doc
  "[PaintVarSolid](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-2-and-3-paintsolid-paintvarsolid) table")
 (field format-table u8 :format 3 :doc "Set to 3.")
 (field palette-index u16 :doc "Index for a CPAL palette entry.")
 (field alpha f2-dot14 :doc
  "Alpha value. For variation, use varIndexBase + 0.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-linear-gradient
 (:doc
  "[PaintLinearGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-4-and-5-paintlineargradient-paintvarlineargradient) table")
 (field format-table u8 :format 4 :doc "Set to 4.")
 (field color-line-offset (offset24 color-line) :doc
  "Offset to ColorLine table.")
 (field x0 f-word :doc "Start point (p₀) x coordinate.")
 (field y0 f-word :doc "Start point (p₀) y coordinate.")
 (field x1 f-word :doc "End point (p₁) x coordinate.")
 (field y1 f-word :doc "End point (p₁) y coordinate.")
 (field x2 f-word :doc "Rotation point (p₂) x coordinate.")
 (field y2 f-word :doc "Rotation point (p₂) y coordinate."))


(table paint-var-linear-gradient
 (:doc
  "[PaintVarLinearGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-4-and-5-paintlineargradient-paintvarlineargradient) table")
 (field format-table u8 :format 5 :doc "Set to 5.")
 (field color-line-offset (offset24 var-color-line) :doc
  "Offset to VarColorLine table.")
 (field x0 f-word :doc
  "Start point (p₀) x coordinate. For variation, use varIndexBase + 0.")
 (field y0 f-word :doc
  "Start point (p₀) y coordinate. For variation, use varIndexBase + 1.")
 (field x1 f-word :doc
  "End point (p₁) x coordinate. For variation, use varIndexBase + 2.")
 (field y1 f-word :doc
  "End point (p₁) y coordinate. For variation, use varIndexBase + 3.")
 (field x2 f-word :doc
  "Rotation point (p₂) x coordinate. For variation, use varIndexBase + 4.")
 (field y2 f-word :doc
  "Rotation point (p₂) y coordinate. For variation, use varIndexBase + 5.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-radial-gradient
 (:doc
  "[PaintRadialGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-6-and-7-paintradialgradient-paintvarradialgradient) table")
 (field format-table u8 :format 6 :doc "Set to 6.")
 (field color-line-offset (offset24 color-line) :doc
  "Offset to ColorLine table.")
 (field x0 f-word :doc "Start circle center x coordinate.")
 (field y0 f-word :doc "Start circle center y coordinate.")
 (field radius0 uf-word :doc "Start circle radius.")
 (field x1 f-word :doc "End circle center x coordinate.")
 (field y1 f-word :doc "End circle center y coordinate.")
 (field radius1 uf-word :doc "End circle radius."))


(table paint-var-radial-gradient
 (:doc
  "[PaintVarRadialGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-6-and-7-paintradialgradient-paintvarradialgradient) table")
 (field format-table u8 :format 7 :doc "Set to 7.")
 (field color-line-offset (offset24 var-color-line) :doc
  "Offset to VarColorLine table.")
 (field x0 f-word :doc
  "Start circle center x coordinate. For variation, use varIndexBase + 0.")
 (field y0 f-word :doc
  "Start circle center y coordinate. For variation, use varIndexBase + 1.")
 (field radius0 uf-word :doc
  "Start circle radius. For variation, use varIndexBase + 2.")
 (field x1 f-word :doc
  "End circle center x coordinate. For variation, use varIndexBase + 3.")
 (field y1 f-word :doc
  "End circle center y coordinate. For variation, use varIndexBase + 4.")
 (field radius1 uf-word :doc
  "End circle radius. For variation, use varIndexBase + 5.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-sweep-gradient
 (:doc
  "[PaintSweepGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-8-and-9-paintsweepgradient-paintvarsweepgradient) table")
 (field format-table u8 :format 8 :doc "Set to 8.")
 (field color-line-offset (offset24 color-line) :doc
  "Offset to ColorLine table.")
 (field center-x f-word :doc "Center x coordinate.")
 (field center-y f-word :doc "Center y coordinate.")
 (field start-angle f2-dot14 :doc
  "Start of the angular range of the gradient, 180° in counter-clockwise degrees per 1.0 of value.")
 (field end-angle f2-dot14 :doc
  "End of the angular range of the gradient, 180° in counter-clockwise degrees per 1.0 of value."))


(table paint-var-sweep-gradient
 (:doc
  "[PaintVarSweepGradient](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-8-and-9-paintsweepgradient-paintvarsweepgradient) table")
 (field format-table u8 :format 9 :doc "Set to 9.")
 (field color-line-offset (offset24 var-color-line) :doc
  "Offset to VarColorLine table.")
 (field center-x f-word :doc
  "Center x coordinate. For variation, use varIndexBase + 0.")
 (field center-y f-word :doc
  "Center y coordinate. For variation, use varIndexBase + 1.")
 (field start-angle f2-dot14 :doc
  "Start of the angular range of the gradient, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 2.")
 (field end-angle f2-dot14 :doc
  "End of the angular range of the gradient, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 3.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-glyph
 (:doc
  "[PaintGlyph](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-10-paintglyph) table")
 (field format-table u8 :format 10 :doc "Set to 10.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint table.")
 (field glyph-id glyph-id16 :doc "Glyph ID for the source outline."))


(table paint-glyph2 (:doc "PaintGlyph2, format 33 (ISO/IEC 14496-22:2026).")
 (field format-table u8 :format 33 :doc "Set to 33.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint table.")
 (field glyph-id glyph-id24 :doc "Glyph ID for the source outline."))


(table paint-colr-glyph
 (:doc
  "[PaintColrGlyph](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-11-paintcolrglyph) table")
 (field format-table u8 :format 11 :doc "Set to 11.")
 (field glyph-id glyph-id16 :doc "Glyph ID for a BaseGlyphList base glyph."))


(table paint-transform
 (:doc
  "[PaintTransform](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-12-and-13-painttransform-paintvartransform) table")
 (field format-table u8 :format 12 :doc "Set to 12.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field transform-offset (offset24 affine2x3) :doc
  "Offset to an Affine2x3 table."))


(table paint-var-transform
 (:doc
  "[PaintVarTransform](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-12-and-13-painttransform-paintvartransform) table")
 (field format-table u8 :format 13 :doc "Set to 13.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field transform-offset (offset24 var-affine2x3) :doc
  "Offset to a VarAffine2x3 table."))


(table affine2x3
 (:doc
  "[Affine2x3](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-12-and-13-painttransform-paintvartransform) record")
 (field xx fixed :doc "x-component of transformed x-basis vector.")
 (field yx fixed :doc "y-component of transformed x-basis vector.")
 (field xy fixed :doc "x-component of transformed y-basis vector.")
 (field yy fixed :doc "y-component of transformed y-basis vector.")
 (field dx fixed :doc "Translation in x direction.")
 (field dy fixed :doc "Translation in y direction."))


(table var-affine2x3
 (:doc
  "[VarAffine2x3](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-12-and-13-painttransform-paintvartransform) record")
 (field xx fixed :doc
  "x-component of transformed x-basis vector. For variation, use varIndexBase + 0.")
 (field yx fixed :doc
  "y-component of transformed x-basis vector. For variation, use varIndexBase + 1.")
 (field xy fixed :doc
  "x-component of transformed y-basis vector. For variation, use varIndexBase + 2.")
 (field yy fixed :doc
  "y-component of transformed y-basis vector. For variation, use varIndexBase + 3.")
 (field dx fixed :doc
  "Translation in x direction. For variation, use varIndexBase + 4.")
 (field dy fixed :doc
  "Translation in y direction. For variation, use varIndexBase + 5.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-translate
 (:doc
  "[PaintTranslate](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-14-and-15-painttranslate-paintvartranslate) table")
 (field format-table u8 :format 14 :doc "Set to 14.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field dx f-word :doc "Translation in x direction.")
 (field dy f-word :doc "Translation in y direction."))


(table paint-var-translate
 (:doc
  "[PaintVarTranslate](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-14-and-15-painttranslate-paintvartranslate) table")
 (field format-table u8 :format 15 :doc "Set to 15.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field dx f-word :doc
  "Translation in x direction. For variation, use varIndexBase + 0.")
 (field dy f-word :doc
  "Translation in y direction. For variation, use varIndexBase + 1.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-scale
 (:doc
  "[PaintScale](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")
 (field format-table u8 :format 16 :doc "Set to 16.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field scale-x f2-dot14 :doc "Scale factor in x direction.")
 (field scale-y f2-dot14 :doc "Scale factor in y direction."))


(table paint-var-scale
 (:doc
  "[PaintVarScale](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")
 (field format-table u8 :format 17 :doc "Set to 17.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field scale-x f2-dot14 :doc
  "Scale factor in x direction. For variation, use varIndexBase + 0.")
 (field scale-y f2-dot14 :doc
  "Scale factor in y direction. For variation, use varIndexBase + 1.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-scale-around-center
 (:doc
  "[PaintScaleAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")
 (field format-table u8 :format 18 :doc "Set to 18.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field scale-x f2-dot14 :doc "Scale factor in x direction.")
 (field scale-y f2-dot14 :doc "Scale factor in y direction.")
 (field center-x f-word :doc "x coordinate for the center of scaling.")
 (field center-y f-word :doc "y coordinate for the center of scaling."))


(table paint-var-scale-around-center
 (:doc
  "[PaintVarScaleAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")
 (field format-table u8 :format 19 :doc "Set to 19.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field scale-x f2-dot14 :doc
  "Scale factor in x direction. For variation, use varIndexBase + 0.")
 (field scale-y f2-dot14 :doc
  "Scale factor in y direction. For variation, use varIndexBase + 1.")
 (field center-x f-word :doc
  "x coordinate for the center of scaling. For variation, use varIndexBase + 2.")
 (field center-y f-word :doc
  "y coordinate for the center of scaling. For variation, use varIndexBase + 3.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-scale-uniform
 (:doc
  "[PaintScaleUniform](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")
 (field format-table u8 :format 20 :doc "Set to 20.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field scale f2-dot14 :doc "Scale factor in x and y directions."))


(table paint-var-scale-uniform
 (:doc
  "[PaintVarScaleUniform](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")
 (field format-table u8 :format 21 :doc "Set to 21.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field scale f2-dot14 :doc
  "Scale factor in x and y directions. For variation, use varIndexBase + 0.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-scale-uniform-around-center
 (:doc
  "[PaintScaleUniformAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")
 (field format-table u8 :format 22 :doc "Set to 22.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field scale f2-dot14 :doc "Scale factor in x and y directions.")
 (field center-x f-word :doc "x coordinate for the center of scaling.")
 (field center-y f-word :doc "y coordinate for the center of scaling."))


(table paint-var-scale-uniform-around-center
 (:doc
  "[PaintVarScaleUniformAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-16-to-23-paintscale-and-variant-scaling-formats) table")
 (field format-table u8 :format 23 :doc "Set to 23.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field scale f2-dot14 :doc
  "Scale factor in x and y directions. For variation, use varIndexBase + 0.")
 (field center-x f-word :doc
  "x coordinate for the center of scaling. For variation, use varIndexBase + 1.")
 (field center-y f-word :doc
  "y coordinate for the center of scaling. For variation, use varIndexBase + 2.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-rotate
 (:doc
  "[PaintRotate](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-24-to-27-paintrotate-paintvarrotate-paintrotatearoundcenter-paintvarrotatearoundcenter) table")
 (field format-table u8 :format 24 :doc "Set to 24.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field angle f2-dot14 :doc
  "Rotation angle, 180° in counter-clockwise degrees per 1.0 of value."))


(table paint-var-rotate
 (:doc
  "[PaintVarRotate](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-24-to-27-paintrotate-paintvarrotate-paintrotatearoundcenter-paintvarrotatearoundcenter) table")
 (field format-table u8 :format 25 :doc "Set to 25.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field angle f2-dot14 :doc
  "Rotation angle, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 0.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-rotate-around-center
 (:doc
  "[PaintRotateAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-24-to-27-paintrotate-paintvarrotate-paintrotatearoundcenter-paintvarrotatearoundcenter) table")
 (field format-table u8 :format 26 :doc "Set to 26.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field angle f2-dot14 :doc
  "Rotation angle, 180° in counter-clockwise degrees per 1.0 of value.")
 (field center-x f-word :doc "x coordinate for the center of rotation.")
 (field center-y f-word :doc "y coordinate for the center of rotation."))


(table paint-var-rotate-around-center
 (:doc
  "[PaintVarRotateAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-24-to-27-paintrotate-paintvarrotate-paintrotatearoundcenter-paintvarrotatearoundcenter) table")
 (field format-table u8 :format 27 :doc "Set to 27.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field angle f2-dot14 :doc
  "Rotation angle, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 0.")
 (field center-x f-word :doc
  "x coordinate for the center of rotation. For variation, use varIndexBase + 1.")
 (field center-y f-word :doc
  "y coordinate for the center of rotation. For variation, use varIndexBase + 2.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-skew
 (:doc
  "[PaintSkew](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-28-to-31-paintskew-paintvarskew-paintskewaroundcenter-paintvarskewaroundcenter) table")
 (field format-table u8 :format 28 :doc "Set to 28.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field x-skew-angle f2-dot14 :doc
  "Angle of skew in the direction of the x-axis, 180° in counter-clockwise degrees per 1.0 of value.")
 (field y-skew-angle f2-dot14 :doc
  "Angle of skew in the direction of the y-axis, 180° in counter-clockwise degrees per 1.0 of value."))


(table paint-var-skew
 (:doc
  "[PaintVarSkew](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-28-to-31-paintskew-paintvarskew-paintskewaroundcenter-paintvarskewaroundcenter) table")
 (field format-table u8 :format 29 :doc "Set to 29.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field x-skew-angle f2-dot14 :doc
  "Angle of skew in the direction of the x-axis, 180° ┬░ in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 0.")
 (field y-skew-angle f2-dot14 :doc
  "Angle of skew in the direction of the y-axis, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 1.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-skew-around-center
 (:doc
  "[PaintSkewAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-28-to-31-paintskew-paintvarskew-paintskewaroundcenter-paintvarskewaroundcenter) table")
 (field format-table u8 :format 30 :doc "Set to 30.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field x-skew-angle f2-dot14 :doc
  "Angle of skew in the direction of the x-axis, 180° in counter-clockwise degrees per 1.0 of value.")
 (field y-skew-angle f2-dot14 :doc
  "Angle of skew in the direction of the y-axis, 180° in counter-clockwise degrees per 1.0 of value.")
 (field center-x f-word :doc "x coordinate for the center of rotation.")
 (field center-y f-word :doc "y coordinate for the center of rotation."))


(table paint-var-skew-around-center
 (:doc
  "[PaintVarSkewAroundCenter](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#formats-28-to-31-paintskew-paintvarskew-paintskewaroundcenter-paintvarskewaroundcenter) table")
 (field format-table u8 :format 31 :doc "Set to 31.")
 (field paint-offset (offset24 paint) :doc "Offset to a Paint subtable.")
 (field x-skew-angle f2-dot14 :doc
  "Angle of skew in the direction of the x-axis, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 0.")
 (field y-skew-angle f2-dot14 :doc
  "Angle of skew in the direction of the y-axis, 180° in counter-clockwise degrees per 1.0 of value. For variation, use varIndexBase + 1.")
 (field center-x f-word :doc
  "x coordinate for the center of rotation. For variation, use varIndexBase + 2.")
 (field center-y f-word :doc
  "y coordinate for the center of rotation. For variation, use varIndexBase + 3.")
 (field var-index-base u32 :doc "Base index into DeltaSetIndexMap."))


(table paint-composite
 (:doc
  "[PaintComposite](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) table")
 (field format-table u8 :format 32 :doc "Set to 32.")
 (field source-paint-offset (offset24 paint) :doc
  "Offset to a source Paint table.")
 (field composite-mode composite-mode :doc
  "A CompositeMode enumeration value.")
 (field backdrop-paint-offset (offset24 paint) :doc
  "Offset to a backdrop Paint table."))


(enum u8 composite-mode
 (:doc
  "[CompositeMode](https://learn.microsoft.com/en-us/typography/opentype/spec/colr#format-32-paintcomposite) enumeration")
 (clear 0) (src 1) (dest 2) (src-over 3) (dest-over 4) (src-in 5) (dest-in 6)
 (src-out 7) (dest-out 8) (src-atop 9) (dest-atop 10) (xor 11) (plus 12)
 (screen 13) (overlay 14) (darken 15) (lighten 16) (color-dodge 17)
 (color-burn 18) (hard-light 19) (soft-light 20) (difference 21) (exclusion 22)
 (multiply 23) (hsl-hue 24) (hsl-saturation 25) (hsl-color 26)
 (hsl-luminosity 27))

