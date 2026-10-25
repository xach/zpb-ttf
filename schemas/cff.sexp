;;;; cff.sexp — OpenType table schema
;;;; Generated from Fontations cff.rs spec


(table cff-header
 (:doc
  "[Compact Font Format](https://learn.microsoft.com/en-us/typography/opentype/spec/cff) table header")
 (field major u8 :doc "Format major version (starting at 1).")
 (field minor u8 :doc "Format minor version (starting at 0).")
 (field hdr-size u8 :doc "Header size (bytes).")
 (field off-size u8 :doc "Absolute offset size.")
 (field -padding (array u8) :count (subtract hdr-size 4) :doc
  "Padding bytes before the start of the Name INDEX.")
 (field trailing-data (array u8) :count :remainder :doc
  "Remaining table data."))


(table cff-index (:doc "An array of variable-sized objects in a `CFF` table.")
 (field count u16 :doc "Number of objects stored in INDEX.")
 (field off-size u8 :doc "Object array element size.")
 (field offsets (array u8) :count (add_multiply count 1 off-size) :doc
  "Bytes containing `count + 1` offsets each of `off_size`.")
 (field data (array u8) :count :remainder :doc
  "Array containing the object data."))


(format-table u8 fd-select
 (:doc "Associates a glyph identifier with a Font DICT.")
 (variant format0 fd-select-format0) (variant format3 fd-select-format3)
 (variant format4 fd-select-format4))


(table fd-select-format0 (:doc "FdSelect format 0.")
 (field format-table u8 :format 0 :doc "Format = 0.")
 (field fds (array u8) :count :remainder :doc
  "FD selector array (one entry for each glyph)."))


(table fd-select-format3 (:doc "FdSelect format 3.")
 (field format-table u8 :format 3 :doc "Format = 3.")
 (field n-ranges u16 :doc "Number of ranges.")
 (field ranges (array fd-select-range3) :count n-ranges :doc "Range3 array.")
 (field sentinel u16 :doc
  "Sentinel GID. Set equal to the number of glyphs in the font."))


(record fd-select-range3 (:doc "Range struct for FdSelect format 3.")
 (field first u16 :doc "First glyph index in range.")
 (field fd u8 :doc "FD index for all glyphs in range."))


(table fd-select-format4 (:doc "FdSelect format 4.")
 (field format-table u8 :format 4 :doc "Format = 4.")
 (field n-ranges u32 :doc "Number of ranges.")
 (field ranges (array fd-select-range4) :count n-ranges :doc "Range4 array.")
 (field sentinel u32 :doc
  "Sentinel GID. Set equal to the number of glyphs in the font."))


(record fd-select-range4 (:doc "Range struct for FdSelect format 4.")
 (field first u32 :doc "First glyph index in range.")
 (field fd u16 :doc "FD index for all glyphs in range."))


(format-table u8 custom-charset
 (:doc "Charset with custom glyph id to string id mappings.")
 (variant format0 charset-format0) (variant format1 charset-format1)
 (variant format2 charset-format2))


(table charset-format0 (:doc "Charset format 0.")
 (field format-table u8 :format 0 :doc "Format; =0")
 (field glyph (array u16) :count :remainder :doc "Glyph name array."))


(table charset-format1 (:doc "Charset format 1.")
 (field format-table u8 :format 1 :doc "Format; =1")
 (field ranges (array charset-range1) :count :remainder :doc "Range1 array."))


(record charset-range1 (:doc "Range struct for Charset format 1.")
 (field first u16 :doc "First glyph in range.")
 (field n-left u8 :doc "Glyphs left in range (excluding first)."))


(table charset-format2 (:doc "Charset format 2.")
 (field format-table u8 :format 2 :doc "Format; =2")
 (field ranges (array charset-range2) :count :remainder :doc "Range2 array."))


(record charset-range2 (:doc "Range struct for Charset format 2.")
 (field first u16 :doc "First glyph in range.")
 (field n-left u16 :doc "Glyphs left in range (excluding first)."))


(record encoding-range1 (:doc "Range struct for Encoding format 1.")
 (field first u8 :doc "First code in range.")
 (field n-left u8 :doc "Codes left in range (excluding first)."))


(record encoding-supplement (:doc "Supplemental encoding record.")
 (field code u8 :doc "Encoding.") (field glyph u16 :doc "Name."))

