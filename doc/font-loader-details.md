[zpb-ttf](https://github.com/xach/zpb-ttf) is a library to read font files. The capability is somewhat limited, but it does handle the most
important format, truetype fonts, however only a subset of those: TrueType-outline fonts (`.ttf`, and `.ttc` collections). It supports 12 basic tables `head`, `hhea`, `hmtx`, `vhea`, `vmtx`, `maxp`, `name`, `post`, `glyf`, `loca`, `cmap`, and the legacy `kern` table (both the Microsoft and the Apple variant, formats 0 and 2).

## What does it mean in practice

When it comes to some modern formats, older formats, or more advanced features, it is not covered. Anyhow, what it does get us far. I have used it in X11 demos, in Garnet and in my own colour picker and termometer app, to render test, via cl-vectors, a path rasterizer also by the author of zpb-ttf. However, for modern font support, we need more.

`zpb-otf`, expands the coverage from 12 tables to 52 table tags (46 distinct tables, plus 6 extended variants from ISO/IEC 14496-22:2026 such as `GVAR` and `HMTX`) and their shared sub-structures. The parsers are generated from the table definitions that Google's Fontations project feeds its own code generator, converted to s-expressions and compiled to Lisp. If you are wondering what "tables" are, it is various data needed to render fonts correctly on the screen. Modern font formats are like video or image formats, they act more like containers for various formats.

"Table" here means a named, offset-delimited byte region inside the font file — not a data table in the database sense. The usage is the same as in PDF or SQLite: a *chunk of structured data* identified by a name, at a known offset.

A `.ttf`/`.otf` file is an `sfnt` container; a `.ttc` is several of them sharing tables, and `.woff`/`.woff2` wrap one in a compressed format (see below). At the top of an sfnt:

```
u32  sfntVersion      ; 0x00010000 (TrueType), 'OTTO' (CFF), 'true', 'typ1'
                      ; (a collection starts with a 'ttcf' header pointing at several of these)
u16  numTables
u16  searchRange      ; binary-search hint, ignored by parsers
u16  entrySelector    ; ditto
u16  rangeShift       ; ditto

numTables × directory entry:
  u32  tag            ; 4 ASCII bytes: 'head', 'cmap', 'GSUB', 'COLR', 'gvar', ...
  u32  checksum
  u32  offset         ; absolute byte offset into the file
  u32  length         ; byte count
```

Then come the tables themselves, at their stated offsets, in any order. `cmap` is the name of *one specific table* — the character-to-glyph mapping. `GSUB` is *one specific table* — glyph substitution rules. Each tag's byte layout is defined by exactly one page of the OpenType spec, e.g. `learn.microsoft.com/.../spec/cmap`.

Parsing a font then means to read the directory, then interpret each table's bytes per its spec. A table that is not in the directory is not present in that font.

Common tables grouped by concern:

- **Administration** — `head`, `maxp`, `name`, `OS/2`, `post`, `hhea`, `hmtx`, `vhea`, `vmtx`
- **Character mapping** — `cmap`
- **TrueType outlines** — `glyf`, `loca`, `cvt`, `fpgm`, `prep` (`cvt`, `fpgm` and `prep` are not parsed; see below)
- **PostScript outlines** — `CFF `, `CFF2`, `VORG`
- **OpenType layout** — `GSUB`, `GPOS`, `GDEF`, `BASE`, `JSTF`, `MATH` (`JSTF` is not parsed; see below)
- **Variations** — `fvar`, `avar`, `gvar`, `cvar`, `HVAR`, `VVAR`, `MVAR`, `STAT`
- **Color** — `COLR`, `CPAL`, `CBDT`, `CBLC`, `sbix`, `SVG `
- **Apple AAT** — `morx`, `mort`, `trak`, `feat`, `ankr`, `kerx`, …

Wrappers reuse the same tables: WOFF is the tables, zlib-compressed, plus a metadata header; WOFF2 is the tables, Brotli-compressed and transformed; TTC is multiple sfnt headers sharing one pool of tables. Of these, only TTC can be opened today; WOFF and WOFF2 files are rejected.

So these "tables" are counting how many distinct tags and byte layouts the parser knows how to interpret. The font file itself may contain more tags that a parser silently skips or it may contain fewer. The parser fails only on the tags it *needs* and doesn't find (or finds and rejects).

Original `zpb-ttf` handled `cmap` format 4 and format 12. It rejects 0, 2, 6, 8, 10, 13, 14. BMP format-0 Mac cmaps were also not handled.

### How is it done

Simply expressed: I have done a Picasso on Google's Fontations.

Fontations is a project to bring memory safety into font parsing after several vulnerabilities in FreeType were found. They achieve this through the use of Rust and always bounds-checked access to data. Their pipeline has three steps, all written in Rust: table text copied from the Microsoft OpenType documentation (`resources/raw_tables`) is run through a small preprocessor into codegen inputs (`resources/codegen_inputs/*.rs`), which their engineers then annotate and fix up by hand; their code generator turns those inputs into the final font-reader and font-writer code. We could have perhaps hacked their code generator to produce Lisp, but it turns out it is easier to take the codegen inputs themselves, since they are a small, uniform Rust-like language. We treat them as text and convert them to symbolic expressions with a relatively simple transformation, which gives the `.sexp` schemas in `schemas/`. The emitter in `codegen/` is a bit more complicated, since we have to interpret that data and emit the actual Lisp code used in the font loader. However, this is done offline as a one-time process; the generated files are checked in.

Why don't we parse the OpenType spec ourselves? There is no machine-readable spec. The spec exists only as HTML prose, which Google copied into text and turned into codegen inputs, partly by hand. That raw text is irregular and needs hand-tuning to process correctly. I leave that to Google engineers, and instead convert their curated codegen inputs, which are uniform and thus simpler to parse. That relieves us also from maintaining a parser for each specific table, which itself is quite a lot of work. We do have to maintain a code generator, but it is simpler than one that would have to process the irregular raw table text. A few things the codegen inputs leave to hand-written Rust in Fontations (for example GPOS value records and fvar instance records) are hand-written here too, in `src/extensions.lisp`.

zpb-otf replaces the parsers for various font tables and data but not the entire zpb-ttf. Only ~25–30% of zpb-ttf is byte-parsing that zpb-otf now generates. The other ~70% is the semantic API that downstream code actually calls: find-glyph, advance-width, contours, do-contours, kerning-offset, string-bounding-box, family-name, the glyph class, the font-loader lifecycle, TTC collection handling, the condition hierarchy, the glyph cache, lazy bounding-box/contour loading. We keep that high-level API code in zpb-ttf, but replace the loading code and the old stream-based parsers for the 12 tables that zpb-ttf already had. Nine of them are now read through zpb-otf's generated parsers; `glyf`, `kern` and `loca` are still decoded by hand in zpb-ttf, although generated `glyf` and `kern` readers exist.

In other words, we are trying to be backwards compatible: the high-level API stays the same, so library clients should not need changes. A few results differ where the old code was wrong: `italic-angle` returns degrees (the old code returned the raw 16.16 bits), components of compound glyphs are returned in file order (the old code reversed them), compound glyphs made only of empty components have no contours, and an out-of-range glyph index signals an error instead of returning `nil`. CFF-based `.otf` fonts, which the old code rejected, now open, but their glyphs have no outlines since there is no charstring interpreter. As a bonus we get more memory-safe parsing, more speed, and a lighter dependency than using FreeType via CFFI bindings, as I believe is the practice in the Common Lisp world. Note that a font-loader is not thread-safe: it fills its glyph cache lazily, so share one between threads only with your own locking. However, it should be lighter on memory usage from multiple threads than FreeType.

### What we don't have

We still don't support WOFF or WOFF2 (zlib and Brotli compression), so those files cannot be opened. Observe also, even if you can read all data, say kerning or hinting data, unless you have code that can use that data, it is still useless. zpb-ttf is not a font renderer, it just loads font files from drive, but it does not do much on its own. You can use it to render fonts, for example via cl-vectors, a path renderer also by Zach, as I have done in Garnet and my X11 demos on top of CLXCB library. In other words, zpb-ttf is enabler, but it is not enough. Currently I don't know of any renderer in Lisp that can make use of hints (you need a virtual machine to interpret them) or charstring interpreter for CFF and CFF2. I do have a 2d graphics library in the works, and I do plan to have full-stack text renderer, but if/when I will implement a hinting vm, I don't know. Some data is though a bit more useful. For example you can use GSUB to decide if you have to pass your data further to say HarfBuzz, or not.

### No streams == no pipes, no sockets

zpb-ttf works on streams, so in theory you could pipe-in or read a font from a socket. However, you probably shouldn't use the stream path anyway.

The reason is that OpenType parsing requires random access across the whole file (table directory, then arbitrary offsets into `glyf`, `loca`, `cmap`, etc.). It means you cannot actually stream a font through a socket into this parser; you must buffer the full byte sequence first. So the natural API for pipes/sockets is to read the bytes into a vector and pass that to `open-font-loader`, which accepts an `(unsigned-byte 8)` vector as well as a pathname or a file stream.

The scenarios where a non-file stream *could* land in `open-font-loader`:

- **CLI tools with stdin redirection** — `myfont-tool < font.ttf`. Plausible but unusual; those callers should read `*standard-input*` to a vector and pass that to `open-font-loader`.

- **Decompressor pipelines** — `woff2_decompress font.woff2 | …` yields a pipe; `file-length` fails. But again, buffering first is the right shape.

- **In-memory streams in tests** — plausible for synthesizing minimal malformed fonts, but a byte vector is the obvious path.

- **Sockets** — really only a font-server design, which this parser isn't shaped for.

So the failure mode is: the API *accepts* any stream and *owns* (closes) it, but on a non-file stream it hard-errors from `file-length` rather than degrading. That's a inconsistency, but I don't think it matters in practice. This could be "considered" in the future, but if you really want, it's ~5 lines and, no you don't need Serapeum for it:

```lisp
(let ((buf (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
      (chunk (make-array 4096 :element-type '(unsigned-byte 8))))
  (loop for n = (read-sequence chunk thing)
        until (zerop n)
        do (loop for i below n do (vector-push-extend (aref chunk i) buf)))
  (open-font-loader buf))
```

(`zpb-otf:make-font-data` itself wants a simple byte vector; `open-font-loader` and `zpb-otf:ensure-font-data` copy an adjustable one like `buf` for you.)
### Not-implemented

#### CVT, fpgm, prep and JSTF (by DeepSeek webchat)

##### What the four tables do

**`JSTF` (Justification Table)** is an OpenType layout table that gives the font developer control over how text is justified. It organizes suggestions by script and language system, listing which `GSUB` or `GPOS` lookups can be enabled or disabled to adjust spacing when a line is flush to both margins. It is a higher-level layout table, not a per-glyph data table, and it depends on `GSUB`/`GPOS` lookup lists being present.

**`cvt ` (Control Value Table)** is a TrueType hinting table. It is a simple array of FWORD (16-bit signed) values that the TrueType instruction stream can reference to control glyph characteristics at different sizes. There is no internal structure—the table is just `FWORD[n]`.

**`fpgm` (Font Program)** is a TrueType hinting table containing bytecode. It is executed once when the font is first used, and its purpose is to define the `FDEF` (function) and `IDEF` (instruction definition) entries that later hinting programs call.

**`prep` (Control Value Program)** is another TrueType bytecode table. It is executed whenever the point size or transformation changes, before any glyph is interpreted, and is used to initialize the CVT and storage areas.

##### Why they have no schema in zpb-otf

All four fall outside the data-oriented table model that the codegen handles:

- **`cvt `, `fpgm`, and `prep` are "raw" tables.** They are either a flat array (`cvt `) or an opaque bytecode blob (`fpgm`, `prep`). There are no records, offsets, or length-prefixed structures to describe; a schema would reduce to `raw-bytes` or `array of FWORD`, which adds nothing over a direct slice. Fontations treats them the same way: they have no codegen input, its table provider returns `fpgm` and `prep` as raw byte slices, and the bytecode is only interpreted by the hinting VM in `skrifa`.

- **`JSTF` is a layout table with a deep dependency on `GSUB`/`GPOS` lookup lists.** Its records are offset-driven and variable-length, and its usefulness requires a shaping engine that understands lookup indices and justification prioritization. It is also rarely present in modern fonts, and Fontations has no `JSTF` reader in read-fonts (and no codegen input for it), which is why there is no schema here.

The 52 table tags with schemas correspond to tables with non-trivial record layouts that benefit from generated accessors. The four above are either trivial to read with a slice (`cvt `, `fpgm`, `prep`) or depend on layout machinery that is not part of zpb-otf's scope (`JSTF`).

##### Where parsers would come from

If you ever want to expose them:

- **`cvt `** needs only a `(font-data-slice fd offset length)` or a small loop over `font-data-read-i16`; no codegen is required.
- **`fpgm` and `prep`** are raw bytecode; a slice is the correct representation, and interpreting the bytes is the job of a hinting VM, not a table parser.
- **`JSTF`** would require either a hand-written reader (like the extensions you already maintain) or a schema that references `GSUB`/`GPOS` lookup-list structures. Given that no shaping engine in Lisp currently consumes `JSTF`, it is reasonable to leave it out of the 52.

##### Fontations

Fontations treats `cvt `, `fpgm`, and `prep` as hinting inputs rather than parsed tables: it stores them as raw bytecode and only decodes them inside `skrifa`'s hinting VM. `JSTF` is not part of Fontations' read-fonts table coverage; the layout focus is on `GSUB`, `GPOS`, `GDEF`, and `BASE`. So the absence of schemas for these four in zpb-otf is consistent with Fontations' own scoping, and the 52-table count remains the accurate set of tables with structured readers.

#### JSTF (by DeepSeek webchat)

JSTF (Justification Table) is an OpenType layout table that gives font developers fine-grained control over how text is justified (flush to both margins). Rather than replacing an application's justification algorithm, it **supplements** it by providing a prioritized list of glyph substitution and positioning actions that the text engine can enable or disable to adjust line length.

##### What JSTF Actually Does

When a line of text is justified, the engine must add or remove space. Simply stretching word spaces can create large gaps or cramped, uneven spacing. JSTF lets the font designer specify more typographically sophisticated adjustments, ranked by priority:

- **Latin scripts**: A JSTF entry might suggest decomposing ligatures (e.g., splitting `fi` back into `f` + `i`) to gain space.
- **Arabic scripts**: It can enable **kashida** (lengthening strokes) or swash forms to justify text without distorting word spacing—a traditional calligraphic solution.
- **General**: Repositioning specific glyphs, expanding space between particular glyph pairs, or applying dedicated justification lookups with maximum adjustment values.

Structurally, JSTF mirrors `GSUB`/`GPOS`: it organizes suggestions by **script** and **language system**, and each suggestion references `GSUB` or `GPOS` lookup indices, plus optionally its own justification lookups.

##### Who Would Need JSTF?

**In practice: almost nobody, today.** The table is part of the OpenType spec but has seen essentially no adoption in fonts or layout engines.

- **Fonts**: Modern fonts almost never include JSTF. (It is often said that the only JSTF-bearing system fonts are some versions of Arial and Times New Roman; I could not find a source confirming that.)
- **Layout engines**: No major shaper (HarfBuzz, CoreText, DirectWrite) is known to implement JSTF-based justification. (A HarfBuzz issue is often quoted on this, but the quoted remark is about Apple's AAT `just` table, not OpenType `JSTF`.) Applications that do sophisticated justification (InDesign, MS Word) use their own proprietary justification engines, not JSTF.
- **Font development tools**: FontTools can read and write JSTF; whether FontForge handles it is unclear (unverified).

**The hypothetical audience** would be a text engine that wants to delegate justification decisions to the font designer rather than hard-coding them—primarily for high-end Arabic typography where kashida justification is culturally expected. But since no such engine exists, the table is effectively dormant.

##### Fontations and JSTF

Fontations does **not** provide a structured JSTF reader. Its `read-fonts` crate focuses on the tables that shaping engines actually consume: `GSUB`, `GPOS`, `GDEF`, `BASE`, variations, color, etc. Its table provider can return the raw bytes for any tag, so JSTF could be parsed manually, but Fontations does not ship a generated or hand-written JSTF parser. This matches the table's real-world status: there is no shaping use case to justify the implementation effort.

##### Summary for zpb-otf

Leaving JSTF out of the 52 schemas is the correct, honest choice. It is a table with no consumer, no meaningful font coverage, and no shaper support—including in Fontations. If you ever wanted to expose it, a raw `font-data-slice` on the JSTF tag would be sufficient until a justification engine actually appears.

