# zpb-ttf / zpb-otf

This repository is a fork [zpb-ttf](https://github.com/xach/zpb-ttf) by Zachary Beane. It provides Common Lisp systems for reading TrueType and OpenType fonts. Two systems are provided, a (hopefully) completely backward compatible zpb-ttf.asd, and a new zpb-otf.asd which should be used by the new code.

`zpb-ttf` is the familiar high-level interface, as provided originally by zpb-ttf. You need if you want to load a font file or a TrueType collection and get its metrics, names, character mapping, kerning and glyph outlines, with the same API the existing zpb-ttf programs already use. Version 2 tries to keep the same high-level API but reads everything through the new `zpb-otf` backend instead of a hand-written stream parser.

`zpb-otf` is the low-level layer underneath. It loads the font into memory once and reads tables straight out of the byte buffer, without copying, and every read is bounds-checked, so a truncated or malicious file signals a `font-out-of-bounds-error` rather than reading garbage. The table accessors are generated: each OpenType table is described as an s-expression schema in `schemas/` (derived from the table definitions in the Fontations project). The code generator in `codegen/` turns those into the Lisp files in `src/tables/`. That covers the core TrueType tables as well as the layout, variation, color, bitmap and Apple tables, and a few layouts whose shape depends on the data itself are decoded by hand in `src/extensions.lisp`. You should not edit the generated files directly; change the schema or the emitter and run `make codegen`.

A typical use looks like this:

```lisp
(zpb-ttf:with-font-loader (font "LiberationSans-Regular.ttf")
  (let ((glyph (zpb-ttf:find-glyph #\A font)))
    (values (zpb-ttf:family-name font)
            (zpb-ttf:advance-width glyph)
            (zpb-ttf:bounding-box glyph))))
```

Both systems load with ASDF, and `zpb-ttf` pulls in `zpb-otf` automatically. The Makefile has targets for the everyday tasks: `make test` runs the test suite against the fonts in `tests/fonts/`, `make bench` runs the benchmarks, and `make codegen` regenerates the table code from the schemas. The original zpb-ttf reference manual is in `doc/index.html`.

The zpb-ttf part is distributed under the BSD 2-Clause license and the zpb-otf part under the MIT license; `LICENSE` has both notices. The test fonts come with their own licenses, described in `tests/fonts/README.md`.
