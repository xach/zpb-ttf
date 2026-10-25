;;;; zpb-otf.asd — OpenType font parsing system for Common Lisp
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(asdf:defsystem #:zpb-otf/runtime
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "Runtime structures, buffer reading, and base condition classes for zpb-otf"
  :license "MIT"
  :depends-on (#:alexandria)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:module "runtime"
                              :serial t
                              :components ((:file "package")
                                           (:file "conditions")
                                           (:file "font-data")
                                           (:file "table-ref")))))))

(asdf:defsystem #:zpb-otf/codegen
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "OpenType table code generator from s-expression schema"
  :license "MIT"
  :depends-on (#:zpb-otf/runtime)
  :serial t
  :components ((:module "codegen"
                :serial t
                :components ((:file "package")
                             (:file "emitter")
                             (:file "generator")))))

(asdf:defsystem #:zpb-otf
  :version "0.1.0"
  :author "Arthur Miller and contributors"
  :description "Zero-copy, bounds-checked OpenType table parser generated from Microsoft OpenType spec"
  :license "MIT"
  :depends-on (#:zpb-otf/runtime)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:module "tables"
                              ;; Low-level tables first, shared layout/variation structures before their users.
                              :serial t
                              :components (
               (:file "font")
               (:file "head")
               (:file "maxp")
               (:file "hhea")
               (:file "hmtx")
               (:file "vhea")
               (:file "vmtx")
               (:file "name")
               (:file "os2")
               (:file "post")
               (:file "cmap")
               (:file "glyf")
               (:file "gasp")
               (:file "hdmx")
               (:file "dsig")
               (:file "meta")
               (:file "ltag")
               (:file "vorg")
               (:file "cpal")
               (:file "svg")
               (:file "cff")
               (:file "cff2")
               (:file "bitmap")
               (:file "cblc")
               (:file "cbdt")
               (:file "eblc")
               (:file "ebdt")
               (:file "sbix")
               (:file "fvar")
               (:file "variations")
               (:file "hvar")
               (:file "vvar")
               (:file "mvar")
               (:file "cvar")
               (:file "gvar")
               (:file "stat")
               (:file "layout")
               (:file "aat")
               (:file "ankr")
               (:file "feat")
               (:file "trak")
               (:file "kerx")
               (:file "kern")
               (:file "mort")
               (:file "morx")
               (:file "gpos")
               (:file "gsub")
               (:file "gdef")
               (:file "base")
               (:file "math")
               (:file "avar")
               (:file "colr")
               (:file "varc")
               (:file "ift")
                              ))
                             (:file "extensions")))))
