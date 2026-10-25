;;;; zpb-ttf.asd — Access TrueType and OpenType font metrics and outlines
;;;; Copyright (c) 2006 Zachary Beane, 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: BSD-2-Clause

(defpackage #:zpb-ttf-system
  (:use #:cl #:asdf))

(in-package #:zpb-ttf-system)

(defsystem #:zpb-ttf
  :version "2.0.0"
  :author "Zachary Beane <xach@xach.com>, Arthur Miller and contributors"
  :description "Access TrueType and OpenType font metrics and outlines from Common Lisp"
  :license "BSD-2-Clause"
  :depends-on (#:zpb-otf)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "conditions")
                             (:file "bounding-box")
                             (:file "font-loader")
                             (:file "glyph")
                             (:file "font-loader-interface")))))
