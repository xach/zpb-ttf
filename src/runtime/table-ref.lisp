;;;; table-ref.lisp — Zero-copy table reference base structures
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(defstruct (table-ref (:constructor %make-table-ref (%table-font-data %table-offset %table-length)))
  "Base struct for all zero-copy OpenType table and subtable views."
  (%table-font-data nil :type font-data :read-only t)
  (%table-offset    0   :type (unsigned-byte 32) :read-only t)
  (%table-length    0   :type (unsigned-byte 32) :read-only t))

(declaim (inline table-ref-font-data table-ref-offset table-ref-length))
(defun table-ref-font-data (tr)
  (table-ref-%table-font-data tr))

(defun table-ref-offset (tr)
  (table-ref-%table-offset tr))

(defun table-ref-length (tr)
  (table-ref-%table-length tr))

(defstruct (record-ref (:constructor %make-record-ref (%record-font-data %record-offset)))
  "Base struct for inline table record views."
  (%record-font-data nil :type font-data :read-only t)
  (%record-offset    0   :type (unsigned-byte 32) :read-only t))

(declaim (inline record-ref-font-data record-ref-offset))
(defun record-ref-font-data (rec)
  (record-ref-%record-font-data rec))

(defun record-ref-offset (rec)
  (record-ref-%record-offset rec))

(declaim (inline resolve-offset16 resolve-offset32))
(defun resolve-offset16 (base-fd base-offset rel-offset)
  "Resolve a 16-bit offset relative to BASE-OFFSET within BASE-FD."
  (declare (ignore base-fd) (type (unsigned-byte 32) base-offset rel-offset))
  (the (unsigned-byte 32) (+ base-offset rel-offset)))

(defun resolve-offset32 (base-fd base-offset rel-offset)
  "Resolve a 32-bit offset relative to BASE-OFFSET within BASE-FD."
  (declare (ignore base-fd) (type (unsigned-byte 32) base-offset rel-offset))
  (the (unsigned-byte 32) (+ base-offset rel-offset)))

(declaim (inline resolve-nullable-offset16 resolve-nullable-offset32))
(defun resolve-nullable-offset16 (base-fd base-offset rel-offset)
  (declare (ignore base-fd) (type (unsigned-byte 32) base-offset rel-offset))
  (if (zerop rel-offset)
      nil
      (+ base-offset rel-offset)))

(defun resolve-nullable-offset32 (base-fd base-offset rel-offset)
  (declare (ignore base-fd) (type (unsigned-byte 32) base-offset rel-offset))
  (if (zerop rel-offset)
      nil
      (+ base-offset rel-offset)))
