;;;; conditions.lisp — Error conditions for OpenType parsing
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(define-condition font-error (error)
  ((message :initarg :message :reader font-error-message :initform "Font error"))
  (:report (lambda (condition stream)
             (format stream "Font error: ~A" (font-error-message condition)))))

(define-condition font-out-of-bounds-error (font-error)
  ((pos    :initarg :pos    :reader font-error-pos)
   (size   :initarg :size   :reader font-error-size)
   (length :initarg :length :reader font-error-length))
  (:report (lambda (condition stream)
             (format stream "Font out-of-bounds: attempted to read ~D bytes at offset ~D in buffer of length ~D"
                     (font-error-size condition)
                     (font-error-pos condition)
                     (font-error-length condition)))))

(define-condition font-format-error (font-error)
  ((table :initarg :table :reader font-error-table :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid font format in table ~A: ~A"
                     (font-error-table condition)
                     (font-error-message condition)))))

(define-condition font-version-error (font-format-error)
  ((expected :initarg :expected :reader font-error-expected)
   (actual   :initarg :actual   :reader font-error-actual))
  (:report (lambda (condition stream)
             (format stream "Unsupported table version in ~A: expected ~A, got ~A"
                     (font-error-table condition)
                     (font-error-expected condition)
                     (font-error-actual condition)))))

(define-condition unsupported-format-error (font-format-error)
  ((format-id :initarg :format-id :reader font-error-format-id))
  (:report (lambda (condition stream)
             (format stream "Unsupported subtable format ~A in ~A"
                     (font-error-format-id condition)
                     (font-error-table condition)))))
