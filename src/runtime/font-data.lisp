;;;; font-data.lisp — Zero-copy, bounds-checked binary reader primitives
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf)

(defmacro define-constant (name value &optional doc)
  "Define a constant using alexandria:define-constant with :test 'equal."
  `(alexandria:define-constant ,name ,value :test 'equal ,@(when doc (list :documentation doc))))

;; Fixed-point scales. Each is 2^N where N is the number of fractional bits
;; in the format: Fixed / Version16Dot16 is 16.16 (N = 16), F2Dot14 is 2.14
;; (N = 14). Written as float literals so the compiler can fold them and
;; the readers avoid producing rationals.
(define-constant +fixed-scale+ 65536.0f0
  "Divisor that converts a 16.16 (Fixed, Version16Dot16) integer to a
single-float.")

(define-constant +f2dot14-scale+ 16384.0f0
  "Divisor that converts a 2.14 (F2Dot14) integer to a single-float.")

(defstruct (font-data (:constructor %make-font-data (bytes offset length)))
  "Zero-copy window into an in-memory font byte vector."
  (bytes  nil :type (simple-array (unsigned-byte 8) (*)) :read-only t)
  (offset 0   :type (unsigned-byte 32) :read-only t)
  (length 0   :type (unsigned-byte 32) :read-only t))

(defun make-font-data (bytes &key (offset 0) (length (- (length bytes) offset)))
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type (unsigned-byte 32) offset length))
  (unless (<= (+ offset length) (length bytes))
    (error 'font-out-of-bounds-error :pos offset :size length :length (length bytes)))
  (%make-font-data bytes offset length))

(declaim (inline font-data-slice))
(defun font-data-slice (fd sub-offset &optional (sub-length (- (font-data-length fd) sub-offset)))
  "Return a child FONT-DATA window starting at SUB-OFFSET relative to FD."
  (declare (type font-data fd)
           (type (unsigned-byte 32) sub-offset sub-length)
           (optimize (speed 3) (safety 1)))
  (let ((parent-len (font-data-length fd)))
    (unless (<= (+ sub-offset sub-length) parent-len)
      (error 'font-out-of-bounds-error :pos sub-offset :size sub-length :length parent-len))
    (%make-font-data (font-data-bytes fd)
                     (the (unsigned-byte 32) (+ (font-data-offset fd) sub-offset))
                     sub-length)))

(declaim (inline font-data-valid-range-p))
(defun font-data-valid-range-p (fd pos size)
  "Return T if POS + SIZE is within the bounds of FD. POS and SIZE may be any
non-negative integers, such as sizes computed from untrusted counts."
  (declare (type font-data fd)
           (type unsigned-byte pos size)
           (optimize (speed 3) (safety 1)))
  (let ((len (font-data-length fd)))
    (and (<= pos len) (<= size (- len pos)))))

(defun ensure-font-data (source)
  "Convert a pathname, filespec, or byte vector into a FONT-DATA object."
  (cond
    ((font-data-p source)
     source)
    ((typep source '(simple-array (unsigned-byte 8) (*)))
     (make-font-data source))
    ((typep source '(array (unsigned-byte 8) (*)))
     (let* ((len (length source))
            (copy (make-array len :element-type '(unsigned-byte 8))))
       (replace copy source)
       (make-font-data copy)))
    ((or (pathnamep source) (stringp source))
     (with-open-file (stream source :direction :input :element-type '(unsigned-byte 8))
       (let* ((len (file-length stream))
              (buf (make-array len :element-type '(unsigned-byte 8))))
         (read-sequence buf stream)
         (make-font-data buf))))
    (t
     (error 'font-error :message (format nil "Cannot convert ~S into FONT-DATA" source)))))

;;; ============================================================================
;;; Big-Endian Scalar Readers
;;; ============================================================================

(declaim (inline font-data-read-u8))
(defun font-data-read-u8 (fd pos)
  (declare (type font-data fd) (type (unsigned-byte 32) pos) (optimize (speed 3) (safety 1)))
  (unless (< pos (font-data-length fd))
    (error 'font-out-of-bounds-error :pos pos :size 1 :length (font-data-length fd)))
  (aref (font-data-bytes fd) (+ (font-data-offset fd) pos)))

(declaim (inline font-data-read-i8))
(defun font-data-read-i8 (fd pos)
  (declare (type font-data fd) (type (unsigned-byte 32) pos) (optimize (speed 3) (safety 1)))
  (let ((u (font-data-read-u8 fd pos)))
    (if (logbitp 7 u)
        (the fixnum (- u 256))
        (the fixnum u))))

(declaim (inline font-data-read-u16))
(defun font-data-read-u16 (fd pos)
  (declare (type font-data fd) (type (unsigned-byte 32) pos) (optimize (speed 3) (safety 1)))
  (let ((abs-pos (+ (font-data-offset fd) pos))
        (bytes (font-data-bytes fd)))
    (unless (<= (+ pos 2) (font-data-length fd))
      (error 'font-out-of-bounds-error :pos pos :size 2 :length (font-data-length fd)))
    (the (unsigned-byte 16)
         (logior (the (unsigned-byte 16) (ash (aref bytes abs-pos) 8))
                 (aref bytes (the (unsigned-byte 32) (+ abs-pos 1)))))))

(declaim (inline font-data-read-i16))
(defun font-data-read-i16 (fd pos)
  (declare (type font-data fd) (type (unsigned-byte 32) pos) (optimize (speed 3) (safety 1)))
  (let ((u (font-data-read-u16 fd pos)))
    (if (logbitp 15 u)
        (the fixnum (- u 65536))
        (the fixnum u))))

(declaim (inline font-data-read-u24))
(defun font-data-read-u24 (fd pos)
  (declare (type font-data fd) (type (unsigned-byte 32) pos) (optimize (speed 3) (safety 1)))
  (let ((abs-pos (+ (font-data-offset fd) pos))
        (bytes (font-data-bytes fd)))
    (unless (<= (+ pos 3) (font-data-length fd))
      (error 'font-out-of-bounds-error :pos pos :size 3 :length (font-data-length fd)))
    (the (unsigned-byte 24)
         (logior (the (unsigned-byte 24) (ash (aref bytes abs-pos) 16))
                 (the (unsigned-byte 24) (ash (aref bytes (the (unsigned-byte 32) (+ abs-pos 1))) 8))
                 (aref bytes (the (unsigned-byte 32) (+ abs-pos 2)))))))

(declaim (inline font-data-read-i24))
(defun font-data-read-i24 (fd pos)
  (declare (type font-data fd) (type (unsigned-byte 32) pos) (optimize (speed 3) (safety 1)))
  (let ((u (font-data-read-u24 fd pos)))
    (if (logbitp 23 u)
        (the fixnum (- u #x1000000))
        (the fixnum u))))

(declaim (inline font-data-read-u32))
(defun font-data-read-u32 (fd pos)
  (declare (type font-data fd) (type (unsigned-byte 32) pos) (optimize (speed 3) (safety 1)))
  (let ((abs-pos (+ (font-data-offset fd) pos))
        (bytes (font-data-bytes fd)))
    (unless (<= (+ pos 4) (font-data-length fd))
      (error 'font-out-of-bounds-error :pos pos :size 4 :length (font-data-length fd)))
    (the (unsigned-byte 32)
         (logior (the (unsigned-byte 32) (ash (aref bytes abs-pos) 24))
                 (the (unsigned-byte 32) (ash (aref bytes (the (unsigned-byte 32) (+ abs-pos 1))) 16))
                 (the (unsigned-byte 32) (ash (aref bytes (the (unsigned-byte 32) (+ abs-pos 2))) 8))
                 (aref bytes (the (unsigned-byte 32) (+ abs-pos 3)))))))

(declaim (inline font-data-read-i32))
(defun font-data-read-i32 (fd pos)
  (declare (type font-data fd) (type (unsigned-byte 32) pos) (optimize (speed 3) (safety 1)))
  (let ((u (font-data-read-u32 fd pos)))
    (if (logbitp 31 u)
        (- u #x100000000)
        u)))

(declaim (inline font-data-read-fixed))
(defun font-data-read-fixed (fd pos)
  "Read a 16.16 Fixed point number."
  (declare (type font-data fd) (type (unsigned-byte 32) pos))
  (/ (font-data-read-i32 fd pos) +fixed-scale+))

(declaim (inline font-data-read-f2dot14))
(defun font-data-read-f2dot14 (fd pos)
  "Read a 2.14 Fixed point number."
  (declare (type font-data fd) (type (unsigned-byte 32) pos))
  (/ (font-data-read-i16 fd pos) +f2dot14-scale+))

(declaim (inline font-data-read-fword))
(defun font-data-read-fword (fd pos)
  (font-data-read-i16 fd pos))

(declaim (inline font-data-read-ufword))
(defun font-data-read-ufword (fd pos)
  (font-data-read-u16 fd pos))

(declaim (inline font-data-read-tag))
(defun font-data-read-tag (fd pos)
  "Read 4-byte OpenType tag as an integer."
  (font-data-read-u32 fd pos))

(defun font-data-read-tag-string (fd pos)
  "Read 4-byte OpenType tag as a string."
  (declare (type font-data fd) (type (unsigned-byte 32) pos))
  (let ((s (make-string 4)))
    (dotimes (i 4 s)
      (setf (schar s i) (code-char (font-data-read-u8 fd (+ pos i)))))))

(defun font-data-read-long-date-time (fd pos)
  "Read 64-bit integer representing seconds since 1904-01-01 GMT."
  (declare (type font-data fd) (type (unsigned-byte 32) pos))
  (let ((hi (font-data-read-u32 fd pos))
        (lo (font-data-read-u32 fd (+ pos 4))))
    (logior (ash hi 32) lo)))

(declaim (inline font-data-read-u64))
(defun font-data-read-u64 (fd pos)
  (font-data-read-long-date-time fd pos))
