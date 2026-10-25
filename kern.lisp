;;; Copyright (c) 2006 Zachary Beane, All Rights Reserved
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;; "kern" table functions
;;;
;;;   https://docs.microsoft.com/en-us/typography/opentype/spec/kern
;;;   http://developer.apple.com/fonts/TTRefMan/RM06/Chap6kern.html
;;;
;;; $Id: kern.lisp,v 1.8 2006/03/28 14:38:37 xach Exp $

(in-package #:zpb-ttf)

(defun load-kerning-format-0 (table stream)
  "Return a hash table keyed on a UINT32 key that represents the glyph
index in the left and right halves with a value of the kerning
distance between the pair."
  (let ((pair-count (read-uint16 stream))
        (search-range (read-uint16 stream))
        (entry-selector (read-uint16 stream))
        (range-shift (read-uint16 stream))
        (bytes-read 8))
    (declare (ignore search-range entry-selector range-shift))
    (dotimes (i pair-count)
      (let ((key (read-uint32 stream))
            (value (read-int16 stream)))
        ;; apple specifies a terminating entry, ignore it
        (unless (and (= key #xffffffff) (= value 0))
          (setf (gethash key table) value))
        (incf bytes-read 6)))
    bytes-read))

(defun parse-offset-table (buffer start)
  (let ((first-glyph (aref buffer start))
        (glyph-count (aref buffer (1+ start)))
        (offsets (make-hash-table)))
    (loop for i from (+ start 2)
          for g from first-glyph
          repeat glyph-count
          collect (setf (gethash g offsets) (aref buffer i)))
    offsets))

(defun load-kerning-format-2 (table stream size)
  "Return a hash table keyed on a UINT32 key that represents the glyph
index in the left and right halves with a value of the kerning
distance between the pair."
  (let* ((buffer (coerce (loop repeat (/ size 2)
                               collect (read-uint16 stream))
                         '(simple-array (unsigned-byte) 1)))
         (row-width (aref buffer 0))
         (left-offset-table (aref buffer 1))
         (right-offset-table (aref buffer 2))
         (array-offset (aref buffer 3))
         (left (parse-offset-table buffer (- (/ left-offset-table 2) 4)))
         (right (parse-offset-table buffer (- (/ right-offset-table 2) 4))))
    (declare (ignorable row-width array-offset))
    (flet ((s16 (x)
             (if (logbitp 15 x)
                 (1- (- (logandc2 #xFFFF x)))
                 x)))
      (maphash (lambda (lk lv)
                 (maphash (lambda (rk rv)
                            (let ((key (logior (ash lk 16) rk))
                                  (value (s16 (aref buffer
                                                    (- (/ (+ lv rv) 2) 4)))))
                              (unless (zerop value)
                                (setf (gethash key table) value))))
                          right))
               left))
    size))

(defmethod load-kerning-subtable ((font-loader font-loader) format size)
  (when (/= format 0 1 2)
    (error 'unsupported-format
           :description "kerning subtable"
           :size 1
           :expected-values (list 0 1 2)
           :actual-value format))
  (case format
    (0
     (load-kerning-format-0 (kerning-table font-loader)
                            (input-stream font-loader)))
    (1
     ;; state table for contextual kerning, ignored for now
     (advance-file-position (input-stream font-loader) (- size 8))
     (- size 8))
    (2
     (load-kerning-format-2 (kerning-table font-loader)
                            (input-stream font-loader)
                            size))))

(defmethod load-kern-info ((font-loader font-loader))
  (when (table-exists-p "kern" font-loader)
    (seek-to-table "kern" font-loader)
    (let* ((stream (input-stream font-loader))
           (maybe-version (read-uint16 stream))
           (maybe-table-count (read-uint16 stream))
           (version 0)
           (table-count 0)
           (apple-p nil))

      ;; These shenanegins are because Apple documents one style of
      ;; kern table and Microsoft documents another. This code
      ;; tries to support both.
      ;; See:
      ;;  https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6kern.html
      ;;  https://learn.microsoft.com/en-us/typography/opentype/spec/kern
      (if (zerop maybe-version)
          (setf version maybe-version
                table-count maybe-table-count)
          (setf version (logand (ash maybe-version 16) maybe-table-count)
                table-count (read-uint32 stream)
                apple-p t))
      (check-version "\"kern\" table" version 0)
      (dotimes (i table-count)
        ;; Each subtable starts with a header whose coverage field says
        ;; which format the subtable is in and what kind of kerning it
        ;; holds.  Microsoft and Apple put its two bytes in opposite
        ;; orders, and their headers differ in size.
        (multiple-value-bind (length format horizontal-p header-size)
            (if apple-p
                ;; Apple: a 32-bit length; coverage with the flags in the
                ;; high byte (#x80 vertical, #x40 cross-stream, #x20
                ;; variation) and the format in the low byte; a tuple
                ;; index.
                (let* ((length (read-uint32 stream))
                       (flags (read-uint8 stream))
                       (format (read-uint8 stream)))
                  (read-uint16 stream)  ; tuple index
                  (values length format (zerop (logand flags #xE0)) 8))
                ;; Microsoft: a version and a 16-bit length; coverage
                ;; with the format in the high byte and the flags in the
                ;; low byte (bit 0 horizontal, bit 1 minimum values, bit
                ;; 2 cross-stream).
                (let* ((version (read-uint16 stream))
                       (length (read-uint16 stream))
                       (format (read-uint8 stream))
                       (flags (read-uint8 stream)))
                  (declare (ignore version))
                  (values length format (= (logand flags #b111) 1) 6)))
          ;; Only horizontal kerning is read: storing other kinds in the
          ;; same table would be confusing, and vertical layout is not
          ;; supported.
          (let ((bytes-read (if horizontal-p
                                (+ (load-kerning-subtable font-loader format
                                                          length)
                                   header-size)
                                header-size)))
            ;; A Microsoft subtable's length is 16 bits, too small for a
            ;; subtable of more than 10920 pairs, so fonts with one store
            ;; it modulo 65536; the pairs read are then all there is.
            (when (> length bytes-read)
              (advance-file-position stream (- length bytes-read)))))))))

(defmethod all-kerning-pairs ((font-loader font-loader))
  (let ((pairs nil))
    (maphash (lambda (k v)
               (let* ((left-index (ldb (byte 16 16) k))
                      (right-index (ldb (byte 16 0) k))
                      (left (index-glyph left-index font-loader))
                      (right (index-glyph right-index font-loader)))
                 (push (list left right v) pairs)))
             (kerning-table font-loader))
    pairs))
