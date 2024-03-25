;;; Copyright (c) 2024 Daniel Kochmański, All Rights Reserved
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
;;;
;;; Loading data from the 'vmtx' table.
;;;
;;;  https://learn.microsoft.com/en-us/typography/opentype/spec/vmtx
;;;  https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6vmtx.html
;;;

(in-package #:zpb-ttf)

;;; Tables 'vhea' and 'vmtx' are not present in some fonts. For that reason we
;;; have a fallback where metrics are supplanted with default values based on
;;; horizontal metrics.

(defmethod load-vmtx-info ((font-loader font-loader))
  (when (or (vhea-missing-p font-loader)
            (null (table-info "vmtx" font-loader)))
    (setf (vmtx-missing-p font-loader) t)
    (let ((line-height (- (ascender font-loader) (descender font-loader))))
      ;; TOP-SIDE-BEARING depends on individual glyph metric YMAX.
      (setf (advance-heights font-loader)
            (make-array 1 :initial-element line-height)))
    (return-from load-vmtx-info))
  (let* ((vertical-metrics-count (vertical-metrics-count font-loader))
         (advance-heights (make-array vertical-metrics-count))
         (top-side-bearings (make-array vertical-metrics-count)))
    (seek-to-table "vmtx" font-loader)
    (with-slots (input-stream) font-loader
      (dotimes (i vertical-metrics-count)
        (setf (svref advance-heights i) (read-uint16 input-stream))
        (setf (svref top-side-bearings i) (read-int16 input-stream))))
    (setf (advance-heights font-loader) advance-heights
          (top-side-bearings font-loader) top-side-bearings)))
