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
;;; Loading data from the "vhea" table.
;;;
;;;  https://learn.microsoft.com/en-us/typography/opentype/spec/vhea
;;;  https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6vhea.html
;;;

(in-package #:zpb-ttf)

;;; Tables 'vhea' and 'vmtx' are not present in some fonts. For that reason we
;;; have a fallback where metrics are supplanted with default values based on
;;; horizontal metrics.

(defmethod load-vhea-info ((font-loader font-loader))
  (unless (table-info "vhea" font-loader)
    (setf (vhea-missing-p font-loader) t)
    (let ((dx (/ (max-width font-loader) 2)))
      (with-slots (vascender vdescender)
          font-loader
        (setf vascender dx
              vdescender (- dx))))
    (return-from load-vhea-info))
  (seek-to-table "vhea" font-loader)
  (with-slots (input-stream vascender vdescender)
      font-loader
    (let ((version (read-fixed input-stream)))
      (check-version "\"vhea\" table" version #x00010000 #x00011000))
    (setf vascender (read-fword input-stream)
          vdescender (read-fword input-stream))))

(defmethod vertical-metrics-count ((font-loader font-loader))
  (when (or (vhea-missing-p font-loader)
            (null (table-info "vhea" font-loader)))
    ;; (warn "Table 'vhea' is missing.")
    (setf (vhea-missing-p font-loader) t)
    (return-from vertical-metrics-count))
  (seek-to-table "vhea" font-loader)
  (with-slots (input-stream) font-loader
    ;; Skip to the end, since all we care about is the last item
    (advance-file-position input-stream 34)
    (read-uint16 input-stream)))
