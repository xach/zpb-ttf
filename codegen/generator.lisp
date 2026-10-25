;;;; generator.lisp — Driver for generating OpenType Common Lisp tables from S-Expressions
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf/codegen)

(defun read-sexp-file (pathname)
  "Read all top-level s-expression forms from PATHNAME."
  (with-open-file (stream pathname :direction :input :element-type 'character)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun generate-table-file (input-pathname output-pathname)
  "Read an individual .sexp spec file and emit the corresponding .lisp file."
  (let ((items (read-sexp-file input-pathname))
        (fname (file-namestring output-pathname)))
    (ensure-directories-exist output-pathname)
    (with-open-file (stream output-pathname :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create
                                           :element-type 'character)
      (emit-file-forms items stream fname))))

(defun generate-all-tables (input-dir output-dir)
  "Read all .sexp spec files in INPUT-DIR and generate all table definitions in OUTPUT-DIR."
  (let* ((input-wildcard (merge-pathnames "*.sexp" (pathname (format nil "~A/" (namestring input-dir)))))
         (spec-files (sort (directory input-wildcard) #'string< :key #'file-namestring))
         (parsed-map (make-hash-table :test 'equal))
         (generated-files '()))

    (format t "Step 1: Reading ~D .sexp spec files and building symbol registries...~%" (length spec-files))
    (reset-registries)
    (dolist (f spec-files)
      (let ((items (read-sexp-file f)))
        (setf (gethash (file-namestring f) parsed-map) items)
        (dolist (item items)
          (register-sexp-item item))))

    (format t "Step 2: Emitting Common Lisp table definitions into ~A via pprint...~%" output-dir)
    (ensure-directories-exist (format nil "~A/" (namestring output-dir)))
    (dolist (f spec-files)
      (let* ((sexp-name (file-namestring f))
             (base-name (pathname-name f))
             (*module-name* base-name)
             (out-file (merge-pathnames (format nil "~A.lisp" base-name)
                                        (pathname (format nil "~A/" (namestring output-dir)))))
             (items (gethash sexp-name parsed-map)))
        (with-open-file (stream out-file :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create
                                         :element-type 'character)
          (emit-file-forms items stream (file-namestring out-file)))
        (push (file-namestring out-file) generated-files)
        (format t "  Generated ~A~%" (file-namestring out-file))))

    (setf generated-files (nreverse generated-files))

    (format t "Generation complete!~%")
    generated-files))
