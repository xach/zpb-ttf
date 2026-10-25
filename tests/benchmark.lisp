;;;; benchmark.lisp — Performance benchmarks for zpb-otf and zpb-ttf
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(defpackage #:zpb-benchmark
  (:use #:cl)
  (:export #:run-benchmarks))

(in-package #:zpb-benchmark)

(defparameter *test-fonts-dir*
  (merge-pathnames "fonts/"
                   (make-pathname :directory (pathname-directory *load-truename*))))

(defun test-font-path (filename)
  (merge-pathnames filename *test-fonts-dir*))

(defmacro time-it (iterations &body body)
  "Measure execution time of BODY over ITERATIONS and return elapsed seconds."
  (let ((start (gensym "START"))
        (i (gensym "I")))
    `(let ((,start (get-internal-real-time)))
       (dotimes (,i ,iterations)
         ,@body)
       (/ (float (- (get-internal-real-time) ,start))
          internal-time-units-per-second))))

(defun bench-table-directory-parsing (iterations)
  (format t "~%--- Benchmark 1: Table Directory Parsing (~:D iterations) ---~%" iterations)
  (let* ((font-path (test-font-path "LiberationSans-Regular.ttf"))
         (fd (zpb-otf:ensure-font-data font-path))
         (elapsed (time-it iterations
                    (let ((td (zpb-otf:read-table-directory fd 0)))
                      (dotimes (j (zpb-otf:table-directory-table-records-count td))
                        (let ((rec (zpb-otf:table-directory-table-records-ref td j)))
                          (zpb-otf:table-record-tag rec)
                          (zpb-otf:table-record-offset rec)
                          (zpb-otf:table-record-length rec))))))
         (rate (/ iterations (max elapsed 1e-6))))
    (format t "Elapsed: ~,3F s | Rate: ~,1F directory parses/sec~%" elapsed rate)))

(defun bench-all-tables-parsing (iterations)
  (format t "~%--- Benchmark 2: Parsing All Tables in Font (~:D iterations) ---~%" iterations)
  (let* ((font-path (test-font-path "Cantarell-VF.otf"))
         (fd (zpb-otf:ensure-font-data font-path))
         (td (zpb-otf:read-table-directory fd 0))
         (cff2-off (dotimes (i (zpb-otf:table-directory-table-records-count td))
                     (let ((r (zpb-otf:table-directory-table-records-ref td i)))
                       (when (string= (zpb-otf:table-record-tag r) "CFF2")
                         (return (zpb-otf:table-record-offset r))))))
         (fvar-off (dotimes (i (zpb-otf:table-directory-table-records-count td))
                     (let ((r (zpb-otf:table-directory-table-records-ref td i)))
                       (when (string= (zpb-otf:table-record-tag r) "fvar")
                         (return (zpb-otf:table-record-offset r))))))
         (head-off (dotimes (i (zpb-otf:table-directory-table-records-count td))
                     (let ((r (zpb-otf:table-directory-table-records-ref td i)))
                       (when (string= (zpb-otf:table-record-tag r) "head")
                         (return (zpb-otf:table-record-offset r))))))
         (hhea-off (dotimes (i (zpb-otf:table-directory-table-records-count td))
                     (let ((r (zpb-otf:table-directory-table-records-ref td i)))
                       (when (string= (zpb-otf:table-record-tag r) "hhea")
                         (return (zpb-otf:table-record-offset r))))))
         (maxp-off (dotimes (i (zpb-otf:table-directory-table-records-count td))
                     (let ((r (zpb-otf:table-directory-table-records-ref td i)))
                       (when (string= (zpb-otf:table-record-tag r) "maxp")
                         (return (zpb-otf:table-record-offset r))))))
         (elapsed (time-it iterations
                    (zpb-otf:read-cff2-header fd cff2-off)
                    (zpb-otf:read-fvar fd fvar-off)
                    (zpb-otf:read-head fd head-off)
                    (zpb-otf:read-hhea fd hhea-off)
                    (zpb-otf:read-maxp fd maxp-off)))
         (rate (/ (* iterations 5) (max elapsed 1e-6))))
    (format t "Elapsed: ~,3F s | Rate: ~,1F table reads/sec~%" elapsed rate)))

(defun bench-font-loader-open (iterations)
  (format t "~%--- Benchmark 3: High-Level open-font-loader (~:D iterations) ---~%" iterations)
  (let* ((font-path (test-font-path "LiberationSans-Regular.ttf"))
         (elapsed (time-it iterations
                    (zpb-ttf:open-font-loader font-path)))
         (rate (/ iterations (max elapsed 1e-6))))
    (format t "Elapsed: ~,3F s | Rate: ~,1F font loads/sec~%" elapsed rate)))

(defun bench-glyph-contour-extraction (iterations)
  (let* ((font-path (test-font-path "LiberationSans-Regular.ttf"))
         (loader (zpb-ttf:open-font-loader font-path))
         (glyph-count (zpb-ttf:glyph-count loader))
         (total-glyphs (* iterations glyph-count)))
    (format t "~%--- Benchmark 4: Glyph Contour Extraction (~:D iterations x ~:D glyphs = ~:D glyphs) ---~%"
            iterations glyph-count total-glyphs)
    (let* ((elapsed (time-it iterations
                      (dotimes (gid glyph-count)
                        (let ((g (zpb-ttf:index-glyph gid loader)))
                          (when g
                            (zpb-ttf:contours g)
                            (zpb-ttf:bounding-box g))))))
           (rate (/ total-glyphs (max elapsed 1e-6))))
      (format t "Elapsed: ~,3F s | Rate: ~,1F glyphs/sec~%" elapsed rate))))

(defun bench-string-bounding-box (iterations)
  (let* ((font-path (test-font-path "LiberationSans-Regular.ttf"))
         (loader (zpb-ttf:open-font-loader font-path))
         (text "The quick brown fox jumps over the lazy dog! 0123456789")
         (chars-per-run (length text))
         (total-chars (* iterations chars-per-run)))
    (format t "~%--- Benchmark 5: String Layout & Bounding Box (~:D chars total) ---~%" total-chars)
    (let* ((elapsed (time-it iterations
                      (zpb-ttf:string-bounding-box text loader :kerning t)))
           (rate (/ total-chars (max elapsed 1e-6))))
      (format t "Elapsed: ~,3F s | Rate: ~,1F characters/sec~%" elapsed rate))))

(defun run-benchmarks ()
  (format t "========================================================~%")
  (format t "Running zpb-otf and zpb-ttf Performance Benchmarks~%")
  (format t "========================================================~%")
  (bench-table-directory-parsing 10000)
  (bench-all-tables-parsing 10000)
  (bench-font-loader-open 200)
  (bench-glyph-contour-extraction 5)
  (bench-string-bounding-box 5000)
  (format t "~%========================================================~%")
  (format t "Benchmarks complete!~%")
  (format t "========================================================~%")
  t)

#+sbcl
(when (member (pathname-name *load-truename*) '("benchmark") :test 'string-equal)
  (run-benchmarks))
