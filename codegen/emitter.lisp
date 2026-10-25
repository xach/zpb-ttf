;;;; emitter.lisp — S-Expression to Common Lisp table compiler
;;;; Copyright (C) 2026 Arthur Miller and contributors
;;;; SPDX-License-Identifier: MIT

(in-package #:zpb-otf/codegen)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :zpb-otf/schema)
    (make-package :zpb-otf/schema :use '(:cl)))
  (export (mapcar (lambda (s) (intern s :zpb-otf/schema))
                  '("TABLE" "RECORD" "FORMAT" "FLAGS" "ENUM" "FIELD" "VARIANT" "ARRAY"))
          :zpb-otf/schema))

(defvar *known-tables* (make-hash-table :test 'equal))
(defvar *known-records* (make-hash-table :test 'equal))
(defvar *known-formats* (make-hash-table :test 'equal))
(defvar *known-enums* (make-hash-table :test 'equal))
(defvar *known-flags* (make-hash-table :test 'equal))
(defvar *known-externs* (make-hash-table :test 'equal)
  "Types declared (extern NAME): variable-size, decoded by hand-written code.")
(defvar *record-size-cache* (make-hash-table :test 'equal))
(defvar *defined-names* (make-hash-table :test 'eq)
  "Function and constant names emitted so far in this run, mapped to their file.")
(defvar *exports* '())
(defvar *module-name* nil)
(defvar *visiting-records* '())

(defun reset-registries ()
  (clrhash *known-tables*)
  (clrhash *known-records*)
  (clrhash *known-formats*)
  (clrhash *known-enums*)
  (clrhash *known-flags*)
  (clrhash *known-externs*)
  (clrhash *record-size-cache*)
  (clrhash *defined-names*))

(defun to-kebab-name (thing)
  "Convert symbols, strings, camelCase, snake_case, and kebab-case identifiers to uppercase kebab-case."
  (when thing
    (let ((str (string thing)))
      (with-output-to-string (s)
        (loop for prev = nil then c
              for i from 0
              for c across str
              do
                 (cond
                   ((char= c #\_) (write-char #\- s))
                   ((and prev
                         (or (and (lower-case-p prev) (upper-case-p c))
                             (and (upper-case-p prev) (upper-case-p c)
                                  (< (1+ i) (length str))
                                  (lower-case-p (char str (1+ i))))))
                    (write-char #\- s)
                    (write-char (char-upcase c) s))
                   (t
                    (write-char (char-upcase c) s))))))))

(defun zsym (name)
  (intern (to-kebab-name name) :zpb-otf))

(defparameter *primitive-types*
  '((:u8              :size 1 :reader font-data-read-u8)
    (:i8              :size 1 :reader font-data-read-i8)
    (:u16             :size 2 :reader font-data-read-u16)
    (:i16             :size 2 :reader font-data-read-i16)
    (:u24             :size 3 :reader font-data-read-u24)
    (:i24             :size 3 :reader font-data-read-i24)
    (:glyph-id24      :size 3 :reader font-data-read-u24)
    (:u32             :size 4 :reader font-data-read-u32)
    (:i32             :size 4 :reader font-data-read-i32)
    (:fixed           :size 4 :reader font-data-read-fixed)
    (:version16-dot16 :size 4 :reader font-data-read-fixed)
    (:f2-dot14        :size 2 :reader font-data-read-f2dot14)
    (:f-word          :size 2 :reader font-data-read-fword)
    (:uf-word         :size 2 :reader font-data-read-ufword)
    (:glyph-id16      :size 2 :reader font-data-read-u16)
    (:name-id         :size 2 :reader font-data-read-u16)
    (:tag             :size 4 :reader font-data-read-tag-string)
    (:major-minor     :size 4 :reader font-data-read-u32)
    (:long-date-time  :size 8 :reader font-data-read-u64)))

(defparameter *type-aliases*
  '(("UINT8" . :u8) ("UINT16" . :u16) ("UINT24" . :u24) ("UINT32" . :u32)
    ("INT8" . :i8) ("INT16" . :i16) ("INT32" . :i32)
    ("OFFSET16" . :u16) ("OFFSET24" . :u24) ("OFFSET32" . :u32)
    ("INT24" . :i24)
    ("VERSION16DOT16" . :fixed) ("LONGDATETIME" . :long-date-time)
    ;; Named types that are plain integers on disk.
    ("TUPLE-VARIATION-COUNT" . :u16) ("TUPLE-INDEX" . :u16)
    ("MATCH-MODE-AND-COUNT" . :u8))
  "Spec spellings of primitive types, mapped onto *PRIMITIVE-TYPES* keys.")

(defun resolve-type-info (type-sym)
  (when (and type-sym (symbolp type-sym))
    (let* ((name (to-kebab-name type-sym))
           (key (or (cdr (assoc name *type-aliases* :test #'string=))
                    (intern name :keyword)))
           (prim (cdr (assoc key *primitive-types*))))
      (if prim
          prim
          (let ((underlying (or (gethash name *known-enums*)
                                (gethash name *known-flags*))))
            (when underlying
              (resolve-type-info underlying)))))))

(defun offset-type-info (offset-sym)
  (let ((name (to-kebab-name offset-sym)))
    (cond
      ((search "32" name) (values 4 'font-data-read-u32))
      ((search "24" name) (values 3 'font-data-read-u24))
      (t (values 2 'font-data-read-u16)))))

(defun compute-record-size (rec-sym)
  (when (symbolp rec-sym)
    (let* ((rec-key (to-kebab-name rec-sym))
           (rec (gethash rec-key *known-records*)))
      (if (null rec)
          nil
          (if (member rec-key *visiting-records* :test #'string=)
              (error "Cyclic record dependency detected in record ~A: ~{~A~^ -> ~}"
                     rec-key (reverse (cons rec-key *visiting-records*)))
              (let ((*visiting-records* (cons rec-key *visiting-records*))
                    (total 0))
                (dolist (f (cddr rec) total)
                  (when (and (consp f) (string-equal (car f) "field"))
                    (let* ((ftype (third f))
                           (sz (cond
                                 ((consp ftype)
                                  (let ((ftype-head (to-kebab-name (car ftype))))
                                    (cond
                                      ((string-equal ftype-head "ARRAY")
                                       (let ((cnt (get-prop :count (cdddr f)))
                                             (el (second ftype)))
                                         (if (numberp cnt)
                                             (let ((esz (or (getf (resolve-type-info el) :size)
                                                            (resolve-record-size el))))
                                               (if esz (* cnt esz) nil))
                                             nil)))
                                      ((search "OFFSET" ftype-head) (nth-value 0 (offset-type-info (car ftype))))
                                      (t nil))))
                                 (t
                                  (let ((pinfo (resolve-type-info ftype)))
                                    (if pinfo
                                        (getf pinfo :size)
                                        (resolve-record-size ftype)))))))
                      (if sz
                          (incf total sz)
                          (return-from compute-record-size nil)))))))))))

(defun resolve-record-size (rec-sym)
  (when (symbolp rec-sym)
    (let ((key (to-kebab-name rec-sym)))
      (or (gethash key *record-size-cache*)
          ;; Only cache successes: a NIL may be due to a record that is
          ;; registered later, or to an in-progress cycle.
          (let ((size (compute-record-size rec-sym)))
            (when size
              (setf (gethash key *record-size-cache*) size)))))))

(defun array-type-p (ftype)
  "True for list array specs: (ARRAY elem), (ARRAY-OF elem), (SEQUENCE elem), (REPEATED elem)."
  (and (consp ftype)
       (member (to-kebab-name (car ftype)) '("ARRAY" "ARRAY-OF" "SEQUENCE" "REPEATED")
               :test #'string=)))

(defun computed-array-element (ftype)
  "For COMPUTED-ARRAY<ELEM<A>> and VAR-LEN-ARRAY<ELEM<A>> symbols, return ELEM as a symbol."
  (when (and ftype (symbolp ftype))
    (let* ((s (symbol-name ftype))
           (lt (position #\< s)))
      (when (and lt (member (subseq s 0 lt) '("COMPUTED-ARRAY" "VAR-LEN-ARRAY")
                            :test #'string-equal))
        (let* ((inner (string-right-trim ">" (subseq s (1+ lt))))
               (lt2 (position #\< inner))
               (elem (if lt2 (subseq inner 0 lt2) inner)))
          (when (member elem '("ARRAY" "COMPUTED-ARRAY" "VAR-LEN-ARRAY") :test #'string-equal)
            (error "Nested computed arrays are not supported: ~S" ftype))
          (intern elem (symbol-package ftype)))))))

(defun static-element-size (elem-type)
  (or (getf (resolve-type-info elem-type) :size)
      (resolve-record-size elem-type)))

(defun normalize-field-type (ftype)
  "Rewrite array spellings to (ARRAY elem). Computed arrays of statically
sized elements become plain arrays; others are returned as (:COMPUTED elem)."
  (cond
    ((array-type-p ftype) `(array ,@(cdr ftype)))
    ((computed-array-element ftype)
     (let ((elem (computed-array-element ftype)))
       (if (static-element-size elem)
           `(array ,elem)
           `(:computed ,elem))))
    (t ftype)))

(defun computed-type-p (ftype)
  (and (consp ftype) (eq (car ftype) :computed)))

(defun defined-names (forms)
  (loop for f in forms
        when (and (consp f) (eq (car f) 'defun)) collect (second f)))

(defun filter-declaims (declaims accessor-forms)
  (let ((defined (defined-names accessor-forms)))
    (remove-if-not (lambda (n) (member n defined)) declaims)))

(defun format-variant-value (target)
  "Discriminator value from the :FORMAT attribute of TARGET's fields, if any.
Only numeric :FORMAT values count; variants whose names end in digits use those instead."
  (let ((tbl (gethash (to-kebab-name target) *known-tables*)))
    (when tbl
      (loop for f in (cddr tbl)
            for v = (and (consp f) (string-equal (car f) "field")
                         (get-prop :format (cdddr f)))
            when (numberp v) return v))))

(defun register-sexp-item (form)
  (when (consp form)
    (let ((kind (string-upcase (string (car form)))))
      (cond
        ((string-equal kind "TABLE")
         (setf (gethash (to-kebab-name (second form)) *known-tables*) form))
        ((string-equal kind "RECORD")
         (setf (gethash (to-kebab-name (second form)) *known-records*) form))
        ((or (string-equal kind "FORMAT") (string-equal kind "FORMAT-TABLE"))
         (setf (gethash (to-kebab-name (third form)) *known-formats*) form))
        ((string-equal kind "ENUM")
         (setf (gethash (to-kebab-name (third form)) *known-enums*) (second form)))
        ((string-equal kind "FLAGS")
         (setf (gethash (to-kebab-name (third form)) *known-flags*) (second form)))
        ((string-equal kind "EXTERN")
         (setf (gethash (to-kebab-name (second form)) *known-externs*) form))))))

(defun find-field (fname table-form)
  (find (to-kebab-name fname) (cddr table-form)
        :key (lambda (f) (and (consp f) (to-kebab-name (second f))))
        :test #'string=))

(defun get-prop (key plist &optional default)
  (let ((pos (position key plist)))
    (if pos (nth (1+ pos) plist) default)))

(defun make-offset-form (base-offset fixed dynamic-terms)
  (cond
    ((null dynamic-terms)
     (if (zerop fixed) base-offset `(+ ,base-offset ,fixed)))
    ((zerop fixed)
     `(+ ,base-offset ,@dynamic-terms))
    (t
     `(+ ,base-offset ,fixed ,@dynamic-terms))))

(defun count-to-lisp-form (count-expr struct-name table-form var-sym &optional extra-args field-call)
  "Compile a schema :COUNT expression into a form, or return NIL if it can't be.
Field names read the field (through FIELD-CALL, a function of the field name,
when given); names in EXTRA-ARGS refer to those accessor arguments."
  (cond
    ((null count-expr) nil)
    ((numberp count-expr) count-expr)
    ((eq count-expr :remainder) nil)
    ((symbolp count-expr)
     (let ((fld (find-field count-expr table-form)))
       (cond
         (fld (if field-call
                  (funcall field-call count-expr)
                  `(,(zsym (format nil "~A-~A" struct-name count-expr)) ,var-sym)))
         ((member (zsym count-expr) extra-args) (zsym count-expr)))))
    ((consp count-expr)
     (let* ((op (intern (to-kebab-name (car count-expr)) :keyword))
            (args (mapcar (lambda (a) (count-to-lisp-form a struct-name table-form var-sym extra-args field-call))
                          (cdr count-expr))))
       (when (every #'identity args)
         (case op
           (:try-into (first args))
           ;; TupleIndex flags: 0 selects the embedded peak tuple, 1 the intermediate region.
           (:tuple-len `(if (logtest ,(if (eql (third args) 0) #x8000 #x4000) ,(first args))
                            ,(second args) 0))
           (:add `(+ ,@args))
           (:subtract `(max 0 (- ,@args)))
           (:half `(floor ,(car args) 2))
           (:add-multiply `(* (+ ,(first args) ,(second args)) ,(third args)))
           (:multiply-add `(+ (* ,(first args) ,(second args)) ,(third args)))
           (:subtract-add-two `(+ (max 0 (- ,(first args) ,(second args))) 2))
           (:bitmap-len `(ceiling ,(car args) 8))
           (otherwise nil)))))))

;;; --- Emitters as Pure Lisp Forms ---

(defun emit-enum (form)
  (let* ((name (third form))
         (doc (option-or-property :doc (cdddr form)))
         (variants (remove-if (lambda (x) (or (not (consp x)) (keywordp (car x)))) (cdddr form)))
         (forms '()))
    (dolist (v variants)
      (let ((cname (zsym (format nil "+~A-~A+" name (car v)))))
        (pushnew (symbol-name cname) *exports* :test #'string=)
        (push `(define-constant ,cname ,(cadr v) ,@(when doc (list doc))) forms)))
    (nreverse forms)))

(defun emit-flags (form)
  (let* ((name (third form))
         (doc (option-or-property :doc (cdddr form)))
         (flags (remove-if (lambda (x) (or (not (consp x)) (keywordp (car x)))) (cdddr form)))
         (var-flags (zsym "FLAGS"))
         (forms '()))
    (dolist (fl flags)
      (let* ((cname (zsym (format nil "+~A-~A+" name (car fl))))
             (pred-name (zsym (format nil "~A-~A-P" name (car fl)))))
        (pushnew (symbol-name cname) *exports* :test #'string=)
        (pushnew (symbol-name pred-name) *exports* :test #'string=)
        (push `(define-constant ,cname ,(cadr fl) ,@(when doc (list doc))) forms)
        (push `(declaim (inline ,pred-name)) forms)
        (push `(defun ,pred-name (,var-flags)
                 (logtest ,var-flags ,cname))
              forms)))
    (nreverse forms)))

(defun emit-record (form)
  (let* ((name (second form))
         (struct-name (zsym name))
         (ctor-name (zsym (format nil "%MAKE-~A" name)))
         (pred-name (zsym (format nil "~A-P" name)))
         (doc (option-or-property :doc (cddr form)))
         (fields (remove-if-not (lambda (x) (and (consp x) (string-equal (car x) "field"))) (cddr form)))
         (offset 0)
         (dynamic-terms '())
         ;; Name of a preceding field whose size cannot be expressed; no field may follow it.
         (unplaceable-after nil)
         (declaims '())
         (struct-forms '())
         (accessor-forms '())
         (var-rec (zsym "REC"))
         (var-index (zsym "INDEX"))
         (var-pos (zsym "POS")))

    (pushnew (symbol-name struct-name) *exports* :test #'string=)
    (pushnew (symbol-name pred-name) *exports* :test #'string=)
    (push `(defstruct (,struct-name (:include record-ref)
                                   (:constructor ,ctor-name (%record-font-data %record-offset)))
             ,@(when doc (list doc)))
          struct-forms)

    (dolist (f fields)
      (let* ((fname (second f))
             (ftype (normalize-field-type (third f)))
             (fdoc (get-prop :doc (cdddr f)))
             (acc-name (zsym (format nil "~A-~A" struct-name fname)))
             (tinfo (resolve-type-info ftype))
             (off-expr (make-offset-form `(record-ref-offset ,var-rec) offset (reverse dynamic-terms))))
        (when unplaceable-after
          (error "Field ~A of record ~A follows variable-length field ~A whose size is unknown"
                 fname name unplaceable-after))
        (pushnew (symbol-name acc-name) *exports* :test #'string=)
        (push acc-name declaims)
        (cond
          ;; 1. Primitive scalar field
          (tinfo
           (push `(defun ,acc-name (,var-rec)
                    ,@(when fdoc (list fdoc))
                    (declare (type ,struct-name ,var-rec))
                    (,(getf tinfo :reader) (record-ref-font-data ,var-rec) ,off-expr))
                 accessor-forms)
           (incf offset (getf tinfo :size)))

          ;; 2. Offset field: (offset16 target), (offset24 target), (offset32 target)
          ((and (consp ftype) (search "OFFSET" (to-kebab-name (car ftype))))
           (multiple-value-bind (off-size off-reader) (offset-type-info (car ftype))
             (push `(defun ,acc-name (,var-rec)
                      ,@(when fdoc (list fdoc))
                      (declare (type ,struct-name ,var-rec))
                      (,off-reader (record-ref-font-data ,var-rec) ,off-expr))
                   accessor-forms)
             (incf offset off-size)))

          ;; 3. Array field: (array elem-type)
          ((array-type-p ftype)
           (let ((elem-type (second ftype)))
             (when (array-type-p elem-type)
               (error "Nested array fields not supported: ~S in record ~A" ftype struct-name))

             (let* ((count-attr (get-prop :count (cdddr f)))
                    (count-acc (zsym (format nil "~A-COUNT" acc-name)))
                    (ref-acc (zsym (format nil "~A-REF" acc-name)))
                    (elem-tinfo (resolve-type-info elem-type))
                    (elem-offset-p (and (consp elem-type) (search "OFFSET" (to-kebab-name (car elem-type)))))
                    (elem-bytes (cond
                                  (elem-tinfo (getf elem-tinfo :size))
                                  (elem-offset-p (nth-value 0 (offset-type-info (car elem-type))))
                                  (t (resolve-record-size elem-type))))
                    (elem-reader (cond
                                   (elem-tinfo (getf elem-tinfo :reader))
                                   (elem-offset-p (nth-value 1 (offset-type-info (car elem-type))))
                                   (t nil)))
                    (elem-ctor (when (null elem-reader) (zsym (format nil "%MAKE-~A" elem-type))))
                    (internal-fld (and count-attr (symbolp count-attr) (find-field count-attr form))))
               (unless elem-bytes
                 (error "Array field ~A of record ~A has element type ~S of unknown size"
                        fname name elem-type))
               (pushnew (symbol-name count-acc) *exports* :test #'string=)
               (pushnew (symbol-name ref-acc) *exports* :test #'string=)
               ;; Count getter
               (cond
                 (internal-fld
                  (push `(defun ,count-acc (,var-rec)
                           ,@(when fdoc (list fdoc))
                           (declare (type ,struct-name ,var-rec))
                           (,(zsym (format nil "~A-~A" struct-name count-attr)) ,var-rec))
                        accessor-forms))
                 ((numberp count-attr)
                  (push `(defun ,count-acc (,var-rec)
                           ,@(when fdoc (list fdoc))
                           (declare (type ,struct-name ,var-rec) (ignore ,var-rec))
                           ,count-attr)
                        accessor-forms))
                 (t
                  (let ((arg-name (zsym (if (and count-attr (symbolp count-attr)) count-attr "COUNT"))))
                    (push `(defun ,count-acc (,var-rec &optional ,arg-name)
                             ,@(when fdoc (list fdoc))
                             (declare (type ,struct-name ,var-rec) (ignore ,var-rec))
                             ,arg-name)
                          accessor-forms))))
               ;; Ref getter
               (push `(defun ,ref-acc (,var-rec ,var-index)
                        ,@(when fdoc (list fdoc))
                        (declare (type ,struct-name ,var-rec) (type (unsigned-byte 32) ,var-index))
                        (let ((,var-pos (+ ,off-expr (* ,var-index ,elem-bytes))))
                          ,(if elem-reader
                               `(,elem-reader (record-ref-font-data ,var-rec) ,var-pos)
                               `(,elem-ctor (record-ref-font-data ,var-rec) ,var-pos))))
                     accessor-forms)
               (cond
                 ((numberp count-attr)
                  (incf offset (* count-attr elem-bytes)))
                 (internal-fld
                  (push `(* (,count-acc ,var-rec) ,elem-bytes) dynamic-terms))
                 (t
                  (setf unplaceable-after fname))))))

          ;; 4. Variable-size data: computed arrays and extern types. Expose the start.
          ((or (computed-type-p ftype) (extern-type-p ftype))
           (let ((start-acc (zsym (format nil "~A-START" acc-name))))
             (pushnew (symbol-name start-acc) *exports* :test #'string=)
             (push `(defun ,start-acc (,var-rec)
                      ,(format nil "Byte position of the ~A field; its data is variable-length ~A."
                               fname (to-kebab-name (if (consp ftype) (second ftype) ftype)))
                      (declare (type ,struct-name ,var-rec))
                      ,off-expr)
                   accessor-forms)
             (setf unplaceable-after fname)))

          ;; 5. Nested record
          (t
           (let ((rsize (resolve-record-size ftype)))
             (cond
               (rsize
                (push `(defun ,acc-name (,var-rec)
                         ,@(when fdoc (list fdoc))
                         (declare (type ,struct-name ,var-rec))
                         (,(zsym (format nil "%MAKE-~A" ftype)) (record-ref-font-data ,var-rec) ,off-expr))
                      accessor-forms)
                (incf offset rsize))
               ((and (symbolp ftype) (gethash (to-kebab-name ftype) *known-records*))
                ;; Variable-size nested record: accessible, but ends the static layout.
                (push `(defun ,acc-name (,var-rec)
                         ,@(when fdoc (list fdoc))
                         (declare (type ,struct-name ,var-rec))
                         (,(zsym (format nil "%MAKE-~A" ftype)) (record-ref-font-data ,var-rec) ,off-expr))
                      accessor-forms)
                (setf unplaceable-after fname))
               (t
                (error "Unrecognized type ~S for field ~A of record ~A" (third f) fname name))))))))

    (if (or dynamic-terms unplaceable-after)
        (warn "Record ~A has dynamic array field and cannot compute static size (+~A-SIZE+)" name name)
        (let ((size-const (zsym (format nil "+~A-SIZE+" name))))
          (pushnew (symbol-name size-const) *exports* :test #'string=)
          (push `(define-constant ,size-const ,offset) struct-forms)))

    (let ((declaims (filter-declaims declaims accessor-forms)))
      (append (reverse struct-forms)
              (when declaims `((declaim (inline ,@(reverse declaims)))))
              (reverse accessor-forms)))))

(defun emit-format (form)
  (let* ((disc-type (second form))
         (name (third form))
         (lisp-name (zsym name))
         (read-fn-name (zsym (format nil "READ-~A" name)))
         (disc-offset (or (option-or-property :offset (cdddr form)) 0))
         (doc (option-or-property :doc (cdddr form)))
         (reader (getf (resolve-type-info disc-type) :reader 'font-data-read-u16))
         (variants (remove-if-not (lambda (x) (and (consp x) (string-equal (car x) "variant"))) (cdddr form)))
         (var-fd (zsym "FONT-DATA"))
         (var-offset (zsym "OFFSET"))
         (var-len (zsym "LENGTH"))
         (var-fmt (zsym "FMT")))
    (pushnew (symbol-name read-fn-name) *exports* :test #'string=)
    (cond
      ((string-equal (to-kebab-name name) "GLYPH")
       (list `(defun ,read-fn-name (,var-fd ,var-offset &optional (,var-len (- (font-data-length ,var-fd) ,var-offset)))
                ,@(when doc (list doc))
                (declare (ignorable ,var-len))
                (let ((,var-fmt (,reader ,var-fd (+ ,var-offset ,disc-offset))))
                  (if (minusp ,var-fmt)
                      (,(zsym "READ-COMPOSITE-GLYPH") ,var-fd ,var-offset ,var-len)
                      (,(zsym "READ-SIMPLE-GLYPH") ,var-fd ,var-offset ,var-len))))))

      ((string-equal (to-kebab-name name) "DEVICE-OR-VARIATION-INDEX")
       (list `(defun ,read-fn-name (,var-fd ,var-offset &optional (,var-len (- (font-data-length ,var-fd) ,var-offset)))
                ,@(when doc (list doc))
                (declare (ignorable ,var-len))
                (let ((,var-fmt (,reader ,var-fd (+ ,var-offset ,disc-offset))))
                  (if (= ,var-fmt #x8000)
                      (,(zsym "READ-VARIATION-INDEX") ,var-fd ,var-offset ,var-len)
                      (,(zsym "READ-DEVICE") ,var-fd ,var-offset ,var-len))))))

      ((string-equal (to-kebab-name name) "PAINT")
       ;; COLRv1 Paint tables formats 1..33
       (let ((cases '()))
         (dolist (v variants)
           (let* ((target (third v))
                  (fmt-val (format-variant-value target)))
             (unless fmt-val
               (error "Paint variant ~A has no :format discriminator in table ~A" (second v) target))
             (push `(,fmt-val (,(zsym (format nil "READ-~A" target)) ,var-fd ,var-offset ,var-len)) cases)))
         (list `(defun ,read-fn-name (,var-fd ,var-offset &optional (,var-len (- (font-data-length ,var-fd) ,var-offset)))
                  ,@(when doc (list doc))
                  (declare (ignorable ,var-len))
                  (let ((,var-fmt (,reader ,var-fd (+ ,var-offset ,disc-offset))))
                    (case ,var-fmt
                      ,@(reverse cases)
                      (otherwise (error 'unsupported-format-error :format ,var-fmt :table ',lisp-name))))))))

      (t
       (let ((cases '()))
         (dolist (v variants)
           (let* ((vname (symbol-name (second v)))
                  (target (third v))
                  (digits (remove-if-not #'digit-char-p vname))
                  (fmt-val (if (plusp (length digits))
                               (parse-integer digits)
                               (format-variant-value target))))
             (unless fmt-val
               (error "Format variant ~A in format ~A has no numeric discriminator" vname name))
             (push `(,fmt-val (,(zsym (format nil "READ-~A" target)) ,var-fd ,var-offset ,var-len)) cases)))
         (list `(defun ,read-fn-name (,var-fd ,var-offset &optional (,var-len (- (font-data-length ,var-fd) ,var-offset)))
                  ,@(when doc (list doc))
                  (declare (ignorable ,var-len))
                  (let ((,var-fmt (,reader ,var-fd (+ ,var-offset ,disc-offset))))
                    (case ,var-fmt
                      ,@(reverse cases)
                      (otherwise (error 'unsupported-format-error :format ,var-fmt :table ',lisp-name)))))))))))

(defun option-or-property (key items)
  "Value of KEY in ITEMS, given either inline (... KEY value ...) or as a (KEY value) sub-list."
  (let ((pos (position key items)))
    (if pos
        (nth (1+ pos) items)
        (let ((sub (find-if (lambda (x) (and (consp x) (eq (car x) key))) items)))
          (when sub (second sub))))))

(defun external-count-names (count-expr table-form)
  "Symbols in COUNT-EXPR (excluding operator names) that are not fields of TABLE-FORM."
  (cond
    ((and count-expr (symbolp count-expr) (not (keywordp count-expr)))
     (unless (find-field count-expr table-form) (list (zsym count-expr))))
    ((consp count-expr)
     (loop for a in (cdr count-expr) append (external-count-names a table-form)))))

(defun since-version-form (since table-form version-call)
  "Form true when the table's version is at least SINCE (a string like \"1\" or
\"1.2\"). VERSION-CALL reads the table's version field."
  (let* ((fld (or (find-field "version" table-form)
                  (error "~A: :since-version needs a VERSION field" (second table-form))))
         (size (getf (resolve-type-info (third fld)) :size))
         (dot (position #\. since))
         (major (parse-integer since :end dot))
         (minor (if dot (parse-integer since :start (1+ dot)) 0)))
    (if (= size 2)
        `(>= ,version-call ,major)
        `(>= ,version-call ,(logior (ash major 16) minor)))))

(defun extern-type-p (ftype)
  "True for types declared with (extern NAME): variable-size data decoded by hand-written code."
  (and ftype (symbolp ftype) (gethash (to-kebab-name ftype) *known-externs*)))

(defun flag-mask-form (flag-spec table-form)
  "Mask form for a :WHEN-FLAG (flag-field flag) spec. FLAG is a number or the
name of a flag in the flag field's FLAGS type."
  (destructuring-bind (flag-field flag) flag-spec
    (if (numberp flag)
        flag
        (let* ((fld (or (find-field flag-field table-form)
                        (error "~A: :when-flag refers to unknown field ~A" (second table-form) flag-field)))
               (flags-type (third fld)))
          (unless (gethash (to-kebab-name flags-type) *known-flags*)
            (error "~A: :when-flag field ~A is not of a FLAGS type" (second table-form) flag-field))
          (zsym (format nil "+~A-~A+" flags-type flag))))))

(defun emit-table (form)
  (let* ((name (second form))
         (struct-name (zsym name))
         (ctor-name (zsym (format nil "%MAKE-~A" name)))
         (read-fn-name (zsym (format nil "READ-~A" name)))
         (san-name (zsym (format nil "SANITIZE-~A" name)))
         (tag (option-or-property :tag (cddr form)))
         (doc (option-or-property :doc (cddr form)))
         (fields (remove-if-not (lambda (x) (and (consp x) (string-equal (car x) "field"))) (cddr form)))
         ;; A field's position is FIXED plus the sum of TERMS (forms in TABLE and EXTRA-ARGS).
         (fixed 0)
         (terms '())
         ;; Counts supplied by the caller (from other tables); every later accessor takes them.
         (extra-args '())
         ;; Name of a preceding field after which no position can be computed.
         (unplaceable-after nil)
         ;; Bytes every valid table has; stops growing at the first dynamic or conditional field.
         (min-size 0)
         (min-size-open t)
         (checks '())
         (declaims '())
         (accessors '())
         ;; Field name -> the extra arguments its accessor was defined with.
         (field-args (make-hash-table :test 'equal))
         (tbl (zsym "TABLE"))
         (fd (zsym "FONT-DATA"))
         (off (zsym "OFFSET"))
         (len (zsym "LENGTH"))
         (index (zsym "INDEX"))
         (pos (zsym "POS"))
         (target-off (zsym "OFF"))
         (cnt (zsym "CNT")))
    (labels ((export-name (sym) (pushnew (symbol-name sym) *exports* :test #'string=))
             (field-pos () (make-offset-form `(table-ref-offset ,tbl) fixed (reverse terms)))
             (table-end () `(+ (table-ref-offset ,tbl) (table-ref-length ,tbl)))
             (fits (size-form) `(<= (+ ,(field-pos) ,size-form) ,(table-end)))
             (lambda-list (&rest more) `(,tbl ,@extra-args ,@more))
             (call (acc) `(,acc ,tbl ,@extra-args))
             (call-field (fname)
               (multiple-value-bind (args found) (gethash (to-kebab-name fname) field-args)
                 (unless found
                   (error "~A: field ~A is referenced before it is defined" name fname))
                 `(,(zsym (format nil "~A-~A" struct-name fname)) ,tbl ,@args)))
             (oob (pos-form size-form)
               `(error 'font-out-of-bounds-error :pos ,pos-form :size ,size-form :length (table-ref-length ,tbl)))
             (def (fn-name args fdoc &rest body)
               (export-name fn-name)
               (push fn-name declaims)
               (push `(defun ,fn-name ,args
                        ,@(when fdoc (list fdoc))
                        (declare (type ,struct-name ,tbl)
                                 ,@(when (cdr args) `((ignorable ,@(cdr args)))))
                        ,@body)
                     accessors))
             (guarded (present body) (if present `(when ,present ,body) body))
             (advance (size-form flag-present conditional-p)
               ;; Move past a field. A flag-conditional field occupies space only
               ;; when FLAG-PRESENT holds; a version-conditional one is followed only
               ;; by fields that are absent whenever it is, so it advances normally.
               (when conditional-p (setf min-size-open nil))
               (cond
                 (flag-present
                  (setf min-size-open nil)
                  (push `(if ,flag-present ,size-form 0) terms))
                 ((numberp size-form)
                  (when (and min-size-open (null terms)) (incf min-size size-form))
                  (incf fixed size-form))
                 (t
                  (setf min-size-open nil)
                  (push size-form terms))))
             (raw-reader (fname reader)
               ;; (FONT-DATA OFFSET) reader for sanitize; only at a static position.
               (when (null terms)
                 (let ((raw (zsym (format nil "~A-~A-RAW" struct-name fname))))
                   (push raw declaims)
                   (push `(defun ,raw (,fd ,off) (,reader ,fd ,(make-offset-form off fixed nil))) accessors)
                   raw)))
             (static-flag-check (when-flag mask size)
               ;; In sanitize: if a flag-conditional field at a static position is present, it must fit.
               (let ((raw (zsym (format nil "~A-~A-RAW" struct-name (first when-flag)))))
                 (when (and (null terms) (member raw (defined-names accessors)))
                   (push `(when (and (logtest ,mask (,raw ,fd ,off)) (> ,(+ fixed size) ,len))
                            (error 'font-out-of-bounds-error :pos ,off :size ,(+ fixed size) :length ,len))
                         checks)))))

      (when tag
        (let ((tag-const (zsym (format nil "+~A-TAG+" struct-name))))
          (export-name tag-const)
          (push `(define-constant ,tag-const ,tag) accessors)))

      (dolist (f fields)
        (let* ((fname (second f))
               (ftype (normalize-field-type (third f)))
               (props (cdddr f))
               (fdoc (get-prop :doc props))
               (since-ver (get-prop :since-version props))
               (when-flag (get-prop :when-flag props))
               (mask (and when-flag (flag-mask-form when-flag form)))
               (acc-name (zsym (format nil "~A-~A" struct-name fname)))
               (flag-present (and when-flag `(logtest ,mask ,(call-field (first when-flag))))))
          (when unplaceable-after
            (error "Field ~A of table ~A follows variable-length field ~A whose size is unknown"
                   fname name unplaceable-after))
          (flet ((presence (size-form)
                   ;; Form that is true when the field exists, or NIL if it always does.
                   ;; Version-gated fields must also fit: e.g. 'post' 3.0 is >= 2.0 but
                   ;; has no glyph names.
                   (cond (when-flag flag-present)
                         (since-ver `(and ,(since-version-form since-ver form (call-field "version"))
                                          ,(fits size-form))))))
            (cond
              ;; 1. Array
              ((array-type-p ftype)
               (let* ((inner (second ftype))
                      (offset-elem-p (and (consp inner) (search "OFFSET" (to-kebab-name (car inner)))))
                      (elem-type (if offset-elem-p (second inner) inner))
                      (elem-info (and (not offset-elem-p) (resolve-type-info elem-type)))
                      (elem-size (cond (offset-elem-p (nth-value 0 (offset-type-info (car inner))))
                                       (elem-info (getf elem-info :size))
                                       (t (resolve-record-size elem-type))))
                      (count-attr (get-prop :count props))
                      (internal-count (and count-attr (symbolp count-attr) (not (keywordp count-attr))
                                           (find-field count-attr form)))
                      (external-count (progn
                                        ;; Names in the count that aren't fields of this table become
                                        ;; arguments of this and every later accessor.
                                        (dolist (sym (external-count-names count-attr form))
                                          (unless (member sym extra-args)
                                            (setf extra-args (append extra-args (list sym)))))
                                        (and count-attr (symbolp count-attr) (not (keywordp count-attr))
                                             (not internal-count) (zsym count-attr))))
                      (remainder-p (or (null count-attr) (eq count-attr :remainder)))
                      (count-expr (cond ((numberp count-attr) count-attr)
                                        (external-count external-count)
                                        (remainder-p
                                         `(max 0 (floor (- ,(table-end) ,(field-pos)) ,elem-size)))
                                        (t (let ((expr (count-to-lisp-form count-attr struct-name form tbl
                                                                           extra-args #'call-field)))
                                             ;; A version-gated count reads as NIL when absent.
                                             (if (and expr since-ver)
                                                 `(if ,(presence 0) (or ,expr 0) 0)
                                                 expr)))))
                      (count-acc (zsym (format nil (if (find-field (format nil "~A-COUNT" fname) form)
                                                       "~A-ARRAY-COUNT" "~A-COUNT")
                                               acc-name)))
                      (ref-acc (zsym (format nil "~A-REF" acc-name))))
                 (when (array-type-p inner)
                   (error "Nested array fields not supported: ~S in table ~A" ftype name))
                 (unless elem-size
                   (error "Array field ~A of table ~A has element type ~S of unknown size" fname name elem-type))
                 (cond
                   ((null count-expr)
                    ;; Count expression we cannot compile: expose the start only.
                    (def (zsym (format nil "~A-START" acc-name)) (lambda-list)
                         (format nil "Byte position of the ~A array; its length is ~(~A~)." fname count-attr)
                         (field-pos))
                    (setf unplaceable-after fname))
                   (t
                    (let ((count-ll (lambda-list))
                          (count-call (call count-acc)))
                      (def count-acc count-ll fdoc
                           `(declare (ignorable ,tbl))
                           (if flag-present `(if ,flag-present ,count-expr 0) count-expr))
                      (def ref-acc (lambda-list index) fdoc
                           `(declare (type (unsigned-byte 32) ,index))
                           `(let ((,pos (+ ,(field-pos) (* ,index ,elem-size))))
                              (unless (<= (+ ,pos ,elem-size) ,(table-end))
                                ,(oob pos elem-size))
                              ,(cond
                                 (offset-elem-p
                                  (let ((target-key (to-kebab-name elem-type)))
                                    `(let ((,target-off (,(nth-value 1 (offset-type-info (car inner)))
                                                         (table-ref-font-data ,tbl) ,pos)))
                                       (unless (zerop ,target-off)
                                         ,(offset-target-form elem-type target-key tbl target-off)))))
                                 (elem-info `(,(getf elem-info :reader) (table-ref-font-data ,tbl) ,pos))
                                 (t `(,(zsym (format nil "%MAKE-~A" elem-type)) (table-ref-font-data ,tbl) ,pos)))))
                      (when (string-equal (to-kebab-name elem-type) "U8")
                        (def (zsym (format nil "~A-BYTES" acc-name)) count-ll fdoc
                             `(font-data-slice (table-ref-font-data ,tbl) ,(field-pos) ,count-call)))
                      ;; Sanitize: an array at a static position whose count is a static field.
                      (when (and (null terms) internal-count)
                        (let ((raw (zsym (format nil "~A-~A-RAW" struct-name count-attr))))
                          (when (member raw (defined-names accessors))
                            (push `(let* ((,cnt (,raw ,fd ,off))
                                          (,pos (+ ,fixed (* ,cnt ,elem-size))))
                                     (when (and ,(if when-flag
                                                     `(logtest ,mask (,(zsym (format nil "~A-~A-RAW" struct-name (first when-flag))) ,fd ,off))
                                                     t)
                                                ,(if since-ver
                                                     `(and (>= ,len ,fixed)
                                                           ,(since-version-form
                                                             since-ver form
                                                             `(,(zsym (format nil "~A-VERSION-RAW" struct-name)) ,fd ,off)))
                                                     t)
                                                (not (and (<= ,pos ,len) (font-data-valid-range-p ,fd ,off ,pos))))
                                       (error 'font-out-of-bounds-error :pos ,off :size ,pos :length (font-data-length ,fd))))
                                  checks))))
                      (cond
                        ((numberp count-expr)
                         (advance (* count-expr elem-size) flag-present (or since-ver when-flag)))
                        (remainder-p
                         (setf unplaceable-after fname))
                        (t
                         (advance `(* ,count-call ,elem-size) flag-present when-flag))))))))

              ;; 2. Offset
              ((and (consp ftype) (search "OFFSET" (to-kebab-name (car ftype))))
               (multiple-value-bind (off-size off-reader) (offset-type-info (car ftype))
                 (let* ((target (second ftype))
                        (raw-acc (zsym (format nil "~A-OFFSET" acc-name)))
                        (present (presence off-size)))
                   (def raw-acc (lambda-list) fdoc
                        (guarded present `(,off-reader (table-ref-font-data ,tbl) ,(field-pos))))
                   (def acc-name (lambda-list) fdoc
                        `(let ((,target-off ,(call raw-acc)))
                           (when (and ,target-off (not (zerop ,target-off)))
                             ,(offset-target-form target (and target (to-kebab-name target)) tbl target-off))))
                   (when (and when-flag (not since-ver)) (static-flag-check when-flag mask off-size))
                   (advance off-size flag-present (or since-ver when-flag)))))

              ;; 3. Scalar
              ((resolve-type-info ftype)
               (let* ((tinfo (resolve-type-info ftype))
                      (size (getf tinfo :size))
                      (reader (getf tinfo :reader))
                      (present (presence size)))
                 (def acc-name (lambda-list) fdoc
                      (guarded present `(,reader (table-ref-font-data ,tbl) ,(field-pos))))
                 (raw-reader fname reader)
                 (when (and when-flag (not since-ver)) (static-flag-check when-flag mask size))
                 (advance size flag-present (or since-ver when-flag))))

              ;; 4. Variable-size data: computed arrays and extern types. Expose the start.
              ((or (computed-type-p ftype) (extern-type-p ftype))
               (def (zsym (format nil "~A-START" acc-name)) (lambda-list)
                    (format nil "Byte position of the ~A field; its elements are variable-length ~A data."
                            fname (to-kebab-name (if (consp ftype) (second ftype) ftype)))
                    (guarded flag-present (field-pos)))
               (setf unplaceable-after fname))

              ;; 5. Inline record
              (t
               (let ((rsize (resolve-record-size ftype)))
                 (unless (or rsize (and (symbolp ftype) (gethash (to-kebab-name ftype) *known-records*)))
                   (error "Unrecognized type ~S for field ~A of table ~A" (third f) fname name))
                 (let ((present (and rsize (presence rsize))))
                   (def acc-name (lambda-list) fdoc
                        (guarded (or present flag-present)
                                 `(,(zsym (format nil "%MAKE-~A" ftype)) (table-ref-font-data ,tbl) ,(field-pos))))
                   (if rsize
                       (advance rsize flag-present (or since-ver when-flag))
                       (setf unplaceable-after fname)))))))
          (setf (gethash (to-kebab-name fname) field-args) extra-args)))

      (dolist (sym (list struct-name (zsym (format nil "~A-P" name)) read-fn-name san-name))
        (export-name sym))
      (let ((declaims (filter-declaims declaims accessors)))
        (append
         (list `(defstruct (,struct-name (:include table-ref)
                                         (:constructor ,ctor-name (%table-font-data %table-offset %table-length)))
                  ,@(when doc (list doc))))
         (when declaims `((declaim (inline ,@(reverse declaims)))))
         (reverse accessors)
         (list `(defun ,san-name (,fd ,off ,len)
                  (declare (ignorable ,len) (type font-data ,fd) (type (unsigned-byte 32) ,off ,len))
                  (unless (and (<= ,min-size ,len) (font-data-valid-range-p ,fd ,off ,min-size))
                    (error 'font-out-of-bounds-error :pos ,off :size ,min-size :length (font-data-length ,fd)))
                  ,@(reverse checks)
                  t)
               `(defun ,read-fn-name (,fd ,off &optional (,len (- (font-data-length ,fd) ,off)))
                  (,san-name ,fd ,off ,len)
                  (,ctor-name ,fd ,off ,len))))))))

(defun offset-target-form (target target-key tbl target-off)
  "Form resolving the offset in TARGET-OFF (relative to table TBL) into a TARGET view."
  (let ((abs `(+ (table-ref-offset ,tbl) ,target-off)))
    (cond
      ((and target-key (or (gethash target-key *known-tables*) (gethash target-key *known-formats*)))
       `(,(zsym (format nil "READ-~A" target)) (table-ref-font-data ,tbl) ,abs))
      ((and target-key (gethash target-key *known-records*))
       `(,(zsym (format nil "%MAKE-~A" target)) (table-ref-font-data ,tbl) ,abs))
      (t `(font-data-slice (table-ref-font-data ,tbl) ,abs)))))

(defun form-defined-names (form)
  "Names a generated top-level FORM defines."
  (when (consp form)
    (case (car form)
      ((defun define-constant) (list (second form)))
      (defstruct (let ((name (if (consp (second form)) (first (second form)) (second form))))
                   (list name (zsym (format nil "~A-P" name)))))
      (t nil))))

(defun emit-file-forms (items stream filename)
  (let ((*exports* '())
        (*print-case* :downcase)
        (*package* (find-package :zpb-otf))
        (all-forms '()))
    ;; 1. Enums & Flags
    (dolist (item items)
      (let ((kind (string-upcase (string (car item)))))
        (cond
          ((string-equal kind "ENUM")  (setf all-forms (append all-forms (emit-enum item))))
          ((string-equal kind "FLAGS") (setf all-forms (append all-forms (emit-flags item)))))))
    ;; 2. Records
    (dolist (item items)
      (when (string-equal (string (car item)) "RECORD")
        (setf all-forms (append all-forms (emit-record item)))))
    ;; 3. Tables & Formats
    (dolist (item items)
      (let ((kind (string-upcase (string (car item)))))
        (cond
          ((string-equal kind "TABLE")  (setf all-forms (append all-forms (emit-table item))))
          ((or (string-equal kind "FORMAT") (string-equal kind "FORMAT-TABLE"))
           (setf all-forms (append all-forms (emit-format item)))))))

    ;; Every name may be defined once across all generated files, and only defined names are exported.
    (let ((defined '()))
      (dolist (f all-forms)
        (dolist (n (form-defined-names f))
          (let ((prev (gethash n *defined-names*)))
            (when prev
              (error "Generated name ~A in ~A is already defined in ~A" n filename prev)))
          (setf (gethash n *defined-names*) filename)
          (push n defined)))
      (setf *exports* (remove-if-not (lambda (s) (member (zsym s) defined)) *exports*)))

    ;; Write file header
    (format stream ";;;; ~A — Generated OpenType tables~%" filename)
    (format stream ";;;; Generated automatically from OpenType sexp schema~%")
    (format stream ";;;; DO NOT EDIT DIRECTLY~%")
    (format stream ";;;; Copyright (C) 2026 Arthur Miller and contributors~%")
    (format stream ";;;; SPDX-License-Identifier: MIT~%")
    (pprint '(in-package #:zpb-otf) stream)
    (terpri stream)
    (when *exports*
      (pprint `(export '(,@(sort (mapcar #'zsym (copy-list *exports*)) #'string< :key #'symbol-name))) stream)
      (terpri stream))

    ;; Pretty-print every generated form; PPRINT starts each with a newline.
    (dolist (f all-forms)
      (pprint f stream)
      (terpri stream))
    *exports*))
