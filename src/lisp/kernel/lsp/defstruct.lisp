;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;  Copyright (c) 2001, Juan Jose Garcia Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;        The structure routines.

(in-package "SYSTEM")

#+(or)(eval-when (:load-toplevel :compile-toplevel :execute)
  (setq *echo-repl-read* t)
)
(defun structure-type-error (value slot-type struct-name slot-name)
  (error 'simple-type-error
	 :format-control "Slot ~A in structure ~A only admits values of type ~A."
	 :format-arguments (list slot-name struct-name slot-type)
	 :datum value
	 :expected-type slot-type))

(defun warn-missing-include (name include)
  ;; FIXME: should be a style warning at most
  (warn "Structure definition for ~a INCLUDEs ~a, unknown at compile time."
        name include))

(defun error-missing-include (name include)
  (error "Cannot define structure ~a - it INCLUDEs ~a, which is undefined."
         name include))

(defun error-incompatible-include (name type include includetype)
  (error "Cannot define structure ~a with type ~a - it INCLUDEs ~a, which has incompatible type ~a"
         name type include includetype))

(defun warn-incompatible-struct-redefinition (name)
  (warn "Redefining structure ~s incompatibly" name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Slot descriptions
;;;

;;; We parse DEFSTRUCT slot descriptions into something like DEFCLASS syntax,
;;; that is, a list (slot-name ...plist)
;;; Valid plist keys are :initform, :initarg, :type, :reader, and :accessor,
;;; and they work as in DEFCLASS.
;;; :initarg is included because any :named position is treated as a slot,
;;; except that its initialization cannot be customized.
;;; Part of the idea here is we make things independent of conc-name
;;; (and thereby, interning) before we do much susbstantial processing.

(defun error-defstruct-slot-syntax (slot-description)
  (simple-program-error
   "~a is not a valid DEFSTRUCT slot specification." slot-description))

(defun defstruct-accessor-name (conc-name slot-name)
  ;; NOTE: No conc-name is not the same as a conc-name of "",
  ;; because in the first case the symbol could be in a different package.
  (if conc-name
      (intern (base-string-concatenate conc-name slot-name))
      slot-name))

(defun parse-slot-description (slot-description conc-name)
  (let ((tail nil) slot-name read-only)
    (cond ((symbolp slot-description)
           (setq slot-name slot-description))
          ((consp slot-description)
           (setq slot-name (car slot-description))
           (cond ((null (cdr slot-description)))
                 ((consp (cdr slot-description))
                  (setq tail (list* :initform (cadr slot-description) tail))
                  (do ((os (cddr slot-description) (cddr os))
                       (seen-read-only nil) (seen-type nil))
                      ((endp os))
                    (case (car os)
                      (:type
                       (if seen-type
                           (error-defstruct-slot-syntax slot-description)
                           (setq seen-type t
                                 tail (list* :type (cadr os) tail))))
                      (:read-only
                       (if seen-read-only
                           (error-defstruct-slot-syntax slot-description)
                           (setq seen-read-only t
                                 ;;; treat :read-only nil correctly
                                 read-only (cadr os))))
                      (otherwise
                       (error-defstruct-slot-syntax slot-description)))))
                 (t (error-defstruct-slot-syntax slot-description))))
          (t (error-defstruct-slot-syntax slot-description)))
    ;; Finally, add access and initarg.
    (list* slot-name
           (if read-only :reader :accessor)
           (defstruct-accessor-name conc-name slot-name)
           :initarg
           (intern (symbol-name slot-name) "KEYWORD")
           tail)))

;;; Make the slot description for the :named option.
;;; It's treated as a slot so that child structures can initialize it simply.
(defun named-slot-description (structure-type-name)
  ;; The slot name is an uninterned symbol, guaranteeing no conflict with
  ;; user slots or included :named slots.
  `(,(copy-symbol structure-type-name) :initform ',structure-type-name))

;;; Convenience function for use with mapcar.
(defun slot-description-parser (conc-name)
  (lambda (slot-description)
    (parse-slot-description slot-description conc-name)))

;;; UNPARSE-SLOT-DESCRIPTION does the opposite, turning one of the above into
;;;  something that would work in DEFSTRUCT.
;;; This is for documentation purposes only (describe uses it) at the moment,
;;;  and it should probably remain this way.

(defun unparse-slot-description (list)
  (let ((slot-name (car list)) (plist (cdr list)) (default (list nil)))
    (let ((initform (getf plist :initform default))
          (read-only (getf plist :read-only nil))
          (type (getf plist :type t)))
      (if (eq initform default) ; no initform; simple specification
          slot-name
          `(,slot-name ,initform :read-only ,read-only :type ,type)))))

;;; Apply an :INCLUDE slot override.
(defun override-slotd (slot-name over-plist old-plist)
  (destructuring-bind (&key (initform nil initformp)
                         (type t typep) initarg
                         reader accessor)
      over-plist
    (let ((old-reader (getf old-plist :reader))
          (old-accessor (getf old-plist :accessor)))
      (when (and accessor old-reader)
        #+(or)(error "Mutable slot ~a cannot override read-only included slot."
                     slot-name)
        (setq reader accessor accessor nil))
      `(,slot-name
        ;; We always have an initarg for any non-:NAMED slot.
        :initarg ,initarg
        ,@(when initformp `(:initform ,initform))
        ;; NOTE: Could check it's a subtype.
        ,@(let* ((default (list nil))
                 (old-type (getf old-plist :type default)))
            (cond (typep `(:type ,type))
                  ((not (eq old-type default))
                   `(:type ,old-type))))
        ,@(cond
            (reader
             ;; Bug #881: Don't define accessor functions redundantly.
             (unless (eq reader (or old-reader old-accessor))
               `(:reader ,reader)))
            (accessor
             (unless (eq accessor old-accessor)
               `(:accessor ,accessor))))))))

;;; Replace the :reader or :accessor in an old slotd with a new name.
(defun fix-old-slotd (conc-name old-slotd)
  (destructuring-bind (slot-name &key (initform nil initformp)
                                   (type t typep) initarg
                                   reader accessor)
      old-slotd
    (let ((accname (defstruct-accessor-name conc-name slot-name)))
      `(,slot-name :initarg ,initarg
                   ,@(when initformp `(:initform ,initform))
                   ,@(when typep `(:type ,type))
                   ,@(cond
                       (reader
                        ;; Bug #881 again.
                        (unless (eq accname reader)
                          `(:reader ,accname)))
                       (accessor
                        (unless (eq accname accessor)
                          `(:accessor ,accname))))))))

;;; Given defstruct slot-descriptions from both the given defstruct
;;; and an included parent, return a final list of descriptions.
;;; This means removing redundant accessors, checking for duplicates,
;;; getting new accessor names, and just appending.
;;; FIXME: An obscure point in DEFSTRUCT is not defining accessor functions
;;; already defined by an :include-d definition. Bug #881 covers the case
;;; when the same slot is implicated in both definitions, but it's possible
;;; for an unrelated slot to imply the same accessor names as a parent
;;; definition, and we don't handle that correctly.
(defun final-slot-descriptions (conc-name new-slotds over-slotds old-slotds)
  (let ((old-slotds (copy-list old-slotds)))
    ;; Apply overrides to old slots.
    (do ((old-slotds old-slotds (rest old-slotds)))
        ((endp old-slotds))
      (let* ((old-slotd (first old-slotds))
             (slot-name (first old-slotd))
             (over-slotd (first (member slot-name over-slotds
                                        :key #'first))))
        (setf (first old-slotds)
              (cond ((null old-slotd) nil)
                    (over-slotd
                     (override-slotd
                      slot-name (rest over-slotd) (rest old-slotd)))
                    (t (fix-old-slotd conc-name old-slotd))))))
    ;; Signal an error for any override wtih nothing to override.
    (dolist (over-slotd over-slotds)
      (let ((slot-name (first over-slotd)))
        (unless (member slot-name old-slotds :key #'first)
          (error "Cannot override nonexistent slot ~a" slot-name))))
    ;; Signal an error for any duplicate slot.
    (dolist (new-slotd new-slotds)
      ;;; check for "dummy" slotdescriptions coming from :initial-offset
      (unless (null new-slotd)
        (let ((slot-name (first new-slotd)))
          (when (member slot-name old-slotds :key #'first)
            (error "Duplicate slot ~a" slot-name)))))
    ;; Done.
    (append old-slotds new-slotds)))

;;; Return new read-only equivalents to the given slot description,
;;; for immutable structs.
(defun immutable-slot-description (slot-description)
  (substitute :reader :accessor slot-description))

(defun immutable-slot-descriptions (slot-descriptions)
  (mapcar #'immutable-slot-description slot-descriptions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Layouts
;;; A layout describes the structure of a structure object.
;;; It includes the basic kind of storage (object, vector, or list) as well as
;;; some information about the slots: their upgraded types.
;;; It does not include, e.g., initforms.
;;; It also doesn't include names. Besides the fact that these are unrelated to
;;; storage, doing so is a problem with :NAMED slots, which are uninterned symbols
;;; that may not be identical between compile and load time.
;;; Basically it's just enough information to lay objects out in memory.
;;; Because this is really early, we represent layouts as lists. FIXME?

(defun layout-type-base (layout) (first layout))
(defun layout-slot-layouts (layout) (rest layout))

(defun make-layout (type-base slot-layouts) (list* type-base slot-layouts))

;;; The type is the layout type, i.e. how it actually is in memory, which may be
;;; upgraded from the declared type. (The declared type is otherwise irrelevant here.)
(defun slot-layout-type (slot-layout) (first slot-layout))

(defun make-slot-layout (type) (list type))

(defun layoutify-slot-description (slot-description)
  ;; Because this is a structure-object struct, there should be no NIL slotds.
  (destructuring-bind (name &key (type t) &allow-other-keys) slot-description
    (declare (ignore name))
    ;; e:u-s-t ignores the environment, so this should be okay, if cheap.
    (make-slot-layout (ext:upgraded-slot-type type))))

(defun layoutify-slot-descriptions (slot-descriptions)
  (mapcar #'layoutify-slot-description slot-descriptions))

(defun slotds->layout (type-base element-type slot-descriptions)
  (case type-base
    (structure-object (make-layout 'structure-object
                                    (layoutify-slot-descriptions slot-descriptions)))
    ;; non structure objects are uniform, so the slots all have the element type.
    (list (make-layout 'list (mapcar (lambda (slot-description)
                                       (declare (ignore slot-description))
                                       (make-slot-layout 't))
                                     slot-descriptions)))
    (vector (make-layout `(vector ,element-type)
                         (mapcar (lambda (slot-description)
                                   (declare (ignore slot-description))
                                   (make-slot-layout element-type))
                                 slot-descriptions)))))

;;; We have this layout merger function rather than just computing a layout from the
;;; effective slotds. That's because the layout information is basically independent of
;;; subtype overrides; in particular, a stricter :type override in a subclass must not
;;; alter the layout type of an inherited slot.
(defun include-layout (name layout super superlayout)
  (unless (equal (layout-type-base layout) (layout-type-base superlayout))
    (error-incompatible-include name (layout-type-base layout)
                                super (layout-type-base superlayout)))
  ;; Actually make the layout
  (make-layout (layout-type-base layout)
               (append (layout-slot-layouts superlayout)
                       (layout-slot-layouts layout))))

(defun slot-layouts-compatible-p (slot-layout1 slot-layout2)
  (and (equal (slot-layout-type slot-layout1) (slot-layout-type slot-layout2))))

(defun layouts-compatible-p (layout1 layout2)
  (and (equal (layout-type-base layout1) (layout-type-base layout2))
       (let ((slots1 (layout-slot-layouts layout1)) (slots2 (layout-slot-layouts layout2)))
         (and (= (length slots1) (length slots2))
              (every #'slot-layouts-compatible-p slots1 slots2)))))

;;; Make a layout known to Clasp.
;;; If a layout by the given name already exists, warn on redefinition.
(defmacro define-layout (name new-layout)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((new-layout ',new-layout))
       ;; Check for redefinition.
       (multiple-value-bind (old-layout existsp) (structure-layout ',name)
         (when (and existsp (not (layouts-compatible-p old-layout new-layout)))
           (warn-incompatible-struct-redefinition ',name)
           ,@(when (eq (layout-type-base new-layout) 'structure-object)
               `((setf (find-class ',name) nil))))
         ;; actually define.
         (setf (structure-layout ',name) new-layout)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment access
;;; NOTE: Several of these are called from DESCRIBE.
;;;

;;; FIXME: these should take environments
(defun structure-layout (name)
  (get-sysprop name 'structure-layout))
(defun (setf structure-layout) (layout name)
  (put-sysprop name 'structure-layout layout))
(defun structure-type (name)
  (layout-type-base (structure-layout name)))
(defun structure-slot-descriptions (name)
  (get-sysprop name 'structure-slot-descriptions))
(defun (setf structure-slot-descriptions) (descriptions name)
  (put-sysprop name 'structure-slot-descriptions descriptions))
(defun structure-constructor (name)
  (get-sysprop name 'structure-constructor))
(defun (setf structure-constructor) (constructor name)
  (put-sysprop name 'structure-constructor constructor))
(defun names-structure-p (name)
  (structure-type name))

;;; We make sure structure constructors etc. use the right code even if the
;;; class is subsequently removed from the environment. For this purpose we use
;;; this table here. It is keyed on not just the name, but the layout, and is
;;; unaffected by (SETF FIND-CLASS).
;;; FIXME: It should be weak-value. It doesn't matter at the moment since the
;;; values are referenced directly by literals tables through LOAD-TIME-VALUE
;;; below.

(defvar *struct-class-holders* (make-hash-table :test #'equal))

(defun find-struct-class-holder (name layout)
  (let ((key (list* name layout)))
    (or (gethash key *struct-class-holders*)
        (setf (gethash key *struct-class-holders*) (ext:make-class-holder)))))

(defun find-struct-class (name layout)
  ;; Should be impossible for a struct class to become unbound here.
  (ext:class-get (find-struct-class-holder name layout)))

(defun (setf find-struct-class) (class name layout)
  (setf (ext:class-get (find-struct-class-holder name layout)) class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Miscellaneous

;;; Used by #S reader
(defun make-structure (name initargs)
  (unless (names-structure-p name) (error "~s is not a structure class." name))
  (let ((constructor (structure-constructor name)))
    (if constructor
        (apply constructor initargs)
        (error "The structure class ~s has no standard constructor." name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; %%DEFSTRUCT is like DEFSTRUCT, but with easier to work with syntax.
;;; Additionally, inclusion has been resolved, so the list of slots is analogous
;;; to the effective slots in CLOS/MOP (rather than the direct).
;;; This is what does the work of DEFSTRUCT.
;;;

(defun defstruct-slotd->defclass-slotd (slot-description)
  (destructuring-bind (slot-name &key (initform nil initformp) initarg (type t)
                       &allow-other-keys)
      slot-description
    `(,slot-name ,@(when initformp `(:initform ,initform))
                 :initarg ,initarg :type ,type)))

(defun gen-defstruct-accessor (struct-type slotd location
                               gen-read gen-write gen-cas)
  (destructuring-bind (slot-name &key (type t) reader accessor
                       &allow-other-keys)
      slotd
    (declare (ignore slot-name))
    (multiple-value-bind (accessor read-only)
        (if accessor (values accessor nil) (values reader t))
      (unless (not accessor) ; no reader OR accessor
        (list*
         `(declaim (ftype (function (,struct-type) ,type) ,accessor)
                   (inline ,accessor))
         `(defun ,accessor (object)
            (declare (type ,struct-type object))
            ,(funcall gen-read 'object location))
         (unless read-only
           (list*
            `(declaim (ftype (function (,type ,struct-type) ,type) (setf ,accessor))
                      (inline (setf ,accessor)))
            `(defun (setf ,accessor) (new object)
               (declare (type ,struct-type object) (type ,type new))
               ,(funcall gen-write 'object location 'new)
               ;; necessary because instance-set doesn't return the value
               new)
            (when gen-cas
              `((mp:define-atomic-expander ,accessor (object) (&rest keys)
                  ,(funcall gen-cas 'object location 'keys)))))))))))

(defun process-boa-lambda-list (original-lambda-list slot-descriptions)
  (let ((lambda-list (copy-list original-lambda-list))
        ;; list of slotds that will be initialized by the lambda list.
        (mentioned-slots nil)
        ;; list of slotds that need set forms in the body.
        (initialized-slots nil)
        ;; whether the lambda list contains &aux. Also represents
        ;; the state of having seen &aux.
        (aux nil))
    ;; Generate a lambda list handler and immediately discard it,
    ;; to ensure syntactic correctness.
    ;; FIXME: Use an actual lambda list parser for all this.
    ;; We can't use the normal one since it puts in NIL initforms.
    (make-lambda-list-handler lambda-list nil 'function)
    (do* ((sublist lambda-list (rest sublist))
          (name (first sublist) (first sublist))
          ;; Whether we should modify the lambda list to insert a default initform.
          (modify nil))
         ((endp sublist))
      (cond ((or (eq name '&optional) (eq name '&key)) (setq modify t))
            ((eq name '&rest) (setq modify nil))
            ;; modify nil because "&aux x" in a BOA lambda list means we
            ;; don't have to initialize, even if there is an initform.
            ((eq name '&aux) (setq modify nil aux t))
            ((eq name '&allow-other-keys))
            ((atom name) ; just a variable.
             (let ((slotd (assoc name slot-descriptions)))
               (when slotd
                 ;; FIXME: check for duplicated names?
                 ;; or does lambda list handler catch that.
                 (push slotd mentioned-slots)
                 (unless aux
                   (push slotd initialized-slots))
                 (when modify
                   ;; We use a default NIL initform instead of conditionally
                   ;; initializing, for similar reasons to the kw case below.
                   (let ((initform (getf (rest slotd) :initform)))
                     (setf (first sublist)
                           `(,name ,initform)))))))
            (t ; complex parameter.
             (let* ((slot-name (if (consp (first name)) ; complicated :key
                                   (second (first name))
                                   (first name)))
                    (slotd (assoc slot-name slot-descriptions)))
               (when slotd
                 (push slotd mentioned-slots)
                 (if (endp (rest name)) ; like &optional (x)
                     (unless aux
                       (setf (first sublist)
                             `(,(first name)
                               ,(getf (rest slotd) :initform)))
                       (push slotd initialized-slots))
                     (push slotd initialized-slots)))))))
    ;; OK, we have our lambda list set up... except anything with an :initform
    ;; that wasn't mentioned needs to be added (as &aux).
    (let ((more-aux nil) (default (list nil)))
      (dolist (slotd (set-difference slot-descriptions mentioned-slots))
        (let ((slot-name (first slotd))
              (initform (getf (rest slotd) :initform default)))
          (unless (eq default initform)
            (push `(,slot-name ,initform) more-aux)
            (push slotd initialized-slots))))
      ;; Actual return values!
      (values
       ;; The lambda list.
       (nconc lambda-list (if aux nil (list '&aux)) more-aux)
       ;; Slots to initialize.
       initialized-slots))))

(defun defstruct-constructor-def (name original-lambda-list
                                  slot-descriptions alloc genset)
  (multiple-value-bind (lambda-list initialized-slots)
      (process-boa-lambda-list original-lambda-list slot-descriptions)
    (let ((osym (gensym "NEW"))
          (forms nil))
      ;; Construct initialization forms.
      (do ((index 0 (1+ index))
           (slot-descriptions slot-descriptions (rest slot-descriptions)))
          ((null slot-descriptions))
        (let ((slotd (first slot-descriptions)))
          (when (member slotd initialized-slots)
            (push (funcall genset osym index (first slotd)) forms))))
      ;; Done
      `(defun ,name ,lambda-list
         (let ((,osym ,alloc))
           ,@forms
           ,osym)))))

(defun defstruct-kw-constructor-def (name slot-descriptions alloc genset)
  (let ((kwparams nil) (aux nil) (forms nil)
        (osym (gensym "NEW")))
    (do ((index 0 (1+ index))
         (slot-descriptions slot-descriptions (cdr slot-descriptions)))
        ((null slot-descriptions))
      (let ((slotd (first slot-descriptions)))
        (unless (null slotd) ; spacer
          (let* ((slot-name (first slotd))
                 ;; we use uninterned symbols as parameters per CLHS defstruct:
                 ;; "The symbols which name the slots must not be used by the
                 ;;  implementation as the names for the lambda variables in the
                 ;;  constructor function, since one or more of those
                 ;;  symbols might have been proclaimed special or..."
                 ;; NOTE: In the BOA case, the programmer digs their own hole.
                 (var (copy-symbol slot-name))
                 (initarg (getf (rest slotd) :initarg))
                 ;; I don't think we'd save any time by checking a
                 ;; -p variable and not initializing, so we just
                 ;; initialize slots to NIL if they aren't passed
                 ;; and have no initform.
                 (initform (getf (rest slotd) :initform)))
            (if initarg
                ;; keyword (normal) argument
                (push (list (list initarg var) initform) kwparams)
                ;; aux argument - :named structure names only
                ;; (so, incidentally, there's always an initform)
                (push (list var initform) aux))
            (push (funcall genset osym index var) forms)))))
    `(defun ,name (&key ,@kwparams)
       (let (,@aux
             (,osym ,alloc))
         ,@forms
         ,osym))))

(defun list-of-length-at-least (list n)
  (dotimes (i n)
    (unless (consp list) (return-from list-of-length-at-least nil))
    (setf list (cdr list)))
  (listp list))

;;; Wrapper that will be better handled later/in cclasp.
;;; NAME, LAYOUT, and also INDEX must be constants.
(defmacro core:struct-slot-value (name layout object index)
  (declare (ignore name layout))
  `(clos::standard-instance-access ,object ,index))

(defun read-form-generator (type name layout)
  (case type
    (structure-object
     (lambda (object index) `(core:struct-slot-value ,name ,layout ,object ,index)))
    (list (lambda (object index) `(nth ,index ,object)))
    (vector
     (lambda (object index) `(row-major-aref ,object ,index)))))

(defun write-form-generator (type name layout)
  (case type
    (structure-object
     (lambda (object index new)
       `(setf (core:struct-slot-value ,name ,layout ,object ,index) ,new)))
    (list (lambda (object index new) `(setf (nth ,index ,object) ,new)))
    (vector
     (lambda (object index new) `(setf (row-major-aref ,object ,index) ,new)))))

(defun cas-form-generator (type name layout)
  (if (eq type 'structure-object)
      (lambda (object index keys)
        `(apply #'mp:get-atomic-expansion
                (list 'core:struct-slot-value ',name ',layout ,object ,index)
                ,keys))
      nil))

(defmacro %%defstruct (name type-base element-type (include included-size)
                       (&rest slot-descriptions)
                       layout
                       &key constructors kw-constructors print-function print-object
                         ((:predicate (predicate name-index)) '(nil nil) predicatep)
                         copier unboxable (documentation nil documentationp))
  (let ((decltype (case type-base
                    (structure-object name)
                    (list 'list)
                    (vector `(simple-array ,element-type (*)))))
        (gen-read (read-form-generator type-base name layout))
        (gen-write (write-form-generator type-base name layout))
        (gen-cas (cas-form-generator type-base name layout))
        (alloc (case type-base
                 (structure-object
                  `(allocate-instance
                    ;; The class may not be available at load-time-value time
                    ;; (e.g. if make-whatever is in the same file as this defstruct)
                    ;; so we have to be a bit indirect.
                    (ext:class-get
                     (load-time-value (find-struct-class-holder ',name ',layout)))))
                 (list `(make-list ,(length slot-descriptions)))
                 (vector `(make-array ,(length slot-descriptions)
                                      :element-type ',element-type)))))
    (when unboxable
      ;; Unboxable structs are immutable.
      (setf slot-descriptions (immutable-slot-descriptions slot-descriptions)))
    `(progn
       (define-layout ,name ,layout)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (structure-slot-descriptions ',name) ',slot-descriptions))
       ,@(when (eq type-base 'structure-object)
           `((defclass ,name ,(if include (list include) nil)
               (,@(mapcar #'defstruct-slotd->defclass-slotd slot-descriptions))
               ,@(when documentationp `((:documentation ,documentation)))
               (:metaclass structure-class)
               (:layout ,@layout)
               ,@(when unboxable '((:unboxable t))))
             (setf (find-struct-class ',name ',layout) (find-class ',name))))
       ,@(do ((slotds slot-descriptions (rest slotds))
              (location 0 (1+ location))
              (result nil))
             ((endp slotds) result)
           (when (first slotds) ; skip filler pseudoslots
             (setq result (nconc (gen-defstruct-accessor
                                  decltype (first slotds) location
                                  gen-read gen-write gen-cas)
                                 result))))
       ,@(do ((constructors constructors (rest constructors))
              (result nil))
             ((endp constructors) result)
           (destructuring-bind (name lambda-list) (first constructors)
             (push (defstruct-constructor-def name lambda-list slot-descriptions alloc gen-write)
                   result)
             (push `(declaim (inline ,name)) result)))
       ,@(do ((kwcons kw-constructors (rest kwcons))
              (result nil))
             ((endp kwcons) result)
           (push (defstruct-kw-constructor-def (first kwcons) slot-descriptions alloc gen-write)
                 result))
       ,@(when print-function
           (let ((obj (gensym "OBJ")) (stream (gensym "STREAM")))
             `((defmethod print-object ((,obj ,name) ,stream)
                 (,print-function ,obj ,stream 0)))))
       ,@(when print-object
           (let ((obj (gensym "OBJ")) (stream (gensym "STREAM")))
             `((defmethod print-object ((,obj ,name) ,stream)
                 (,print-object ,obj ,stream)))))
       ,@(when predicatep
           (list
            (case type-base
              (structure-object
               `(defgeneric ,predicate (object)
                  (:method (object) (declare (ignore object)) nil)
                  (:method ((object ,name)) t)))
              (list
               `(defun ,predicate (object)
                  (and (list-of-length-at-least object ,(length slot-descriptions))
                       (eq (nth ,(+ name-index included-size) object) ',name))))
              (vector
               `(defun ,predicate (object)
                  (and (typep object ',decltype)
                       (>= (length object) ,(length slot-descriptions))
                       (eq (row-major-aref object ,(+ name-index included-size)) ',name)))))))
       ,@(when copier
           `((defun ,copier (instance)
               (,(case type-base
                   (structure-object 'copy-structure)
                   (list 'copy-list)
                   (vector 'copy-seq))
                instance))))
       ,@(when documentationp
           `((set-documentation ',name 'structure ',documentation)))
       ,@(when kw-constructors
           `((setf (structure-constructor ',name) ',(first kw-constructors))))
       ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; %DEFSTRUCT is like DEFSTRUCT, but with easier to work with syntax.
;;; Once any INCLUDE is available, this expands into %%DEFSTRUCT, with an
;;; explicit list of slots, and all interning done.
;;; %DEFSTRUCT forms are macroexpansions of the actual DEFSTRUCT macro,
;;; in which syntax has been checked already.
;;;

(defmacro %defstruct ((name conc-name) type include
                      (&rest overriding-slot-descriptions)
                      (&rest slot-descriptions)
                      &rest options)
  (multiple-value-bind (type-base element-type)
      ;; NOTE about :type. CLHS says the structure :TYPE "must be one of"
      ;; LIST, VECTOR, or (VECTOR element-type). Nothing about subtypes
      ;; or expanding deftypes or whatever. We used to use SUBTYPEP here
      ;; but this is simpler and apparently in line with the standard.
      ;; the element-type value here only matters for vectors.
      (cond ((null type) (values 'structure-object 't))
            ((eq type 'list) (values type 't))
            ((eq type 'vector) (values 'vector 't))
            ((and (consp type) (eq (car type) 'vector)
                  (consp (cdr type)) (null (cddr type)))
             (values 'vector (second type)))
            (t (simple-program-error
                "~a is not a valid :TYPE in structure definition for ~a"
                type name)))
    ;; Note that we make the layouts at compile time, meaning we don't need to
    ;; worry about e.g. dumping initforms.
    (cond ((null include)
           `(%%defstruct ,name ,type-base ,element-type (,include 0)
                         (,@slot-descriptions)
                         ,(slotds->layout type-base element-type slot-descriptions)
                         ,@options))
          ((names-structure-p include) ; normal include case
           (let* ((old-layout (structure-layout include))
                  (sub-layout (slotds->layout type-base element-type slot-descriptions))
                  (layout (include-layout name sub-layout include old-layout))
                  (old-slotds (structure-slot-descriptions include))
                  (slotds (final-slot-descriptions
                           conc-name slot-descriptions
                           overriding-slot-descriptions
                           old-slotds)))
             `(%%defstruct ,name ,type-base ,element-type (,include ,(length old-slotds))
                           (,@slotds)
                           ,layout
                           ,@options)))
          (t ; include not defined yet
           ;; FIXME: It's nonconforming to err here -
           ;; the included class could be defined later.
           (error-missing-include name include)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The DEFSTRUCT macro.
;;;

(defun check-defstruct-option-too-many-args (name extra)
  (unless (null extra)
    (simple-program-error "Too many options to ~a" name)))

(defun error-defstruct-option-duplicated (name)
  (simple-program-error "Multiple ~a options to defstruct" name))

(defun error-defstruct-options-incompatible (name1 name2)
  (simple-program-error "~a and ~a options to defstruct are incompatible"
                        name1 name2))

(defun error-unknown-defstruct-option (name)
  (simple-program-error "~a is not a valid option to defstruct" name))

(defun default-constructor-name (name)
  (intern (base-string-concatenate "MAKE-" name)))

(defun default-copier-name (name)
  (intern (base-string-concatenate "COPY-" name)))

(defun default-predicate-name (name)
  (intern (base-string-concatenate name "-P")))

;;; Given the second of a defstruct, returns values:
;;; name, type, include or NIL, overriding slot specs,
;;; conc-name (normalized to a string), list of constructors with BOAs,
;;; list of keyword-driven constructors, copier name or NIL, ditto predicate,
;;; a boolean indicating whether the structure is named,
;;; print-function name or NIL, ditto print-object, initial-offset or NIL,
;;; a boolean indicating whether the structure is unboxable.
;;; Any symbols that need to be interned are interned in *package*.
(defun parse-defstruct-options (name&opts)
  (multiple-value-bind (name options)
      (cond ((consp name&opts)
             (values (car name&opts) (cdr name&opts)))
            ((symbolp name&opts)
             (values name&opts nil))
            (t
             (simple-program-error
              "Name of a structure class must be a symbol, not ~a"
              name&opts)))
    (let (type include conc-name seen-conc-name
          overriding-slot-descriptions
          constructors kw-constructors no-constructor
          predicate seen-predicate copier seen-copier (named nil)
          print-function print-object initial-offset seen-initial-offset
          (unboxable nil))
      (do ((os options (cdr os)))
          ((endp os))
        (let ((option (car os)))
          (if (and (consp option)
                   (consp (cdr option)))
              (let ((opt-name (car option))
                    (second (cadr option))
                    (rest (cddr option)))
                (case opt-name
                  ((:conc-name)
                   (check-defstruct-option-too-many-args :conc-name rest)
                   (if seen-conc-name
                       (simple-program-error "Specified ~a more than once"
                                             :conc-name)
                       (setq conc-name (if (null second)
                                           nil
                                           (string second))
                             seen-conc-name t)))
                  ((:constructor)
                   (cond ((null second) ; no constructor
                          (setq no-constructor t))
                         ((null rest) ; keyword constructor
                          (push second kw-constructors))
                         (t ; BOA constructor
                          (let ((boa (first rest)))
                            (check-defstruct-option-too-many-args
                             :constructor (rest rest))
                            (push (list second boa) constructors)))))
                  ((:copier)
                   (check-defstruct-option-too-many-args :copier rest)
                   (if seen-copier
                       (error-defstruct-option-duplicated :copier)
                       (setq seen-copier t))
                   (unless (symbolp second)
                     (simple-program-error "~a option must specify a symbol"
                                           :copier))
                   (setq copier second))
                  ((:predicate)
                   (check-defstruct-option-too-many-args :predicate rest)
                   (if seen-predicate
                       (error-defstruct-option-duplicated :predicate)
                       (setq seen-predicate t))
                   (unless (symbolp second)
                     (simple-program-error
                      "~a option must specify a symbol" :predicate))
                   (setq predicate second))
                  ((:initial-offset)
                   (check-defstruct-option-too-many-args :initial-offset rest)
                   (if seen-initial-offset
                       (error-defstruct-option-duplicated :initial-offset)
                       (setq seen-initial-offset t))
                   (unless (and (integerp second) (>= second 0))
                     (simple-program-error
                      "~a option must specify a nonnegative integer"
                      :initial-offset))
                   (setq initial-offset second))
                  ((:print-function)
                   (check-defstruct-option-too-many-args :print-function rest)
                   (when print-function
                     (error-defstruct-option-duplicated :print-function))
                   (if second
                       (setq print-function second)
                       (simple-program-error
                        "~a option must specify a function name"
                        :print-function)))
                  ((:print-object)
                   (check-defstruct-option-too-many-args :print-object rest)
                   (when print-object
                     (error-defstruct-option-duplicated :print-object))
                   (if second
                       (setq print-object second)
                       (simple-program-error
                        "~a option must specify a function name, not NIL"
                        :print-object)))
                  ((:type)
                   (check-defstruct-option-too-many-args :type rest)
                   (if type
                       (error-defstruct-option-duplicated :type)
                       (setq type second)))
                  ((:include)
                   (if (null second)
                       (simple-program-error
                        "NIL is not a valid included structure name")
                       (setq include second))
                   (setq overriding-slot-descriptions rest))
                  (otherwise
                   (error-unknown-defstruct-option opt-name))))
              (let ((opt-name (if (consp option) (car option) option)))
                (case opt-name
                  ((:constructor)
                   (push (default-constructor-name name) kw-constructors))
                  ((:conc-name)
                   (if seen-conc-name
                       (simple-program-error
                        "Specified ~a more than once" :conc-name)
                       (setq conc-name nil seen-conc-name t)))
                  ((:copier)
                   (if seen-copier
                       (error-defstruct-option-duplicated :copier)
                       (setq seen-copier t))
                   (setq copier (default-copier-name name)))
                  ((:predicate)
                   (if seen-predicate
                       (error-defstruct-option-duplicated :predicate)
                       (setq seen-predicate t))
                   (setq predicate (default-predicate-name name)))
                  ((:print-function :print-object)) ; FIXME: What do these mean...?
                  ((:named)
                   (cond ((consp option)
                          (simple-program-error
                           "~a was specified but is invalid syntax - it should just be ~a"
                           option :named))
                         (named
                          (error-defstruct-option-duplicated :named))
                         (t (setq named t))))
                  ((:unboxable)
                   (cond ((consp option)
                          (simple-program-error
                           "~a was specified but is invalid syntax - it should just be ~a"
                           option :unboxable))
                         (unboxable (error-defstruct-option-duplicated :unboxable))
                         (t (setq unboxable t))))
                  (otherwise
                   (error-unknown-defstruct-option opt-name)))))))
      ;; We have all the options. Do some final consistency checks,
      ;; and set defaults.
      (if no-constructor
          (unless (and (null constructors) (null kw-constructors))
            (simple-program-error
             "~a was specified, but there were other ~a options"
             '(:constructor nil) :constructor))
          (when (and (null constructors) (null kw-constructors))
            (push (default-constructor-name name) kw-constructors)))
      (when (and (not seen-copier) (null copier))
        (setq copier (default-copier-name name)))
      ;; default predicate + consistency
      (if (and type (not named))
          (when predicate
            (simple-program-error
             "Cannot specify :TYPE and a PREDICATE but not :NAMED, in structure definition for ~a"
             name))
          ;;; This option takes one argument, which specifies the name of the type predicate. 
          ;;; If the argument is provided and is nil, no predicate is defined.
          ;;; If the argument is not supplied or if the option itself is not supplied,
          ;;; the name of the predicate is made by concatenating the name of the structure to the string "-P",
          ;;; interning the name in whatever package is current at the time defstruct is expanded. 
          (unless (or predicate seen-predicate)
            (setq predicate (default-predicate-name name))))
      ;; default conc-name
      (unless seen-conc-name
        (setq conc-name (base-string-concatenate name "-")))
      ;; check initial-offset and type consistency.
      (when initial-offset
        (unless type
          (simple-program-error
           "Structure definition for ~a cannot have :INITIAL-OFFSET without :TYPE."
           name)))
      ;; :named and type consistency.
      (when named
        (unless type
          (simple-program-error
           "Structure definition for ~a cannot have :NAMED without :TYPE."
           name)))
      ;; :unboxable and type consistency.
      (when unboxable
        (when type
          (simple-program-error
           "Structure definition for ~a cannot have both ~a and ~a."
           name type unboxable)))
      ;; :print-object or :print-function and type consistency.
      (when (and print-object print-function)
        (error-defstruct-options-incompatible :print-object :print-function))
      (when type
        (when print-object
          (error-defstruct-options-incompatible :print-object :type))
        (when print-function
          (error-defstruct-options-incompatible :print-function :type)))
      ;; Parse overriding slot descriptions
      (setq overriding-slot-descriptions
            (mapcar (slot-description-parser conc-name) overriding-slot-descriptions))

      (values name type include overriding-slot-descriptions
              conc-name constructors kw-constructors
              copier predicate named
              print-function print-object
              initial-offset unboxable))))

(defmacro defstruct (name&opts &rest slots &environment env)
  "Syntax: (defstruct
         {name | (name {:conc-name | (:conc-name prefix-string) |
                        :constructor | (:constructor symbol [lambda-list]) |
                        :copier | (:copier symbol) |
                        :predicate | (:predicate symbol) |
                        (:include symbol) |
                        (:print-function function) |
                        (:print-object function) |
                        (:type {vector | (vector type) | list}) |
                        :named |
                        (:initial-offset number)}*)}
         [doc]
         {slot-name |
          (slot-name [default-value-form] {:type type | :read-only flag}*) }*
         )
Defines a structure named by NAME.  The doc-string DOC, if supplied, is saved
as a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure)."
  (multiple-value-bind (name type include overriding-slot-descriptions
                        conc-name constructors kw-constructors
                        copier predicate named
                        print-function print-object initial-offset unboxable)
      (parse-defstruct-options name&opts)
    (let ((slot-descriptions slots) name-offset documentation)
      ;; Skip the documentation string.
      (when (and (not (endp slot-descriptions))
                 (stringp (car slot-descriptions)))
        (setq documentation (car slot-descriptions))
        (setq slot-descriptions (cdr slot-descriptions)))

      ;; A specialized vector can't have a name if symbols can't be put in.
      (when named
        (unless (or (subtypep '(vector symbol) type env)
                    (subtypep type 'list env))
          (simple-program-error
           "Structure cannot have type ~S and be :NAMED." type))
        (setq name-offset (or initial-offset 0)))

      ;; Parse slot-descriptions.
      (setq slot-descriptions
            (mapcar (slot-description-parser conc-name) slot-descriptions))

      ;; If TYPE structure is named,
      ;;  add the slot for the structure-name to the slot-descriptions.
      (when named
        (setq slot-descriptions
              (cons (named-slot-description name) slot-descriptions)))

      ;; Pad the slot-descriptions with the initial-offset number of NILs.
      (when initial-offset
        (setq slot-descriptions
              (append (make-list initial-offset) slot-descriptions)))

      `(%defstruct (,name ,conc-name) ,type ,include
         (,@overriding-slot-descriptions)
         (,@slot-descriptions)
         ,@(when constructors `(:constructors (,@constructors)))
         ,@(when kw-constructors `(:kw-constructors (,@kw-constructors)))
         ,@(when print-function `(:print-function ,print-function))
         ,@(when print-object `(:print-object ,print-object))
         ,@(when predicate `(:predicate (,predicate ,name-offset)))
         ,@(when copier `(:copier ,copier))
         ,@(when unboxable `(:unboxable t))
         ,@(when (and documentation *keep-documentation*)
             `(:documentation ,documentation))))))
