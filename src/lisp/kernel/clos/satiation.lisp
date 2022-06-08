(in-package "CLOS")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Satiation of generic functions to start fastgf
;;;
;;; Ideas copied from Sicl/Code/CLOS/satiation.lisp
;;;

;;; Essentially, for some gfs we need in a consistent state for the system to work,
;;; during boot we fake a call history so that they can be called without invoking
;;; gfs such as themselves that haven't yet been placed in a working state.
;;; A fake call history is also nice for efficiency - if we install an anticipated
;;; history beforehand, we can avoid repeatedly compiling new discriminators.

;;; In order to do things both at boot and in an exported interface, we duplicate
;;; some code. Unfortunate but I don't see a good way to avoid it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UTILITY
;;;

;;; Add a portion of call history into a gf's existing call history.
;;; If any entry to be added duplicates an existing entry, the new entry prevails.
(defun append-generic-function-call-history (generic-function new-entries)
  (loop for call-history = (mp:atomic (safe-gf-call-history generic-function))
        ;; By keeping the new entry, remove-if will return immediately in the
        ;; usual case that the existing history is empty.
        for cleaned-call-history = (remove-if (lambda (entry)
                                                (call-history-find-key
                                                 new-entries (car entry)))
                                              call-history)
        for new-history = (append new-entries cleaned-call-history)
        for exchange = (mp:cas (safe-gf-call-history generic-function)
                               call-history new-history)
        until (eq exchange call-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BOOT-TIME SATIATION

;;; Satiation should occur before any generic function calls are performed, and
;;; involve no generic function calls.

;;; effective-slot-from-accessor-method, but without gfs
(defun early-effective-slot-from-accessor-method (method class)
  (with-early-accessors (+standard-accessor-method-slots+
                         +standard-slot-definition-slots+
                         +class-slots+)
    (let* ((direct-slot-definition (accessor-method-slot-definition method))
           (direct-slot-name (slot-definition-name direct-slot-definition))
           (slot (loop for effective-slot in (class-slots class)
                       when (eq direct-slot-name (slot-definition-name effective-slot))
                         return effective-slot)))
      (if slot
          slot
          (error "Bug during satiation: Could not find effective slot definition for ~a"
                 direct-slot-name)))))

;;; Used by add-satiation-entries to compute an effective method without going through
;;; compute-effective-method and method-combinations, which call a few gfs.
;;; Standard method combination only. No qualifiers allowed (checks this).
;;; It could be improved to handle them; I just don't think we need to satiate anything
;;; that uses qualified methods.
;;; Does handle accessor methods so that they're fast, yet.
;;; FIXME: Duplication of other code, in this case compute-outcome, sucks.
(defun early-compute-outcome (methods specializers)
  (with-early-accessors (+standard-method-slots+
                         +standard-slot-definition-slots+)
    (mapc (lambda (method)
            (when (method-qualifiers method)
              ;; Hopefully the write won't trigger a recursive error...?
              (error "Bug during satiation: method to be satiated ~a has qualifiers"
                     method)))
          methods)
    ;; Methods are sorted by std-compute-applicable-methods-using-classes, so
    ;; we're just doing (call-method ,first (,@rest))
    (let ((first (first methods)))
      ;; realistically, anything we satiate is going to be standard classes, but
      ;; paranoia doesn't hurt here.
      (cond ((optimizable-reader-method-p first)
             (let* ((class (first specializers))
                    (slot (early-effective-slot-from-accessor-method
                           first (first specializers))))
               (make-optimized-slot-reader :index (slot-definition-location slot)
                                           :slot-name (slot-definition-name slot)
                                           :methods (list first)
                                           :class class)))
            ((optimizable-writer-method-p first)
             (let* ((class (second specializers))
                    (slot (early-effective-slot-from-accessor-method
                           first (first specializers))))
               (make-optimized-slot-writer :index (slot-definition-location slot)
                                           :slot-name (slot-definition-name slot)
                                           :methods (list first)
                                           :class class)))
            (t ; general effective method function
             (let ((form `(call-method ,first (,@(rest methods)))))
               (make-effective-method-outcome
                :function (early-effective-method-function form)
                :form form
                :methods methods)))))))

;;; Add fictitious call history entries.
(defun add-satiation-entries (generic-function lists-of-specializers)
  (let ((new-entries
          (loop for specific-specializers in lists-of-specializers
                for methods = (std-compute-applicable-methods-using-classes
                               generic-function specific-specializers)
                ;; Simple cache to avoid duplicate outcomes.
                for cached-outcome
                  = (cdr (assoc methods outcome-cache :test #'equal))
                ;; Everything in early satiation uses standard method combination.
                for outcome = (or cached-outcome
                                  (early-compute-outcome
                                   methods specific-specializers))
                unless cached-outcome
                  collect (cons methods outcome)
                    into outcome-cache
                collect (cons (coerce specific-specializers 'vector) outcome))))
    (append-generic-function-call-history generic-function new-entries)))

(defun early-satiate (generic-function &rest lists-of-specializers)
  ;; Many generic functions at startup will be missing specializer-profile at startup
  ;;    so we compute one here using the number of required arguments in the lambda-list.
  ;; The call-history may be incorrect because of improper initialization as
  ;;    clos starts up - so lets wipe it out and then satiate it.
  (gf-log "Starting satiate-generic-function%N")
  ;; Wipe out the call-history and satiate it using methods
  (gf-log "About to set call history%N")
  (erase-generic-function-call-history generic-function)
  (add-satiation-entries generic-function lists-of-specializers)
  ;; Now when the function is called the discriminating-function will be invalidated-dispatch-function
  ;; This well set up the real discriminating function. This shouldn't involve a dispatch miss, and
  ;; no generic-function calls (other than the one for the actual call, of course).
  (invalidate-discriminating-function generic-function))

;;; Satiate the minimum set of functions to make the system work.
;;; Essentially those that are used to compute new call history entries in the full system.
(defun satiate-minimal-generic-functions ()
  (macrolet ((satiate-one (gf-name &body lists-of-class-names)
               `(prog2
                    (gf-log ,(concatenate 'string "Satiating " (string gf-name) "%N"))
                    (early-satiate
                     (fdefinition ',gf-name)
                     ,@(loop for list in lists-of-class-names
                             collect `(list ,@(loop for name in list
                                                    collect `(find-class ',name)))))
                  (gf-log ,(concatenate 'string "Done satiating " (string gf-name) "%N")))))
    (satiate-one class-slots
                 (standard-class)
                 (funcallable-standard-class))
    ;; instance updates we shouldn't need to satiate...
    (satiate-one compute-applicable-methods-using-classes
                 (standard-generic-function cons)
                 (standard-generic-function null)) ; nulls may not be necessary, but no big.
    (satiate-one compute-applicable-methods
                 (standard-generic-function cons)
                 (standard-generic-function null))
    (satiate-one compute-effective-method
                 (standard-generic-function method-combination cons)
                 (standard-generic-function method-combination null))
    ;; We should satiate the method combination accessors, but we actually
    ;; just use early accessors at the moment... which is probably wrong (FIXME?)
    ;; Method readers are used by make-effective-accessor-method.
    (macrolet ((satiate-method-reader (name)
                 `(satiate-one ,name
                               (standard-method)
                               (standard-reader-method)
                               (standard-writer-method)
                               (effective-reader-method)
                               (effective-writer-method))))
      (satiate-method-reader method-generic-function)
      (satiate-method-reader method-lambda-list)
      (satiate-method-reader method-specializers)
      (satiate-method-reader method-qualifiers)
      (satiate-method-reader method-function)
      (satiate-method-reader method-source-position)
      (satiate-method-reader method-plist)
      (satiate-method-reader method-keywords)
      (satiate-method-reader method-allows-other-keys-p)
      (satiate-method-reader leaf-method-p))
    (satiate-one accessor-method-slot-definition
                 (standard-reader-method) (standard-writer-method))
    (satiate-one slot-definition-allocation
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one slot-definition-name
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one slot-definition-location
                 (standard-direct-slot-definition)
                 (standard-effective-slot-definition))
    (satiate-one generic-function-name
                 (standard-generic-function))
    (satiate-one generic-function-method-combination
                 (standard-generic-function))
    ;; This one is needed for the initial specializer profile computation in fixup.
    ;; (i.e., it's called by initialize-generic-function-specializer-profile)
    (satiate-one generic-function-lambda-list
                 (standard-generic-function))
    (satiate-one leaf-method-p
                 (standard-method)
                 (standard-reader-method) (standard-writer-method))))

;;; Used in the make-load-form for METHOD.
;;; It's like find-method, but always signals an error, and only accepts
;;; actual specializers.
(defun load-method (gf-name qualifiers specializers)
  (let ((gf (fdefinition gf-name)))
    (if (eq (class-of gf) (find-class 'standard-generic-function))
        ;; Get method without calling generic functions, in case we're
        ;; loading a CLOS satiated definition.
        (with-early-accessors (+eql-specializer-slots+
                               +standard-generic-function-slots+
                               +standard-method-slots+)
          (when (/= (length specializers)
                    (length (generic-function-argument-precedence-order gf)))
            (error
             "The specializers list~%~A~%does not match the number of required arguments in ~A"
             specializers (core:low-level-standard-generic-function-name gf)))
          (loop for method in (generic-function-methods gf)
                when (and (equal qualifiers (method-qualifiers method))
                          ;; We can use EQ because it obviously works for classes,
                          ;; and eql specializers have an internment mechanism.
                          (every #'eq specializers (method-specializers method)))
                  do (return-from load-method method))
          (error "There is no method on the generic function ~S that agrees on qualifiers ~S and specializers ~S"
                 (core:low-level-standard-generic-function-name gf)
                 qualifiers specializers))
        (find-method gf qualifiers specializers t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DISCRIMINATING FUNCTIONS IN COMPILE-FILE
;;;

;; Given a list of specializer names or specializers, returns a list of specializers.
(defun coerce-to-list-of-specializers (list-of-specializer-names)
  (mapcar (lambda (sname)
            (etypecase sname
              (specializer sname)
              ;; (eql 'something)
              ((cons (eql eql)
                     (cons t null))
               (intern-eql-specializer (second sname)))
              (symbol (find-class sname))))
          list-of-specializer-names))

(defun compile-time-call-history (generic-function &rest lists-of-specializer-names)
  (loop with mc = (generic-function-method-combination generic-function)
        for list-of-specializer-names in lists-of-specializer-names
        for list-of-specializers
          = (coerce-to-list-of-specializers list-of-specializer-names)
        for am = (compute-applicable-methods-using-specializers
                  generic-function list-of-specializers)
        for cached-outcome = (find am outcome-cache :test #'equal
                                   :key #'outcome-methods)
        for outcome = (or cached-outcome
                          (compute-outcome generic-function mc am list-of-specializers))
        when (not cached-outcome)
          collect outcome into outcome-cache
        collect (cons (coerce list-of-specializers 'vector) outcome)
          into entries
        nconc (remove-if-not (lambda (sp) (typep sp 'class))
                             list-of-specializers)
          into all-classes
        finally (return (values entries all-classes))))

;;; Given an outcome, return a form that, when evaluated, returns an outcome
;;; similar to it.
(defun outcome-producer (outcome &optional (arg-info '(t)))
  ;; Note that this relies on methods being dumpable, which we
  ;; establish with their MAKE-LOAD-FORM.
  (cond ((optimized-slot-reader-p outcome)
         `(make-optimized-slot-reader
           ;; FIXME: Probably not correct for :allocation :class.
           :index ',(optimized-slot-reader-index outcome)
           :slot-name ',(optimized-slot-reader-slot-name outcome)
           :methods ',(outcome-methods outcome)
           :class ,(optimized-slot-reader-class outcome)))
        ((optimized-slot-writer-p outcome)
         `(make-optimized-slot-writer
           ;; FIXME: Probably not correct for :allocation :class.
           :index ',(optimized-slot-writer-index outcome)
           :slot-name ',(optimized-slot-writer-slot-name outcome)
           :methods ',(outcome-methods outcome)
           :class ,(optimized-slot-writer-class outcome)))
        ((effective-method-outcome-p outcome)
         ;; The handling of :function is basically the only reason
         ;; we don't just dump the call history and let the usual
         ;; literal mechanisms handle it.
         (let ((form (effective-method-outcome-form outcome))
               (required (rest arg-info))
               (rest (if (car arg-info) 'satiated-more nil)))
           `(make-effective-method-outcome
             :methods ',(outcome-methods outcome)
             :form ',form
             :function (lambda (,@required
                                ,@(when rest `(core:&va-rest ,rest)))
                         (with-effective-method-parameters
                             ((,@required) ,rest)
                           ,form)))))
        (t (error "BUG: Don't know how to reconstruct outcome: ~a"
                  outcome))))

;;; Given a call history, return a form that, when evaluated,
;;; returns a call history similar to the
;;; provided one, and furthermore is dumpable.
(defun call-history-producer (call-history &optional (arg-info '(t)))
  (loop for (key . outcome) in call-history
        for cached-outcome-info = (assoc outcome outcome-cache)
        for name = (or (second cached-outcome-info) (gensym "OUTCOME"))
        for outcome-form
          = (or (third cached-outcome-info)
                (outcome-producer outcome arg-info))
        unless cached-outcome-info
          collect (list outcome name outcome-form) into outcome-cache
        ;; Keys are vectors of classes and lists (representing eql specializers)
        ;; and should therefore be dumpable.
        collect `(cons ,key ,name) into new-ch
        finally
           (return
             `(let (,@(loop for (_ name form) in outcome-cache
                            collect `(,name ,form)))
                (list ,@new-ch)))))

(defun compile-time-discriminator (generic-function call-history)
  (multiple-value-bind (min max)
      (generic-function-min-max-args generic-function)
    (let ((name (generic-function-name generic-function)))
      (values
       (generate-discriminator-from-data
        call-history (safe-gf-specializer-profile generic-function)
        `(load-time-value (fdefinition ',name) t) min max
        :inline-effective-methods 'cl:require
        :generic-function-name name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GENERAL SATIATION INTERFACE

;;; Main entry point
;;; Actually quite simple.
;;; Possible improvement: Put in some handler-case stuff to reduce any errors to
;;; warnings, given that this is just for optimization.
(defun satiate (generic-function &rest lists-of-specializer-designators)
  "Prepare a generic function so that its discriminating function will not have
to be recompiled very much when called with pre-specified specializers.
GENERIC-FUNCTION is a generic function. LISTS-OF-SPECIALIZER-DESIGNATORS is a
list of lists of specializer designator. Each inner list should have as many
elements as the generic function has specializable (i.e. required) arguments.
A specializer designator is either a specializer, or a symbol naming a class, or
a list (EQL object) - just like DEFMETHOD."
  (flet ((coerce-specializer-designator (specializer-designator)
           (etypecase specializer-designator
             (specializer specializer-designator)
             (symbol (find-class specializer-designator))
             ((cons (eql eql) (cons t null)) ; (eql thing)
              (intern-eql-specializer (second specializer-designator))))))
    (loop with method-combination = (generic-function-method-combination generic-function)
          for list in lists-of-specializer-designators
          for specializers = (mapcar #'coerce-specializer-designator list)
          for applicable-methods
            = (compute-applicable-methods-using-specializers generic-function specializers)
          for outcome = (compute-outcome generic-function method-combination
                                         applicable-methods specializers)
          collect (cons (coerce specializers 'simple-vector) outcome) into history
          finally (append-generic-function-call-history generic-function history))))

;;; The less simple part is doing things at compile-time.
;;; We can't put discriminating functions into FASLs because they include the class
;;; stamps, which are hard to synchronize between compile- and load-time.
;;; But we can dump (invented) call histories, including ones with functions in them.
;;; It's somewhat convoluted however.
;;; Anywho, we do that for SATIATE calls provided
;;; 1) the GENERIC-FUNCTION form is #'foo
;;; 2) all the lists of specializer designators are constant.
;;; At the moment, it also requires that the generic function and all relevant methods
;;; are defined at compile time. If we store information about DEFMETHODs in the
;;; environment at compile time, though, that shouldn't be required.
;;; (EQL specializers will be a bit weird, though.)
;;; NOTE: As of now we dump with class stamps anyway. This is okay for system code
;;; where the stamps work out identically, but for library code it would be bad.
;;; Might want to make a separate macro or something.

(define-compiler-macro satiate
    (&whole form generic-function &rest lists &environment env)
  (if (and (consp generic-function)
           (eq (car generic-function) 'function)
           (consp (cdr generic-function))
           (null (cddr generic-function))
           (fboundp (second generic-function))
           (loop for list in lists always (constantp list env)))
      `(%satiate ,(second generic-function)
                 ,@(mapcar (lambda (form) (ext:constant-form-value form env)) lists))
      form))

;;; This function checks that stamps at satiation time match stamps at load
;;; time. If this is not true, objects will end up in methods for other
;;; classes, which can cause very strange problems; verifying will check for
;;; this ahead of time, when the precompiled discriminating function is loaded.
(defun verify-stamp-consistency (alist)
  (let ((inconsistent
          (loop for (class . compile-stamp) in alist
                unless (= (core:class-stamp-for-instances class)
                          compile-stamp)
                  collect class)))
    (unless (null inconsistent)
      (error "Stamps for the following classes changed between build and load time: ~a"
             inconsistent))))

;;; Macro version of SATIATE, that the exported function sometimes expands into.
(defmacro %satiate (generic-function-name &rest lists-of-specializer-names)
  (let ((generic-function (fdefinition generic-function-name)))
    (multiple-value-bind (call-history classes)
        (apply #'compile-time-call-history
               generic-function lists-of-specializer-names)
      `(let* ((gf (fdefinition ',generic-function-name)))
         (verify-stamp-consistency
          '(,@(loop for class in classes
                    collect (cons class
                                  (core:class-stamp-for-instances class)))))
         (append-generic-function-call-history
          gf
          ,(call-history-producer call-history (gf-arg-info generic-function)))
         (set-funcallable-instance-function
          gf ,(compile-time-discriminator generic-function call-history))))))

;;; Exported auxiliary version for the common case of wanting to skip recompilations
;;; of shared-initialize etc. Just pass it a list of class designators and it'll fix
;;; up the CLOS initialization functions.
(defun satiate-initialization (&rest class-designators)
  (let ((tail (mapcar #'list class-designators)))
    (apply #'satiate #'initialize-instance tail)
    (apply #'satiate #'reinitialize-instance tail))
  (apply #'satiate #'shared-initialize
         (loop for classd in class-designators
               collect (list classd 'symbol)
               collect (list classd 'cons)
               collect (list classd 'null))))

(define-compiler-macro satiate-initialization (&whole form &rest classdfs &environment env)
  (if (every (lambda (classdf) (constantp classdf env)) classdfs)
      (let* ((classds (mapcar (lambda (classdf) (ext:constant-form-value classdf env)) classdfs))
             (tail (mapcar #'list classds)))
        `(progn
           (%satiate initialize-instance ,@tail)
           (%satiate reinitialize-instance ,@tail)
           (%satiate shared-initialize
                     ,@(loop for classd in classds
                             collect `(,classd symbol)
                             collect `(,classd cons)
                             collect `(,classd null)))))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SATIATION OF SPECIFIC CLOS FUNCTIONS
;;;
;;; Used in boot

;;; This macro is a holdover from the older satiation mechanism, which had to do
;;; more special things early on. I'm keeping it at least for now in case we
;;; need to go back to doing that kind of thing.

(defmacro %early-satiate (generic-function-name &rest lists-of-specializer-names)
  `(%satiate ,generic-function-name ,@lists-of-specializer-names))

(defmacro satiate-clos ()
  ;; This is the ahead-of-time satiation. If we get as much as possible we can speed startup a bit.
  (labels ((readers-from-slot-description (slot-description)
             (loop for (key arg) on (cdr slot-description) by #'cddr
                   when (eq key :reader)
                     collect arg
                   when (eq key :accessor)
                     collect arg))
           (satiate-readers (slot-descriptions specializerses)
             (loop for slotdesc in slot-descriptions
                   for readers = (readers-from-slot-description slotdesc)
                   nconc (loop for reader in readers
                               collect `(%early-satiate ,reader ,@specializerses)))))
    `(progn
       ,@(satiate-readers +specializer-slots+ '((eql-specializer)
                                                (standard-class) (funcallable-standard-class)
                                                (structure-class) (built-in-class) (core:cxx-class)
                                                (core:derivable-cxx-class) (core:clbind-cxx-class)))
       ;; eql-specializer has only the one special slot, so we just do it manually.
       (%early-satiate eql-specializer-object (eql-specializer))
       ,@(satiate-readers (set-difference +class-slots+ +specializer-slots+)
                          '((standard-class) (funcallable-standard-class)
                            (structure-class) (built-in-class) (core:cxx-class)
                            (core:derivable-cxx-class) (core:clbind-cxx-class)))
       ,@(satiate-readers +standard-method-slots+ '((standard-method)
                                                    (standard-reader-method) (standard-writer-method)
                                                    (effective-reader-method) (effective-writer-method)))
       ,@(satiate-readers (set-difference +standard-accessor-method-slots+ +standard-method-slots+)
                          '((standard-reader-method) (standard-writer-method)
                            (effective-reader-method) (effective-writer-method)))
       ,@(satiate-readers +standard-slot-definition-slots+
                          '((standard-direct-slot-definition) (standard-effective-slot-definition)))
       ,@(satiate-readers +standard-generic-function-slots+
                          '((standard-generic-function)))
       (%early-satiate generic-function-name (standard-generic-function))
       (%early-satiate (setf generic-function-name)
                       (cons standard-generic-function)
                       (symbol standard-generic-function))
       ;; Writers are done manually since the new-value classes are tricky to sort out
       (macrolet ((satiate-specializer-writer (name &rest types) ; i mean, the types are classes though.
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for class in '(eql-specializer standard-class funcallable-standard-class
                                             structure-class built-in-class core:cxx-class
                                             core:clbind-cxx-class core:derivable-cxx-class)
                              nconc (loop for type in types
                                          collect `(,type ,class))))))
         (satiate-specializer-writer %specializer-direct-methods null cons))
       (macrolet ((satiate-class-writer (name &rest types)
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for class in '(standard-class funcallable-standard-class
                                             structure-class built-in-class core:cxx-class
                                             core:clbind-cxx-class core:derivable-cxx-class)
                              nconc (loop for type in types collect `(,type ,class))))))
         (satiate-class-writer class-name symbol)
         (satiate-class-writer %class-direct-superclasses null cons)
         (satiate-class-writer %class-direct-subclasses null cons)
         (satiate-class-writer %class-slots null cons)
         (satiate-class-writer %class-precedence-list null cons)
         (satiate-class-writer %class-direct-slots null cons)
         (satiate-class-writer %class-default-initargs null cons)
         (satiate-class-writer %class-finalized-p symbol) ; don't really "unfinalize", so no null
         (satiate-class-writer class-size fixnum)
         (satiate-class-writer class-dependents null cons)
         (satiate-class-writer class-valid-initargs null cons)
         (satiate-class-writer creator core:funcallable-instance-creator core:instance-creator))
       (macrolet ((satiate-method-writer (name &rest types)
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for class in '(standard-method
                                             standard-writer-method standard-reader-method)
                              nconc (loop for type in types collect `(,type ,class))))))
         (satiate-method-writer %method-generic-function standard-generic-function)
         (satiate-method-writer method-plist null cons)
         (satiate-method-writer method-keywords null cons) ; note: why is this being called?
         (satiate-method-writer method-allows-other-keys-p null cons))
       (macrolet ((satiate-slotd-writer (name &rest types)
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for class in '(standard-direct-slot-definition
                                             standard-effective-slot-definition)
                              nconc (loop for type in types collect `(,type ,class))))))
         (satiate-slotd-writer %slot-definition-location fixnum cons))
       (macrolet ((satiate-gf-writer (name &rest types)
                    `(%early-satiate
                      (setf ,name)
                      ,@(loop for type in types collect `(,type standard-generic-function)))))
         (satiate-gf-writer %generic-function-method-combination method-combination)
         (satiate-gf-writer %generic-function-argument-precedence-order cons)
         (satiate-gf-writer %generic-function-methods null cons)
         (satiate-gf-writer generic-function-dependents null cons))
       ;; also done in function-to-method
       (%early-satiate compute-applicable-methods-using-classes
                       (standard-generic-function cons)
                       (standard-generic-function null))
       ;; also done in function-to-method
       (%early-satiate compute-applicable-methods
                       (standard-generic-function cons)
                       (standard-generic-function null))
       ;; also done in function-to-method
       (%early-satiate compute-effective-method
                       (standard-generic-function method-combination cons)
                       (standard-generic-function method-combination null))
       (%early-satiate make-instance (symbol) (standard-class) (funcallable-standard-class))
       (%early-satiate allocate-instance (standard-class) (funcallable-standard-class) (structure-class))
       (%early-satiate add-direct-subclass
                       (standard-class standard-class) (funcallable-standard-class funcallable-standard-class)
                       (built-in-class standard-class) ; for gray streams
                       (structure-class structure-class))
       (%early-satiate validate-superclass
                       (standard-class standard-class) (funcallable-standard-class funcallable-standard-class)
                       (structure-class structure-class) (standard-class built-in-class))
       (macrolet ((satiate-classdefs (&rest classes)
                    (let ((tail (mapcar #'list classes)))
                      `(progn (%early-satiate finalize-inheritance ,@tail)
                              (%early-satiate compute-class-precedence-list ,@tail)
                              (%early-satiate compute-slots ,@tail)
                              (%early-satiate class-name ,@tail)
                              (%early-satiate class-prototype ,@tail)
                              (%early-satiate compute-default-initargs ,@tail)
                              (%early-satiate direct-slot-definition-class ,@tail)
                              (%early-satiate effective-slot-definition-class ,@tail)))))
         (satiate-classdefs standard-class funcallable-standard-class structure-class
                            built-in-class core:derivable-cxx-class core:clbind-cxx-class))
       (%early-satiate compute-effective-slot-definition-initargs
                       (standard-class cons) (funcallable-standard-class cons)
                       (structure-class cons))
       (%early-satiate compute-effective-slot-definition
                       (standard-class symbol cons) (funcallable-standard-class symbol cons)
                       (structure-class symbol cons))
       (%early-satiate ensure-class-using-class (standard-class symbol) (null symbol))
       (%early-satiate function-keywords (standard-method) (standard-reader-method) (standard-writer-method))
       (%early-satiate add-direct-method
                       (structure-class standard-method) (eql-specializer standard-method)
                       (standard-class standard-method) (funcallable-standard-class standard-method)
                       (standard-class standard-reader-method) (standard-class standard-writer-method)
                       (built-in-class standard-method)
                       (built-in-class standard-writer-method) ; for the new-value argument
                       (funcallable-standard-class standard-reader-method)
                       (funcallable-standard-class standard-writer-method))
       (%early-satiate remove-direct-method
                       (structure-class standard-method) (eql-specializer standard-method)
                       (standard-class standard-method) (funcallable-standard-class standard-method)
                       (standard-class standard-reader-method) (standard-class standard-writer-method)
                       (built-in-class standard-method)
                       (built-in-class standard-writer-method) ; for the new-value argument
                       (funcallable-standard-class standard-reader-method)
                       (funcallable-standard-class standard-writer-method))
       (%early-satiate ensure-generic-function-using-class
                       (standard-generic-function symbol) (null symbol))
       ;; these are obviously not complete, but we can throw em in.
       (macrolet ((partly-satiate-initializations (&rest classes)
                    (let ((tail (mapcar #'list classes)))
                      `(progn
                         (%early-satiate initialize-instance ,@tail)
                         (%early-satiate shared-initialize
                                         ,@(loop for class in classes
                                                 collect `(,class symbol)
                                                 collect `(,class cons)
                                                 collect `(,class null)))
                         (%early-satiate reinitialize-instance ,@tail)))))
         (partly-satiate-initializations
          standard-generic-function standard-method standard-class structure-class
          standard-reader-method standard-writer-method
          standard-direct-slot-definition standard-effective-slot-definition
          eql-specializer method-combination funcallable-standard-class))
       (%early-satiate make-instances-obsolete (standard-class) (funcallable-standard-class) (structure-class)))))
