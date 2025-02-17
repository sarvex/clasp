;;; Optimization of cons functions
;;; No further relation to ECL file of similar name

(in-package #:cmp)

(define-compiler-macro list (&whole form &rest elements)
  (case (length elements)
    (0 'nil)
    ;; This is a little bit faster and doesn't really involve more code.
    (1 `(cons ,(first elements) nil))
    (t form)))

(define-compiler-macro list* (&whole form object &rest more-objects)
  (case (length more-objects)
    (0 object)
    (1 `(cons ,object ,(first more-objects)))
    (t form)))

(define-compiler-macro null (object) `(if ,object nil t))
(define-compiler-macro endp (object)
  ;; awkward THE to prevent very inefficient arbitrary-values type check in cclasp
  ;; see the notes in opt-number for a little more information
  `(if (the (values list &rest nil) (values ,object)) nil t))

(defconstant +nthcdr-inline-limit+ 8) ; totally arbitrary

(define-compiler-macro nthcdr (&whole whole index list &environment env)
  (if (constantp index env)
      (let ((i (ext:constant-form-value index env)))
        (if (and (integerp i)
                 (<= 0 i)
                 (< i +nthcdr-inline-limit+))
            (loop for result = list then `(cdr ,result)
                  repeat i
                  finally (return result))
            ;; could do optimization notes
            whole))
      whole))

;;; This is of course basically an inline definition, but we do it at this level
;;; so that the nthcdr can have its compiler macro used (which it wouldn't be, with ASTs)
(define-compiler-macro nth (index list)
  `(car (nthcdr ,index ,list)))

(defmacro do-in-list ((%elt %sublist list &rest output) &body body)
  `(do* ((,%sublist ,list (cdr ,%sublist)))
        ((null ,%sublist) ,@output)
     (let ((,%elt (car ,%sublist)))
       ,@body)))

;;; TODO: Avoid iteration for constant list (but watch out for growth)

;;;
;;; MEMBER
;;;

;;; (member foo '(...)) is a common idiom. Notably, our *CASE expand into it.
;;; So we put something in so it won't actually iterate.
(defun expand-constant-member (valuef list key-function test-function init)
  (si::with-unique-names (%value)
    `(let ((,%value ,valuef)
           ,@init)
       (cond ,@(loop for lst on list
                     for elt = (car lst)
                     collecting `(,(funcall test-function
                                            %value
                                            (funcall key-function `',elt))
                                  ',lst))))))

(defun expand-member (env value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init ignores
                        key-flag test-flag)
      (two-arg-test-parse-args 'member sequence-args :start-end nil :environment env)
    (declare (ignore key-flag test-flag))
    ;; When having complex arguments (:allow-other-keys, etc)
    ;; we just give up.
    (when (null key-function)
      (return-from expand-member nil))
    ;; If the list is constant and short, use the special expansion.
    (when (constantp list env)
      (let ((list (ext:constant-form-value list env)))
        (if (core:proper-list-p list)
            (when (< (length list) 10) ; completely arbitrary
              (return-from expand-member
                (expand-constant-member value list key-function test-function init)))
            ;; improper constant list
            (return-from expand-member nil))))
    (si::with-unique-names (%value %list %sublist %elt)
      `(let ((,%value ,value)
             (,%list ,list)
             ,@init)
         (declare (ignore ,@ignores))
         (do-in-list (,%elt ,%sublist ,%list)
           (when ,(funcall test-function %value
                           (funcall key-function %elt))
             (return ,%sublist)))))))

(define-compiler-macro member (&whole whole value list &rest sequence-args &environment env)
  ;; FIXME: pay attention to policy, e.g. don't inline for high SPACE.
  (or (apply #'expand-member env value list sequence-args)
      whole))

;;;
;;; ASSOC
;;;

(defun expand-assoc (env value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init ignores
                        key-flag test-flag)
      (two-arg-test-parse-args 'assoc sequence-args :start-end nil :environment env)
    (declare (ignore key-flag test-flag))
    (when test-function
      (si::with-unique-names (%value %list %sublist %elt %car)
        `(let ((,%value ,value)
               (,%list ,list)
               ,@init)
           (declare (ignore ,@ignores))
           (do-in-list (,%elt ,%sublist ,%list)
             (if (consp ,%elt)
                 (let ((,%car (car (the (values cons &rest nil) (values ,%elt)))))
                   (when ,(funcall test-function %value
                                   (funcall key-function %car))
                     (return ,%elt)))
                 (when ,%elt
                   (error 'type-error :datum ,%elt :expected-type 'list)))))))))

(define-compiler-macro assoc (&whole whole value list &rest sequence-args &environment env)
  (or (apply #'expand-assoc env value list sequence-args)
      whole))

;;;
;;; ADJOIN
;;;

(defun expand-adjoin (env value list &rest sequence-args)
  (multiple-value-bind (key-function test-function init ignores
                        key-flag test-flag)
      (two-arg-test-parse-args 'adjoin sequence-args :start-end nil :environment env)
    (declare (ignore test-flag key-flag))
    (when test-function
      (si::with-unique-names
	  (%value %sublist %elt %list %value-after-key-function-)
	`(let ((,%value ,value)
	       (,%list ,list)
	       ,@init)
           (declare (ignore ,@ignores))
	   (let ((,%value-after-key-function- ,(funcall key-function %value)))
	     (do-in-list (,%elt ,%sublist ,%list (cons ,%value ,%list))
	       (when ,(funcall test-function %value-after-key-function- 
			       (funcall key-function %elt))
		 (return ,%list)))))))))

(define-compiler-macro adjoin (&whole whole value list &rest sequence-args &environment env)
  (declare (ignore value list sequence-args))
  (or (apply #'expand-adjoin env (rest whole))
      whole))

;;;
;;; APPEND
;;;

;;; backquote expands into this kind of thing sometimes.
(define-compiler-macro append (&whole form &rest lists &environment env)
  (flet ((constant-nil-p (form)
           (and (constantp form env)
                (null (ext:constant-form-value form env))))
         (list-form-p (form)
           (and (consp form)
                (eq (first form) 'list)
                (core:proper-list-p (rest form)))))
    (cond ((null lists) 'nil)
          ((null (cdr lists)) `,(first lists))
          ((member-if #'constant-nil-p (butlast lists))
           ;; Remove NILs.
           ;; We do butlast because of issue #799:
           ;; (append x nil) has to copy x but (append x) doesn't.
           `(append ,@(remove-if #'constant-nil-p (butlast lists))
                    ,@(last lists)))
          ;; (append (list ...) ...) => (list* ... (append ...))
          ((list-form-p (first lists))
           (loop for (list . rem) on lists
                 if (list-form-p list)
                   append (rest list) into elems
                 else
                   do (return `(list* ,@elems (append ,list ,@rem)))
                 ;; every element was a list form.
                 finally (return `(list ,@elems))))
          (t form))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (compiler-macro-function 'core:backquote-append)
        (compiler-macro-function 'append)))
