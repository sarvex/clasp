;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;; -*- Package: PRETTY-PRINT -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;
;;; CMU Common Lisp pretty printer.
;;; Written by William Lott.  Algorithm stolen from Richard Waters' XP.
;;;
  
(in-package "SI")

;;; The guts of print-unreadable-object, inspired by SBCL. This is
;;; a redefinition of the function in iolib.lisp which add support
;;; for pprint-logical-block.
(defun %print-unreadable-object (object stream type identity body)
  (cond (*print-readably*
         (error 'print-not-readable :object object))
        ((and *print-pretty* (inravina:pretty-stream-p stream))
         (pprint-logical-block (stream nil :prefix "#<" :suffix ">")
           (print-unreadable-object-contents object stream type identity body)))
        (t
         (let ((stream (cond ((null stream)
                              *standard-output*)
                             ((eq t stream)
                              *terminal-io*)
                             (t
                              stream))))
           (write-string "#<" stream)
           (print-unreadable-object-contents object stream type identity body)
           (write-char #\> stream))))
  nil)
