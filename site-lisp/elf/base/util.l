;;; -*- mode: lisp; package: elf.util -*-
;;;
;;; elf/base/util.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "elf".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "elf/base/dependency"))

(defpackage :elf.util
  (:use :lisp :editor)
  (:shadowing-import-from :ansi-loop
   #:loop #:loop-finish)
  (:shadowing-import-from :ansify
   #:assert #:check-type #:etypecase #:destructuring-bind))

(in-package :elf.util)


;;;; misc

(export '(set-indent
          symb))

(defun set-indent (operator indent)
  (setf (get operator 'ed::lisp-indent-hook) indent))

(defun symb (name &rest args)
  (intern (apply #'format nil name args)))

(defun kwd (name &rest args)
  (intern (apply #'format nil name args) :keyword))

(defun gensym* (name &rest args)
  (gensym (apply #'format nil name args)))


;;;; namespace (a.k.a. one-package-per-one-file)

(export '(namespace
          elf-export
          ))

(defmacro namespace (name &rest options)
  #+elf/safety (check-type name (or symbol string))
  `(progn
     (defpackage ,name ,@options)
     (in-package ,name)))
(set-indent 'namespace 1)

(defmacro elf-export (symbols)
  "Export SYMBOLS form package \"elf\"."
  `(eval-when (:execute :compile-toplevel :load-toplevel)
     (import ,symbols :elf)
     (export ,symbols :elf)))



;;;; development support

(export '(@debug
          @not-implemented
          @should-not-see-mee
          ))

(defmacro @debug (datum &rest args)
  `(format *debug-io*
           ,@(cond ((stringp datum) `(,datum ,@args))
                   (t `("~:{* ~S => ~S~%~}"
                        (list ,@(loop for form in (cons datum args)
                                  collect `(list ',form ,form))))))))

(defmacro @not-implemented (&rest args)
  (if (stringp (car args))
    `(error ,(concat "Not Implemented: " (car args)) ,@(cdr args))
    `(error "Not Implemented:~{ ~S~}" ,@args)))

(defmacro @should-not-see-me ()
  `(error "Should not see me..."))


;;;; types

(export '(string-designator
          list-of
          callable
          ))

(deftype string-designator ()
  `(or string symbol))

(defparameter *list-element-predicate-table* (make-hash-table :test 'equal))

(defun get-list-element-predicate (element-type)
  (or (gethash element-type *list-element-predicate-table*)
      (let ((expr (ansify.ext:optimize-type-check 'x element-type))
            (fn   (coerce `(lambda (list) (every (lambda (x) ,expr) list))
                          'function))
            (pred (gensym* "(list-of ~S)" element-type)))
        (setf (symbol-function pred) fn
              (gethash element-type *list-element-predicate-table*) pred))))

(deftype list-of (&optional element-type)
  (cond ((eql element-type '*) 'list)
        (t `(and list (satisfies ,(get-list-element-predicate element-type))))))

(deftype callable ()
  `(or function
       (and symbol (satisfies fboundp))))


;;;; binding macros

(export '(with-gensyms
          with-struct
          ))

(defmacro with-gensyms (names &body body)
  `(let (,@(loop for name in names
             collect `(,name (gensym ,(format nil "~A-" name)))))
     ,@body))
(set-indent 'with-gensyms 1)

(defmacro with-struct ((struct &rest slots) object &body body)
  (let ((package (symbol-package struct)))
    (labels ((acc (slot)
               (let ((name (format nil "~A-~A" struct slot)))
                 (or (find-symbol name package)
                     (error "with-struct - accessor `~A' not found in package ~S"
                            name package)))))
      `(let ((,struct ,object))
         (symbol-macrolet
             (,@(loop for slot in slots
                  collect (etypecase slot
                            (symbol `(,slot (,(acc slot) ,struct)))
                            (cons   `(,(car slot) (,(acc (cdr slot)) ,struct))))))
           ,@body)))))

(set-indent 'with-struct 2)


;;;; macro helper

(export '(operator-form-p
          find-key-option collect-key-options
          parse-body keyargs+body-bind
          ))

(defun operator-form-p (form operator)
  (and (consp form) (eql (car form) operator)))

(defun find-key-option (options key &optional (default nil default-supplied-p))
  "Find option in OPTIONS correspond to KEY, like `getf`.
Difference from `getf` is that if DEFAULT is not present in OPTIONS or
specified as argument, signal an error."
  (let ((found (getf options key '#1=#:not-found)))
    (cond ((not (eql found '#1#)) found)
          (default-supplied-p     default)
          (t  (error "Keyword option `~S' not specified.")))))

(defun collect-key-options (options key)
  "Collect all options correspond to KEY from OPTIONS."
  (let ((all nil))
    (loop for (key% value) on options by #'cddr
      when (eql key% key)
      collect value)))

(defun mappend (fn list &rest more-lists)
  (apply #'append (apply #'mapcar fn list more-lists)))

(defun parse-body (body &rest things)
  "Split THINGS at beginning of BODY and return them as multiple values.
What can be placed at beginning is specified by following keywords. Any
other forms are considered beginning of actual body forms.

  :doc      documentation string
      only one documentation string is acceptable. If literal string is
      present for second time, it is considered beginning of actual body.
  :decl     declarations
  :intr     `interactive` expression
      any list whose car is symbol `ed:interactive`. only one interactive
      expression as doc-string.
  :keyargs  keyword arguments
      keyword arguments before actual body. In other words, other elements
      (doc, decl, intr) must precede to keyargs.
      if keyword is present but corresponding value form is not, error of
      type `invalid-keyword-list` is signalled.

First returned value is actual body forms, and other things follows in the
order they specified as arguments."
  #+elf/safety (check-type things (list-of (member :doc :decl :intr :keyargs)))
  (let ((doc-allowed-p  (member :doc  things))
        (decl-allowed-p (member :decl things))
        (intr-allowed-p (member :intr things))
        (keys-allowed-p (member :keyargs things))
        ;; plist like (:doc nil :decl nil)
        (results  (mappend (lambda (x) (list x nil)) things)))
    (labels ((parse-keyargs (forms)
               (when (and forms keys-allowed-p)
                 (while (keywordp (first forms))
                   (unless (cdr forms) (error 'invalid-keyword-list :datum body))
                   (destructuring-bind (key value &rest rest) forms
                     (push key   (getf results :keyargs))
                     (push value (getf results :keyargs))
                     (setf forms rest))))
               forms)
             (finish (forms)
               (setf (getf results :keyargs) (nreverse (getf results :keyargs))
                     (getf results :decl) (nreverse (getf results :decl)))
               (values-list (cons forms
                                  (loop for x in things
                                    collect (getf results x)))))
             (form-p (x car-name)
               (and (consp x) (symbolp (car x)) (string= (car x) car-name))))
      (do ((forms body (cdr forms)))
          ((null forms) (finish nil))
        (let ((x (car forms)))
          (cond ((and doc-allowed-p (stringp x) (cdr forms))
                 (setf (getf results :doc) x   doc-allowed-p nil))
                ((and intr-allowed-p (form-p x "interactive"))
                 (setf (getf results :intr) x  intr-allowed-p nil))
                ((and decl-allowed-p (form-p x "declare"))
                 (push x (getf results :decl)))
                (t
                 (return (finish (parse-keyargs forms))))))))))

(defmacro keyargs+body-bind (lambda-list forms &body body)
  "like `destructuring-bind` but for (&key KEY-PARAMS... &body BODY-VAR)."
  (labels ((parse-lambda-list (lambda-list)
             "Return keyword-part and body-var out of LAMBDA-LIST."
             #+elf/safety (assert (and (listp lambda-list) (eq (car lambda-list) '&key)))
             (do ((rest (cdr lambda-list) (cdr rest))
                  (key-params nil (unless (eql (car rest) '&allow-other-keys)
                                    (cons (car rest) key-params))))
                 ((null rest) (values (nreverse key-params) nil))
               (case (car rest)
                 (&body
                  (assert (and (symbolp (second rest)) (null (cddr rest))) ()
                          'invalid-lambda-list :datum lambda-list)
                  (return (values (nreverse key-params) (second rest))))
                 (&allow-other-keys
                  (assert (and (eq (second rest) &body) (symbolp (third rest)) (null (cdddr rest))) ()
                          'invalid-lambda-list :datum lambda-list)
                  (return (values (nreverse key-params) (third rest))))))))))
(set-indent 'keyargs+body-bind 2)


(provide "elf/base/util")
;;; elf/base/util.l ends here.
