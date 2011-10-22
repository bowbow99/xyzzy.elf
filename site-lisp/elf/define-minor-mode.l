;;; -*- mode: lisp; package: elf/define-minor-mode -*-
;;;
;;; elf/define-minor-mode.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "elf".

;;; Commentary
;;
;; ref: http://www.gnu.org/software/emacs/elisp/html_node/Defining-Minor-Modes.html
;;
;; Some hints to port elisp code: Emacs's `define-minor-mode` (can) take
;; positional arguments, INIT-VALUE, LIGHTER, KEYMAP in this order and I
;; often see codes using this form. Basically these can be keyword arguments
;; by just adding appropriate keyword before them.
;;
;;     ;; original elisp code
;;     (define-minor-mode some-minor-mode
;;       "documentation string..."
;;       nil
;;       "Something"
;;       '(([KEY] . command)..)
;;       ...)
;;
;;     ;; can be transformed into this
;;     (define-minor-mode some-minor-mode
;;       "documentation string..."
;;       :init-value nil
;;       :lighter "Something"
;;       :keymap '(([KEY] . command)..)
;;       ...)
;;

;;; TODO:
;; - :global
;; - hooks
;; - documentation
;; - :earmuff
;; - autoload'ing
;;   - you need to remove call for function defined in this file from
;;     expansion of `define-minor-mode`.

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "elf/base/setup"))

(elf.util:namespace :elf/define-minor-mode
  (:use :lisp :editor :elf.util)
  (:shadowing-import-from :ansify
   #:assert #:check-type #:etypecase #:destructuring-bind
   ))

(elf-export '(define-minor-mode
              ))


;;;; each option

;;;;; lighter

(defun default-lighter-string (name)
  (let ((name (if (string-match "\\(.+\\)-mode$" (string name))
                (match-string 1)
                (string name))))
    (format nil "~:(~A~)" name)))

(defun get-lighter (name options)
  (let ((lighter (getf options :lighter t)))
    (etypecase lighter
      ((eql t) (default-lighter-string name))
      ((eql nil) nil)
      ((or string symbol) lighter))))

;;;;; keymap

(defun make-keymap-variable (name options)
  ;; TODO: make-keymap-variable  - I might implement :earmuff option
  (intern (format nil "*~A-map*" name)))

(defun key-binding-list-p (x)
  (labels ((keyp (x)
             (or (characterp x)
                 (and (listp x) (every #'keyp x)))))
    (and (consp x)
         (destructuring-bind (key command) x
           (and (keyp key)
                (or (commandp command)
                    (keymapp command)))))))

(defun make-keymap-def-form (var key-bindings name)
  (with-gensyms ($bindings $keymap $key+cmd)
    `(progn
       (defvar ,var nil ,(format nil "マイナーモード ~A で使用するキーマップ" name))
       (unless ,var
         (let ((,$bindings ,key-bindings))
           (cond ((keymapp ,$bindings)
                  (setf ,var ,$bindings))
                 ((key-binding-list-p ,$bindings)
                  (let ((,$keymap (make-sparse-keymap)))
                    (dolist (,$key+cmd ,$bindings)
                      (define-key ,$keymap (car ,$key+cmd) (cdr ,$key+cmd)))
                    (setf ,var ,$keymap)))
                 (t
                  (error 'type-error :datum ,$bindings
                         :expected-type '(or keymap key-binding-alist)))
                 ))))))

(defun get-keymap-var/def-form (name options)
  (let ((keymap (getf options :keymap nil)))
    (etypecase keymap
      ((eql nil) (values nil nil))
      (symbol
       (when (constantp keymap)
         (warn "その定数ほんとにキーマップ？: ~S" keymap))
       (values keymap nil))
      (list
       (let* ((var  (make-keymap-variable name options))
              (form (make-keymap-def-form var keymap name)))
         (values var form))))))

;;;;; hook?


;;;; Macro define-minor-mode

(defmacro define-minor-mode (name doc &rest options-and-body)
  #+elf/safety (check-type name symbol)
  #+elf/safety (check-type doc  string)
  "マイナーモード MODE-NAME を定義します。"
  (expand-define-minor-mode name doc options-and-body))
(set-indent 'define-minor-mode 0)

(defun expand-define-minor-mode (name doc options-and-body)
  (multiple-value-bind (body options)
      (parse-body options-and-body :keyargs)
    (multiple-value-bind (init-form lighter keymap-var keymap-def-form)
        (multiple-value-call #'values
          (getf options :init-value nil)
          (get-lighter name options) ; lighter
          (get-keymap-var/def-form name options)) ; keymap-var keymap-def-form

      #+elf/safety (check-type lighter (or string symbol))
      `(progn
         ;; minor-mode variable
         (defvar-local ,name ,init-form
           ,(format nil "マイナーモード ~S の状態保持変数" name))
         (pushnew '(,name . ,lighter) *minor-mode-alist* :test #'equal)
         ;; minor-mode keymap
         ,keymap-def-form
         ;; minor-mode command
         (defun ,name (&optional (arg nil sv))
           ,@(when doc `(,doc))
           (interactive "p")
           (ed::toggle-mode ',name arg sv)
           ,(when keymap-var
              `(if ,name
                 (set-minor-mode-map ,keymap-var)
                 (unset-minor-mode-map ,keymap-var)))
           ,@body
           (update-mode-line t))
         ;; return
         ',name))))


#+elf/quicktest
(progn
  (define-minor-mode example-mode
    "blah blah blah"
    :lighter "just an example...")
  (assert (commandp 'example-mode))
  (assert (and (boundp 'example-mode)
               (null example-mode)))
  (assert (assoc 'example-mode *minor-mode-alist*))
  (assert (equal (cdr (assoc 'example-mode *minor-mode-alist*))
                 "just an example..."))
  )

(provide "elf/define-minor-mode")
;;; elf/define-minor-mode.l ends here.
