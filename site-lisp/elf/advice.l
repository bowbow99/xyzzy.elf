;;; -*- mode: lisp; package: elf/advice -*-
;;;
;;; elf/advice.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "elf".

;;; References:
;; [Advising Functions - GNU Emacs Lisp Reference Manual][1]
;;   [1]: http://www.gnu.org/software/emacs/elisp/html_node/Advising-Functions.html

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "elf/base/setup"))

#+elf/reset
(when (find-package :elf/advice) (delete-package :elf/advice))

(elf.util:namespace :elf/advice
  (:use :lisp :editor :elf.util)
  (:nicknames "advice" "ad")
  (:import-from :ansify
    #:destructuring-bind #:assert #:ecase #:etypecase
    #:&allow-other-keys)
  (:shadowing-import-from :ansify
    #:check-type)
  (:import-from :ansify.util
    #:awhen)
  (:shadowing-import-from :ansi-loop
    #:loop))

#+nil
(elf-export '(defadvice
              ))

(export '(defadvice
          activate
          deactivate
          ))


;;;; utilities

(defun insert-at (list item position)
  "Return a list which is LIST with ITEM at POSITION.
If POSITION is zero or negative, ITEM is added at beginning. If POSITION
is same or greater than length of LIST, ITEM is added at end.

CAUTION: This fnuction is destructive."
  #+elf/safety (check-type list list)
  #+elf/safety (check-type position integer)
  (cond ((null list) (list item))
        ((<= position 0) (cons item list))
        ((>= position (length list)) (append list (list item)))
        (t (let ((rest (nthcdr position list)))
             (setf (cdr rest) (cons (car rest) (cdr rest))
                   (car rest) item)
             list))))

(defconstant +undefined+ '#:undefined)

(deftype undefined () `(eql ,+undefined+))

(defun undefinedp (x) (eql x +undefined+))

(defun error-if-undefined (x &optional datum &rest args)
  (cond ((not (undefinedp x)) x)
        (datem (apply 'error datum args))
        (t (error "初期化されていません"))))

(defconstant +bare-documentation-function+ #'documentation)

(defun %%fdoc (function)
  (funcall +bare-documentation-function+ function 'function))


;;;; misc

(defparameter +advice-classes+ '(:before :around :after)
  "List of appricable advice class keywords.")

(deftype advice-class ()
  `(member ,@+advice-classes+))

(define-condition non-adviced-function (error)
  (name)
  (:report (lambda (x stream)
             (format stream "Function ~S is not adviced."
                   (non-adviced-function-name x)))))

(defparameter *arguments* nil)
(defparameter *return-values* nil)



;;;; piece of advice

;; (NAME DEFINITION . FLAGS..)

(defun make-advice (name definition &rest flags &key enabled arglist)
  (check-type name symbol)
  (check-type definition (or function list))
  (list* name definition flags))

(defun proper-advice-p (advice)
  (and (listp advice)
       (>= (length advice) 2)
       (destructuring-bind (name definition &key enabled arglist)
           advice
         (and (symbolp name)
              (or (functionp definition)
                  (listp definition))))))

(deftype proper-piece-of-advice ()
  `(and list (satisfies proper-advice-p)))

(defun advice-name (advice)
  #+elf/safety (check-type advice proper-piece-of-advice)
  (first advice))

(defun advice-definition (advice)
  #+elf/safety (check-type advice proper-piece-of-advice)
  (second advice))

(defun advice-enabled-p (advice)
  #+elf/safety (check-type advice proper-piece-of-advice)
  (getf (cddr advice) :enabled))

(defun expand-advice (advice)
  #+elf/safety (check-type advice proper-piece-of-advice)
  (destructuring-bind (name def &key arglist &allow-other-keys)
      advice
    (etypecase def
      (function
       (cond ((undefinedp arglist) `(funcall ,def))
             (t `(apply ,def *arguments*))))
      (list
       (cond ((undefinedp arglist) `(progn ,@def))
             (t `(destructuring-bind ,arglist ,@def)))))))


;;;; store advices on function

(defstruct fn-info
  original-definition
  original-documentation
  (advices (list :before nil :after nil :around nil))
  active-p)

(defun fn-info (function &optional required)
  #+elf/safety (check-type function symbol)
  (let ((info (get function 'function-info +undefined+)))
    (cond ((and (undefinedp info) required)
           (error 'non-adviced-function :name function))
          (t info))))

(defsetf fn-info (function) (info)
  `(setf (get ,function 'function-info) ,info))

(defun fn-orig-def (function)
  #+elf/safety (check-type function symbol)
  (fn-info-original-definition (fn-info function t)))

(defun fn-orig-doc (function)
  #+elf/safety (check-type function symbol)
  (fn-info-original-documentation (fn-info function t)))

(defun fn-advices (function &optional class)
  #+elf/safety (check-type function symbol)
  (let ((all (fn-info-advices (fn-info function t))))
    (cond (class (error-if-undefined
                  (getf all class +undefined+)
                  "advice slot for class ~S missing." class))
          (t all))))

(defun fn-advice-active-p (function)
  #+elf/safety (check-type function symbol)
  (fn-info-active-p (fn-info function t)))

(defun initialize-function-info (function)
  "Store original function information of FUNCTION."
  ;; NOTE: assuming FUNCTION isn't initialized yet.
  #+elf/safety (check-type function symbol)
  #+elf/safety (assert (undefinedp (fn-info function)))
  (setf (fn-info function)
        (make-fn-info
         :original-definition (cond ((fboundp function) (symbol-function function))
                                    (t +undefined+))
         :original-documentation (or (%%fdoc function)
                                     +undefined+)))
  function)

(defun add-advice (function advice class &optional (position -1))
  "Add piece of ADVICE to FUNCTION."
  #+elf/safety (progn
                 (check-type function symbol)
                 (check-type advice proper-piece-of-advice)
                 (check-type class  advice-class)
                 (check-type position (or integer (member :first :last))))
  (when (undefinedp (fn-info function))
    (initialize-function-info function))
  (let* ((all (fn-advices function))
         (advices (fn-advices function class))
         (pos (case position
                (:first -1)
                (:last (length advices))
                (t position))))
    (setf (getf all class) (insert-at advices advice pos))))


;;;; activation

(defparameter *inner-thunk* nil
  "Bound to a function contains original and inner around advices.")

(defun combine-around-advices (odef arounds)
  (let ((root (coerce `(lambda ()
                         (setf *return-values*
                               (multiple-value-list (apply ,odef *arguments*))))
                      'function)))
    `(funcall
      ,(reduce (lambda (outer-advice inner-thunk)
                 `(lambda ()
                    (let ((*inner-thunk* ,inner-thunk))
                      ,(expand-advice outer-advice))))
               arounds :from-end t :initial-value root))))

(defun make-combined-function (odef advices)
  (labels ((expand-advices (class)
             (loop for advice in (getf advices class)
               when (advice-enabled-p advice)
               collect (expand-advice advice))))
    (coerce `(lambda (&rest *arguments*)
               (let ((*return-values* nil))
                 ,@(expand-advices :before)
                 ,(combine-around-advices odef (getf advices :around))
                 ,@(expand-advices :after)
                 (values-list *return-values*)))
            'function)))

(defun activate (function)
  (with-struct (fn-info original-definition original-documentation advices active-p)
      (fn-info function t)
    (let ((ndef (make-combined-function original-definition advices)))
      (setf (symbol-function function) ndef
            active-p t))
    function))

(defun deactivate (function)
  (with-struct (fn-info original-definition active-p)
      (fn-info function t)
    (setf (symbol-function function) original-definition
          active-p nil)
    function))


;;;; Macro defadvice

(defun gen-make-advice-form (class name arglist body &key enabled)
  (let ((lambda `(lambda ,(cond ((undefinedp arglist) nil)
                                (t arglist))
                   ,@body))
        (fname  `(#:advice ,class ,name)))
    `(make-advice ',name (si:*set-function-name ,lambda ',fname)
                  :enabled ,enabled :arglist ',arglist)))

(defun check-defadvice-arguments (function class name arglist)
  (check-type function symbol)
  (check-type class advice-class)
  (check-type name symbol)
  (check-type arglist (or list undefined)))

(defun check-defadvice-options (position activate enabled)
  (check-type position (or integer (member :first :last))))

(defun expand-defadvice (function class name arglist options body)
  (check-defadvice-arguments function class name arglist)
  (destructuring-bind (&key (position :first) activate (enabled t))
      options
    (check-defadvice-options position activate enabled)
    `(progn
       (add-advice ',function
                   ,(gen-make-advice-form class name arglist body
                                          :enabled enabled)
                   ,class ,position)
       ',function)))

(defmacro defadvice (function (class name &optional arglist &rest options)
                     &body body)
  "関数 FUNCTION へのアドバイスを定義します。

アドバイスを使うと、関数が呼び出された時に関数の本来の処理の前後に処理を
追加することができます。

引数の説明:

  FUNCTION  シンボル
      アドバイスの対象である関数の名前を指定します。

  CLASS  [:before :around :after] のどれか
      アドバイスが実行されるタイミングを指定します。

  NAME  シンボル
      アドバイスの名前を指定します。関数 FUNCTION に既に CLASS, NAME 共に
      同じアドバイスがあった場合、アドバイスを再定義することになります。

  ARGLIST  引数リスト
      通常の関数と同様の引数リストを指定しておくと、アドバイスの BODY 内で
      関数 FUNCTION に与えられた引数を利用することができます。
      マクロに対するアドバイスの場合、defmacro と同様の形式で指定できます。

  OPTIONS  {キーワード 値}*
      キーワード引数の形で色々と指定できます。詳しくは下の方で。

  BODY  任意の式を好きなだけ
      アドバイスの本処理。

{{{TODO: ad:activate の説明}}}

{{{TODO: 使用例}}}


オプションの説明:

  :enable  BOOLEAN
      このアドバイスを有効とするかどうか。（デフォルトは有効。）

  :activate  BOOLEAN
      アドバイスを定義した際に（他のアドバイスも含めて）活性化させるか
      どうか。（デフォルトは活性化させない。）

  :position  [:first :last INTEGER]
      指定した CLASS に既にアドバイスがある場合、どこに追加するか。
        :first  - 先頭
        :last   - 最後
        INTEGER - 元 N 番目の前に挿入
      アドバイスが実行されるときは先頭側から順に実行されていきます。
      （デフォルトは :first）

  *** 以下未実装 ***

  :inline  BOOLEAN
      通常のアドバイスは関数として保存され、アドバイス実行時にはその関数
      が呼び出されますが、:inline を指定すると関数では

  :protect  BOOLEAN
      （Emacs では unwind-protect される、らしいんだけどイマイチどう動く
      のかイメージが掴めない）
"
  (expand-defadvice function class name (or arglist +undefined+) options body))
(set-indent 'defadvice 2)



(provide "elf/advise")
;;; elf/advise.l ends here.
