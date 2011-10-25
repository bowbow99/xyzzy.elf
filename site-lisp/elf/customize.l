;;; -*- mode: lisp; package: elf/customize -*-
;;;
;;; elf/customize.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "elf".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "elf/base/setup"))

#+elf/reset
(progn
  (when (find-package :elf/customize)
    (delete-package :elf/customize)))

(elf.util:namespace :elf/customize
  (:use :lisp :editor :elf.util)
  (:shadowing-import-from :ansi-loop
    #:loop #:loop-finish)
  (:shadowing-import-from :ansify
    #:assert #:check-type
    #:etypecase))

(elf-export '(defgroup
              defcustom
              defcustom-local
              ))


;;;; customize groups
;;
;; ref: http://www.gnu.org/software/emacs/elisp/html_node/Group-Definitions.html

(defstruct (customize-group
            )
  (name nil
    :type string-designator)
  (description nil
    :type string)
  (members nil
    :type (list-of symbol))
  (prefixes nil
    :type (list-of string))
  (package nil
    :type (or null package)))

(defun proper-customize-group-p (group)
  (and (customize-group-p group)
       (with-struct (customize-group name description members prefixes package) group
         (and (stringp name)
              (stringp description)
              (listp members)
              (every #'symbolp members)
              (listp prefixes)
              (every #'stringp prefixes)
              (or (null package)
                  (packagep package))))))

(deftype proper-customize-group ()
  `(and customize-group (satisfies proper-customize-group-p)))

(defun customize-group-add-member (group new-member)
  (with-struct (customize-group members) (find-customize-group group)
    (unless (member new-member members)
      (push new-member members))
    members))

(defvar *customize-group-list* nil)

(defun find-customize-group (name)
  (etypecase name
    (group
     (when (member name *customize-group-list*)
       name))
    (string-designator
     (find (string name) *customize-group-list*
           :key #'customize-group-name :test #'string=))))

(defun register-customize-group (group)
  #+elf/safety (check-type group proper-customize-group)
  (setf *customize-group-list*
        (cons group
              (remove (customize-group-name group) *customize-group-list*
                      :test 'string=)))
  group)

(defun expand-defgroup (name members description options)
  (let ((name (string name)))
    (with-gensyms ($group $x)
      `(let ((,$group (make-customize-group :name ,name
                                            :description ,description)))
         (register-customize-group ,$group)
         ,(when members
            `(dolist (,$x ',members)
               (customize-group-add-member ,$group ,$x)))
         ,@(loop for (key form) on options by #'cddr
             collect (case key
                       (:group `(customize-group-add-member ,form ,$group))
                       (:prefix `(let ((,$x ,form))
                                   #+elf/safety (check-type ,$x string)
                                   (pushnew ,$x (customize-group-prefixes ,$group))))
                       (t (warn "defgroup - unknown option ~S ~S" key form))))
         ,name))))

(defmacro defgroup (name (&rest members) description &rest options)
  "NAME という名前のカスタマイズグループを定義します。

引数:

  NAME - シンボルか文字列（評価されません）
      新たに定義するグループの名前を指定します。

  MEMBERS - シンボルのリスト（評価されません）
      最初からグループのメンバとするシンボルがある場合、それらを指定して
      しておきます。
      通常は defcustom などで後から追加するので defgroup では空にしておい
      てだいじょぶです

  DESCRIPTION - 文字列（評価されません）
      グループの説明を書いておきます。

  :group GROUP - グループ名（評価されます）
      新たに定義するグループが所属する親グループを指定します。
      親グループは先に定義されている必要があります。
      :group を複数指定することで複数の親グループに属するグループを作る
      事もできますが、あまりやりすぎると混乱するかも。

  :prefix PREFIX - 文字列（評価されます）
      このグループのメンバが使うプレフィックス文字列を指定します。
      Emacs ではカスタマイズ画面でプレフィックスを省略して表示するという
      事をするつもりだった（けどしてない）ようですが、xyzzy/elf では今の
      ところ使用していません。
"
  #+elf/safety (check-type name symbol)
  #+elf/safety (check-type members (list-of symbol))
  #+elf/safety (check-type description string)
  #+elf/safety (check-type options (plist keyword))
  (expand-defgroup name members description options))



(provide "elf/customize")
;;; elf/customize.l ends here.
