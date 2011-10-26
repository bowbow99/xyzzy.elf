;;; -*- mode: lisp; package: elf/customization -*-
;;;
;;; elf/customization.l
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
  (when (find-package :elf/customization)
    (delete-package :elf/customization)))

(elf.util:namespace :elf/customization
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
  (with-struct (customize-group members)
      (find-customize-group-or-error group)
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

(defun find-customize-group-or-error (name)
  (or (find-customize-group name)
      (error "�J�X�^�}�C�Y�O���[�v��������܂���: ~S" name)))

(defun register-customize-group (group)
  #+elf/safety (check-type group proper-customize-group)
  (setf *customize-group-list*
        (cons group
              (remove (customize-group-name group) *customize-group-list*
                      :test 'string=)))
  group)

(defun expand-defgroup (name members description options)
  #+elf/safety (check-type name symbol)
  #+elf/safety (check-type members (list-of symbol))
  #+elf/safety (check-type description string)
  #+elf/safety (check-type options (plist keyword))
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
  "NAME �Ƃ������O�̃J�X�^�}�C�Y�O���[�v���`���܂��B

����:

  NAME - �V���{����������i�]������܂���j
      �V���ɒ�`����O���[�v�̖��O���w�肵�܂��B

  MEMBERS - �V���{���̃��X�g�i�]������܂���j
      �ŏ�����O���[�v�̃����o�Ƃ���V���{��������ꍇ�A�������w�肵��
      ���Ă����܂��B
      �ʏ�� defcustom �ȂǂŌォ��ǉ�����̂� defgroup �ł͋�ɂ��Ă���
      �Ă�������Ԃł�

  DESCRIPTION - ������i�]������܂���j
      �O���[�v�̐����������Ă����܂��B

  :group GROUP - �V���{����������i�]������܂��j
      �V���ɒ�`����O���[�v����������e�O���[�v���w�肵�܂��B
      �e�O���[�v�͐�ɒ�`����Ă���K�v������܂��B
      :group �𕡐��w�肷�邱�Ƃŕ����̐e�O���[�v�ɑ�����O���[�v�����
      �����ł��܂����A���܂��肷����ƍ������邩���B

  :prefix PREFIX - ������i�]������܂��j
      ���̃O���[�v�̃����o���g���v���t�B�b�N�X��������w�肵�܂��B
      Emacs �ł̓J�X�^�}�C�Y��ʂŃv���t�B�b�N�X���ȗ����ĕ\������Ƃ���
      ����������肾�����i���ǂ��ĂȂ��j�悤�ł����Axyzzy/elf �ł͍���
      �Ƃ���g�p���Ă��܂���B
"
  (expand-defgroup name members description options))
(set-indent 'defgroup 2)


;;;; customize option types
;;
;; Customize option type describes a) type of value acceptable for the
;; option, and b) UI (widget) for modifying the value of the option on
;; customization buffer.
;;


;;;; customize variable (option)

(defstruct (customize-variable)
  symbol
  description
  default-form
  type
  options
  buffer-local-p)

(defun expand-defcustom (name default description options local-p)
  #+elf/safety (check-type name symbol)
  #+elf/safety (check-type description string)
  #+elf/safety (check-type options (plist keyword))
  (unless (find-key-option options :group)
    (warn "�O���[�v���w�肳��Ă��܂���: (~S ~S...)"
          (if local-p 'defcustom-local 'defcustom) name))
  (with-gensyms ($def $x)
    `(let ((,$def (make-customize-group
                   :symbol ',name :default-form ',default
                   :description ',description :buffer-local-p ',(not (not local-p)))))
       ,@(loop for (key form) on options by #'cddr
           collect (case key
                     (:group `(customize-group-add-member ,form ',name))
                     (:type `(setf (customize-variable-type ,$def) ,form))
                     (:options `(dolist (,$x ,form)
                                  (pushnew ,$x (customize-variable-options ,$def))))))
       (setf (get ',name 'ed::customize-variable-definition) ,$def)
       (,(if local-p 'defvar-local 'defvar) ,name ,default ,description))))


(defmacro defcustom (name default description &rest options)
  "�J�X�^�}�C�Y�ϐ����`���܂��B

�iEmacs �ł� M-x customize �ŋN������J�X�^�}�C�Y�o�b�t�@�� GUI ���ۂ�
�ݒ�ł����肵�܂����A���̂Ƃ��낻��Ȃ��̖����̂ł��܂��t�� defvar ��
�����ł��B�j

����:

  NAME - �V���{���i�]������܂���j
      �J�X�^�}�C�Y�ϐ��̃V���{�����w�肵�܂��B

  DEFAULT - �C�ӂ̎��i�K�v�ɉ����ĕ]������܂��j
      �J�X�^�}�C�Y�ϐ��̏����l��Ԃ������w�肵�܂��B

  DESCRIPTION - ������i�]������܂���j
      �J�X�^�}�C�Y�ϐ��̐����������Ă����܂��B

  :group GROUP - �V���{����������i�]������܂��j
      �J�X�^�}�C�Y�ϐ���������O���[�v���w�肵�܂��B

  :type CUSTOMIZE-TYPE - �H�H�H�i�]������܂��j
      ���̕ϐ��ɐݒ�ł���l�̃^�C�v���w�肵�܂��B
      �������Ń^�C�v�ƌĂ�ł���̂̓J�X�^�}�C�Y�ϐ��̒l��p�̃^�C�v��
      typep �ȂǂŎw�肷�� lisp �̌^�w��q�Ƃ͕ʕ��ł��B
"
  (expand-defcustom name default description options nil))
(set-indent 'defcustom 2)

(defmacro defcustom-local (name default description &rest options)
  "�o�b�t�@�E���[�J���ȃJ�X�^�}�C�Y�ϐ����`���܂��B
�ڂ����� defcustom �Q�ƁB"
  (expand-defcustom name default description options t))
(set-indent 'defcustom-local 2)


(provide "elf/customization")
;;; elf/customization.l ends here.