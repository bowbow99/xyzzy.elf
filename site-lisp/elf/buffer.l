;;; -*- mode: lisp; package: elf/buffer -*-
;;;
;;; elf/buffer.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "elf".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "elf/base/setup"))

(elf.util:namespace :elf/buffer
  (:use :lisp :editor :elf.util)
  (:shadowing-import-from :ansify
    #:check-type))

(elf-export '(with-current-buffer
              with-temp-buffer
              ))

(defmacro with-current-buffer (buffer-or-name &body body)
  "�ꎞ�I�Ƀo�b�t�@ BUFFER-OR-NAME �ֈړ����� BODY �����s���܂��B"
  `(with-set-buffer
     (let ((#1=#:obuf (selected-buffer))
           (#2=#:nbuf ,buffer-or-name))
       #+elf/safety (check-type #2# (or string buffer))
       (when (stringp #2#)
         (setf #2# (or (find-buffer #2#)
                       (error "�o�b�t�@��������܂���: ~A" #2#))))
       (cond ((eql #2# #1#) ,@body)
             (t (unwind-protect
                    (progn (set-buffer #2#) ,@body)
                  (ignore-errors
                   (set-buffer #1#))))))))
(set-indent 'with-current-buffer 1)

(defmacro with-temp-buffer (&body body)
  "�ꎞ�o�b�t�@�ֈړ����� BODY ��]�����܂��B"
  `(with-set-buffer
     (let ((#1=#:obuf (selected-buffer))
           (#2=#:tbuf (create-new-buffer " TEMP")))
       (setup-temp-buffer #2#)
       (unwind-protect
           (progn (set-buffer #2#) ,@body)
         (ignore-errors
          (set-buffer #1#)
          (delete-buffer #2#))))))
(set-indent 'with-temp-buffer 0)


(provide "elf/buffer")
;;; elf/buffer.l ends here.