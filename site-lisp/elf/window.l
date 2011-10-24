;;; -*- mode: lisp; package: elf/window -*-
;;;
;;; elf/window.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "elf".

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "elf/base/setup"))

(elf.util:namespace :elf/window
  (:use :lisp :editor :elf.util))

(elf-export '(walk-windows
              window-list
              ))

(defun walk-windows (proc &optional minibuf all-frames)
  "関数 PROC を各ウィンドウを引数に呼び出します。
MINIBUF はミニバッファのウィンドウを含むかどうかを指定します。
  t        - 常に含む
  nil      - ミニバッファがアクティブであれば含む
  それ以外 - 常に含まない
ALL-FRAMES は未実装です。"
  (let* ((owin (selected-window))
         (minibuff (cond ((member minibuf '(t nil)) minibuf)
                         (t  (minibuffer-window-p (selected-window))))))
    (funcall proc owin)
    (do ((win (next-window owin minibuf)
              (next-window win minibuf)))
        ((eql win owin))
      (funcall proc win))))

(provide "elf/window")
;;; elf/window.l ends here.
