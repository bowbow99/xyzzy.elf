;;; -*- mode: lisp; package: elf/timer -*-
;;;
;;; elf/timer.l
;;;
;;; Author:    bowbow99  <bowbow99@gmail.com>
;;; License:   MIT (See COPYING.mit)
;;;
;;; This file is part of xyzzy extension "elf".

;;; Commentary
;;
;; ref: http://www.gnu.org/s/emacs/manual/html_node/elisp/Timers.html
;; ref: http://www.gnu.org/s/emacs/manual/html_node/elisp/Idle-Timers.html
;;

;;; TODO:
;;
;; - formatted string as DELAY
;; - *timer-list*
;; - cancel-timer
;; - *this-timer*
;; - store informations on timer object
;;   - `(timer-status TIMER)`
;;   - `(timer-delay  TIMER)`
;;   - `(timer-repeat TIMER)`
;; - run-with-idle-timer
;; - *idle-timer-list*
;; - *idle-timer-delay*

;;; Code:

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "elf/base/setup"))

(elf.util:namespace :elf/timer
  (:use :lisp :editor :elf.util)
  (:shadowing-import-from :ansify
   #:assert #:check-type #:etypecase))

(elf-export '(run-with-timer
              cancel-timer
              *active-timer-list*
              *pre-idle-hook*
              *pre-idle-hook-delay*
              ))


;;;; check whether `ed:start-timer` is patched or not

(when (boundp '+start-timer-patched-p+)
  (makunbound '+start-timer-patched-p+))

(let ((initial nil))

  (defun detect-timer-patch ()
    (cond ((null initial)
           (setf initial (get-internal-real-time)))
          (t
           (let ((elapsed (- (get-internal-real-time) initial)))
             (defparameter +start-timer-patched-p+ (> elapsed 30))
             (stop-timer 'detect-timer-patch)))))

  (unless (boundp '+start-timer-patched-p+)
    (start-timer 0.1 'detect-timer-patch)))

(defun patch-detection-finished-p ()
  (and (boundp '+start-timer-patched-p+)
       (member +start-timer-patched-p+ '(t nil))))


;;;; timer object

(defstruct (timer
            (:type vector) (:named t)
            (:copier nil) (:predicate nil))
  (delay nil
    :type (or integer float))
  (interval nil
    :type (or integer float null))
  (function nil
    :type callable)
  (arguments nil
    :type list)
  (symbol nil
    :type symbol)
  (thunk nil
    :type function))

(defun create-timer (delay interval fn args)
  #+elf/safety (check-type delay (or integer float))
  #+elf/safety (check-type interval (or null integer float))
  #+elf/safety (check-type fn callable)
  (make-timer :delay delay :interval interval
              :function fn :arguments args
              :symbol (gensym "timer-")
              :thunk (lambda () (apply fn args))))

(defun timer-p (x)
  (and (vectorp x)
       (eq (svref x 0) 'timer)))

(deftype timer ()
  `(and vector (satisfies timer-p)))

(defun timer-one-shot-p (timer)
  (null (timer-interval timer)))

(defun timer-repeat-p (timer)
  (not (null (timer-interval timer))))

(defvar *active-timer-list* nil)

(defun timer-activate-one-shot (timer)
  (with-struct (timer delay symbol thunk) timer
    (push timer *active-timer-list*)
    (setf (symbol-function symbol)
          (si:*set-function-name
           (lambda ()
             (funcall thunk)
             (setf *active-timer-list* (delete timer *active-timer-list*)))
           `(:one-shot ,symbol)))
    (start-timer delay symbol t)
    timer))

(defun timer-make-repeat-thunk (timer)
  (with-struct (timer symbol thunk) timer
    (cond (+start-timer-patched-p+ thunk)
          (t
           ;; NOTE: start-timer workaround
           ;; official version of xyzzy has bug in `start-timer` that
           ;; execute second execution of repeat with no interval.
           ;; as a workaround, simply don't execute the function (`thunk`)
           ;; at first execution.
           (let ((first-execution-p t))
             (lambda ()
               (cond (first-execution-p (setf first-execution-p nil))
                     (t (funcall thunk)))))))))

(defun timer-activate-repeat (timer)
  (with-struct (timer delay interval symbol thunk) timer
    (push timer *active-timer-list*)
    (setf (symbol-function symbol)
          (si:*set-function-name
           (lambda ()
             (funcall thunk)
             (setf (symbol-function symbol) (timer-make-repeat-thunk timer))
             (start-timer interval symbol))
           `(:repeat ,symbol)))
    (start-timer delay symbol t)
    timer))

(defun timer-activate (timer)
  (if (timer-one-shot-p timer)
    (timer-activate-one-shot timer)
    (timer-activate-repeat timer)))

(defun cancel-timer (timer)
  "タイマー TIMER を停止します。"
  (prog1
      (stop-timer (timer-symbol timer))
    (while (stop-timer (timer-symbol timer)))
    (setf *active-timer-list* (delete timer *active-timer-list*))))


;;;; Function run-with-timer

(defun run-with-timer (delay interval fn &rest args)
  "関数 FN に引数 ARGS を与えて、DELAY 秒後に実行します。
INTERVAL が integer か float であれば、最初の実行後 INTERVAL 秒ごとに繰り返し
実行します。
`run-with-timer` は timer object と呼ばれるものを返します。この timer object
は `cancel-timer` などで使う事ができます。"
  (check-type delay (or integer float))
  (check-type interval (or integer float null))
  (check-type fn callable)
  (let ((timer (create-timer delay interval fn args)))
    (timer-activate timer)
    timer))


;;;; *pre-idle-hook*

(defvar *pre-idle-hook* nil
  "一定時間操作しなかった時に実行されるフック")

(defvar *pre-idle-hook-delay* 1
  "最後に操作してから何秒後に *pre-idle-hook* を実行するか")

(defparameter *pre-idle-hook-timer* nil)

;; @post-command-hook
(defun run-idle-hooks ()
  (when *pre-idle-hook*
    (setf *pre-idle-hook-timer*
          (run-with-timer *pre-idle-hook-delay* nil
                          'run-hooks '*pre-idle-hook*))))

(add-hook '*post-command-hook* 'run-idle-hooks)

;; @pre-command-hook
(defun cancel-idle-hooks ()
  (when *pre-idle-hook-timer*
    (cancel-timer *pre-idle-hook-timer*)
    (setf *pre-idle-hook-timer* nil)))

(add-hook '*pre-command-hook* 'cancel-idle-hooks)


(provide "elf/timer")
;;; elf/timer.l ends here.
