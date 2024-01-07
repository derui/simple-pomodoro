;;; simple-pomodoro.el --- A simple pomodoro timer for Emacs -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: timer

;;; Commentary:
;;
;; simple-pomodoro.el provides functionalities to count pomodoro and manage timer for it.
;;
;; Start pomodoro timer
;;   M-x simple-pomodoro-start
;;
;; Reset pomodoro timer and count
;;   M-x simple-pomodoro-reset

;;: Customization:
;; You can customize some variables. Use customize-group or setq.
;;   M-x customize-group simple-pomodoro

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (defmacro sps--get (slot)
    "internal macro to get value of global state. This macro is used only byte compile."
    `(cl-struct-slot-value 'simple-pomodoro--internal-state ,slot simple-pomodoro--state))

  (defmacro sps--set (slot value)
    "internal macro to set value for global state. This macro is used only byte compile."
    `(setf (cl-struct-slot-value 'simple-pomodoro--internal-state ,slot simple-pomodoro--state) ,value)))

(defgroup simple-pomodoro
  nil
  "simple pomodoro timer and sets"
  :prefix "simple-pomodoro-"
  :group 'timer)

(defcustom simple-pomodoro-task-time 25
  "Minutes of task"
  :type 'integer
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-short-break-time 5
  "Minutes of short break"
  :type 'integer
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-long-break-time 15
  "Minutes of long break"
  :type 'integer
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-cycle-task-count 4
  "Count of a cycle. Default is 4 that means 4 task and short break."
  :type 'integer
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-tick-function nil
  "
Function to call when tick. Passed function must have three arguments,
 first is seconds of current task, second is seconds of reminder.
3rd argument is symbol what is type of timer, such as `task' `short-break'
 `long-break'.

Do not call function if state is `stopped'.
"
  :type 'function
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-notification-function nil
  "
Function to call when state changed. Passed function must have one argument,
 first is symbol what is type of timer, such as `task' `short-break'
 `long-break', `stopped'.
"
  :type 'function
  :group 'simple-pomodoro)

;; global variables

(cl-defstruct (simple-pomodoro--internal-state (:constructor simple-pomodoro--internal-state-create))
  "State of pomodoro."
  (kind 'stopped)
  (task-count 0)
  (cycle-count 0)
  (timer nil)
  (tick-timer nil)
  (time-keeper nil))

(defvar simple-pomodoro--state (simple-pomodoro--internal-state-create)
  "Global states of simple-pomodoro")

;; functions

(defun simple-pomodoro--notify (kind)
  "Notify `KIND' to user."
  (when (functionp simple-pomodoro-notification-function)
    (funcall simple-pomodoro-notification-function kind)))

(defun simple-pomodoro--update-task-count (kind)
  "Update some state of pomodoro from `KIND'."
  (sps--set 'kind kind)
  (cl-case kind
    (task
     (sps--set 'task-count (1+ (sps--get 'task-count))))
    (long-break
     (sps--set 'cycle-count (1+ (sps--get 'cycle-count)))
     (sps--set 'task-count 0))
    (t nil)))

(defun simple-pomodoro--next-state (kind)
  "Return state of pomodoro to next state from `KIND'."
  (cl-case kind
    (stopped 'task)
    (task
     (if (<= simple-pomodoro-cycle-task-count (sps--get 'task-count))
         'long-break
       'short-break))
    (short-break 'stopped)
    (long-break 'stopped)
    (t
     (error "Invalid state: %s" kind))))

(defun simple-pomodoro--tick ()
  "Tick function for pomodoro. This function calls per second."
  (unless (eq (simple-pomodoro-current-state) 'stopped)
    (let* ((measured (simple-pomodoro-measuring-time))
           (elapsed (car measured))
           (duration (cdr measured)))
      (when (and simple-pomodoro-tick-function
                 (functionp simple-pomodoro-tick-function))
        (funcall simple-pomodoro-tick-function
                 elapsed
                 duration
                 simple-pomodoro--state))
      (sps--set 'time-keeper (cons (1+ elapsed) (cdr (sps--get 'time-keeper)))))))

(defun simple-pomodoro--finish ()
  "Finish function for pomodoro. This function calls when timer is finished."
  (simple-pomodoro--stop-timer)
  
  (let ((next-state (simple-pomodoro--next-state (simple-pomodoro-current-state))))

    (simple-pomodoro--update-task-count next-state)
    (simple-pomodoro--notify next-state)

    (cl-case next-state
      (short-break (simple-pomodoro--start-timer simple-pomodoro-short-break-time))
      (long-break (simple-pomodoro--start-timer simple-pomodoro-long-break-time))
      (t nil))))

(defun simple-pomodoro--minutes-of-kind (kind)
  "Return minutes of `KIND'."
  (cl-case kind
    (task simple-pomodoro-task-time)
    (short-break simple-pomodoro-short-break-time)
    (long-break simple-pomodoro-long-break-time)
    (t 0)))

(defun simple-pomodoro--start-timer (kind)
  "Start timer for pomodoro with `KIND'."

  (let* ((minutes (simple-pomodoro--minutes-of-kind kind))
         (seconds (* minutes 60)))
    (sps--set 'time-keeper (cons 0 seconds))
    (sps--set 'timer (run-at-time (format "%d sec" seconds) nil #'simple-pomodoro--finish))
    (sps--set 'tick-timer (run-at-time t 1 #'simple-pomodoro--tick))))

(defun simple-pomodoro--stop-timer ()
  "Stop timer for pomodoro."
  (dolist (slot '(timer tick-timer))
    (and (timerp (sps--get slot))
         (cancel-timer (sps--get slot))
         (sps--set slot nil))))

(defun simple-pomodoro-reset ()
  "Stop timer and reset all state."
  (interactive)
  (simple-pomodoro--stop-timer)

  (setq simple-pomodoro--state (simple-pomodoro--internal-state-create)))

(defun simple-pomodoro--timer-running-p ()
  "Return `t' if timer is running."
  (or (timerp (sps--get 'timer))
      (timerp (sps--get 'tick-timer))))

(defun simple-pomodoro-start ()
  "Start pomodoro."
  (interactive)
  (if (simple-pomodoro--timer-running-p)
      (progn
        (message "Pomodoro is already running")
        (cl-return))
    (let ((next-state (simple-pomodoro--next-state (sps--get 'kind))))
      (simple-pomodoro--update-task-count next-state)
      (simple-pomodoro--start-timer (cl-case next-state
                                      (task simple-pomodoro-task-time)
                                      (short-break simple-pomodoro-short-break-time)
                                      (long-break simple-pomodoro-long-break-time)
                                      (t (error "Invalid state: %s" next-state)))))))

(defun simple-pomodoro-measuring-time ()
  "Return current time of pomodoro if counted.
Time is cons, car is elapsed seconds from start, cdr is duration seconds of
current timer.
`simple-pomodoro-state' is returned `STOPPED', this function returnes nil.
"
  (cl-case (simple-pomodoro-current-state)
    (stopped nil)
    (t (let* ((elapsed-time (car (sps--get 'time-keeper)))
              (total-duration-seconds (- (cdr (sps--get 'time-keeper)) elapsed-time)))
         (cons elapsed-time total-duration-seconds)))))

(defun simple-pomodoro-current-state ()
  "Return current state of pomodoro."
  (sps--get 'kind))

(eval-when-compile
  ;; remove internal macro.
  (fmakunbound 'sps--get)
  (fmakunbound 'sps--set))

(provide 'simple-pomodoro)

