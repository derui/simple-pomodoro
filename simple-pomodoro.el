;;; simple-pomodoro.el --- A simple pomodoro timer for Emacs -*- lexical-binding: t; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: { KEYWORDS }

;;; Commentary:
;;
;; simple-pomodoro.el provides functionalities to count pomodoro and manage timer for it.
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'notification)

(defgroup simple-pomodoro
  nil
  "simple pomodoro timer and sets"
  :prefix "simple-pomodoro-"
  :group 'timer)

(defcustom simple-pomodoro-task-time 25
  "Minutes of task"
  :type '(integer)
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-short-break-time 5
  "Minutes of short break"
  :type '(integer)
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-long-break-time 15
  "Minutes of long break"
  :type '(integer)
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-cycle-task-count 4
  "Count of a cycle"
  :type '(integer)
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-tick-function nil
  "
Function to call when tick. Passed function must have three arguments, first is seconds of current task, second is seconds of reminder.
3rd argument is symbol what is type of timer, such as `task' `short-break' `long-break'
"
  :type '(function)
  :group 'simple-pomodoro)

;; global variables

(defvar simple-pomodoro--task-count 0
  "Current count of task. This value reset when call `simple-pomodoro-reset'.")

(defvar simple-pomodoro--timer nil
  "Timer for pomodoro.")

(defvar simple-pomodoro--tick-timer nil
  "Timer for pomodoro. This timer is used for tick.")

(defvar simple-pomodoro--state 'stopped
  "State of pomodoro. This value is one of `stopped' `task' `short-break' `long-break'.")

(defvar simple-pomodoro--start-time 0
  "Start time of current `simple-pomodoro--timer'.")

(defvar simple-pomodoro--end-time 0
  "Start time of current `simple-pomodoro--timer'.")

(defun simple-pomodoro--next-state (state)
  "Change state of pomodoro to next state from `STATE'."
  (simple-pomodoro--stop-timer)
  
  (cond
   ((eq state 'stopped)
    (setq simple-pomodoro--state 'task
          simple-pomodoro--task-count (1+ simple-pomodoro--task-count))
    (simple-pomodoro--start-timer simple-pomodoro-task-time))
   ((eq state 'task)
    (if (<= simple-pomodoro-cycle-task-count simple-pomodoro--task-count )
        (progn
          (setq simple-pomodoro--state 'long-break)
          (simple-pomodoro--start-timer simple-pomodoro-long-break-time))
      (progn
        (setq simple-pomodoro--state 'short-break)
        (simple-pomodoro--start-timer simple-pomodoro-short-break-time))))
   ((eq state 'short-break)
    (setq simple-pomodoro--state 'stopped))
   ((eq state 'long-break)
    (setq simple-pomodoro--state 'stopped))
   (t
    (error "Invalid state: %s" state))
   )
  )

(defun simple-pomodoro--tick ()
  "Tick function for pomodoro. This function calls per second."

  (let* ((elapsed-time (time-since simple-pomodoro--start-time))
         (duration-time (time-subtract simple-pomodoro--end-time simple-pomodoro--start-time)))
    (when (and simple-pomodoro-tick-function
               (functionp simple-pomodoro-tick-function))
      (funcall simple-pomodoro-tick-function
               (time-to-seconds elapsed-time)
               (time-to-seconds duration-time)
               simple-pomodoro--state))
    )
  )

(defun simple-pomodoro--finish ()
  "Finish function for pomodoro. This function calls when timer is finished."
  (simple-pomodoro--next-state simple-pomodoro--state))

(defun simple-pomodoro--start-timer (minutes)
  "Start timer for pomodoro with `MINUTES'."
  (let* ((start-time (time-to-seconds))
         (end-time (time-add start-time (format "%d min" minutes))))
    (setq simple-pomodoro--start-time start-time
          simple-pomodoro--end-time end-time
          simple-pomodoro--timer (run-at-time (format "%d min" minutes) nil #'simple-pomodoro--finish)
          simple-pomodoro--tick-timer (run-at-time t 1 #'simple-pomodoro--tick))
    )
  )

(defun simple-pomodoro--stop-timer ()
  "Stop timer for pomodoro."
  (when (and simple-pomodoro--timer
             (timerp simple-pomodoro--timer))
    (cancel-timer simple-pomodoro--timer))

  (when (and simple-pomodoro--tick-timer
             (timerp simple-pomodoro--tick-timer))
    (cancel-timer simple-pomodoro--tick-timer))
  (setq simple-pomodoro--timer nil
        simple-pomodoro--tick-timer nil
        simple-pomodoro--start-time 0
        simple-pomodoro--end-time 0
        ))

(defun simple-pomodoro-reset ()
  "Reset cycle and stop timer."
  (interactive)
  (simple-pomodoro--stop-timer)
  
  (setq simple-pomodoro--task-count 0
        simple-pomodoro--state 'stopped
        ))

(defun simple-pomodoro-start ()
  "Start pomodoro."
  (interactive)
  (simple-pomodoro--next-state simple-pomodoro--state))

(provide 'simple-pomodoro)
