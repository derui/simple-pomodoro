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
  (require 'cl-lib))

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

(defvar simple-pomodoro--task-count 0
  "Current count of task. This value reset when call `simple-pomodoro-reset'.")

(defvar simple-pomodoro--cycle-count 0
  "Current cycle of pomodoro.
This value reset when call `simple-pomodoro-reset'.")

(defvar simple-pomodoro--timer nil
  "Timer for pomodoro.")

(defvar simple-pomodoro--tick-timer nil
  "Timer for pomodoro. This timer is used for tick.")

(defvar simple-pomodoro--state 'stopped
  "State of pomodoro.
This value is one of `stopped' `task' `short-break' `long-break'.")

(defvar simple-pomodoro--start-time 0
  "Start time of current `simple-pomodoro--timer'.")

(defvar simple-pomodoro--end-time 0
  "Start time of current `simple-pomodoro--timer'.")

;; functions

(defun simple-pomodoro--notify (state)
  "Notify `STATE' to user."
  (when (and simple-pomodoro-notification-function
             (functionp simple-pomodoro-notification-function))
    (funcall simple-pomodoro-notification-function state)))

(defun simple-pomodoro--update-counts (state)
  "Update task count from `STATE'."
  (pcase state
    ('task
     (setq simple-pomodoro--task-count (1+ simple-pomodoro--task-count)))
    ('long-break
     (setq simple-pomodoro--task-count 0
           simple-pomodoro--cycle-count (1+ simple-pomodoro--cycle-count)))
    (_ nil)))

(defun simple-pomodoro--next-state (state)
  "Change state of pomodoro to next state from `STATE'."
  (simple-pomodoro--stop-timer)
  
  (pcase state
    ('stopped
     (setq simple-pomodoro--state 'task)
     (simple-pomodoro--start-timer simple-pomodoro-task-time))
    ('task
     (if (<= simple-pomodoro-cycle-task-count simple-pomodoro--task-count )
         (progn
           (setq simple-pomodoro--state 'long-break)
           (simple-pomodoro--start-timer simple-pomodoro-long-break-time))
       (progn
         (setq simple-pomodoro--state 'short-break)
         (simple-pomodoro--start-timer simple-pomodoro-short-break-time))))
    ('short-break
     (setq simple-pomodoro--state 'stopped))
    ('long-break
     (setq simple-pomodoro--state 'stopped))
    (_
     (error "Invalid state: %s" state)))

  (let ((state (simple-pomodoro-state)))
    (simple-pomodoro--update-task-count state)
    (simple-pomodoro--notify state)))

(defun simple-pomodoro--tick ()
  "Tick function for pomodoro. This function calls per second."

  (let* ((measured (simple-pomodoro-measuring-time))
         (elapsed (car measured))
         (duration (cdr measured)))
    (when (and simple-pomodoro-tick-function
               (functionp simple-pomodoro-tick-function))
      (funcall simple-pomodoro-tick-function
               elapsed
               duration
               simple-pomodoro--state))))

(defun simple-pomodoro--finish ()
  "Finish function for pomodoro. This function calls when timer is finished."
  (simple-pomodoro--next-state simple-pomodoro--state))

(defun simple-pomodoro--start-timer (minutes)
  "Start timer for pomodoro with `MINUTES'."

  ;; stop timer first.
  (simple-pomodoro--stop-timer)
  
  (let* ((start-time (time-convert nil 'integer))
         (end-time (time-add start-time (* 60 minutes))))
    (setq simple-pomodoro--start-time start-time
          simple-pomodoro--end-time end-time
          simple-pomodoro--timer (run-at-time (format "%d min" minutes) nil #'simple-pomodoro--finish)
          simple-pomodoro--tick-timer (run-at-time t 1 #'simple-pomodoro--tick))))

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
        simple-pomodoro--end-time 0))

(defun simple-pomodoro-reset ()
  "Reset cycle and stop timer."
  (interactive)
  (simple-pomodoro--stop-timer)

  (setq simple-pomodoro--task-count 0
        simple-pomodoro--cycle-count 0
        simple-pomodoro--state 'stopped))

(defun simple-pomodoro-start ()
  "Start pomodoro."
  (interactive)
  (simple-pomodoro--next-state simple-pomodoro--state))

(defun simple-pomodoro-measuring-time ()
  "Return current time of pomodoro if counted.
Time is cons, car is elapsed seconds from start, cdr is duration seconds of
current timer.
`simple-pomodoro-state' is returned `STOPPED', this function returnes nil.
"
  (pcase (simple-pomodoro-state)
    ('stopped nil)
    (_ (let* ((elapsed-time (time-since simple-pomodoro--start-time))
              (total-duration-seconds (time-subtract simple-pomodoro--end-time simple-pomodoro--start-time))
              (duration-time (time-subtract total-duration-seconds elapsed-time)))
         (cons (time-to-seconds elapsed-time)
               (time-to-seconds duration-time))))))

(defun simple-pomodoro-state ()
  "Return current state of pomodoro."
  simple-pomodoro--state)

(provide 'simple-pomodoro)
