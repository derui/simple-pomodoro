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

(cl-defstruct simple-pomodoro-state
  "State of pomodoro."
  (kind 'stopped)
  (task-count 0)
  (cycle-count 0)
  (timer nil)
  (tick-timer nil)
  (start-time 0)
  (end-time 0))

(defvar simple-pomodoro--state (make-simple-pomodoro-state)
  "Global states of simple-pomodoro")

;; functions

(defun simple-pomodoro--notify (kind)
  "Notify `KIND' to user."
  (when (functionp simple-pomodoro-notification-function)
    (funcall simple-pomodoro-notification-function kind)))

(defun simple-pomodoro--update-state (kind)
  "Update some state of pomodoro from `KIND'."
  (setf (simple-pomodoro-state-kind simple-pomodoro--state) kind)
  (cl-case kind
    (task
     (setf (simple-pomodoro-state-task-count simple-pomodoro--state) (1+ (simple-pomodoro-state-task-count simple-pomodoro--state))))
    (long-break
     (setf (simple-pomodoro-state-cycle-count simple-pomodoro--state) (1+ (simple-pomodoro-state-cycle-count simple-pomodoro--state))
           (simple-pomodoro-state-task-count simple-pomodoro--state) 0))
    (t nil)))

(defun simple-pomodoro--next-state (kind)
  "Return state of pomodoro to next state from `KIND'."
  (cl-case kind
    (stopped 'task)
    (task
     (let ((task-count (simple-pomodoro-state-task-count simple-pomodoro--state)))
       (if (<= simple-pomodoro-cycle-task-count task-count)
           'long-break
         'short-break)))
    (short-break 'stopped)
    (long-break 'stopped)
    (t
     (error "Invalid state: %s" kind))))

(defun simple-pomodoro--tick ()
  "Tick function for pomodoro. This function calls per second."
  (unless (eq (simple-pomodoro-state-kind simple-pomodoro--state) 'stopped)
    (let* ((measured (simple-pomodoro-measuring-time))
           (elapsed (car measured))
           (duration (cdr measured)))
      (when (and simple-pomodoro-tick-function
                 (functionp simple-pomodoro-tick-function))
        (funcall simple-pomodoro-tick-function
                 elapsed
                 duration
                 simple-pomodoro--state)))))

(defun simple-pomodoro--finish ()
  "Finish function for pomodoro. This function calls when timer is finished."
  (simple-pomodoro--stop-timer)
  
  (let ((next-state (simple-pomodoro--next-state (simple-pomodoro-current-state))))

    (simple-pomodoro--update-state next-state)
    (simple-pomodoro--notify next-state)

    (cl-case next-state
      (short-break (simple-pomodoro--start-timer simple-pomodoro-short-break-time))
      (long-break (simple-pomodoro--start-timer simple-pomodoro-long-break-time))
      (t nil))))

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
  (dolist (timer (list (simple-pomodoro-state-timer simple-pomodoro--state)
                       (simple-pomodoro-state-tick-timer simple-pomodoro--state)))
    (and (timerp timer)
         (cancel-timer timer)
         (setf timer nil))))

(defun simple-pomodoro-reset ()
  "Stop timer and reset all state."
  (interactive)
  (simple-pomodoro--stop-timer)

  (setq simple-pomodoro--state (make-simple-pomodoro-state)))

(defun simple-pomodoro--timer-running-p ()
  "Return `t' if timer is running."
  (or (timerp (simple-pomodoro-state-timer simple-pomodoro--state))
      (timerp (simple-pomodoro-state-tick-timer simple-pomodoro--state))))

(defun simple-pomodoro-start ()
  "Start pomodoro."
  (interactive)
  (if (simple-pomodoro--timer-running-p)
      (progn
        (message "Pomodoro is already running")
        (cl-return))
    (let ((next-state (simple-pomodoro--next-state (simple-pomodoro-state-kind simple-pomodoro--state))))
      (simple-pomodoro--update-state next-state)
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
  (let ((state simple-pomodoro--state))
    (cl-case (simple-pomodoro-current-state)
      (stopped nil)
      (t (let* ((elapsed-time (time-since (simple-pomodoro-state-start-time state)))
                (total-duration-seconds (time-subtract (simple-pomodoro-state-end-time state)
                                                       (simple-pomodoro-state-start-time state)))
                (duration-time (time-subtract total-duration-seconds elapsed-time)))
           (cons (time-to-seconds elapsed-time)
                 (time-to-seconds duration-time)))))))

(defun simple-pomodoro-current-state ()
  "Return current state of pomodoro."
  (simple-pomodoro-state-kind simple-pomodoro--state))

(provide 'simple-pomodoro)
