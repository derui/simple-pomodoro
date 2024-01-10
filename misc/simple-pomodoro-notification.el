;;; simple-pomodoro-notification.el --- Notification function for simple-pomodoro -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (alert "1.2") (simple-pomodoro "0.1.0"))
;; Keywords: timer, notification

;;; Commentary:
;;
;; simple-pomodoro-notification.el provides function to notify user when state of pomodoro is changed.
;;
;;: Customization:
;; You can customize some variables. Use customize-group or setq.
;;   M-x customize-group simple-pomodoro

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'alert)
(require 'simple-pomodoro)

(defvar simple-pomodoro-notification--alert-style
  (cond
   ((eq system-type 'darwin) 'osx-notifier)
   (t 'notifications))
  "Notification style for alert. This variable is defined under user's environment.")

;; functions

;;;###autoload
(defun simple-pomodoro-notification (kind)
  "Notify `KIND' to user."
  (cl-case kind
    (task
     (alert "Task is started"
            :title "Simple Pomodoro"
            :style simple-pomodoro-notification--alert-style))
    (task-finished
     (alert "Task is finished."
            :title "Simple Pomodoro"
            :style simple-pomodoro-notification--alert-style))
    (short-break
     (alert "Short break is started."
            :title "Simple Pomodoro"
            :style simple-pomodoro-notification--alert-style))
    (short-break-finished
     (alert "Short break is finished."
            :title "Simple Pomodoro"
            :style simple-pomodoro-notification--alert-style))
    (long-break
     (alert "Long break is started."
            :title "Simple Pomodoro"
            :style simple-pomodoro-notification--alert-style))
    (long-break-finished
     (alert "Long break is finished."
            :title "Simple Pomodoro"
            :style simple-pomodoro-notification--alert-style))
    (stopped
     (alert "Timer is stopped"
            :title "Simple Pomodoro"
            :style simple-pomodoro-notification--alert-style))))

(provide 'simple-pomodoro-notification)
