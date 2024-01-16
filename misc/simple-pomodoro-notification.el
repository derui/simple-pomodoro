;;; simple-pomodoro-notification.el --- Notification function for simple-pomodoro -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Created: 2024
;; Package-Requires: ((emacs "27.1") (alert "1.2") (simple-pomodoro "0.1.0"))
;; Keywords: timer, notification

;;; Commentary:
;;
;; simple-pomodoro-notification.el provides function to notify user when state of pomodoro is changed.
;;
;;; Customization:

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'alert)
(require 'simple-pomodoro)

(defcustom simple-pomodoro-notification-icon nil
  "Icon image path for notification. When this value is `nil', notification does not display any icon.
User can set icon path for all state, or each state of pomodoro. If alist have a key `t', it is used as fallback icon.
 Like configurations below.

(custom-set-variable \='simple-pomodoro-notificatio-icon \"path of variable\") ;; set icon for all state
(custom-set-variable \='simple-pomodoro-notificatio-icon \='((task . \"icon for task\") (fallback . \"fallback icon\"))) ;; set icon for each state.

"
  :type '(choice (alist :key-type symbol :value-type string
                        :options (task task-finished short-break short-break-finished long-break long-break-finished stopped fallback))
                 string)
  :group 'simple-pomodoro)

(defvar simple-pomodoro-notification--alert-style
  (cond
   ((eq system-type 'darwin) 'osx-notifier)
   (t 'notifications))
  "Notification style for alert. This variable is defined under user's environment.")

;; functions

(defun simple-pomodoro-notification--get-icon (state)
  "Get icon path for `state'"
  (or (and (stringp simple-pomodoro-notification-icon) simple-pomodoro-notification-icon)
      (alist-get state simple-pomodoro-notification-icon)
      (alist-get 'fallback simple-pomodoro-notification-icon)))

;;;###autoload
(defun simple-pomodoro-notification (kind)
  "Notify `KIND' to user."
  (let ((icon (simple-pomodoro-notification--get-icon kind)))
    (cl-case kind
      (task
       (alert "Task is started"
              :icon icon
              :title "Simple Pomodoro"
              :style simple-pomodoro-notification--alert-style))
      (task-finished
       (alert "Task is finished."
              :icon icon
              :title "Simple Pomodoro"
              :style simple-pomodoro-notification--alert-style))
      (short-break
       (alert "Short break is started."
              :icon icon
              :title "Simple Pomodoro"
              :style simple-pomodoro-notification--alert-style))
      (short-break-finished
       (alert "Short break is finished."
              :icon icon
              :title "Simple Pomodoro"
              :style simple-pomodoro-notification--alert-style))
      (long-break
       (alert "Long break is started."
              :icon icon
              :title "Simple Pomodoro"
              :style simple-pomodoro-notification--alert-style))
      (long-break-finished
       (alert "Long break is finished."
              :icon icon
              :title "Simple Pomodoro"
              :style simple-pomodoro-notification--alert-style))
      (stopped
       (alert "Timer is stopped"
              :icon icon
              :title "Simple Pomodoro"
              :style simple-pomodoro-notification--alert-style)))))

(provide 'simple-pomodoro-notification)
