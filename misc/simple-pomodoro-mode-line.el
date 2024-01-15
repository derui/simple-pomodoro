;;; simple-pomodoro-mode-line.el --- Mode line element for simple-pomodoro -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Created: 2024
;; Package-Requires: ((emacs "27.1") nerd-icon (simple-pomodoro "0.1.0"))
;; Keywords: timer, mode-line

;;; Commentary:
;;
;; simple-pomodoro-mode-line.el provides funciton and update function for mode line.
;;
;;; Customization:
;;
;; Use `customize' to customize `simple-pomodoro-mode-line'.
;;   (customize 'simple-pomodoro-mode-line)
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'simple-pomodoro)

(defgroup simple-pomodoro-mode-line nil
  "Mode line element for simple-pomodoro."
  :group 'simple-pomodoro)

(defcustom simple-pomodoro-mode-line-task-icon ""
  "Icon for task."
  :type 'string
  :group 'simple-pomodoro-mode-line)

(defcustom simple-pomodoro-mode-line-short-break-icon ""
  "Icon for short break."
  :type 'string
  :group 'simple-pomodoro-mode-line)

(defcustom simple-pomodoro-mode-line-long-break-icon ""
  " icon for long break"
  :type 'string
  :group 'simple-pomodoro-mode-line)

(defvar simple-pomodoro-mode-line--text ""
  "Internal state of mode line")

(defvar simple-pomodoro-mode-line '(:eval (simple-pomodoro-mode-line-text))
  "The element for mode line to display pomodoro remain time")

;;;###autoload
(defun simple-pomodoro-mode-line-text ()
  "Return text for mode line."
  (let ((text simple-pomodoro-mode-line--text)
        (icon (cl-case (simple-pomodoro-current-state)
                (task simple-pomodoro-mode-line-task-icon)
                (short-break simple-pomodoro-mode-line-short-break-icon)
                (long-break simple-pomodoro-mode-line-long-break-icon)
                (t ""))))
    (if (or (string-empty-p text) (string-empty-p icon))
        ""
      (concat icon text))))

;;;###autoload
(defun simple-pomodoro-mode-line-update-text ()
  "Update text for mode line. Call this function in `simple-pomodoro-tick-function'"
  (setq simple-pomodoro-mode-line--text
        (let ((time (simple-pomodoro-measuring-time)))
          (if time
              (format-time-string "%02M:%02S" (cdr time))
            ""))))

(provide 'simple-pomodoro-mode-line)
