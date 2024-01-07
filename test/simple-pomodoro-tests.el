;;; simple-pomodoro-tests.el --- tests for simple-pomodoro -*- lexical-binding: t; byte-compile-docstring-max-column: 120; -*-
;; Copyright (C) 2024 derui

;; Author: derui <derutakayu@gmail.com>
;; Maintainer: derui <derutakayu@gmail.com>
;; URL: 
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: timer

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'cort)
(require 'simple-pomodoro)

;; support macro from source.
(defmacro sps--get (slot)
  "internal macro to get value of global state. This macro is used only byte compile."
  `(cl-struct-slot-value 'simple-pomodoro--internal-state ,slot simple-pomodoro--state))

(defmacro sps--set (slot value)
  "internal macro to set value for global state. This macro is used only byte compile."
  `(setf (cl-struct-slot-value 'simple-pomodoro--internal-state ,slot simple-pomodoro--state) ,value))

(cort-deftest start-task
              (cl-letf (((symbol-function 'run-at-time) (lambda (&rest) nil)))
                (simple-pomodoro-start)
                (:equal (sps--get 'time-keeper) (cons 0 (* 60 simple-pomodoro-task-time))))
              )
