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
(require 'ert)
(require 'simple-pomodoro)

;; support macro from source.
(defmacro sps--get (slot)
  "internal macro to get value of global state. This macro is used only byte compile."
  `(cl-struct-slot-value 'simple-pomodoro--internal-state ,slot simple-pomodoro--state))

(defmacro sps--set (slot value)
  "internal macro to set value for global state. This macro is used only byte compile."
  `(setf (cl-struct-slot-value 'simple-pomodoro--internal-state ,slot simple-pomodoro--state) ,value))

(ert-deftest start-task ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) '(0 0 0 0))))
    (simple-pomodoro-reset)
    (simple-pomodoro-start)
    (should (equal (sps--get 'task-count) 1))
    (should (equal (sps--get 'time-keeper) (cons 0 (* 60 simple-pomodoro-task-time))))
    (should (equal (simple-pomodoro-current-state) 'task))))

(ert-deftest ticked-time ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) '(0 0 0 0))))
    (simple-pomodoro-reset)
    (simple-pomodoro-start)
    (simple-pomodoro--tick)
    (should (equal (sps--get 'task-count) 1))
    (should (equal (sps--get 'time-keeper) (cons 1 (* 60 simple-pomodoro-task-time))))
    (should (equal (simple-pomodoro-current-state) 'task))))

(ert-deftest finish-task ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (simple-pomodoro-reset)
    (simple-pomodoro-start)
    (simple-pomodoro--tick)
    (simple-pomodoro--finish)
    (should (equal (sps--get 'task-count) 1))
    (should (equal (sps--get 'time-keeper) (cons 0 (* 60 simple-pomodoro-short-break-time))))
    (should (equal (simple-pomodoro-current-state) 'short-break))))

(ert-deftest call-notification-function-after-finished ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-notification-function (lambda (state) (should (equal state 'short-break)))))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)
      (should (equal (sps--get 'task-count) 1))
      (should (equal (sps--get 'time-keeper) (cons 0 (* 60 simple-pomodoro-short-break-time))))
      (should (equal (simple-pomodoro-current-state) 'short-break)))))

(ert-deftest call-notification-function-for-long-break ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-cycle-task-count 2))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)         ;finish task
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)         ;finish short break
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)         ;finish task
      (let ((simple-pomodoro-notification-function (lambda (state) (should (equal state 'long-break)))))
        (simple-pomodoro--tick)
        (simple-pomodoro--finish)
        (should (equal (sps--get 'task-count) 0))
        (should (equal (sps--get 'time-keeper) (cons 0 (* 60 simple-pomodoro-long-break-time))))
        (should (equal (simple-pomodoro-current-state) 'long-break))))))

(ert-deftest call-tick-function ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-tick-function (lambda (elapsed duration state)
                                           (should (eq state 'task))
                                           (should (equal elapsed 1))
                                           (should (equal duration (1- (* 60 simple-pomodoro-task-time)))))))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick))))
