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

;; define tests

(ert-deftest start-task ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) '(0 0 0 0))))
    (simple-pomodoro-reset)
    (simple-pomodoro-start)
    (should (equal (sps--get 'task-count) 0))
    (should (equal (sps--get 'time-keeper) (cons 0 (* 60 simple-pomodoro-task-time))))
    (should (equal (simple-pomodoro-current-state) 'task))))

(ert-deftest ticked-time ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) '(0 0 0 0))))
    (simple-pomodoro-reset)
    (simple-pomodoro-start)
    (simple-pomodoro--tick)
    (should (equal (sps--get 'task-count) 0))
    (should (equal (sps--get 'time-keeper) (cons 1 (* 60 simple-pomodoro-task-time))))
    (should (equal (simple-pomodoro-current-state) 'task))))

(ert-deftest finish-task ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (simple-pomodoro-reset)
    (simple-pomodoro-start)
    (simple-pomodoro--tick)
    (simple-pomodoro--finish)
    (should (equal (sps--get 'task-count) 1))
    (should (equal (simple-pomodoro-current-state) 'task-finished))))

(ert-deftest call-notification-function-after-finished ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-notification-function (lambda (state) (should (equal state 'task-finished)))))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)
      (should (equal (sps--get 'task-count) 1))
      (should (equal (simple-pomodoro-current-state) 'task-finished)))))

(ert-deftest call-notification-function-for-long-break ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-cycle-task-count 2))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)         ;finish task
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)         ;finish short break
      (simple-pomodoro-start)           ; start new task
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)         ;finish task, then start long-break
      (let ((simple-pomodoro-notification-function (lambda (state) (should (equal state 'long-break-finished)))))
        (simple-pomodoro-start)           ; start new long-break
        (simple-pomodoro--tick)
        (simple-pomodoro--finish)
        (should (equal (sps--get 'task-count) 0))
        (should (equal (sps--get 'cycle-count) 1))
        (should (equal (simple-pomodoro-current-state) 'long-break-finished))))))

(ert-deftest call-tick-function ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-tick-function (lambda (elapsed duration state)
                                           (should (equal state 'task))
                                           (should (equal elapsed 1))
                                           (should (equal duration (1- (* 60 simple-pomodoro-task-time)))))))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (should (equal (cons 1 (- (* 60 simple-pomodoro-task-time) 1)) (simple-pomodoro-measuring-time))))))

(ert-deftest stop-and-restore-timer ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) t))
            ((symbol-function 'cancel-timer) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-notification-function (lambda (state) (should (equal state 'stopped)))))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (cl-letf (((symbol-function 'simple-pomodoro--timer-running-p) (lambda () t)))
        (simple-pomodoro-stop)
        (should (equal nil (simple-pomodoro-measuring-time)))))))
