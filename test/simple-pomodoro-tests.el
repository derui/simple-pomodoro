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
(require 'simple-pomodoro-mode-line)

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
    (let ((simple-pomodoro-notification-function (lambda (state) (should (or (equal state 'task-finished)
                                                                             (equal state 'task))))))
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
      (let ((simple-pomodoro-notification-function (lambda (state) (should (or (equal state 'long-break-finished)
                                                                               (equal state 'long-break))))))
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
    (let ((simple-pomodoro-notification-function (lambda (state) (should (or (equal state 'stopped)
                                                                             (equal state 'task))))))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (cl-letf (((symbol-function 'simple-pomodoro--timer-running-p) (lambda () t)))
        (simple-pomodoro-stop)
        (should (equal 'stopped (simple-pomodoro-current-state)))
        (should (equal nil (simple-pomodoro-measuring-time))))
      ;; start from stored timer
      (cl-letf (((symbol-function 'simple-pomodoro--timer-running-p) (lambda () nil)))
        (simple-pomodoro-start)
        (should (equal 'task (simple-pomodoro-current-state)))
        (should (equal (cons 1 (1- (* 60 simple-pomodoro-task-time))) (simple-pomodoro-measuring-time)))))))

(ert-deftest auto-short-break ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) t))
            ((symbol-function 'y-or-n-p) (lambda (&rest rests) t)))
    (let ((simple-pomodoro-auto-short-break t))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)
      ;; start short break automatically.
      (should (equal 'short-break (simple-pomodoro-current-state)))
      (should (equal (cons 0 (* 60 simple-pomodoro-short-break-time)) (simple-pomodoro-measuring-time))))))

(ert-deftest do-not-expose-notification-error ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-notification-function (lambda (state) (error "errored"))))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (simple-pomodoro--finish)
      (should t))))

(ert-deftest display-mode-line-for-task ()
  (cl-letf (((symbol-function 'run-at-time) (lambda (&rest rests) nil)))
    (let ((simple-pomodoro-notification-function (lambda (state) (error "errored"))))
      (simple-pomodoro-reset)
      (simple-pomodoro-start)
      (simple-pomodoro--tick)
      (simple-pomodoro-mode-line-update-text)

      (should (string= "24:59" (simple-pomodoro-mode-line-text))))))
