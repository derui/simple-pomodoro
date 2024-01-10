# simple-pomodoro
Simple pomodoro timer for Emacs.

# Installation
This package does not register MELPA, so you should use straight.el or other method to install this.

We recommended to use [leaf.el](https://github.com/conao3/leaf.el) with straight.el.

```emacs-lisp
(leaf simple-pomodoro
  :straight (simple-pomodoro :type git :host github :repo "derui/simple-pomodoro"))
```

# Basic Usage
```
Start timer with new task
  M-x simple-pomodoro-start

Stop timer.
  M-x simple-pomodoro-stop
  
Reset current pomodoro state
  M-x simple-pomodoro-reset
```

When the task finished, start short break automatilically in default. And then, a short break finished, do not start new task automatically. If you want to start new task, press `y` or `yes` (depending your configuration) in confirmation.

`simple-pomodoro-stop` has behaviors you should be are of. That function **stop timer** , but does not change state. Then you call `simple-pomodoro-start` after `simple-pomodoro-stop`, it is counted from the remaining time when `simple-pomodoro-stop` was called previously.

# Customization
This package provides customization point. You can use `M-x customize RET simple-pomodoro` with customization UI. Or you can change those values by hand.
Please see documents in customize UI.

# Advanced usage

## Extension
This package does not any notification/display functions. Providing some extension points only. Because things what user want are very difference each other, and we can not make everything.

Those functions are extension points or utility function from this package.

- variable: `simple-pomodoro-tick-function`
  - Call like `(simple-pomodoro-tick-function <elapsed> <duration> <state>)` by tick if it set.
  - `state` is one of `stopped`, `task`, `short-break`, `long-break`
- variable: `simple-pomodoro-notification-function`
  - When state is changed, call like `(simple-pomodoro-notification-function <state>)` if it set.
  - Passed argument `<state>` is same as passed to `simple-pomodoro-tick-function`.
  - Passed argument `<state>` is **changed** state.
    - For example, changed to `stopped` state to `task` state (you executes `simple-pomodoro-start`), give `task` state to this function.
- function: `simple-pomodoro-measuring-time`
  - This function returns elapsed/duration time if those are counting.
  - You can use this function to display or do something what you want.
  
  
### Samples

```emacs-lisp
;; notify for each state
(setq simple-pomodoro-notification-function 
      (lambda (state) 
        (pcase 
          ('task
           (notification-notify :title "Pomodoro" :body "Task start"))
          ('task-finished
           (notification-notify :title "Pomodoro" :body "Task finished"))
          ('short-break
           (notification-notify :title "Pomodoro" :body "Short break start"))
          (`long-break
           (notification-notify :title "Pomodoro" :body "Long break start")))))
```

## With org-clock
If you want to start pomodoro timer when you executes org-clock-in, you can add hook to start `simple-pomodoro-start` 

```emacs-lisp
(leaf org-clock
  :hook
  (org-clock-in-hook . simple-pomodoro-start))
```
