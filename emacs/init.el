;;; Emacs Configuration with EXWM

;; Set up package manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Handle signature verification issues
(setq package-check-signature 'allow-unsigned)

(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; Basic settings
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq backup-directory-alist '(("." . "/tmp")))
(setq auto-save-file-name-transforms '((".*" "/tmp/" t)))

;; Set default font
(set-face-attribute 'default nil :height 120)

;; EXWM Configuration (only when running as window manager)
;; Check if we're running EXWM by looking for the EXWM environment variable
;; or if no other window manager is running
(when (and (require 'exwm nil 'noerror)
           (or (getenv "EXWM")
               (string-match "exwm" (or (getenv "DESKTOP_SESSION") ""))
               (and (not (getenv "DESKTOP_SESSION"))
                    (not (getenv "XDG_CURRENT_DESKTOP")))))
  (condition-case err
      (progn
        (require 'exwm-config)
        (exwm-config-default)
        
        ;; Set the initial workspace number
        (setq exwm-workspace-number 4)
        
        ;; Global keybindings
        (setq exwm-input-global-keys
              `(([?\s-r] . exwm-reset)
                ([?\s-w] . exwm-workspace-switch)
                ,@(mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda ()
                                (interactive)
                                (exwm-workspace-switch-create ,i))))
                          (number-sequence 0 9))
                ([?\s-&] . (lambda (command)
                             (interactive (list (read-shell-command "$ ")))
                             (start-process-shell-command command nil command))))))
    (error
     ;; Silently ignore if another window manager is running
     nil)))

;; Erlang mode setup
(setq erlang-root-dir "/home/nisbus/.guix-profile")
(when (file-exists-p (concat erlang-root-dir "/lib/erlang/lib/tools-3.5.3"))
  (setq exec-path (cons (concat erlang-root-dir "/lib/erlang/bin") exec-path))
  (require 'erlang-start nil 'noerror))

;; Custom keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Install and configure Zenburn theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Flymake configuration for Erlang and JavaScript
(use-package flymake
  :ensure t
  :hook ((erlang-mode . flymake-mode)
         (js-mode . flymake-mode)
         (js2-mode . flymake-mode))
  :config
  ;; Show errors in minibuffer
  (setq flymake-no-changes-timeout 0.5)
  (setq flymake-start-syntax-check-on-newline t))

;; JavaScript mode with better support
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :config
  (setq js2-basic-offset 2))

;; Erlang flymake backend
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "erlc" (list "-Wall" "+debug_info" "-I../include" "-I../deps" "-o/tmp/" local-file))))

(when (require 'erlang nil 'noerror)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.erl\\'" flymake-erlang-init)))