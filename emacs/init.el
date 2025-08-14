;;; Emacs Configuration with Development Support
;;; Supports: Go, Erlang, JavaScript/TypeScript, and EXWM

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

;; ============================================================================
;; Basic Settings
;; ============================================================================

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode 1)
(setq backup-directory-alist '(("." . "/tmp")))
(setq auto-save-file-name-transforms '((".*" "/tmp/" t)))
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Set default font and size
(set-face-attribute 'default nil :height 120)
(when (member "JetBrains Mono" (font-family-list))
  (set-face-attribute 'default nil :font "JetBrains Mono-12"))

;; Enable line wrapping
(global-visual-line-mode 1)

;; Show matching parentheses
(show-paren-mode 1)

;; Better scrolling
(setq scroll-conservatively 100)

;; Highlight current line
(global-hl-line-mode 1)

;; Auto-refresh buffers when files change on disk
(global-auto-revert-mode 1)

;; ============================================================================
;; Theme
;; ============================================================================

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; ============================================================================
;; Editor Enhancements
;; ============================================================================

;; Company mode for auto-completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Which-key for discovering keybindings
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Magit for Git
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Treemacs for file navigation
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; Projectile for project management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; ============================================================================
;; Go Development
;; ============================================================================

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (go-mode . company-mode)
         (before-save . gofmt-before-save))
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-g" . go-goto-imports)
              ("C-c C-k" . godoc))
  :config
  (setq gofmt-command "goimports")
  (setq compile-command "go build -v && go test -v && go vet"))

;; Go debugging with dap-mode
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-go)
  (dap-go-setup))

;; ============================================================================
;; Erlang Development
;; ============================================================================

;; Erlang mode
(use-package erlang
  :ensure t
  :mode (("\\.erl\\'" . erlang-mode)
         ("\\.hrl\\'" . erlang-mode)
         ("\\.xrl\\'" . erlang-mode)
         ("\\.yrl\\'" . erlang-mode)
         ("\\.app\\.src\\'" . erlang-mode)
         ("rebar\\.config\\'" . erlang-mode)
         ("relx\\.config\\'" . erlang-mode)
         ("sys\\.config\\'" . erlang-mode))
  :hook ((erlang-mode . lsp-deferred)
         (erlang-mode . company-mode))
  :config
  ;; Set Erlang root based on system
  (cond
   ((file-exists-p "/usr/lib/erlang")
    (setq erlang-root-dir "/usr/lib/erlang"))
   ((file-exists-p "/usr/local/lib/erlang")
    (setq erlang-root-dir "/usr/local/lib/erlang"))
   ((file-exists-p (expand-file-name "~/.guix-profile/lib/erlang"))
    (setq erlang-root-dir (expand-file-name "~/.guix-profile/lib/erlang"))))
  
  (when erlang-root-dir
    (add-to-list 'exec-path (concat erlang-root-dir "/bin")))
  
  ;; Erlang shell history
  (setq erlang-shell-history-file "~/.erlang_history")
  (setq erlang-shell-history-size 10000))

;; Elixir mode (often used with Erlang)
(use-package elixir-mode
  :ensure t
  :mode (("\\.ex\\'" . elixir-mode)
         ("\\.exs\\'" . elixir-mode)
         ("\\.eex\\'" . web-mode))
  :hook ((elixir-mode . lsp-deferred)
         (elixir-mode . company-mode)))

;; Alchemist for Elixir development
(use-package alchemist
  :ensure t
  :after elixir-mode)

;; ============================================================================
;; JavaScript/TypeScript Development
;; ============================================================================

;; JavaScript mode with better support
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :hook ((js2-mode . lsp-deferred)
         (js2-mode . company-mode))
  :config
  (setq js2-basic-offset 2)
  (setq js-indent-level 2))

;; TypeScript mode
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook ((typescript-mode . lsp-deferred)
         (typescript-mode . company-mode))
  :config
  (setq typescript-indent-level 2))

;; TSX support
(use-package web-mode
  :ensure t
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode))
  :hook ((web-mode . lsp-deferred)
         (web-mode . company-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t))

;; JSON mode
(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :config
  (setq json-reformat:indent-width 2))

;; Prettier formatting
(use-package prettier
  :ensure t
  :hook ((js2-mode . prettier-mode)
         (typescript-mode . prettier-mode)
         (web-mode . prettier-mode)
         (json-mode . prettier-mode)))

;; npm integration
(use-package npm-mode
  :ensure t
  :hook ((js2-mode . npm-mode)
         (typescript-mode . npm-mode)
         (web-mode . npm-mode)))

;; ============================================================================
;; LSP (Language Server Protocol)
;; ============================================================================

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (erlang-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet t)
  (setq lsp-enable-on-type-formatting t)
  (setq lsp-enable-indentation t)
  (setq lsp-diagnostic-package :flycheck)
  (setq lsp-completion-provider :company)
  (setq lsp-idle-delay 0.500)
  (setq lsp-log-io nil)
  
  ;; Go specific
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (unusedwrite . t)
                          (unusedparams . t)))
  
  ;; Erlang specific
  (setq lsp-erlang-server-path "erlang_ls"))

;; LSP UI for better IDE experience
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-diagnostics t))

;; LSP Treemacs integration
(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :config
  (lsp-treemacs-sync-mode 1))

;; ============================================================================
;; YAML Support
;; ============================================================================

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

;; ============================================================================
;; Docker Support
;; ============================================================================

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package docker-compose-mode
  :ensure t
  :mode "docker-compose\\.yml\\'")

;; ============================================================================
;; Terminal
;; ============================================================================

(use-package vterm
  :ensure t
  :bind ("C-c t" . vterm))

;; ============================================================================
;; EXWM Configuration (only when running as window manager)
;; ============================================================================

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

;; ============================================================================
;; Custom Keybindings
;; ============================================================================

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c r") 'recompile)
(global-set-key (kbd "M-/") 'company-complete)
(global-set-key (kbd "C-c d") 'lsp-describe-thing-at-point)
(global-set-key (kbd "C-c a") 'lsp-execute-code-action)
(global-set-key (kbd "C-c f") 'lsp-format-buffer)
(global-set-key (kbd "C-c s") 'lsp-workspace-symbol)

;; ============================================================================
;; Custom Functions
;; ============================================================================

(defun my/run-go-tests ()
  "Run Go tests for the current project."
  (interactive)
  (compile "go test -v ./..."))

(defun my/run-npm-dev ()
  "Run npm dev server."
  (interactive)
  (compile "npm run dev"))

(defun my/run-npm-build ()
  "Run npm build."
  (interactive)
  (compile "npm run build"))

(global-set-key (kbd "C-c g t") 'my/run-go-tests)
(global-set-key (kbd "C-c n d") 'my/run-npm-dev)
(global-set-key (kbd "C-c n b") 'my/run-npm-build)

;; ============================================================================
;; Final Setup
;; ============================================================================

;; Start server for emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Display startup time
(defun display-startup-echo-area-message ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Custom variables (auto-generated, keep at end)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )