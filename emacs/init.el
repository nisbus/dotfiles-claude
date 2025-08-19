;;; Emacs Configuration with Development Support
;;; Supports: Go, Erlang, JavaScript/TypeScript, Python, and EXWM

;; Startup profiling (add at very top)
(setq use-package-compute-statistics t)

;; Set up package manager with optimizations
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Handle signature verification issues
(setq package-check-signature 'allow-unsigned)

;; Prevent frequent archive refreshes
(setq package-refresh-contents-on-install 'no-cache)

;; Suppress common warnings
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors nil)
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Add language server paths to exec-path
(let ((paths '("/home/nisbus/.npm-global/bin"
               "/home/nisbus/go/bin"
               "/home/nisbus/.cargo/bin"
               "/usr/local/bin")))
  (dolist (path paths)
    (when (file-directory-p path)
      (add-to-list 'exec-path path))))

(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load use-package with optimization settings
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)  ; Defer by default
(setq use-package-verbose t)       ; See what's loading
(setq use-package-expand-minimally t) ; Minimize code expansion

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
  :demand t  ; Theme should load immediately
  :config
  (load-theme 'zenburn t))

;; ============================================================================
;; Editor Enhancements
;; ============================================================================

;; Company mode for auto-completion
(use-package company
  :ensure t
  :defer t
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

;; Flycheck for syntax checking
(use-package flycheck
  :ensure t
  :defer t
  :hook (after-init . global-flycheck-mode))

;; Which-key for discovering keybindings
(use-package which-key
  :ensure t
  :defer t
  :hook (after-init . which-key-mode))

;; Magit for Git
(use-package magit
  :ensure t
  :defer t
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
  :defer t
  :hook (after-init . projectile-mode)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; ============================================================================
;; Go Development
;; ============================================================================

(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :hook ((go-mode . lsp-deferred)
         (go-mode . company-mode))
  :bind (:map go-mode-map
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-g" . go-goto-imports)
              ("C-c C-k" . godoc))
  :config
  ;; Use goimports if available
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  
  ;; Format on save
  (add-hook 'before-save-hook 
            (lambda ()
              (when (eq major-mode 'go-mode)
                (gofmt-before-save)))
            nil t)
  
  (setq compile-command "go build -v && go test -v && go vet"))

;; Go debugging with dap-mode
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  ;; Suppress DAP warnings and only setup when needed
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip))
  (when (featurep 'go-mode)
    (condition-case nil
        (progn
          (require 'dap-go)
          (dap-go-setup))
      (error nil))))

;; ============================================================================
;; Erlang Development
;; ============================================================================

;; Erlang mode
(use-package erlang
  :ensure t
  :defer t
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
  :defer t
  :mode "\\.js\\'"
  :hook ((js2-mode . lsp-deferred)
         (js2-mode . company-mode))
  :config
  (setq js2-basic-offset 2)
  (setq js-indent-level 2))

;; TypeScript mode
(use-package typescript-mode
  :ensure t
  :defer t
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
  (setq web-mode-enable-current-element-highlight t)
  
  ;; Disable Python-specific features in web-mode
  (add-hook 'web-mode-hook
            (lambda ()
              (setq-local fill-column 120)  ; Longer lines for web content
              ;; Disable python-docstring-mode if it's somehow active
              (when (bound-and-true-p python-docstring-mode)
                (python-docstring-mode -1)))))

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
;; Python Development
;; ============================================================================

;; Python helper functions (defined before python-mode to avoid void-function errors)
(defun my/python-add-import (import-line)
  "Add IMPORT-LINE to the top of the Python file."
  (interactive "sImport statement: ")
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^\\(from\\|import\\)\\|^[^#\n]" nil t)
      (beginning-of-line)
      (unless (looking-at "^\\(from\\|import\\)")
        (newline)
        (forward-line -1))
      (insert import-line "\n"))))

(defun my/python-run-buffer ()
  "Run current Python buffer."
  (interactive)
  (compile (format "cd %s && python %s"
                   (file-name-directory buffer-file-name)
                   (file-name-nondirectory buffer-file-name))))

(defun my/python-run-pytest-file ()
  "Run pytest on current file."
  (interactive)
  (compile (format "cd %s && python -m pytest %s -v"
                   (projectile-project-root)
                   (file-name-nondirectory buffer-file-name))))

;; Python mode with enhanced features
(use-package python-mode
  :ensure t
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :hook ((python-mode . lsp-deferred)
         (python-mode . company-mode))
  :bind (:map python-mode-map
              ("C-c C-p" . my/python-run-buffer)
              ("C-c C-t" . my/python-run-pytest-file)
              ("C-c i" . my/python-add-import)
              ("C-c C-z" . python-shell-switch-to-shell))
  :config
  ;; Set Python interpreter
  (when (executable-find "python3")
    (setq python-shell-interpreter "python3"))
  
  ;; Enable autopep8 formatting on save
  (add-hook 'python-mode-hook
            (lambda ()
              (when (executable-find "autopep8")
                (add-hook 'before-save-hook 'py-autopep8-before-save nil t))))
  
  ;; Set indentation
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset-verbose nil))

;; LSP Python server (Pyright)
(use-package lsp-pyright
  :ensure t
  :after lsp-mode
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  :config
  ;; Configure Pyright settings
  (setq lsp-pyright-langserver-command "pyright")
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-multi-root t)
  (setq lsp-pyright-use-library-code-for-types t))

;; Python virtual environment management
(use-package pyvenv
  :ensure t
  :after python-mode
  :config
  ;; Auto-activate virtualenv if .venv directory exists
  (add-hook 'python-mode-hook
            (lambda ()
              (let ((venv-dir (locate-dominating-file default-directory ".venv")))
                (when venv-dir
                  (pyvenv-activate (expand-file-name ".venv" venv-dir))))))
  
  ;; Set pyvenv mode line display
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

;; Python code formatting with Black
(use-package blacken
  :ensure t
  :after python-mode
  :hook (python-mode . blacken-mode)
  :config
  (setq blacken-fast-unsafe t)
  (setq blacken-line-length 88))

;; Python import sorting with isort
(use-package py-isort
  :ensure t
  :after python-mode
  :hook (python-mode . py-isort-before-save))

;; Python debugging with dap-mode
(use-package dap-python
  :ensure dap-mode
  :after (lsp-mode dap-mode)
  :config
  ;; Only configure if Python is available
  (when (executable-find "python3")
    (condition-case nil
        (progn
          (require 'dap-python)
          (setq dap-python-executable "python3")
          ;; Add Python debug templates
          (dap-register-debug-template "My App"
            (list :type "python"
                  :args ""
                  :cwd nil
                  :module nil
                  :program nil
                  :request "launch"
                  :name "My App")))
      (error nil))))

;; Python testing with pytest
(use-package python-pytest
  :ensure t
  :after python-mode
  :bind (:map python-mode-map
              ("C-c t t" . python-pytest-run-all)
              ("C-c t f" . python-pytest-run-file)
              ("C-c t F" . python-pytest-run-function)
              ("C-c t r" . python-pytest-repeat)
              ("C-c t p" . python-pytest-popup))
  :config
  (setq python-pytest-arguments '("--color=yes" "--failed-first" "--maxfail=5")))

;; Python REPL integration (built-in)
(with-eval-after-load 'python-mode
  ;; Enable completion in Python shell
  (setq python-shell-completion-native-enable t)
  ;; Set shell prompt regex
  (setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
  (setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
  ;; Use IPython if available
  (when (executable-find "ipython")
    (setq python-shell-interpreter "ipython")
    (setq python-shell-interpreter-args "--simple-prompt --pprint")))

;; Jupyter notebook support
(use-package ein
  :ensure t
  :defer t
  :config
  (setq ein:output-area-inlined-images t))

;; Python docstring support
(use-package python-docstring
  :ensure t
  :after python-mode
  :hook (python-mode . python-docstring-mode)
  :config
  ;; Only apply docstring rules to Python files
  (setq python-docstring-length-limit 88)  ; Match Black line length
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local fill-column 88))))

;; Python functions moved above to avoid void-function errors

;; ============================================================================
;; LSP (Language Server Protocol)
;; ============================================================================

(use-package lsp-mode
  :ensure t
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (erlang-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (js2-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (python-mode . lsp-deferred))
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
  (setq lsp-warn-no-matched-clients nil)
  (setq lsp-signature-auto-activate nil)  ; Reduce signature popup noise
  (setq lsp-eldoc-hook nil)  ; Disable eldoc integration to reduce warnings
  
  ;; Suppress warnings about missing functions
  (declare-function lsp-rename "lsp-mode")
  (declare-function lsp-execute-code-action-by-kind "lsp-mode")
  
  ;; Go specific
  (setq lsp-go-analyses '((fieldalignment . t)
                          (nilness . t)
                          (unusedwrite . t)
                          (unusedparams . t)))
  
  ;; Erlang specific - only set if erlang_ls is available
  (when (executable-find "erlang_ls")
    (setq lsp-erlang-server-path "erlang_ls"))
  
  ;; TypeScript/JavaScript specific
  (when (executable-find "typescript-language-server")
    (setq lsp-clients-typescript-server-args '("--stdio"))))

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

(defun my/run-python-tests ()
  "Run Python tests for the current project."
  (interactive)
  (compile "python -m pytest -v"))

(defun my/run-python-file ()
  "Run current Python file."
  (interactive)
  (compile (format "python %s" (buffer-file-name))))

(defun my/activate-python-venv ()
  "Activate Python virtual environment in current project."
  (interactive)
  (let ((venv-dir (locate-dominating-file default-directory ".venv")))
    (if venv-dir
        (pyvenv-activate (expand-file-name ".venv" venv-dir))
      (message "No .venv directory found in project root"))))

(global-set-key (kbd "C-c g t") 'my/run-go-tests)
(global-set-key (kbd "C-c n d") 'my/run-npm-dev)
(global-set-key (kbd "C-c n b") 'my/run-npm-build)
(global-set-key (kbd "C-c p t") 'my/run-python-tests)
(global-set-key (kbd "C-c p r") 'my/run-python-file)
(global-set-key (kbd "C-c p v") 'my/activate-python-venv)

;; ============================================================================
;; Final Setup
;; ============================================================================

;; Start server for emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;; Display startup time and profiling info
(defun display-startup-echo-area-message ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Add command to view use-package statistics
(defun my/show-package-stats ()
  "Show use-package loading statistics."
  (interactive)
  (use-package-report))

;; LSP server verification function
(defun my/check-lsp-servers ()
  "Check if LSP servers are available and working."
  (interactive)
  (let ((servers '(("gopls" . "Go")
                   ("pyright" . "Python") 
                   ("typescript-language-server" . "TypeScript")
                   ("prettier" . "Prettier")
                   ("erlang_ls" . "Erlang")
                   ("elixir-ls" . "Elixir")))
        (results '()))
    (dolist (server servers)
      (let* ((cmd (car server))
             (lang (cdr server))
             (status (if (executable-find cmd) "✅ Found" "❌ Not found")))
        (push (format "%s (%s): %s" lang cmd status) results)))
    
    (with-current-buffer (get-buffer-create "*LSP Server Status*")
      (erase-buffer)
      (insert "LSP Server Status Check\n")
      (insert "======================\n\n")
      (dolist (result (reverse results))
        (insert result "\n"))
      (insert "\nFor installation instructions, see:\n")
      (insert "- LSP_SETUP.md in your dotfiles directory\n")
      (insert "- Run: ./install-lsp-servers.sh\n")
      (insert "- Or manually verify: ./verify-lsp-setup.sh\n\n")
      (insert "LSP Troubleshooting:\n")
      (insert "- Enable LSP logging: (setq lsp-log-io t)\n")
      (insert "- Check LSP session: M-x lsp-describe-session\n")
      (insert "- Restart LSP server: M-x lsp-workspace-restart\n")
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(global-set-key (kbd "C-c L") 'my/check-lsp-servers)

;; Custom variables (auto-generated, keep at end)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm docker-compose-mode dockerfile-mode yaml-mode lsp-ui npm-mode prettier json-mode web-mode typescript-mode alchemist elixir-mode erlang dap-mode go-mode projectile treemacs magit which-key flycheck company exwm desktop-environment zenburn-theme use-package js2-mode))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
