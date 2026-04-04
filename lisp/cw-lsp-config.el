;;; cw-lsp-config.el --- LSP + formatting config -*- lexical-binding: t; -*-

(require 'use-package)

;; ----------------------------
;; Global indentation defaults
;; ----------------------------
(setq-default tab-width 8)
(setq-default standard-indent 8)
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset 8)
(setq-default evil-shift-width 8)
(setq c-ts-mode-indent-offset 8)
(setq c++-ts-mode-indent-offset 8)

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(x pgtk wayland))
  :config
  (exec-path-from-shell-initialize))

;; ----------
;; Completion
;; ----------
(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t)
  :hook (prog-mode . company-mode))

(use-package elec-pair
  :ensure nil
  :init
  (electric-pair-mode 1)
  (electric-indent-mode 1))

;; -------------------
;; Smart include-jump
;; -------------------
(defun cw/c-angle-include-at-point ()
  (let ((pt (point)))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward "^\\s-*#\\s-*include\\s-*<\\([^>]+\\)>" (line-end-position) t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (when (and (>= pt beg) (<= pt end))
            (match-string 1)))))))

(defun cw/lsp-find-definition-smart ()
  (interactive)
  (if (and (cw/c-angle-include-at-point)
           (fboundp 'tab-bar-new-tab))
      (progn
        (tab-bar-new-tab)
        (lsp-find-definition))
    (lsp-find-definition)))

(defun cw/lsp-ripgrep-declarations ()
  (interactive)
  (require 'consult)
  (let* ((sym (thing-at-point 'symbol t))
         (root (or (bound-and-true-p cw/global-cwd)
                   (when-let ((proj (project-current nil)))
                     (project-root proj))
                   default-directory)))
    (consult-ripgrep root sym)))

(defun cw/lsp-inline-suggestions ()
  (cond
   ((fboundp 'company-mode) (company-mode 1))
   ((require 'completion-preview nil 'noerror)
    (completion-preview-mode 1))))

(defun cw/lsp-inlay-hints-maybe ()
  (when (lsp-feature? "textDocument/inlayHint")
    (lsp-inlay-hints-mode 1)))

;; --------
;; lsp-mode
;; --------
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-snippet nil
        lsp-prefer-capf t
        lsp-inlay-hint-enable nil
        ;; Stop LSP touching indentation entirely
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-clients-clangd-args
        '("--fallback-style=none"))
  :hook
  ((c-mode          . lsp-deferred)
   (c++-mode        . lsp-deferred)
   (c-ts-mode       . lsp-deferred)
   (c++-ts-mode     . lsp-deferred)
   (python-mode     . lsp-deferred)
   (rust-mode       . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   (tsx-ts-mode     . lsp-deferred)
   (js-mode         . lsp-deferred)
   (js-ts-mode      . lsp-deferred)
   (go-mode         . lsp-deferred)
   (lua-mode        . lsp-deferred)
   (lsp-mode        . cw/lsp-inline-suggestions)
   (lsp-mode        . cw/lsp-inlay-hints-maybe))
  :commands (lsp lsp-deferred))

;; -----------------------------------
;; C/C++ formatting — one source of truth
;; -----------------------------------
(defun cw/c-style-formatting ()
  (setq-local tab-width 8)
  (setq-local standard-indent 8)
  (setq-local indent-tabs-mode t)
  (setq-local c-basic-offset 8)
  (setq-local evil-shift-width 8)
  (c-set-style "bsd")
  (setq-local c-electric-flag t)
  ;; Disable electric-indent so pressing enter doesn't reindent the line below
  (electric-indent-local-mode -1)
  (electric-pair-local-mode 1)
  ;; Kill LSP indentation interference
  (setq-local lsp-enable-on-type-formatting nil)
  (setq-local lsp-enable-indentation nil))

(defun cw/c-newline ()
  "Insert a newline and indent to match the current line's indentation only."
  (interactive)
  (let ((indent (save-excursion
                  (back-to-indentation)
                  (buffer-substring (line-beginning-position) (point)))))
    (newline)
    (insert indent)))

(defun cw/c-open-line-below ()
  "Open a new line below, matching current line indent."
  (interactive)
  (end-of-line)
  (cw/c-newline)
  (evil-insert-state))

(defun cw/c-open-line-above ()
  "Open a new line above, matching current line indent."
  (interactive)
  (beginning-of-line)
  (save-excursion (newline))
  (evil-insert-state))

(defun cw/c-fix-enter ()
  "Stop c-mode and evil from reindenting on newline."
  (local-set-key (kbd "RET") #'cw/c-newline)
  (local-set-key (kbd "<return>") #'cw/c-newline)
  (with-eval-after-load 'evil
    (evil-local-set-key 'insert (kbd "RET") #'cw/c-newline)
    (evil-local-set-key 'insert (kbd "<return>") #'cw/c-newline)
    (evil-local-set-key 'normal (kbd "o") #'cw/c-open-line-below)
    (evil-local-set-key 'normal (kbd "O") #'cw/c-open-line-above)))

(add-hook 'c-mode-common-hook #'cw/c-style-formatting)
(add-hook 'c-mode-common-hook #'cw/c-fix-enter)
(add-hook 'c-ts-mode-hook     #'cw/c-style-formatting)
(add-hook 'c-ts-mode-hook     #'cw/c-fix-enter)
(add-hook 'c++-ts-mode-hook   #'cw/c-style-formatting)
(add-hook 'c++-ts-mode-hook   #'cw/c-fix-enter)

;; ------------------------------------------------------------
;; clang-format on save — BSD, 8 spaces, spaces only
;; ------------------------------------------------------------
(use-package clang-format
  :ensure t
  :commands (clang-format-buffer))

(defconst cw/clang-format-style
  "{ BasedOnStyle: GNU, IndentWidth: 8, TabWidth: 8, UseTab: ForIndentation, \
BreakBeforeBraces: Allman, \
AllowShortIfStatementsOnASingleLine: Never, \
AllowShortBlocksOnASingleLine: Never, \
AllowShortFunctionsOnASingleLine: None }")

(defun cw/clang-format-on-save ()
  (when (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
    (let ((clang-format-style cw/clang-format-style)
          (inhibit-read-only t))
      (clang-format-buffer))))

(add-hook 'before-save-hook #'cw/clang-format-on-save)

;; -------
;; Haskell
;; -------
(use-package haskell-mode
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :after (lsp-mode haskell-mode)
  :hook ((haskell-mode . lsp-deferred)
         (haskell-literate-mode . lsp-deferred))
  :config
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"))

;; ----------
;; TypeScript
;; ----------
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

;; --------
;; lsp-ui
;; --------
(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 0.2
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil))

(use-package consult-lsp
  :ensure t
  :after (consult lsp-mode))

;; -----------------------
;; Evil keybindings (LSP)
;; -----------------------
(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "SPC l l") #'lsp
    (kbd "SPC l R") #'lsp-rename
    (kbd "SPC l d") #'cw/lsp-find-definition-smart
    (kbd "SPC l D") #'cw/lsp-ripgrep-declarations
    (kbd "SPC l t") #'lsp-find-type-definition
    (kbd "SPC l i") #'lsp-find-implementation
    (kbd "SPC l r") #'lsp-find-references
    (kbd "SPC l a") #'lsp-execute-code-action
    (kbd "SPC l e") #'consult-lsp-diagnostics
    (kbd "SPC l s") #'lsp-ui-doc-glance))

;; -----------------
;; AngelScript mode
;; -----------------
(define-derived-mode angelscript-mode c++-mode "AngelScript"
  "Major mode for AngelScript.")

(add-to-list 'auto-mode-alist '("\\.as\\'" . angelscript-mode))

(with-eval-after-load 'cc-mode
  (c-add-style
   "angelscript-csharp"
   '("java"
     (c-basic-offset . 4)
     (indent-tabs-mode . nil)
     (c-hanging-braces-alist . ((class-open after)
                                (class-close before)
                                (defun-open after)
                                (defun-close before)
                                (block-open after)
                                (block-close before)
                                (substatement-open after)
                                (statement-case-open after)))
     (c-offsets-alist . ((substatement-open . 0)
                         (case-label . +)
                         (statement-case-intro . +)
                         (arglist-intro . +)
                         (arglist-cont-nonempty . +)
                         (arglist-close . 0)
                         (brace-list-intro . +)
                         (innamespace . 0)))))

  (add-hook 'angelscript-mode-hook
            (lambda ()
              (c-set-style "angelscript-csharp")
              (setq-local tab-width 4)
              (setq-local indent-tabs-mode nil))))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(angelscript-mode . "angelscript"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("angel-lsp"))
    :activation-fn (lsp-activate-on "angelscript")
    :server-id 'angel-lsp
    :major-modes '(angelscript-mode))))

(add-hook 'angelscript-mode-hook #'lsp-deferred)

;; ============================================================
;; NASM + asm-lsp
;; ============================================================
(use-package nasm-mode
  :ensure t
  :mode (("\\.nasm\\'" . nasm-mode)
         ("\\.nas\\'"  . nasm-mode)
         ("\\.asm\\'"  . nasm-mode)
         ("\\.inc\\'"  . nasm-mode)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(nasm-mode . "asm"))
  (add-to-list 'lsp-language-id-configuration '(asm-mode  . "asm"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () (list "asm-lsp")))
    :activation-fn (lsp-activate-on "asm")
    :server-id 'asm-lsp
    :major-modes '(nasm-mode asm-mode))))

(add-hook 'nasm-mode-hook #'lsp-deferred)

(provide 'cw-lsp-config)
;;; cw-lsp-config.el ends here
