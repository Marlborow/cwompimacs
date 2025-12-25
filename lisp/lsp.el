(require 'use-package)

(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode t)
(setq-default c-basic-offset 4)
(setq-default evil-shift-width 4)

(setq c-ts-mode-indent-offset 4)
(setq c++-ts-mode-indent-offset 4)

(setq-default tab-stop-list
              (number-sequence 4 200 4))

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
         (root (or cw/global-cwd
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

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-snippet nil
        lsp-prefer-capf t
        lsp-inlay-hint-enable nil
        lsp-clients-clangd-args
        '("--fallback-style={BasedOnStyle: BSD, IndentWidth: 4, TabWidth: 4, UseTab: Never}"))
  :hook
  ((c-mode . lsp-deferred)
   (c++-mode . lsp-deferred)
   (c-ts-mode . lsp-deferred)
   (c++-ts-mode . lsp-deferred)
   (python-mode . lsp-deferred)
   (rust-mode . lsp-deferred)
   (typescript-ts-mode . lsp-deferred)
   (tsx-ts-mode . lsp-deferred)
   (js-mode . lsp-deferred)
   (js-ts-mode . lsp-deferred)
   (go-mode . lsp-deferred)
   (lua-mode . lsp-deferred)
   (lsp-mode . cw/lsp-inline-suggestions)
   (lsp-mode . cw/lsp-inlay-hints-maybe))
  :commands (lsp lsp-deferred))

(defun cw/lsp-nasm-deferred ()
  (require 'lsp-mode)
  (require 'lsp-asm)
  (unless (assoc 'nasm-mode lsp-language-id-configuration)
    (push '(nasm-mode . "asm") lsp-language-id-configuration))
  (lsp-deferred))

(defun cw/c-style-formatting ()
  (setq-local tab-width 4)
  (setq-local standard-indent 4)
  (setq-local indent-tabs-mode nil)
  (setq-local c-basic-offset 4)
  (c-set-style "bsd")
  (setq-local c-electric-flag nil)
  (setq-local electric-indent-inhibit nil)
  (setq-local electric-pair-inhibit-predicate #'electric-pair-default-inhibit)
  (setq-local tab-stop-list (number-sequence 4 200 4))
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (setq-local lsp-enable-on-type-formatting nil)
  (setq-local lsp-enable-indentation nil))

(add-hook 'c-mode-common-hook #'cw/c-style-formatting)

(defun cw/c-ts-style-formatting ()
  (setq-local tab-width 4)
  (setq-local standard-indent 4)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-stop-list (number-sequence 4 200 4))
  (setq-local electric-indent-inhibit nil)
  (setq-local electric-pair-inhibit-predicate #'electric-pair-default-inhibit)
  (electric-pair-local-mode 1)
  (electric-indent-local-mode 1)
  (setq-local lsp-enable-on-type-formatting nil)
  (setq-local lsp-enable-indentation nil))

(add-hook 'c-ts-mode-hook #'cw/c-ts-style-formatting)
(add-hook 'c++-ts-mode-hook #'cw/c-ts-style-formatting)

(add-hook 'lsp-mode-hook #'cw/lsp-inline-suggestions)

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred))

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

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global
    (kbd "SPC l l") #'lsp
    (kbd "SPC l r") #'lsp-rename
    (kbd "SPC l d") #'cw/lsp-find-definition-smart
    (kbd "SPC l D") #'cw/lsp-ripgrep-declarations
    (kbd "SPC l t") #'lsp-find-type-definition
    (kbd "SPC l i") #'lsp-find-implementation
    (kbd "SPC l r") #'lsp-find-references
    (kbd "SPC l a") #'lsp-execute-code-action
    (kbd "SPC l e") #'consult-lsp-diagnostics
    (kbd "SPC l s") #'lsp-ui-doc-glance))

;; --- Force clang-format style on save for C/C++ (ignores .clang-format) ------
(use-package clang-format
  :ensure t
  :commands (clang-format-buffer))

(defconst cw/clang-format-style
  "{ BasedOnStyle: LLVM, IndentWidth: 8, TabWidth: 8, UseTab: Never, \
NamespaceIndentation: All, BreakBeforeBraces: Custom, \
BraceWrapping: { AfterNamespace: true, AfterFunction: true, \
AfterControlStatement: true, AfterStruct: true, AfterEnum: true, AfterUnion: true, \
BeforeElse: true, BeforeCatch: true }, \
SpaceBeforeParens: ControlStatements, \
AllowShortIfStatementsOnASingleLine: Never, AllowShortBlocksOnASingleLine: Never }")

(defun cw/clang-format-on-save ()
  "Run clang-format on save for C/C++ buffers, forcing our style."
  (when (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
    (let ((clang-format-style cw/clang-format-style)
          ;; prevents 'Text is read-only' when some modes mark regions read-only
          (inhibit-read-only t))
      (clang-format-buffer))))

(add-hook 'before-save-hook #'cw/clang-format-on-save)

(provide 'lsp)
