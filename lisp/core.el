(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  ;; Don't let C-z toggle to emacs state; avoid accidental mode exits.
  ;; Use an unreachable key instead of nil to keep evil's key setup happy.
  (setq evil-toggle-key "<f13>")
  (setq evil-respect-visual-line-mode t)
  :config
  (evil-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))








;;; yuck indentation ----------------------------------------------------------

(defun cw/yuck-indent-line ()
  "Indent current line by 4 spaces per paren depth (no alignment)."
  (interactive)
  (let* ((ppss (syntax-ppss (line-beginning-position)))
         (in-string (nth 3 ppss))
         (in-comment (nth 4 ppss)))
    (unless (or in-string in-comment)
      (let* ((depth (car ppss))
             (bol (line-beginning-position))
             (offset (- (current-column) (current-indentation)))
             (indent 0))
        (save-excursion
          (goto-char bol)
          ;; If line starts with a closing delimiter, dedent one level.
          (when (looking-at-p "\\s-*[])}]")
            (setq depth (max 0 (1- depth))))
          (setq indent (* 4 depth))
          (indent-line-to indent))
        ;; Keep point in a sensible place if it was in the indent.
        (when (> offset 0)
          (move-to-column (+ indent offset) t))))))

(defun cw/yuck-format-buffer ()
  "Force yuck buffer to spaces + 4-space indent."
  (when (derived-mode-p 'yuck-mode)
    (setq-local indent-tabs-mode nil)
    (setq-local tab-width 4)
    (setq-local indent-line-function #'cw/yuck-indent-line)

    ;; Clean + reindent whole buffer.
    (untabify (point-min) (point-max))
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))))

(defun cw/yuck-setup ()
  "Setup strict 4-space indentation for yuck."
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local indent-line-function #'cw/yuck-indent-line)

  ;; Auto-fix on save.
  (add-hook 'before-save-hook #'cw/yuck-format-buffer nil t))

(use-package yuck-mode
  :mode "\\.yuck\\'"
  :hook (yuck-mode . cw/yuck-setup))




















(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list (remove 'org evil-collection-mode-list))
  (evil-collection-init))

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.3)
  (setq which-key-max-description-length 40))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package nerd-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
    (nerd-icons-install-fonts t)))

(use-package general
  :after evil
  :demand t
  :config
  (general-evil-setup t))

(defun cw/evil--revive ()
  "Re-enable evil-mode when it gets turned off."
  (when (and (fboundp 'evil-mode)
             (not (bound-and-true-p evil-mode)))
    (evil-mode 1)))

(with-eval-after-load 'evil
  ;; Keep evil-mode alive on focus or new frames without logging.
  (add-hook 'focus-in-hook (lambda () (cw/evil--revive)))
  (add-hook 'after-make-frame-functions (lambda (_frame) (cw/evil--revive)))
  (cw/evil--revive))

(setq scroll-margin 5)
(setq scroll-conservatively 101)
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq frame-resize-pixelwise t)
(global-hl-line-mode t)
(add-hook 'dired-mode-hook 'auto-revert-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 1)
(electric-indent-mode -1)


(when (display-graphic-p)
  (let* ((cw/fonts '( "FantasqueSansM Nerd Font" "DejaVu Sans Mono" "Monospace"))
         (cw/font (seq-find (lambda (f) (find-font (font-spec :name f))) cw/fonts)))
    (when cw/font
      (set-face-attribute 'default nil :font (format "%s-11" cw/font))
      (set-face-attribute 'fixed-pitch nil :font (format "%s-11" cw/font))
      (add-to-list 'default-frame-alist `(font . ,(format "%s-11" cw/font))))))



(use-package doom-modeline
 :ensure t
 :init
	(setq doom-modeline-workspace-name nil)
	(doom-modeline-mode 1))

;;GLOBALS
(require 'project)

(defvar cw/global-cwd nil)
(defvar cw/global-cwd-project nil)

(defun cw/update-global-cwd (&rest _)
  (let* ((proj (project-current nil))
         (root (when proj (project-root proj))))
    (when (and root (not (equal root cw/global-cwd)))
      (setq cw/global-cwd (cw/dir-normalize root))
      (setq cw/global-cwd-project proj)
      (when (and (fboundp 'treemacs-get-local-window)
                 (treemacs-get-local-window)
                 (fboundp 'treemacs-add-and-display-current-project-exclusively))
        (let ((default-directory cw/global-cwd))
          (treemacs-add-and-display-current-project-exclusively))))))
(add-hook
 'after-init-hook
 (lambda ()
   (setq cw/global-cwd
         (cw/dir-normalize (or (getenv "PWD") default-directory)))
   (setq cw/global-cwd-project (project-current nil))
   (add-hook 'find-file-hook #'cw/update-global-cwd)))

(provide 'core)
