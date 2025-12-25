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

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode 1)
(electric-indent-mode -1)


(when (display-graphic-p)
  (let* ((cw/fonts '( "Cascadia Code" "DejaVu Sans Mono" "Monospace"))
         (cw/font (seq-find (lambda (f) (find-font (font-spec :name f))) cw/fonts)))
    (when cw/font
      (set-face-attribute 'default nil :font (format "%s-12" cw/font))
      (set-face-attribute 'fixed-pitch nil :font (format "%s-12" cw/font))
      (add-to-list 'default-frame-alist `(font . ,(format "%s-12" cw/font))))))



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
      (setq cw/global-cwd root)
      (setq cw/global-cwd-project proj)
      (when (and (fboundp 'treemacs-get-local-window)
                 (treemacs-get-local-window)
                 (fboundp 'treemacs-add-and-display-current-project-exclusively))
        (let ((default-directory cw/global-cwd))
          (treemacs-add-and-display-current-project-exclusively))))))

(add-hook
 'after-init-hook
 (lambda ()
   (setq cw/global-cwd (file-name-as-directory (or (getenv "PWD") default-directory)))
   (setq cw/global-cwd-project (project-current nil))
   (add-hook 'find-file-hook #'cw/update-global-cwd)))



(provide 'core)
