;;(use-package anti-zenburn-theme
;;  :config
;;  (mapc #'disable-theme custom-enabled-themes)
;;  (load-theme 'anti-zenburn t))

;;(use-package mindre-theme
;;    :ensure t
;;    :straight (:host github :repo "erikbackman/mindre-theme")
;;    :custom
;;    (mindre-use-more-bold nil)
;;    (mindre-use-faded-lisp-parens t)
;;    :config
;;    (load-theme 'mindre t))

(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme))

(require 'color)

(defvar cw/mode-line-orig nil
  "Plist of original modeline face attributes captured after theme load.")

(defun cw/modeline--capture-orig ()
  (setq cw/mode-line-orig
        (list
         :bg (face-attribute 'mode-line :background nil t)
         :fg (face-attribute 'mode-line :foreground nil t)
         :box (face-attribute 'mode-line :box nil t)
         :bg-inactive (face-attribute 'mode-line-inactive :background nil t)
         :fg-inactive (face-attribute 'mode-line-inactive :foreground nil t)
         :box-inactive (face-attribute 'mode-line-inactive :box nil t)
         :doom-bar (when (facep 'doom-modeline-bar)
                     (face-attribute 'doom-modeline-bar :background nil t)))))

(defun cw/modeline--ensure-orig ()
  (unless (and (listp cw/mode-line-orig)
               (plist-member cw/mode-line-orig :bg))
    (cw/modeline--capture-orig)))


(setq cw/inactive-darken 18)

(defun cw/bevel-box (bg &optional inactive)
  ;; Bevel works in GUI frames; in terminal it won't look beveled.
  (when (display-graphic-p)
    (list :line-width (if inactive '(2 . 2) '(2 . 2))
          ;; a slightly lighter edge color makes the "bevel" visible
          :color (color-lighten-name bg (if inactive 8 14))
          :style 'released-button)))

(defun cw/modeline-apply (bg)
  (cw/modeline--ensure-orig)
  (let* ((fg (plist-get cw/mode-line-orig :fg))
         (inactive-bg (color-darken-name bg cw/inactive-darken))
         (inactive-fg (plist-get cw/mode-line-orig :fg-inactive)))
    (set-face-attribute 'mode-line nil
                        :background bg
                        :foreground fg
                        :box (cw/bevel-box bg nil))
    (set-face-attribute 'mode-line-inactive nil
                        :background inactive-bg
                        :foreground inactive-fg
                        :box (cw/bevel-box inactive-bg t))
    (when (facep 'doom-modeline-bar)
      (set-face-attribute 'doom-modeline-bar nil :background bg))))

(defun cw/modeline-reset ()
  (cw/modeline--ensure-orig)
  (let* ((bg (plist-get cw/mode-line-orig :bg))
         (fg (plist-get cw/mode-line-orig :fg))
         (inactive-bg (color-darken-name bg cw/inactive-darken))
         (inactive-fg (plist-get cw/mode-line-orig :fg-inactive)))
    (set-face-attribute 'mode-line nil
                        :background bg
                        :foreground fg
                        :box (cw/bevel-box bg nil))
    (set-face-attribute 'mode-line-inactive nil
                        :background inactive-bg
                        :foreground inactive-fg
                        :box (cw/bevel-box inactive-bg t))
    (when (facep 'doom-modeline-bar)
      (set-face-attribute 'doom-modeline-bar nil
                          :background (plist-get cw/mode-line-orig :doom-bar)))))

(defvar cw/resize-modeline-segment
  '(:eval
    (when (and (boundp 'cw/resize-mode) cw/resize-mode)
      (propertize " RESIZE MODE " 'face '(:foreground "red" :weight bold)))))

(unless (member cw/resize-modeline-segment global-mode-string)
  (setq global-mode-string (append global-mode-string (list cw/resize-modeline-segment))))

(defun cw/dimmer-exclude-treemacs (buf)
  (with-current-buffer buf
    (derived-mode-p 'treemacs-mode)))

(defun cw/dimmer-exclude-minibuffer (buf)
  (minibufferp buf))

(use-package dimmer
  :config
  (setq dimmer-fraction 0.0)
  (setq dimmer-buffer-exclusion-predicates
        (list #'cw/dimmer-exclude-treemacs
              #'cw/dimmer-exclude-minibuffer))
  (dimmer-mode 1))


(provide 'theme)
