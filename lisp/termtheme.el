(defvar-local cw/terminal-face-cookie nil)

(defun cw/terminal-setup ()
  (display-line-numbers-mode 0)
  (setq-local display-line-numbers nil)

  (hl-line-mode -1)
  (setq-local global-hl-line-mode nil))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook #'cw/terminal-setup))

(with-eval-after-load 'eat
  (add-hook 'eat-mode-hook #'cw/terminal-setup))

(with-eval-after-load 'term
  (add-hook 'term-mode-hook #'cw/terminal-setup))

(provide 'termtheme)
