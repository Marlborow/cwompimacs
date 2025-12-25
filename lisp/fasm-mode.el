(require 'asm-mode)

;;;###autoload
(define-derived-mode fasm-mode asm-mode "FASM"
  "Minimal major mode for FASM, derived from `asm-mode'."
  (setq-local comment-start ";")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fasm\\'" . fasm-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fas\\'" . fasm-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asm\\'" . fasm-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.inc\\'" . fasm-mode))

(provide 'fasm-mode)
