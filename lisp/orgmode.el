(unless (featurep 'orgmode)
  (require 'org)

  (setq org-startup-folded 'content)
  (setq org-startup-with-inline-images t)
  (setq org-startup-with-latex-preview nil)
  (setq org-highlight-latex-and-related '(latex script entities))
  (setq org-return-follows-link t)
  (setq org-image-actual-width nil)
  (setq org-hide-emphasis-markers t)
  (setq org-export-with-sub-superscripts nil)
  (setq org-pretty-entities nil)

  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.8))
  (setq org-preview-latex-default-process 'dvisvgm)


  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
  (add-hook 'org-mode-hook (lambda ()
                             (setq-local line-spacing 0.12)
                             (variable-pitch-mode 1)
                             (setq-local org-hide-leading-stars t)
                             (orgmode-apply-heading-faces)))

  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook #'org-indent-mode)



(defun orgmode--set-link-display (descriptive)
  (when (derived-mode-p 'org-mode)
    (setq-local org-link-descriptive descriptive)
    (when (fboundp 'font-lock-flush) (font-lock-flush))
    (when (fboundp 'font-lock-ensure) (font-lock-ensure))))

(defun orgmode--evil-org-link-display-setup ()
  (when (and (derived-mode-p 'org-mode) (featurep 'evil))
    (add-hook 'evil-normal-state-entry-hook (lambda () (orgmode--set-link-display t)) nil t)
    (add-hook 'evil-motion-state-entry-hook (lambda () (orgmode--set-link-display t)) nil t)
    (add-hook 'evil-insert-state-entry-hook (lambda () (orgmode--set-link-display nil)) nil t)
    (add-hook 'evil-visual-state-entry-hook (lambda () (orgmode--set-link-display nil)) nil t)
    (add-hook 'evil-replace-state-entry-hook (lambda () (orgmode--set-link-display nil)) nil t)
    (orgmode--set-link-display (memq evil-state '(normal motion)))))

(add-hook 'org-mode-hook #'orgmode--evil-org-link-display-setup)




  (defvar-local orgmode--latex-preview-on nil)

  (defun orgmode--latex-preview-enable ()
    (when (derived-mode-p 'org-mode)
      (unless orgmode--latex-preview-on
        (org-latex-preview '(16))
        (setq orgmode--latex-preview-on t))))

  (defun orgmode--latex-preview-disable ()
    (when (derived-mode-p 'org-mode)
      (when orgmode--latex-preview-on
        (org-latex-preview '(64))
        (setq orgmode--latex-preview-on nil))))

  (defun orgmode--latex-preview-sync ()
    (when (and (derived-mode-p 'org-mode) (featurep 'evil))
      (if (memq evil-state '(normal motion))
          (orgmode--latex-preview-enable)
        (orgmode--latex-preview-disable))))

  (defun orgmode--evil-org-latex-setup ()
    (when (featurep 'evil)
      (add-hook 'evil-normal-state-entry-hook #'orgmode--latex-preview-enable nil t)
      (add-hook 'evil-motion-state-entry-hook #'orgmode--latex-preview-enable nil t)
      (add-hook 'evil-insert-state-entry-hook #'orgmode--latex-preview-disable nil t)
      (add-hook 'evil-visual-state-entry-hook #'orgmode--latex-preview-disable nil t)
      (add-hook 'evil-replace-state-entry-hook #'orgmode--latex-preview-disable nil t)
      (add-hook 'evil-emacs-state-entry-hook #'orgmode--latex-preview-disable nil t)
      (add-hook 'post-command-hook #'orgmode--latex-preview-sync nil t)
      (orgmode--latex-preview-sync)))

  (add-hook 'org-mode-hook #'orgmode--evil-org-latex-setup)

  (when (require 'org-appear nil t)
    (setq org-appear-autolinks t)
    (setq org-appear-autoemphasis t)
    (setq org-appear-autosubmarkers t)
    (setq org-appear-autokeywords t)
    (setq org-appear-inside-latex nil)
    (add-hook 'org-mode-hook #'org-appear-mode))

  (when (require 'org-modern nil t)
    (setq org-modern-star '("◉" "○" "●" "◈" "◇"))
    (setq org-modern-hide-stars t)
    (add-hook 'org-mode-hook #'org-modern-mode)
    (add-hook 'org-modern-mode-hook #'orgmode-apply-heading-faces))

  (when (require 'org-bullets nil t)
    (add-hook 'org-mode-hook #'org-bullets-mode))

  (when (and (featurep 'evil) (require 'evil-org nil t))
    (add-hook 'org-mode-hook #'evil-org-mode)
    (when (fboundp 'evil-org-set-key-theme)
      (evil-org-set-key-theme '(navigation insert textobjects additional)))
    (when (require 'evil-org-agenda nil t)
      (evil-org-agenda-set-keys)))

  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))

  (provide 'orgmode))
