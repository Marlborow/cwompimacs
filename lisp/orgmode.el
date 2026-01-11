(unless (featurep 'orgmode)
  (require 'org)

(with-eval-after-load 'org
  (setq org-format-latex-header
        (concat org-format-latex-header
                "\n% orgmode-shorthand macros\n"
                "\\usepackage{amsfonts,amssymb}\n"

                "% sets: \\N \\Z \\Q \\R etc.\n"
                "\\newcommand{\\N}{\\mathbb{N}}\n"
                "\\newcommand{\\I}{\\mathbb{Z}}\n"
                "\\newcommand{\\RA}{\\mathbb{Q}}\n"
                "\\newcommand{\\RE}{\\mathbb{R}}\n"

                "% type tags: \\tN -> ^{\\N}, etc.\n"
                "\\newcommand{\\tN}{^{\\N}}\n"
                "\\newcommand{\\tI}{^{\\Z}}\n"
                "\\newcommand{\\tRA}{^{\\Q}}\n"
                "\\newcommand{\\tRE}{^{\\R}}\n"
                "\\newcommand{\\tIR}{^{\\R\\setminus\\Q}}\n"

                "% arrows\n"
                "\\newcommand{\\la}{\\ensuremath{\\leftarrow}}\n"
                "\\newcommand{\\ra}{\\ensuremath{\\rightarrow}}\n")))

    ;; ----------------------------
    ;; Org defaults
    ;; ----------------------------
    (setq org-startup-folded 'content)
    (setq org-startup-with-inline-images t)
    (setq org-startup-with-latex-preview nil)

    (setq org-return-follows-link t)
    (setq org-image-actual-width nil)

    (setq org-hide-emphasis-markers t)
    (setq org-export-with-sub-superscripts nil)
    (setq org-pretty-entities nil)

    (setq org-highlight-latex-and-related '(latex script entities))

    ;; LaTeX preview rendering
    (setq org-preview-latex-default-process 'dvisvgm)
    (setq org-format-latex-options
          (plist-put org-format-latex-options :scale 1.8))
    (setq org-format-latex-options
          (plist-put org-format-latex-options :justify 'center))

    ;; ----------------------------
    ;; NEW: ~20px padding for Org buffers (window margins)
    ;; ----------------------------
    (defun orgmode--padding-cols (&optional px)
      "Return approx columns for PX pixels of padding."
      (let* ((px (or px 20))
             (cw (max 1 (frame-char-width))))
        (max 1 (ceiling (/ (float px) cw)))))

    (defun orgmode--apply-padding (&optional win)
      "Apply left/right margins to WIN if it's showing an Org buffer."
      (let ((win (or win (selected-window))))
        (when (window-live-p win)
          (with-selected-window win
            (when (derived-mode-p 'org-mode)
              (let ((cols (orgmode--padding-cols 20)))
                (set-window-margins win cols cols)))))))

    (defun orgmode--apply-padding-all-windows ()
      "Apply Org padding to all visible Org windows."
      (dolist (w (window-list))
        (orgmode--apply-padding w)))

    (add-hook 'org-mode-hook #'orgmode--apply-padding)
    (add-hook 'window-configuration-change-hook #'orgmode--apply-padding-all-windows)

    ;; ----------------------------
    ;; Org mode hooks
    ;; ----------------------------
    (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
    (add-hook 'org-mode-hook
              (lambda ()
                (setq-local line-spacing 0.12)
                (variable-pitch-mode 1)
                (setq-local org-hide-leading-stars t)
                (orgmode-apply-heading-faces)))

    (add-hook 'org-mode-hook #'visual-line-mode)
    (add-hook 'org-mode-hook #'org-indent-mode)

    ;; ----------------------------
    ;; Evil link display: descriptive in normal/motion, raw in insert/etc
    ;; ----------------------------
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

    ;; ----------------------------
    ;; Evil LaTeX preview toggle (normal/motion = on, insert/etc = off)
    ;; ----------------------------
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

    ;; ----------------------------
    ;; Optional addons
    ;; ----------------------------
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

    ;; ----------------------------
    ;; Babel
    ;; ----------------------------
    (setq org-confirm-babel-evaluate nil)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (shell . t)
       (python . t))))

  (provide 'orgmode)
