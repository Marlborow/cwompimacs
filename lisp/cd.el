
(use-package consult
  :ensure t)

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))





(defun cw/consult-ripgrep-global ()
  (interactive)
  (let ((dir (or cw/global-cwd default-directory)))
    (consult-ripgrep dir)))



(defun cw/change-global-cwd-source-config ()
  (interactive)
  (let* ((home (getenv "HOME"))
         (source-root (expand-file-name "Source" home))
         (config-root (expand-file-name ".config" home))
         (default-directory home)
         (cmd (format "fd --type d --hidden --absolute-path --exclude .git --exclude unity3d --exclude Unity --exclude db_ui_queries . %s %s"
                      source-root config-root))
         (dirs (split-string (shell-command-to-string cmd) "\n" t))
         (choice (completing-read "Change global cwd: " dirs nil t)))
    (when choice
      (let* ((dir choice)
             (proj (let ((default-directory dir))
                     (project-current nil)))
             (root (if proj (project-root proj) dir)))
        (setq cw/global-cwd (file-name-as-directory root))
        (setq cw/global-cwd-project proj)
        (when (and (fboundp 'treemacs-get-local-window)
                   (treemacs-get-local-window)
                   (fboundp 'treemacs-add-and-display-current-project-exclusively))
          (let ((default-directory cw/global-cwd))
            (treemacs-add-and-display-current-project-exclusively)))
        (message "Changed global cwd to: %s" cw/global-cwd)))))

(with-eval-after-load 'general
  (when (fboundp 'cw/leader)
    (cw/leader
     "g r" #'cw/consult-ripgrep-global
     "c d" #'cw/change-global-cwd-source-config)))


(provide 'cd)
