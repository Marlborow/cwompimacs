(use-package treemacs
  :defer t
  :config
  (setq treemacs-width 30)
  (setq treemacs-is-never-other-window t)
  (setq treemacs-space-between-root-nodes nil))

(use-package treemacs-evil
  :after (treemacs evil))

;;;(use-package treemacs-nerd-icons
;;;  :after treemacs
;;;  :config
;;;  (treemacs-load-theme "nerd-icons"))

(require 'seq)
(require 'cl-lib)

(defun cw/treemacs--call-add (fn name root)
  (when (fboundp fn)
    (let* ((arity (ignore-errors (function-arity (symbol-function fn))))
           (min (and arity (car arity)))
           (max (and arity (cdr arity))))
      (cond
       ((and min (<= min 2) (or (null max) (>= max 2)))
        (ignore-errors (funcall fn name root))
        t)
       ((and min (= min 0))
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) name))
                  ((symbol-function 'read-from-minibuffer) (lambda (&rest _) name))
                  ((symbol-function 'completing-read) (lambda (&rest _) name))
                  ((symbol-function 'read-directory-name) (lambda (&rest _) root)))
          (ignore-errors (funcall fn))
          t))
       (t nil)))))

(defun cw/treemacs-add-root (root)
  (let ((name (file-name-nondirectory (directory-file-name root))))
    (or (cw/treemacs--call-add 'treemacs-do-add-project-to-workspace name root)
        (cw/treemacs--call-add 'treemacs--add-project-to-workspace name root)
        (cw/treemacs--call-add 'treemacs-add-project-to-workspace name root))))

(defun cw/treemacs-ensure-root (root)
  "Make sure Treemacs is anchored to ROOT, not the current buffer dir."
  (require 'treemacs)
  (when (and (fboundp 'treemacs-current-workspace) root)
    (let* ((workspace (treemacs-current-workspace))
           (projects (when workspace (treemacs-workspace->projects workspace)))
           (root-present
            (seq-some (lambda (p)
                        (string= (file-name-as-directory (treemacs-project->path p))
                                 (file-name-as-directory root)))
                      projects)))
      (unless root-present
        (cw/treemacs-add-root root))
      ;; Always re-focus Treemacs on ROOT, even if other projects are present.
      (when (fboundp 'treemacs-add-and-display-current-project-exclusively)
        (let ((default-directory root))
          (ignore-errors (treemacs-add-and-display-current-project-exclusively)))))))

(defun cw/filetree-open ()
  (interactive)
  (let* ((root (file-name-as-directory (or cw/global-cwd default-directory)))
         (w (and (fboundp 'treemacs-get-local-window)
                 (treemacs-get-local-window))))
    (cw/treemacs-ensure-root root)
    (if (window-live-p w)
        (select-window w)
      (let ((default-directory root))
        (treemacs))
      (cw/treemacs-ensure-root root)
      (setq w (and (fboundp 'treemacs-get-local-window)
                   (treemacs-get-local-window)))
      (when (window-live-p w)
        (select-window w)))))


(defun cw/filetree-escape-right ()
  (interactive)
  (let ((w (window-in-direction 'right)))
    (cond
     (w (select-window w))
     ((not (one-window-p t))
      (other-window 1)))))

(defun cw/treemacs-ret ()
  (interactive)
  (let* ((btn (treemacs-node-at-point))
         (path (and btn (treemacs-button-get btn :path))))
    (if (and path (file-directory-p path))
        (treemacs-toggle-node)
      (treemacs-visit-node-in-most-recently-used-window))))

(with-eval-after-load 'general
  (if (fboundp 'cw/leader)
      (cw/leader
       "<tab>" #'cw/filetree-open)
    (general-define-key
     :states '(normal visual emacs)
     :keymaps 'override
     :prefix "SPC"
     "<tab>" #'cw/filetree-open)))

(with-eval-after-load 'treemacs

  (define-key treemacs-mode-map (kbd "RET") #'cw/treemacs-ret)
  (define-key treemacs-mode-map (kbd "<return>") #'cw/treemacs-ret)
  (define-key treemacs-mode-map (kbd "C-<right>") #'cw/filetree-escape-right)
  (define-key treemacs-mode-map (kbd "C-l") #'cw/filetree-escape-right)
  (define-key treemacs-mode-map (kbd "C-<left>") #'cw/ctrl-left)
  (define-key treemacs-mode-map (kbd "C-h") #'cw/ctrl-left)

  (treemacs-resize-icons 16)
  (treemacs-follow-mode 1)
  (add-hook 'treemacs-mode-hook
    (lambda ()
      (display-line-numbers-mode 0)
      (setq-local display-line-numbers nil)))

  (with-eval-after-load 'evil
    (evil-define-key '(normal visual emacs motion) treemacs-mode-map
      (kbd "q") #'treemacs-quit
      (kbd "C-q") #'treemacs-quit
      (kbd "Q") #'treemacs-kill-buffer
      (kbd "RET") #'cw/treemacs-ret
      (kbd "<return>") #'cw/treemacs-ret
      (kbd "C-<right>") #'cw/filetree-escape-right
      (kbd "C-l") #'cw/filetree-escape-right
      (kbd "C-<left>") #'cw/ctrl-left
      (kbd "C-h") #'cw/ctrl-left)))

(provide 'filetree)
