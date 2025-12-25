(defvar cw/resize-step 5)
(require 'windmove)
(setq windmove-wrap-around nil)

(defun cw/xref-below ()
  (interactive)
  (split-window-below)
  (other-window 1))

(defun cw/xref-right ()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun cw/dired-here ()
  (interactive)
  (let ((dir (file-name-as-directory
              (or cw/global-cwd
                  (and buffer-file-name (file-name-directory buffer-file-name))
                  default-directory))))
    (dired dir)))

(defun cw/kill-buffer-and-window ()
  (interactive)
  (let* ((buf (current-buffer))
         (wins (get-buffer-window-list buf nil t)))
    (if (> (length wins) 1)
        (delete-window)
      (kill-buffer buf)
      (when (not (one-window-p t))
        (delete-window)))))

(defvar cw/resize-mode nil)

(defun cw/toggle-resize-mode ()
  (interactive)
  (setq cw/resize-mode (not cw/resize-mode))
  (message "resize-mode: %s" (if cw/resize-mode "on" "off")))

(defun cw/ctrl-left ()
  (interactive)
  (if cw/resize-mode
      (shrink-window-horizontally cw/resize-step)
    (let ((w (windmove-find-other-window 'left)))
      (cond
       (w (select-window w))
       ((and (fboundp 'treemacs-get-local-window)
             (treemacs-get-local-window))
        (select-window (treemacs-get-local-window)))))))

(defun cw/ctrl-right ()
  (interactive)
  (if cw/resize-mode
      (enlarge-window-horizontally cw/resize-step)
    (let ((w (windmove-find-other-window 'right)))
      (when w (select-window w)))))


(defun cw/ctrl-down ()
  (interactive)
  (if cw/resize-mode
      (shrink-window cw/resize-step)
    (evil-window-down 1)))

(defun cw/ctrl-up ()
  (interactive)
  (if cw/resize-mode
      (enlarge-window cw/resize-step)
    (evil-window-up 1)))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-kill-buffer-on-exit t))

(use-package eat
  :commands eat
  :config
  (setopt eat-kill-buffer-on-exit t))

(defun cw/terminal-below ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (let ((default-directory (or cw/global-cwd default-directory)))
    (cond
     ((fboundp 'vterm) (vterm))
     ((fboundp 'eat) (eat))
     (t (ansi-term (or (getenv "SHELL") "/bin/bash"))))))

(defun cw/shift-right-dwim (count)
  (interactive "p")
  (if (evil-visual-state-p)
      (let ((type (evil-visual-type))
            (mbeg (copy-marker (region-beginning)))
            (mend (copy-marker (region-end) t)))
        (evil-shift-right (region-beginning) (region-end) count)
        (evil-visual-make-selection (marker-position mbeg) (marker-position mend) type)
        (setq deactivate-mark nil)
        (set-marker mbeg nil)
        (set-marker mend nil))
    (evil-shift-right-line count)))

(defun cw/tab-complete-or-shift (count)
    "Accept inline completion preview when active; otherwise shift right COUNT."
    (interactive "p")
    (if (and (fboundp 'completion-preview-insert)
	    (bound-and-true-p completion-preview-active-mode))
	(completion-preview-insert)
    (cw/shift-right-dwim count)))

(defun cw/shift-left-dwim (count)
(interactive "p")
(if (evil-visual-state-p)
    (let ((type (evil-visual-type))
	    (mbeg (copy-marker (region-beginning)))
	    (mend (copy-marker (region-end) t)))
	(evil-shift-left (region-beginning) (region-end) count)
	(evil-visual-make-selection (marker-position mbeg) (marker-position mend) type)
	(setq deactivate-mark nil)
	(set-marker mbeg nil)
	(set-marker mend nil))
    (evil-shift-left-line count)))


(defun cw/filetree-open ()
  (interactive)
  (let ((w (and (fboundp 'treemacs-get-local-window)
                (treemacs-get-local-window))))
    (if (window-live-p w)
        (select-window w)
      (treemacs)
      (setq w (and (fboundp 'treemacs-get-local-window)
                   (treemacs-get-local-window)))
      (when (window-live-p w)
        (select-window w)))))

(defun cw/filetree-escape-right ()
  (interactive)
  (let ((w (window-in-direction 'right)))
    (cond
     (w (select-window w))
     ((fboundp 'windmove-find-other-window)
      (setq w (windmove-find-other-window 'right))
      (when w (select-window w)))
     ((not (one-window-p t))
      (other-window 1)))))


(with-eval-after-load 'evil
  (setq evil-esc-delay 0)
  (evil-ex-define-cmd "so" #'eval-buffer)
  (evil-ex-define-cmd "source" #'eval-buffer)
  (with-eval-after-load 'org
    (evil-define-key '(normal visual) org-mode-map
      (kbd "<tab>") #'cw/shift-right-dwim
      (kbd "<backtab>") #'cw/shift-left-dwim
      (kbd "C-<tab>") #'cw/shift-left-dwim)))

(with-eval-after-load 'general
  (general-evil-setup t)

  (general-create-definer cw/leader
    :states '(normal visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-define-key
   :states '(normal visual)
   "<tab>" #'cw/shift-right-dwim
   "<backtab>" #'cw/shift-left-dwim
   "C-<tab>" #'cw/shift-left-dwim
   "C-," #'cw/xref-below
   "C-." #'cw/xref-right
   "C-<return>" #'cw/terminal-below
   "C-RET" #'cw/terminal-below
   "C-a" #'mark-whole-buffer
   "C-q" #'cw/kill-buffer-and-window
   "C-r" #'cw/toggle-resize-mode
   "C-h" #'cw/ctrl-left
   "C-j" #'cw/ctrl-down
   "C-k" #'cw/ctrl-up
   "C-l" #'cw/ctrl-right
   "C-<left>" #'cw/ctrl-left
   "C-<down>" #'cw/ctrl-down
   "C-<up>" #'cw/ctrl-up
   "C-<right>" #'cw/ctrl-right
   "S-<down>" #'evil-goto-line
   "S-<up>" #'evil-goto-first-line)
  
  (general-define-key
   :states '(insert)
   "<tab>" #'cw/tab-complete-or-shift
   "<backtab>" #'cw/shift-left-dwim
   "C-<tab>" #'cw/shift-left-dwim)
  
  (general-define-key
   :states '(normal)
   "A" #'back-to-indentation
   "D" #'evil-end-of-line)

  (cw/leader
   "d" '(:ignore t)
   "d f" #'cw/dired-here))

(with-eval-after-load 'dired
  (require 'dired-aux)
  (with-eval-after-load 'evil
    (evil-define-key* '(normal motion visual) dired-mode-map
      (kbd "f") #'dired-create-empty-file
      (kbd "d") #'dired-create-directory
      (kbd "D") #'dired-do-delete
      (kbd "RET") #'dired-find-file
      (kbd "<return>") #'dired-find-file
      (kbd "<escape>") #'quit-window)
    (evil-normalize-keymaps)))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook
            (lambda ()
              (let ((p (get-buffer-process (current-buffer))))
                (when p (set-process-query-on-exit-flag p nil)))))
  (define-key vterm-mode-map (kbd "C-q") #'cw/kill-buffer-and-window)
  (when (boundp 'vterm-copy-mode-map)
    (define-key vterm-copy-mode-map (kbd "C-q") #'cw/kill-buffer-and-window)))

(with-eval-after-load 'eat
  (add-hook 'eat-mode-hook
            (lambda ()
              (let ((p (get-buffer-process (current-buffer))))
                (when p (set-process-query-on-exit-flag p nil)))))
  (define-key eat-mode-map (kbd "C-q") #'cw/kill-buffer-and-window))

(with-eval-after-load 'term
  (add-hook 'term-mode-hook
            (lambda ()
              (let ((p (get-buffer-process (current-buffer))))
                (when p (set-process-query-on-exit-flag p nil)))))
  (define-key term-mode-map (kbd "C-q") #'cw/kill-buffer-and-window))


(with-eval-after-load 'general
  (if (fboundp 'cw/leader)
      (cw/leader
       "<tab>" #'cw/filetree-open)
    (general-define-key
     :states '(normal visual emacs)
     :keymaps 'override
     :prefix "SPC"
     "<tab>" #'cw/filetree-open)))

(defun cw/tab-apply-leader-keys ()
  (when (fboundp 'cw/leader)
    (cw/leader
     "1" #'cw/tab-select-1
     "2" #'cw/tab-select-2
     "3" #'cw/tab-select-3
     "4" #'cw/tab-select-4
     "5" #'cw/tab-select-5
     "6" #'cw/tab-select-6
     "7" #'cw/tab-select-7
     "8" #'cw/tab-select-8
     "9" #'cw/tab-select-9
     "0" #'cw/tab-select-0)))

(with-eval-after-load 'general
  (cw/tab-apply-leader-keys))

(with-eval-after-load 'keys
  (cw/tab-apply-leader-keys))


(provide 'keys)
