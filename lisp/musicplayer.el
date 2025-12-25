;;; musicplayer.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(defvar vterm-shell) ;; ensure `vterm-shell` is treated as a special (dynamic) var even before vterm loads

(defvar cw/musicplayer--cleaning nil)
(defvar-local cw/musicplayer-tab-name nil)

(defun cw/musicplayer--strip-tab-prefix (name)
  (replace-regexp-in-string "\\`[0-9]+[ ]*:[ ]*" "" (or name "")))

(defun cw/musicplayer--tab-base-name (buffer-name)
  (let* ((trimmed (replace-regexp-in-string "\\`\\*+\\|\\*+\\'" "" (or buffer-name ""))))
    (if (string-empty-p trimmed) "cmus" trimmed)))

(defun cw/musicplayer--close-tab-by-base-name (base-name)
  (when (and (fboundp 'tab-bar-tabs) (stringp base-name) (not (string-empty-p base-name)))
    (let* ((tabs (tab-bar-tabs))
           (idx (cl-position-if
                 (lambda (tab)
                   (string=
                    (cw/musicplayer--strip-tab-prefix (alist-get 'name tab))
                    base-name))
                 tabs)))
      (when (and idx (> (length tabs) 1))
        (let ((cur (tab-bar--current-tab-index)))
          (tab-bar-select-tab (1+ idx))
          (tab-bar-close-tab)
          (let ((new-tabs (tab-bar-tabs)))
            (when (and (< cur (length new-tabs)) (> (length new-tabs) 0))
              (tab-bar-select-tab (1+ (min cur (1- (length new-tabs))))))))))))

(defun cw/musicplayer--cleanup (&optional buffer tab-name)
  (unless cw/musicplayer--cleaning
    (let ((cw/musicplayer--cleaning t))
      (let* ((buf (or buffer (current-buffer)))
             (tab (or tab-name
                      (and (buffer-live-p buf)
                           (with-current-buffer buf
                             (when (boundp 'cw/musicplayer-tab-name)
                               cw/musicplayer-tab-name))))))
        (when tab
          (cw/musicplayer--close-tab-by-base-name tab))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(defun cw/musicplayer--cleanup-on-kill ()
  (cw/musicplayer--cleanup (current-buffer)))

(defun cw/musicplayer--process-sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (cw/musicplayer--cleanup
     (process-buffer proc)
     (process-get proc 'cw-musicplayer-tab-name))))

(defun cw/musicplayer--install-exit-hooks (buf tab-base-name)
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq-local cw/musicplayer-tab-name tab-base-name)
      (add-hook 'kill-buffer-hook #'cw/musicplayer--cleanup-on-kill nil t))
    (let ((proc (get-buffer-process buf)))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil)
        (process-put proc 'cw-musicplayer-tab-name tab-base-name)
        (let ((orig-sentinel (process-sentinel proc)))
          (set-process-sentinel
           proc
           (lambda (p e)
             (when orig-sentinel (funcall orig-sentinel p e))
             (cw/musicplayer--process-sentinel p e))))))))

(defun cw/musicplayer--spawn (cmus-bin buffer-name)
  (cond
   ((fboundp 'vterm)
    (let ((vterm-shell cmus-bin))
      (vterm buffer-name)))
   ((fboundp 'ansi-term)
    (ansi-term cmus-bin buffer-name))
   (t (user-error "No terminal backend available for cmus"))))

(defun cw/musicplayer-open ()
  "Launch cmus in a dedicated tab."
  (interactive)
  (let ((cmus-bin (executable-find "cmus")))
    (unless cmus-bin
      (user-error "cmus not found on PATH"))
    (let ((tab-created nil)
          (success nil)
          buffer
          tab-base
          buffer-name)
      (unwind-protect
          (progn
            (tab-bar-new-tab)
            (setq tab-created t)
            (setq buffer-name (generate-new-buffer-name "*cmus*"))
            (setq tab-base (cw/musicplayer--tab-base-name buffer-name))
            (let ((default-directory (or cw/global-cwd default-directory)))
              (setq buffer (cw/musicplayer--spawn cmus-bin buffer-name)))
            (when (buffer-live-p buffer)
              (tab-bar-rename-tab tab-base)
              (with-current-buffer buffer
                (rename-buffer buffer-name t))
              (cw/musicplayer--install-exit-hooks buffer tab-base)
              (switch-to-buffer buffer))
            (setq success t))
        (unless success
          (when (and tab-created (> (length (tab-bar-tabs)) 1))
            (tab-bar-close-tab)))))))

(with-eval-after-load 'evil-ex
  ;; Allow invoking via :cmus in Evil's ex mode; guard against missing evil-ex.
  (when (fboundp 'evil-ex-define-cmd)
    (evil-ex-define-cmd "cmus" #'cw/musicplayer-open)))

(provide 'musicplayer)
