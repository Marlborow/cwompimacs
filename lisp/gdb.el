(setq gdb-many-windows t)
(setq gdb-show-main t)

(require 'cl-lib)
(defvar cw/gdb--cleaning nil)

(defun cw/gdb--strip-tab-prefix (name)
  (replace-regexp-in-string "\\`[0-9]+[ ]*:[ ]*" "" (or name "")))

(defun cw/gdb--program-from-cmdline (cmdline)
  (when (and (stringp cmdline) (not (string-empty-p cmdline)))
    (let* ((parts (split-string-and-unquote cmdline))
           (args-pos (cl-position "--args" parts :test #'string=)))
      (cond
       ((and args-pos (nth (1+ args-pos) parts)) (nth (1+ args-pos) parts))
       ((and parts (> (length parts) 0)) (car (last parts)))
       (t nil)))))

(defun cw/gdb--current-program ()
  (or (and (boundp 'gud-target-name) gud-target-name)
      (and (boundp 'gdb-main-file) gdb-main-file)
      (and (buffer-live-p gud-comint-buffer)
           (with-current-buffer gud-comint-buffer
             (or (and (boundp 'gud-target-name) gud-target-name)
                 (and (boundp 'gdb-main-file) gdb-main-file))))
      nil))

(defun cw/gdb--gdb-name (prog)
  (format "*GDB: %s*"
          (file-name-nondirectory
           (or prog "gdb"))))

(defun cw/gdb--kill-gdb-buffers ()
  (let ((bufs (delq nil
                    (list
                     gud-comint-buffer
                     (ignore-errors (gdb-get-buffer 'gdb-locals-values-buffer))
                     (ignore-errors (gdb-get-buffer 'gdb-locals-buffer))
                     (ignore-errors (gdb-get-buffer 'gdb-registers-buffer))
                     (ignore-errors (gdb-get-buffer 'gdb-breakpoints-buffer))
                     (ignore-errors (gdb-get-buffer 'gdb-stack-buffer))
                     (ignore-errors (gdb-get-buffer 'gdb-inferior-io))
                     (ignore-errors (gdb-get-buffer 'gdb-threads-buffer))
                     (ignore-errors (gdb-get-buffer 'gdb-disassembly-buffer))
                     (ignore-errors (gdb-get-buffer 'gdb-memory-buffer))))))
    (dolist (b bufs)
      (when (buffer-live-p b)
        (kill-buffer b)))))

(defun cw/gdb--close-tab-by-base-name (base-name)
  (when (and (fboundp 'tab-bar-tabs) (stringp base-name) (not (string-empty-p base-name)))
    (let* ((tabs (tab-bar-tabs))
           (idx (cl-position-if
                 (lambda (tab)
                   (string=
                    (cw/gdb--strip-tab-prefix (alist-get 'name tab))
                    base-name))
                 tabs)))
      (when (and idx (> (length tabs) 1))
        (let ((cur (tab-bar--current-tab-index)))
          (tab-bar-select-tab (1+ idx))
          (tab-bar-close-tab)
          (let ((new-tabs (tab-bar-tabs)))
            (when (and (< cur (length new-tabs)) (> (length new-tabs) 0))
              (tab-bar-select-tab (1+ (min cur (1- (length new-tabs))))))))))))

(defun cw/gdb--cleanup (&optional gudbuf)
  (unless cw/gdb--cleaning
    (let ((cw/gdb--cleaning t))
      (let* ((b (or gudbuf gud-comint-buffer))
             (nm (and (buffer-live-p b)
                      (with-current-buffer b
                        (or (and (boundp 'cw/gdb-tab-name) cw/gdb-tab-name)
                            (let ((p (get-buffer-process (current-buffer))))
                              (and p (process-get p 'cw-gdb-tab-name))))))))
        (cw/gdb--kill-gdb-buffers)
        (when nm
          (cw/gdb--close-tab-by-base-name nm))))))

(defun cw/gdb--cleanup-on-kill ()
  (cw/gdb--cleanup (current-buffer)))

(defun cw/gdb--after-gud-sentinel (proc _msg)
  (when (memq (process-status proc) '(exit signal))
    (let ((b (process-buffer proc)))
      (when (buffer-live-p b)
        (with-current-buffer b
          (when (and (boundp 'cw/gdb-tab-name) cw/gdb-tab-name)
            (cw/gdb--cleanup b)))))))

(defun cw/gdb--install-exit-hooks (tab-base-name)
  (when (buffer-live-p gud-comint-buffer)
    (with-current-buffer gud-comint-buffer
      (setq-local cw/gdb-tab-name tab-base-name)
      (add-hook 'kill-buffer-hook #'cw/gdb--cleanup-on-kill nil t))
    (let ((proc (get-buffer-process gud-comint-buffer)))
      (when (processp proc)
        (set-process-query-on-exit-flag proc nil)
        (process-put proc 'cw-gdb-tab-name tab-base-name)))))

(defun cw/gdb--start-in-new-tab (orig-fun &rest args)
  (let* ((cmdline (car args))
         (cmd-prog (cw/gdb--program-from-cmdline cmdline)))
    (tab-bar-new-tab)
    (apply orig-fun args)
    (let* ((prog (or cmd-prog (cw/gdb--current-program)))
           (name (cw/gdb--gdb-name prog)))
      (tab-bar-rename-tab name)
      (when (buffer-live-p gud-comint-buffer)
        (with-current-buffer gud-comint-buffer
          (rename-buffer name t)))
      (cw/gdb--install-exit-hooks name))))

(with-eval-after-load 'gud
  (advice-add 'gud-sentinel :after #'cw/gdb--after-gud-sentinel))

(with-eval-after-load 'gdb-mi
  (defun cw/gdb-setup-windows ()
    (gdb-get-buffer-create 'gdb-locals-values-buffer)
    (gdb-get-buffer-create 'gdb-locals-buffer)
    (gdb-get-buffer-create 'gdb-registers-buffer)
    (gdb-get-buffer-create 'gdb-breakpoints-buffer)
    (set-window-dedicated-p (selected-window) nil)
    (switch-to-buffer gud-comint-buffer)
    (delete-other-windows)
    (let* ((main (selected-window))
           (total-width (window-total-width main))
           (left-width (max window-min-width (floor (* total-width 0.6))))
           (left main)
           (right (split-window left left-width 'right))
           (left-height (window-total-height left))
           (src-height (max window-min-height (floor (* left-height 0.6))))
           (src left)
           (cmd (split-window src src-height 'below))
           (right-height (window-total-height right))
           (locals-height (max window-min-height (floor (/ right-height 3))))
           (regs-height (max window-min-height (floor (/ right-height 3))))
           (locals right)
           (regs (split-window locals locals-height 'below))
           (bp (split-window regs regs-height 'below)))
      (setq gdb-source-window-list (list src))
      (select-window src)
      (let ((src-buf (or (gdb-get-source-buffer)
                         (and gdb-main-file (gud-find-file gdb-main-file)))))
        (when src-buf
          (gdb-display-source-buffer src-buf)))
      (gdb-set-window-buffer gud-comint-buffer nil cmd)
      (gdb-set-window-buffer (gdb-locals-buffer-name) nil locals)
      (gdb-set-window-buffer (gdb-registers-buffer-name) nil regs)
      (gdb-set-window-buffer (gdb-breakpoints-buffer-name) nil bp)
      (select-window cmd)))
  (advice-add 'gdb-setup-windows :override #'cw/gdb-setup-windows)
  (advice-add 'gdb :around #'cw/gdb--start-in-new-tab)
  (when (fboundp 'gdb-quit)
    (advice-add 'gdb-quit :after (lambda (&rest _) (cw/gdb--cleanup gud-comint-buffer))))
  (when (fboundp 'gdb-kill)
    (advice-add 'gdb-kill :after (lambda (&rest _) (cw/gdb--cleanup gud-comint-buffer)))))

(provide 'gdb)
