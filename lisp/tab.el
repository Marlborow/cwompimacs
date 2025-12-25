(setq tab-bar-show 1)

(defvar cw/tab-bar--renumbering nil)

(defun cw/tab-bar--base-name (name)
  (replace-regexp-in-string "\\`[0-9]+[ ]*:[ ]*" "" (or name "")))

(defun cw/tab-bar--buffer-title (&optional buffer)
  "Return a short title for BUFFER using its file name when available."
  (let* ((buf (or buffer (window-buffer (selected-window))))
         (file (and buf (buffer-file-name buf))))
    (cond
     (file (file-name-nondirectory file))
     (buf (buffer-name buf))
     (t "untitled"))))

(defun cw/tab-bar--rename-tab-silently (name)
  "Rename current tab to NAME without echo-area spam."
  (let ((inhibit-message t)
        (message-log-max nil))
    (tab-bar-rename-tab name)))

(defun cw/tab-bar-renumber (&rest _)
  (unless cw/tab-bar--renumbering
    (let ((cw/tab-bar--renumbering t))
      (when (fboundp 'tab-bar-tabs)
        (let* ((tabs (tab-bar-tabs))
               (cur (tab-bar--current-tab-index)))
          (unwind-protect
              (dotimes (i (length tabs))
                (tab-bar-select-tab (1+ i))
                (let* ((tab (nth i tabs))
                       (fallback (cw/tab-bar--base-name (alist-get 'name tab)))
                       (title (or (cw/tab-bar--buffer-title) fallback)))
                  (cw/tab-bar--rename-tab-silently (format "%d : %s" (1+ i) title))))
            (when cur
              (tab-bar-select-tab (1+ cur)))))))))

(defun cw/tab-bar-rename-current (&rest _)
  "Rename the current tab to mirror the visible buffer."
  (unless cw/tab-bar--renumbering
    (let* ((tabs (tab-bar-tabs))
           (index (tab-bar--current-tab-index))
           (tab (and index (nth index tabs))))
      (when tab
        (let* ((fallback (cw/tab-bar--base-name (alist-get 'name tab)))
               (title (or (cw/tab-bar--buffer-title) fallback))
               (new-name (format "%d : %s" (1+ index) title))
               (current-name (alist-get 'name tab)))
          (unless (equal current-name new-name)
            (let ((cw/tab-bar--renumbering t))
              (cw/tab-bar--rename-tab-silently new-name))))))))

(when (boundp 'tab-bar-tab-post-open-functions)
  (add-hook 'tab-bar-tab-post-open-functions #'cw/tab-bar-renumber))
(when (boundp 'tab-bar-tab-post-close-functions)
  (add-hook 'tab-bar-tab-post-close-functions #'cw/tab-bar-renumber))
(when (boundp 'tab-bar-tab-post-rename-functions)
  (add-hook 'tab-bar-tab-post-rename-functions #'cw/tab-bar-renumber))
(add-hook 'after-init-hook #'cw/tab-bar-renumber)
(add-hook 'window-buffer-change-functions #'cw/tab-bar-rename-current)
(when (boundp 'window-selection-change-functions)
  (add-hook 'window-selection-change-functions #'cw/tab-bar-rename-current))

(defun cw/tab-select-0 ()
  (interactive)
  (tab-bar-select-tab (length (tab-bar-tabs))))

(dotimes (i 9)
  (let* ((n (1+ i))
         (sym (intern (format "cw/tab-select-%d" n))))
    (fset sym (eval `(lambda () (interactive) (tab-bar-select-tab ,n))))))

(with-eval-after-load 'general
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


;; 1) No gap between tabs
(setq tab-bar-separator "")

;; 2) Remove the built-in “ %s ” padding but keep all click/face properties
(defun cw/tab-bar-tab-name-format-no-padding (tab i)
  (let ((s (tab-bar-tab-name-format-default tab i)))
    ;; default adds a leading+trailing space; strip exactly one char each side
    (when (and (stringp s) (>= (length s) 2)
               (eq (aref s 0) ?\s)
               (eq (aref s (1- (length s))) ?\s))
      (setq s (substring s 1 -1)))
    s))

(setq tab-bar-tab-name-format-function #'cw/tab-bar-tab-name-format-no-padding)

(provide 'tab)
