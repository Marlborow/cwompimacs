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


(defun cw/dir-normalize (dir)
  "Expand ~, make DIR end with /, and error if it isn't a directory."
  (let ((d (file-name-as-directory (expand-file-name dir))))
    (unless (file-directory-p d)
      (user-error "Not a directory: %s" d))
    d))


;;; Per-tab cwd ----------------------------------------------------------------
;;
;; Strategy: mint a UUID for every tab at open time using
;; `tab-bar-tab-post-open-functions', store it in the tab alist under the key
;; `cw-uid' by temporarily replacing the frame `tabs' parameter entry — but
;; ONLY after tab-bar has finished building the alist, so we never interfere
;; with tab-bar internals mid-operation.
;;
;; The cwd itself lives in `cw/tab-cwd-table', a hash keyed by that UUID.
;; On close, we clean up via `tab-bar-tab-post-close-functions'.

(defvar cw/tab-cwd-table (make-hash-table :test 'equal)
  "Maps tab UUID string -> (dir . project).")

(defun cw/tab--tabs ()
  "Return the raw tabs list from the frame parameter — the live cons."
  (frame-parameter nil 'tabs))

(defun cw/tab--current-index ()
  "Return 0-based index of the current tab in the frame `tabs' list."
  (let ((tabs (cw/tab--tabs))
        (i 0)
        result)
    (catch 'found
      (dolist (tab tabs)
        (when (eq (car tab) 'current-tab)
          (setq result i)
          (throw 'found nil))
        (setq i (1+ i))))
    result))

(defun cw/tab--uid-for-index (idx)
  "Return the cw-uid stored in tab at IDX, or nil."
  (let ((tab (nth idx (cw/tab--tabs))))
    (alist-get 'cw-uid tab)))

(defun cw/tab--set-uid-for-index (idx uid)
  "Write UID into the live tab alist at IDX."
  (let* ((tabs (cw/tab--tabs))
         (tab  (nth idx tabs)))
    (when tab
      ;; setf on alist-get mutates the existing cons if present,
      ;; otherwise we need to push a new cell onto the tab alist.
      (if (assq 'cw-uid tab)
          (setcdr (assq 'cw-uid tab) uid)
        ;; Append new key: mutate the list cell in-place.
        (nconc tab (list (cons 'cw-uid uid)))))))

(defun cw/tab--current-uid ()
  "Return the UUID for the current tab, or nil."
  (let ((idx (cw/tab--current-index)))
    (and idx (cw/tab--uid-for-index idx))))

(defun cw/tab--mint-uid-for-new-tab (&rest _)
  "Assign a fresh UUID to the newly opened tab."
  ;; The new tab becomes current immediately after open.
  (let ((idx (cw/tab--current-index)))
    (when idx
      (let ((uid (format "%04x%04x-%04x-%04x"
                         (random 65536) (random 65536)
                         (random 65536) (random 65536))))
        (cw/tab--set-uid-for-index idx uid)))))

(defun cw/tab--cleanup-closed-tab (tab &rest _)
  "Remove cwd entry for the closed TAB."
  (let ((uid (alist-get 'cw-uid tab)))
    (when uid (remhash uid cw/tab-cwd-table))))

(add-hook 'tab-bar-tab-post-open-functions  #'cw/tab--mint-uid-for-new-tab)
(add-hook 'tab-bar-tab-post-close-functions #'cw/tab--cleanup-closed-tab)

;; Also mint a UID for the very first tab that exists at startup.
(defun cw/tab--ensure-initial-uid ()
  (let ((idx (cw/tab--current-index)))
    (when (and idx (not (cw/tab--uid-for-index idx)))
      (cw/tab--mint-uid-for-new-tab))))
(add-hook 'after-init-hook #'cw/tab--ensure-initial-uid)


;;; Public API -----------------------------------------------------------------

(defun cw/tab-cwd ()
  "Return the cwd for the current tab, falling back to `cw/global-cwd'."
  (let* ((uid   (cw/tab--current-uid))
         (entry (and uid (gethash uid cw/tab-cwd-table))))
    (or (car entry) cw/global-cwd default-directory)))

(defun cw/tab--set-cwd (dir proj)
  "Store DIR and PROJ for the current tab."
  (let ((uid (cw/tab--current-uid)))
    (when uid
      (puthash uid (cons dir proj) cw/tab-cwd-table))))

(defun cw/tab--sync-globals-from-current-tab (&rest _)
  "After switching tabs, pull that tab's cwd into the global vars and refresh treemacs."
  (let* ((uid   (cw/tab--current-uid))
         (entry (and uid (gethash uid cw/tab-cwd-table))))
    (when entry
      (setq cw/global-cwd         (car entry))
      (setq cw/global-cwd-project (cdr entry))
      ;; If treemacs is focused after the tab switch, defer focus move
      ;; with run-with-timer so it fires after tab-bar restores window state.
      (run-with-timer
       0 nil
       (lambda ()
         (when (and (fboundp 'treemacs-get-local-window)
                    (eq (selected-window) (treemacs-get-local-window)))
           (let ((win (seq-find
                       (lambda (w)
                         (and (window-live-p w)
                              (not (eq w (treemacs-get-local-window)))
                              (not (window-minibuffer-p w))))
                       (window-list))))
             (when win (select-window win))))))
      ;; Refresh treemacs to reflect the new tab's cwd.
      (when (and (fboundp 'treemacs-get-local-window)
                 (treemacs-get-local-window)
                 (fboundp 'cw/treemacs-ensure-root))
        (cw/treemacs-ensure-root cw/global-cwd)))))

(advice-add 'tab-bar-select-tab :after #'cw/tab--sync-globals-from-current-tab)


;;; Commands -------------------------------------------------------------------

(defun cw/consult-ripgrep-global ()
  (interactive)
  (consult-ripgrep (cw/dir-normalize (cw/tab-cwd))))

(defun cw/change-global-cwd-source-config ()
  "Pick a directory with fd and set it as the cwd for the *current tab*."
  (interactive)
  (let* ((home        (getenv "HOME"))
         (source-root (expand-file-name "Source" home))
         (config-root (expand-file-name ".config" home))
         (default-directory home)
         (cmd  (format
                "fd --type d --hidden --absolute-path --exclude .git \
--exclude unity3d --exclude Unity --exclude db_ui_queries . %s %s"
                source-root config-root))
         (dirs   (split-string (shell-command-to-string cmd) "\n" t))
         (choice (completing-read "Change tab cwd: " dirs nil t)))
    (when choice
      (let* ((proj (let ((default-directory choice))
                     (project-current nil)))
             (root (if proj (project-root proj) choice))
             (dir  (cw/dir-normalize root)))
        (cw/tab--set-cwd dir proj)
        (setq cw/global-cwd dir)
        (setq cw/global-cwd-project proj)
        (when (and (fboundp 'treemacs-get-local-window)
                   (treemacs-get-local-window)
                   (fboundp 'treemacs-add-and-display-current-project-exclusively))
          (let ((default-directory dir))
            (treemacs-add-and-display-current-project-exclusively)))
        (message "Tab cwd -> %s" dir)))))

(with-eval-after-load 'general
  (when (fboundp 'cw/leader)
    (cw/leader
     "g r" #'cw/consult-ripgrep-global
     "c d" #'cw/change-global-cwd-source-config)))


(provide 'cd)
