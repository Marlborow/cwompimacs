;;; telescope.el -*- lexical-binding: t -*-

(defun cw/global-root ()
  (file-name-as-directory
   (or (and (stringp cw/global-cwd) (file-directory-p cw/global-cwd) cw/global-cwd)
       default-directory)))

(defun cw/consult--fd-global-builder (paths)
  "Return a `consult-fd' builder that lists everything on empty input.
PATHS is the set of search roots, typically `cw/global-root'."
  (require 'consult)
  (let ((base (consult--fd-make-builder paths))
        (cmd (consult--build-args consult-fd-args))
        (search-paths (mapcan (lambda (x) `("--search-path" ,x)) paths)))
    (lambda (input)
      (pcase-let* ((`(,arg . ,opts) (consult--command-split input)))
        (if (string-blank-p arg)
            ;; Empty query: list everything under PATHS.
            (cons (append cmd opts search-paths) nil)
          (funcall base input))))))

(defun cw/consult-find-file-global ()
  (interactive)
  (require 'consult)
  (let* ((dir (cw/global-root))
         (paths (list dir))
         (default-directory dir)
         ;; Show candidates immediately; typing will just narrow.
         (consult-async-min-input 0)
         (builder (cw/consult--fd-global-builder paths)))
    (find-file
     (consult--read
      (consult--process-collection
       builder
       :transform
       (consult--async-map
        (lambda (x)
          (let* ((path (string-remove-prefix "./" x))
                 (name (file-name-nondirectory path)))
            ;; Show only the filename, keep full path for lookup.
            (propertize name 'consult--candidate path))))
       :highlight t :file-handler t)
      :prompt "Global file: "
      :sort nil
      :require-match t
      :add-history (thing-at-point 'filename)
      :category 'file
      :lookup #'consult--lookup-candidate
      :history '(:input consult--find-history)))))

(with-eval-after-load 'general
  (when (fboundp 'cw/leader)
    (cw/leader
     "t" '(:ignore t)
     "t e" #'cw/consult-find-file-global)))


(provide 'telescope)
