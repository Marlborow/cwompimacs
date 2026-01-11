;;; lambda-mode.el --- Codewars Lambda Calculus major mode -*- lexical-binding: t; -*-
;; - Highlights: types, program decl, λ, args, includes, strings/chars, builtins, :: namespaces, comments
;; - Converts "\" into "λ" while in lambda-mode

(require 'rx)

(defgroup lambda-mode nil
  "Major mode for Codewars Lambda Calculus."
  :group 'languages)

(defface lambda-lambda-face
  '((t :inherit font-lock-keyword-face))
  "Face for λ."
  :group 'lambda-mode)

(defface lambda-operator-face
  '((t :inherit font-lock-builtin-face))
  "Face for operators like =."
  :group 'lambda-mode)

(defconst lambda-mode--types
  '("str" "bool" "num" "char" "void")
  "Builtin types.")

(defconst lambda-mode--builtins
  '("arg" "print" "#arg")
  "Builtin identifiers.")

;; Comments are: -- ... end of line
(defvar lambda-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Treat _ as word constituent (common for identifiers)
    (modify-syntax-entry ?_ "w" st)

    ;; Make newline end comments.
    (modify-syntax-entry ?\n ">" st)

    ;; We'll do line comments via `comment-start-skip` etc.
    st)
  "Syntax table for `lambda-mode`.")

(defun lambda-mode--in-string-or-comment-p ()
  "Non-nil if point is in string or comment."
  (let ((ppss (syntax-ppss)))
    (or (nth 3 ppss) (nth 4 ppss))))

(defun lambda-mode-insert-lambda ()
  "Insert λ. Bound to \\ in `lambda-mode`."
  (interactive)
  (insert "λ"))

(defun lambda-mode--post-self-insert ()
  "If user typed \\ insert λ instead (unless in string/comment)."
  (when (and (eq last-command-event ?\\)
             (not (lambda-mode--in-string-or-comment-p)))
    ;; Replace the just-inserted backslash with λ
    (delete-char -1)
    (insert "λ")))

;; Regex helpers
(defconst lambda-mode--re-types
  (regexp-opt lambda-mode--types 'symbols))

(defconst lambda-mode--re-builtins
  (regexp-opt '("arg" "print") 'symbols))

(defconst lambda-mode--re-program
  (rx symbol-start "program" (* space) ":" (* space)
      (group (or "str" "bool" "num" "char" "void"))
      (* space) "="))

(defconst lambda-mode--re-include-angle
  (rx symbol-start "import" (+ space) "<" (+ (not (any ">"))) ">"))

(defconst lambda-mode--re-include-quote
  (rx symbol-start "import" (+ space) "\"" (* (not (any "\""))) "\""))

;; Namespace: Foo::Bar
(defconst lambda-mode--re-namespace
  (rx symbol-start (group (+ (or word ?_))) (group "::") (group (+ (or word ?_))) symbol-end))

;; Name at start of line:   name =
(defconst lambda-mode--re-defname
  (rx line-start (* space) (group (+ (or word ?_))) (* space) "="))

;; Lambda args region-ish: λ x y .
;; We'll highlight λ and '.' and variable-ish tokens; keep it simple.
(defconst lambda-mode--re-lambda
  (rx "λ"))

(defconst lambda-mode--re-equals
  (rx "="))

(defconst lambda-mode--re-hasharg
  (rx "#arg" symbol-end))

;; Strings and chars
(defconst lambda-mode--re-string
  (rx "\"" (* (or "\\\"" (not (any "\"")))) "\""))

(defconst lambda-mode--re-char
  ;; Vim had: /'\w'/
  ;; We'll accept common escaped chars too: '\n' etc.
  (rx "'" (or (any "[:alnum:]_")
              (seq "\\" anychar))
      "'"))

(defvar lambda-mode-font-lock-keywords
  (list
   ;; Comments (line comment starting with --)
   ;; We'll do it as a font-lock rule; comment variables still set so M-; works.
   (cons (rx line-start (* space) "--" (* not-newline))
         'font-lock-comment-face)

   ;; program: <type> =
   (list lambda-mode--re-program
         0 'font-lock-preprocessor-face)     ; whole match similar to Vim Include
   (list lambda-mode--re-program
         1 'font-lock-type-face)             ; the type inside

   ;; includes
   (cons lambda-mode--re-include-angle 'font-lock-preprocessor-face)
   (cons lambda-mode--re-include-quote 'font-lock-preprocessor-face)

   ;; strings / chars
   (cons lambda-mode--re-string 'font-lock-string-face)
   (cons lambda-mode--re-char   'font-lock-constant-face)

   ;; types keywords anywhere
   (cons lambda-mode--re-types 'font-lock-type-face)

   ;; builtins: arg / print
   (cons lambda-mode--re-builtins 'font-lock-builtin-face)
   ;; builtin #arg
   (cons lambda-mode--re-hasharg 'font-lock-builtin-face)

   ;; namespace Foo::Bar with different faces
   (list lambda-mode--re-namespace
         1 'font-lock-type-face)            ; prefix
   (list lambda-mode--re-namespace
         2 'font-lock-delimiter-face)       ; ::
   (list lambda-mode--re-namespace
         3 'font-lock-variable-name-face)   ; name

   ;; definition name at start of line
   (list lambda-mode--re-defname
         1 'font-lock-function-name-face)

   ;; λ
   (cons lambda-mode--re-lambda 'lambda-lambda-face)

   ;; equals
   (cons lambda-mode--re-equals 'lambda-operator-face)

   ;; generic identifiers (last so keywords win)
   (cons (rx symbol-start (+ (or word ?_)) symbol-end) 'font-lock-variable-name-face))
  "Font-lock keywords for `lambda-mode`.")

;;;###autoload
(define-derived-mode lambda-mode prog-mode "Lambda"
  "Major mode for Codewars Lambda Calculus."
  :syntax-table lambda-mode-syntax-table
  (setq-local font-lock-defaults '(lambda-mode-font-lock-keywords))
  (setq-local comment-start "--")
  (setq-local comment-end "")
  (setq-local comment-start-skip (rx (* space) "--" (+ space)))
  ;; Convert "\" -> "λ"
  (add-hook 'post-self-insert-hook #'lambda-mode--post-self-insert nil t))

;; Keybinding: also map "\" directly to λ (instant, no hook needed)
(let ((map lambda-mode-map))
  (define-key map (kbd "\\") #'lambda-mode-insert-lambda))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lc\\'" . lambda-mode))

(defun lambda-mode--import-path-at-line ()
  "Return absolute path for import on current line, or nil if none.
Supports:
  import <name>    -> ~/.config/lambda/name.lc
  import \"path\"  -> (project/cwd)/path"
  (let* ((line (thing-at-point 'line t))
         (angle (and line (string-match (rx "import" (+ space) "<" (group (+ (not (any ">")))) ">") line)
                     (match-string 1 line)))
         (quote (and line (string-match (rx "import" (+ space) "\"" (group (* (not (any "\"")))) "\"") line)
                     (match-string 1 line))))
    (cond
     (angle (expand-file-name (concat angle ".lc") (expand-file-name "~/.config/lambda/")))
     (quote (expand-file-name quote default-directory))
     (t nil))))

(defun lambda-mode-open-imported-file (mode)
  "Open import target from current line.
MODE is either the symbol `split` or `tab`."
  (interactive
   (list (intern (completing-read "Open import in: " '("split" "tab") nil t))))
  (let ((path (lambda-mode--import-path-at-line)))
    (cond
     ((null path)
      (user-error "No import on this line"))
     ((not (file-readable-p path))
      (user-error "Cannot open import: %s" path))
     (t
      (pcase mode
        ('split (find-file-other-window path))
        ('tab   (progn
                  (unless (fboundp 'tab-bar-mode)
                    (user-error "tab-bar not available in this Emacs"))
                  (tab-bar-mode 1)
                  (tab-bar-new-tab)
                  (find-file path)))
        (_ (user-error "Unknown mode: %S" mode)))))))

(defun lambda-mode-open-import-split ()
  "Open imported file from current line in a split (other window)."
  (interactive)
  (lambda-mode-open-imported-file 'split))

(defun lambda-mode-open-import-tab ()
  "Open imported file from current line in a new Emacs tab."
  (interactive)
  (lambda-mode-open-imported-file 'tab))

(with-eval-after-load 'evil
  (evil-define-key 'normal lambda-mode-map (kbd "C-I") #'lambda-mode-open-import-split)

  ;; Optional: if you want tab-open instead:
  ;; (evil-define-key 'normal lambda-mode-map (kbd "C-I") #'lambda-mode-open-import-tab)
  ;; (evil-define-key 'normal lambda-mode-map (kbd "<C-tab>") #'lambda-mode-open-import-tab)
)



(provide 'lambda-mode)
