(defun cw/make-floating-frame-centered (&optional name width height)
  "Create a small centered (child-ish) frame.

NAME defaults to \"floating\".
WIDTH/HEIGHT are in character columns/rows (defaults: 100x30)."
  (interactive)
  (unless (display-graphic-p)
    (user-error "Floating frames require a GUI Emacs (display-graphic-p)"))
  (let* ((name (or name "floating"))
         (width (or width 100))
         (height (or height 30))
         ;; Work area of the display the selected frame is on:
         (wa (frame-monitor-workarea (selected-frame)))
         (mx (nth 0 wa)) (my (nth 1 wa))
         (mw (nth 2 wa)) (mh (nth 3 wa))
         ;; Convert char size to pixels
         (cw (frame-char-width))
         (ch (frame-char-height))
         (pw (* width cw))
         (ph (* height ch))
         ;; Center inside monitor workarea
         (left (+ mx (/ (- mw pw) 2)))
         (top  (+ my (/ (- mh ph) 2))))
    (select-frame-set-input-focus
     (make-frame
      `((name . ,name)
        (minibuffer . t)
        (width . ,width)
        (height . ,height)
        (left . ,left)
        (top . ,top)
        ;; looks nicer as a popup
        (undecorated . t)
        (internal-border-width . 12)
        (drag-internal-border . t)
        (no-accept-focus . nil)
        (no-focus-on-map . nil)
        (skip-taskbar . t)
        (no-other-frame . t)
        (unsplittable . t)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil))))))
