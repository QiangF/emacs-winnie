;;; hl-line-global.el --- highlight the current line  -*- lexical-binding:t -*-

(defface hl-line
  '((t :inherit highlight :extend t))
  "Default face for highlighting the current line in Hl-Line mode.")

(defvar hl-line-overlay-priority -50)

(defvar-local hl-line-global-overlay nil)

(defvar-local hl-line-global-inactive-overlay nil)

(defvar hl-line-global-previous-window nil)

(defvar hl-line-global-sync-inactive nil)

(defface hl-line-inactive
  '((t :inherit highlight :extend t :underline (:color "green" :style dots)))
  "Default face for highlighting the current line of inactive buffer.")

(defun hl-line-global-highlight-active ()
  (unless (window-minibuffer-p)         ; Current line is always the first in minibuffer
    (let ((b (line-beginning-position))
          (e (line-beginning-position 2)))
      (when (and (overlayp hl-line-global-inactive-overlay)
                 hl-line-global-sync-inactive)
        (move-overlay hl-line-global-inactive-overlay b e))
      (if (overlayp hl-line-global-overlay)
          (move-overlay hl-line-global-overlay b e)
        (let ((ol (make-overlay b e)))
          (overlay-put ol 'priority hl-line-overlay-priority)
          (overlay-put ol 'face 'hl-line)
          (setq hl-line-global-overlay ol)))
      ;; Bind active overlay to window
      (overlay-put hl-line-global-overlay 'window (selected-window)))))

(defun hl-line-global-highlight-inactive ()
  (when (and hl-line-global-previous-window
             (window-live-p hl-line-global-previous-window))
    (let* ((current-window (selected-window)))
      (unless (or (eq hl-line-global-previous-window current-window)
                  (window-minibuffer-p hl-line-global-previous-window))
        (with-selected-window hl-line-global-previous-window
          (let ((b (line-beginning-position))
                (e (line-beginning-position 2)))
            (if (overlayp hl-line-global-inactive-overlay)
                (move-overlay hl-line-global-inactive-overlay b e)
              (let ((ol (make-overlay b e)))
                (overlay-put ol 'priority (- hl-line-overlay-priority 10))
                (overlay-put ol 'face 'hl-line-inactive)
                (setq hl-line-global-inactive-overlay ol))))
          (when (overlayp hl-line-global-overlay)
            ;; bind previous overlay's window to current to supress its active overlay
            (overlay-put hl-line-global-overlay 'window
                         current-window)))))))

(defun hl-line-global-highlight ()
  "Highlight the current line in the current window."
  (when hl-line-global-mode           ; Might be changed outside the mode function
    (hl-line-global-highlight-active)
    (hl-line-global-highlight-inactive)
    (setq hl-line-global-previous-window (selected-window))))

(defun hl-line-global-highlight-all ()
  "Highlight the current line in all live windows."
  (let ((window (selected-window)))
    (walk-windows (lambda (w)
                    (with-selected-window w
                      (if (eq window w)
                          (hl-line-global-highlight-active)
                        (hl-line-global-highlight-inactive))))
                  nil t)))

(defun hl-line-global-unhighlight (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (when (overlayp hl-line-global-inactive-overlay)
      (delete-overlay hl-line-global-inactive-overlay))
    (when (overlayp hl-line-global-overlay)
      (delete-overlay hl-line-global-overlay))))

(defun hl-line-global-unhighlight-all ()
  (dolist (buffer (buffer-list))
    (hl-line-global-unhighlight buffer)))

;;;###autoload
(define-minor-mode hl-line-mode "dummy")

;;;###autoload
(define-minor-mode hl-line-global-mode
  "hl-line-global-mode"
  :global t
  (if hl-line-global-mode
      (progn (hl-line-global-highlight-all)
             (add-hook 'post-command-hook #'hl-line-global-highlight))
    (hl-line-global-unhighlight-all)
    (remove-hook 'post-command-hook #'hl-line-global-highlight)))

(provide 'hl-line-global)

;;; hl-line-global.el ends here
