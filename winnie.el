;;; winnie.el --- Restore old window configurations the cleaner way  -*- lexical-binding: t; -*-

(eval-when-compile (require 'cl-lib))
(defvar winnie-max-size-diff 3)
(defvar winnie-alist nil)
(setq winnie-max-num 20)
(defvar winnie-traversal-position)
(setq winnie-boring-buffers '("*Completions*" "*lispy-message*"))
(setq winnie-boring-buffers-regexp "^ \\*")

(defun winnie-set-winbuf (win win-info fallback-buf)
  (let* ((buf-name (nth 0 win-info))
         (buf (get-buffer buf-name))
         (buf-file-name (nth 1 win-info))
         (buf-point (nth 2 win-info))
         (window-start (nth 3 win-info))
         (selected (nth 4 win-info)))
    (cond (buf (set-window-buffer win buf))
          (buf-file-name
           (if (directory-name-p buf-file-name)
               (dired buf-file-name)
             (find-file buf-file-name)))
          (t (set-window-buffer win fallback-buf)))
    (when selected (select-window win 'mark-for-redisplay))))

(defun winnie-to-window-tree (conf win fallback-buf)
  "Resume the window from saved list CONF, WIN is `selected-window', on which performs the
split, SET-WINBUF is a function with parameter WIN & BUF, which associate them."
  (if
      (stringp (car conf))
      (winnie-set-winbuf win conf fallback-buf)
    (let* ((horizontal (eq (car conf) 'h))
           (newwin (split-window win
                                 (if horizontal
                                     (+ (cadr conf) 1)
                                   (+ (cadr conf) 1))
                                 horizontal))
           (others (nthcdr 3 conf)))
      (winnie-to-window-tree (cl-third conf) win fallback-buf)
      (winnie-to-window-tree (if (> (length others) 2) others (car others))
                             newwin fallback-buf))))

(defun winnie-set (configs winnie-position)
  (let* ((winnie-length (length configs))
         (winnie-position (mod winnie-position winnie-length))
         scratch-buf)
    (when (window-dedicated-p)
      (setq scratch-buf (get-buffer-create "*scratch*"))
      (pop-to-buffer scratch-buf))
    (delete-other-windows)
    (winnie-to-window-tree (nth winnie-position configs)
                           (selected-window)
                           scratch-buf)
    (message "Win-config %s / %s" (+ 1 winnie-position) winnie-length)))

(defun winnie-window-tree-to-list (tree selected)
  "TREE is the output of `window-tree' except `minibuffer'."
  (if (windowp tree)
      (with-selected-window tree
        (let ((buf-name (buffer-name))
              (file-name (or (buffer-file-name)
                             (and (equal major-mode 'dired-mode)
                                  default-directory))))
          (unless
              (or (member buf-name winnie-boring-buffers)
                  (and winnie-boring-buffers-regexp
                       (string-match winnie-boring-buffers-regexp
                                     buf-name)))
            (list buf-name
                  (and file-name
                       (abbreviate-file-name file-name))
                  (point)
                  (window-start)
                  (equal tree selected)))))
    (let* ((vertical-split (car tree))
           (children (cddr tree))
           (branch1 (car children))
           (branch2 (cdr children))
           (branch1-list (winnie-window-tree-to-list branch1 selected))
           (branch2-list (if (> (length branch2) 1)
                             (winnie-window-tree-to-list (cons vertical-split (cons nil branch2)))
                           (winnie-window-tree-to-list (car branch2) selected))))
      (if (and branch1-list branch2-list)
          (list (if vertical-split 'v 'h)
                (if vertical-split (winnie-trans-window-height branch1)
                  (winnie-trans-window-width branch1))
                branch1-list
                branch2-list)
        (or branch1-list branch2-list)))))

(defun winnie-dump-window-tree ()
  "Return a list containing the current window tree info. The format looks like:
     ('h(orizontal)/'v(ertical) size-of-left-window xxx xxx ...)
where the first element is the split direction, second is the size used as the
argument of `split-winow'. The rest is a list of win-info, or another list with the given format.
win-info has the format of (window-buffer-name file-name point window-start)."
  (winnie-window-tree-to-list (car (window-tree)) (selected-window)))

(defun winnie-trans-window-width (win)
  "Obtain the width of window if the argument is `windowp', or calculate the width
from the second element of output from `window-tree'."
  (if (windowp win)
      (window-width win)
    (let ((edge (cadr win)))
      (- (cl-third edge) (first edge)))))

(defun winnie-trans-window-height (win)
  "Obtain the height of window if the argument is `windowp', or calculate the height
from the second element of output from `window-tree'."
  (if (windowp win)
      (window-height win)
    (let ((edge (cadr win)))
      (- (fourth edge) (second edge)))))

(defsubst winnie-equal (conf-a conf-b)
  (if (and (stringp (car conf-a)) (stringp (car conf-b)))
      ;; one window config
      (let ((buf-name-a (nth 0 conf-a))
            (buf-file-a (nth 1 conf-a))
            (buf-name-b (nth 0 conf-b))
            (buf-file-b (nth 1 conf-b))
            (buf-selected-a (nth 4 conf-a))
            (buf-selected-b (nth 4 conf-b)))
        (and (equal buf-selected-a buf-selected-b)
             (cond ((and buf-file-a buf-file-b)
                    (string-equal buf-file-a buf-file-b))
                   ((or buf-file-a buf-file-b) nil) ; one buffer has filename
                   (t (string-equal buf-name-a buf-name-b)))))
    ;; two or more windows
    (and (equal (car conf-a) (car conf-b)) ; equal in split direction
         (let ((size-a (cadr conf-a))
               (size-b (cadr conf-b))
               (others-a (nthcdr 3 conf-a))
               (others-b (nthcdr 3 conf-b)))
           (and (< (abs (- size-a size-b)) winnie-max-size-diff) ; equal in split size
                (equal (length others-a) (length others-b))
                (if (> (length others-a) 2)
                    (winnie-equal others-a others-b)
                  (winnie-equal (car others-a) (car others-b))))))))

(defun winnie-find-index-for-conf (configs conf)
  "Return index of ITEM if on configs, else nil.
Comparison is done via `equal'.  The index is 0-based."
  (catch 'found
    (dotimes (ind (length configs))
      (when (winnie-equal conf (nth ind configs))
        (throw 'found ind)))))

(defun nth-delq (n list-in)
  (delq (setcar (nthcdr n list-in) (gensym)) list-in))

(defun winnie-region-active ()
  (declare (gv-setter (lambda (store) `(if ,store (activate-mark) (deactivate-mark)))))
  (region-active-p))

(defun winnie-save (&optional win)
  (interactive)
  (unless (or (active-minibuffer-window)
              (winnie-command-p last-command)
              (minibufferp))
    (unless (or (equal 'self-insert-command real-this-command)
                (winnie-command-p real-last-command))
      (let* ((frame (selected-frame))
             (win (or win (selected-window)))
             (conf (winnie-dump-window-tree))
             (configs (cdr (assoc frame winnie-alist)))
             (ind (winnie-find-index-for-conf configs conf)))
        (if ind
            (progn
              (setq configs (nth-delq ind configs)))
          (when (> (length configs) winnie-max-num)
            (setq configs (butlast configs))))
        (push conf configs)
        (alist-set 'winnie-alist frame configs)))))

(defun alist-set (alist-symbol key value)
  "Set KEY to VALUE in alist ALIST-SYMBOL."
  (set alist-symbol
       (cons (cons key value)
             (assoc-delete-all key (eval alist-symbol)))))

(defun winnie-set-relative (step)
  (cl-letf ((window-state-change-functions nil))
    (let* ((configs (cdr (assoc (selected-frame) winnie-alist))))
      (if configs
          (progn (cl-incf winnie-traversal-position step)
                 (winnie-set configs winnie-traversal-position))
        (message "Frame has no saved winnie configs")))))

(defun winnie-command-p (cmd)
  (or (equal cmd 'winnie-previous)
      (equal cmd 'winnie-next)))

(defun winnie-reset ()
  "Abort window config jumping, and go back to config before jump."
  (setq winnie-traversal-position -1)
  (winnie-set-relative 1))

(defun winnie-previous ()
  (interactive)
  (if (winnie-command-p last-command)
      (winnie-set-relative 1)
    (setq winnie-traversal-position 0)
    (winnie-set-relative 1)))

(defun winnie-next ()
  (interactive)
  (if (winnie-command-p last-command)
      (winnie-set-relative -1)
    (setq winnie-traversal-position 0)
    (winnie-set-relative -1)))

(defvar winnie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) left] #'winnie-previous)
    (define-key map [(control c) right] #'winnie-next)
    map)
  "Keymap for winnie mode.")

(defun winnie-keyboard-quit (&rest arg)
  (when (winnie-command-p last-command)
    (winnie-reset)))

;;;###autoload
(define-minor-mode winnie-mode
  "Toggle winnie mode on or off.

winnie mode is a global minor mode that records the changes in
the window configuration (i.e. how the frames are partitioned
into windows)."
  :global t
  (if winnie-mode
      (progn
        (add-hook 'window-state-change-functions 'winnie-save)
        (advice-add #'keyboard-quit :after #'winnie-keyboard-quit))
    (remove-hook 'window-state-change-functions 'winnie-save)
    (advice-remove #'keyboard-quit #'winnie-keyboard-quit)))

(provide 'winnie)
;;; winnie.el ends here
