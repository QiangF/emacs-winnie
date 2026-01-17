;;; winnie.el --- Restore old window configurations the cleaner way  -*- lexical-binding: t; -*-

(eval-when-compile (require 'cl-lib))
(defvar winnie-max-size-diff 4)
(defvar winnie-alist nil)
(setq winnie-max-conf-num 20)
(defvar winnie-traverse-position 0)
(defvar winnie-traverse-promoted t)
(setq winnie-boring-buffers '("*Completions*" "*lispy-message*"))
(setq winnie-boring-buffers-regexp "^ \\*")

(defun winnie-set-winbuf (win win-info fallback-buf)
  (let* ((buf-name (nth 0 win-info))
         (buf (if (numberp buf-name)
                  (alist-get buf-name exwm--id-buffer-alist)
                  (get-buffer buf-name)))
         (buf-file-name (nth 1 win-info))
         (buf-point (nth 2 win-info))
         (window-start (nth 3 win-info))
         (selected (nth 4 win-info)))
    (cond (buf
           (set-window-buffer win buf))
          (buf-file-name
           (set-window-buffer win (if (directory-name-p buf-file-name)
                                      (dired-internal-noselect buf-file-name)
                                    (find-file-noselect buf-file-name))))
          (t (and fallback-buf (set-window-buffer win fallback-buf))))
    (when selected (select-window win 'mark-for-redisplay))))

(defun winnie-list-to-tree (conf win fallback-buf)
  "Resume the window from saved list CONF, WIN is `selected-window', on which performs the
split, SET-WINBUF is a function with parameter WIN & BUF, which associate them."
  (if (winnie-buf-namep (car conf))
      (winnie-set-winbuf win conf fallback-buf)
    (let* ((horizontal (eq (car conf) 'h))
           (newwin (split-window win
                                 (if horizontal (cadr conf) (cadr conf))
                                 ;; (if horizontal (+ (cadr conf) 1) (+ (cadr conf) 1))
                                 horizontal))
           (others (nthcdr 3 conf)))
      (winnie-list-to-tree (cl-third conf) win fallback-buf)
      (winnie-list-to-tree (if (> (length others) 2) others (car others))
                             newwin fallback-buf))))

(defun winnie-set (confs winnie-position)
  (let* ((winnie-length (length confs))
         scratch-buf)
    (when (window-dedicated-p)
      (setq scratch-buf (get-buffer-create "*scratch*"))
      (pop-to-buffer scratch-buf))
    (delete-other-windows)
    (winnie-list-to-tree (nth winnie-position confs)
                           (selected-window)
                           scratch-buf)
    (message "Winnie %s / %s" winnie-position winnie-length)))

(defun winnie-tree-to-list (tree selected)
  "TREE is the output of `window-tree' except `minibuffer'."
  (if (windowp tree)
      (with-selected-window tree
        (let ((buf-name (if (eq major-mode 'exwm-mode)
                            exwm--id
                            (buffer-name)))
              (file-name (or (buffer-file-name)
                             (and (equal major-mode 'dired-mode)
                                  default-directory))))
          (unless (and
                   ;; exwm-mode buffer has exwm--id as the buf-name
                   (stringp buf-name)
                   (or (member buf-name winnie-boring-buffers)
                       (and winnie-boring-buffers-regexp
                            (string-match winnie-boring-buffers-regexp
                                          buf-name))))
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
           (branch1-list (winnie-tree-to-list branch1 selected))
           (branch2-list (if (> (length branch2) 1)
                             (winnie-tree-to-list (cons vertical-split (cons nil branch2)) selected)
                           (winnie-tree-to-list (car branch2) selected))))
      (if (and branch1-list branch2-list)
          (list (if vertical-split 'v 'h)
                (if vertical-split (winnie-get-window-height branch1)
                  (winnie-get-window-width branch1))
                branch1-list
                branch2-list)
        (or branch1-list branch2-list)))))

(defun winnie-dump-window-tree ()
  "Return a list containing the current window tree info. The format looks like:
     ('h(orizontal)/'v(ertical) size-of-left-window xxx xxx ...)
where the first element is the split direction, second is the size used as the
argument of `split-winow'. The rest is a list of win-info, or another list with the given format.
win-info has the format of (window-buffer-name file-name point window-start)."
  (winnie-tree-to-list (car (window-tree)) (selected-window)))

(defun winnie-get-window-width (win)
  "Obtain the width of window if the argument is `windowp', or calculate the width
from the second element of output from `window-tree'."
  (if (windowp win)
      (window-width win)
    (let ((edge (cadr win)))
      (- (cl-third edge) (first edge)))))

(defun winnie-get-window-height (win)
  "Obtain the height of window if the argument is `windowp', or calculate the height
from the second element of output from `window-tree'."
  (if (windowp win)
      (window-height win)
    (let ((edge (cadr win)))
      (- (fourth edge) (second edge)))))

(defun winnie-buf-namep (buf-name)
  (or (stringp buf-name)
      ;; exwm--id
      (numberp buf-name)))

(defsubst winnie-equal (conf-a conf-b)
  (if (and (winnie-buf-namep (car conf-a))
           (winnie-buf-namep (car conf-b)))
      ;; one window conf
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
                   (t (equal buf-name-a buf-name-b)))))
    ;; two or more windows
    (and (equal (car conf-a) (car conf-b)) ; equal in split direction
         (let ((size-a (cadr conf-a))
               (size-b (cadr conf-b))
               (split-1-a (nthcdr 2 conf-a))
               (split-1-b (nthcdr 2 conf-b))
               (split-2-a (nthcdr 3 conf-a))
               (split-2-b (nthcdr 3 conf-b)))
           (and (< (abs (- size-a size-b)) winnie-max-size-diff) ; equal in split size
                (equal (length split-1-a) (length split-1-b))
                (equal (length split-2-a) (length split-2-b))
                (if (> (length split-1-a) 2)
                    (winnie-equal split-1-a split-1-b)
                  (winnie-equal (car split-1-a) (car split-1-b)))
                (if (> (length split-2-a) 2)
                    (winnie-equal split-2-a split-2-b)
                  (winnie-equal (car split-2-a) (car split-2-b))))))))

(defun winnie-find-index-for-conf (confs conf)
  "Return index of ITEM if on confs, else nil.
Comparison is done via `equal'.  The index is 0-based."
  (catch 'found
    (dotimes (ind (length confs))
      (when (winnie-equal conf (nth ind confs))
        (throw 'found ind)))))

(defun nth-delq (n list-in)
  (delq (setcar (nthcdr n list-in) (gensym)) list-in))

(defun winnie-region-active ()
  (declare (gv-setter (lambda (store) `(if ,store (activate-mark) (deactivate-mark)))))
  (region-active-p))

;; winnie saves at traverse-beginning and window-state-change-hook that triggers not by traversing
(defun winnie-save-current (&optional frame)
  (interactive)
  (unless (active-minibuffer-window)
    (let* ((frame (or frame (selected-frame)))
           (win (selected-window))
           (conf (winnie-dump-window-tree))
           (confs (cdr (assoc frame winnie-alist)))
           (ind (winnie-find-index-for-conf confs conf)))
      (if ind
          (setq confs (nth-delq ind confs))
        (when (> (length confs) winnie-max-conf-num)
          (setq confs (butlast confs))))
      ;; save current conf at slot 0 and reset winnie-traverse-position
      (push conf confs)
      (alist-set 'winnie-alist frame confs))))

;; when put in window-state-change-hook, at the time this function is run,
;; this-command has become last-command, this-command is nil
(defun winnie-save-for-window-state-change (&optional frame-or-window)
  (interactive)
  (unless (or (equal 'self-insert-command real-this-command)
              (winnie-command-p last-command))
    (unless winnie-traverse-promoted
      ;; promote last conf at winnie-traverse-position
      (let* ((frame (selected-frame))
             (confs (cdr (assoc frame winnie-alist)))
             (conf (and confs (nth winnie-traverse-position confs))))
        (setq confs (nth-delq winnie-traverse-position confs))
        (setq winnie-traverse-promoted t)
        (push conf confs)
        (alist-set 'winnie-alist frame confs)))
    (winnie-save-current)))

(defun alist-set (alist-symbol key value)
  "Set KEY to VALUE in alist ALIST-SYMBOL."
  (set alist-symbol
       (cons (cons key value)
             (assoc-delete-all key (eval alist-symbol)))))

(defun winnie-set-relative (step)
  ;; reset on step equal 0
  ;; window-state-change-hook is a hook that is run from redisplay.
  ;; Redisplay runs asynchronously to your code. It looks up the global value of window-state-change-functions.
  ;; To let-bound window-state-change-hook locally won't affect the global value.
  (cl-letf (;; supress winnie save in window state change functions
            (window-state-change-hook (remove 'winnie-save-for-window-state-change window-state-change-hook)))
    (let* ((confs (cdr (assoc (selected-frame) winnie-alist)))
           (winnie-length (length confs)))
      (if confs
          (progn (setq winnie-traverse-position
                       (if (zerop step) 0 (mod (cl-incf winnie-traverse-position step) winnie-length)))
                 (winnie-set confs winnie-traverse-position))
        (message "Frame has no saved winnie confs")))))

(defun winnie-command-p (cmd)
  (or (equal cmd 'winnie-previous)
      (equal cmd 'winnie-next)))

(defun winnie-previous ()
  (interactive)
  (if (winnie-command-p last-command)
      (winnie-set-relative 1)
    (winnie-save-current)
    (setq winnie-traverse-position 0)
    (setq winnie-traverse-promoted nil)
    (winnie-set-relative 1)))

(defun winnie-next ()
  (interactive)
  (if (winnie-command-p last-command)
      (winnie-set-relative -1)
    (winnie-save-current)
    (setq winnie-traverse-position 0)
    (setq winnie-traverse-promoted nil)
    (winnie-set-relative -1)))

(defvar winnie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) left] #'winnie-previous)
    (define-key map [(control c) right] #'winnie-next)
    map)
  "Keymap for winnie mode.")

(defun winnie-keyboard-quit (&rest arg)
  "Abort window conf jumping, and go back to conf before jump."
  (when (winnie-command-p last-command)
    (winnie-set-relative 0)
    (setq winnie-traverse-promoted t)))

;;;###autoload
(define-minor-mode winnie-mode
  "Toggle winnie mode on or off.

winnie mode is a global minor mode that records the changes in
the window configuration (i.e. how the frames are partitioned
into windows)."
  :global t
  (if winnie-mode
      (progn
        (add-hook 'window-state-change-hook 'winnie-save-for-window-state-change)
        (advice-add #'keyboard-quit :before #'winnie-keyboard-quit))
    (remove-hook 'window-state-change-hook 'winnie-save-for-window-state-change)
    (advice-remove #'keyboard-quit #'winnie-keyboard-quit)))

(provide 'winnie)
;;; winnie.el ends here
