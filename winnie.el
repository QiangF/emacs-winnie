;;; winnie.el --- Restore old window configurations the cleaner way  -*- lexical-binding: t; -*-

(eval-when-compile (require 'cl-lib))
(defvar winnie-max-size-diff 4)
(defvar winnie-alist nil)
(defvar winnie-max-conf-num 20)
(defvar winnie-traverse-position 0)
(defvar winnie-boring-buffers '("*Completions*" "*lispy-message*" "*Ilist*"))
(defvar winnie-boring-buffers-regexp "^ \\*")

(defun winnie-restore-buffer (win conf fallback-buf)
  (let* ((buf-name (nth 0 conf))
         (buf (if (numberp buf-name)
                  (alist-get buf-name exwm--id-buffer-alist)
                (get-buffer buf-name)))
         (buf-file-name (nth 1 conf))
         (selected (nth 2 conf))
         (dedicated (nth 3 conf)))
    (cond (buf
           (progn (set-window-buffer win buf)
                  (and dedicated (set-window-dedicated-p win t))))
          (buf-file-name
           (if (directory-name-p buf-file-name)
               (set-window-buffer win (dired-internal-noselect buf-file-name))
             (when (file-exists-p buf-file-name)
               (set-window-buffer win (find-file-noselect buf-file-name)))))
          (t (and fallback-buf (set-window-buffer win fallback-buf))))
    (when selected (select-window win 'mark-for-redisplay))))

(defvar winnie-dead-buf-found nil)
(defun winnie-find-dead-buffer (conf)
  (unless winnie-dead-buf-found
    (if (winnie-buffer-p (car conf))
        (let* ((buf-name (nth 0 conf))
               (buf (if (numberp buf-name)
                        (alist-get buf-name exwm--id-buffer-alist)
                      (get-buffer buf-name)))
               (buf-file-name (nth 1 conf)))
          (unless (or buf-file-name buf)
            (setq winnie-dead-buf-found t)))
      (let ((others (nthcdr 3 conf)))
        (winnie-find-dead-buffer (cl-third conf))
        (winnie-find-dead-buffer (if (> (length others) 2) others (car others)))))))

(defun winnie-clean-confs ()
  (let* ((frame (selected-frame))
         (confs (cdr (assoc frame winnie-alist)))
         confs-new)
    (delete-dups confs)
    (dolist (conf (reverse confs))
      (setq winnie-dead-buf-found nil)
      (unless (winnie-find-dead-buffer conf)
        (push conf confs-new)))
    (alist-set 'winnie-alist frame confs-new)))

(defun winnie-list-to-tree (conf win fallback-buf)
  "Resume the window from saved list CONF, WIN is `selected-window', on which performs the
split, SET-WINBUF is a function with parameter WIN & BUF, which associate them."
  (if (winnie-buffer-p (car conf))
      (winnie-restore-buffer win conf fallback-buf)
    (let* ((horizontal (eq (car conf) 'h))
           (newwin (split-window win
                                 ;; (if horizontal (cadr conf) (cadr conf))
                                 (if horizontal (+ (cadr conf) 1) (cadr conf))
                                 horizontal))
           (others (nthcdr 3 conf)))
      (winnie-list-to-tree (cl-third conf) win fallback-buf)
      (winnie-list-to-tree (if (> (length others) 2) others (car others))
                           newwin fallback-buf))))

;; window-state-change-functions is a hook that is run from redisplay.
;; Redisplay runs asynchronously to your code. It looks up the global value of window-state-change-functions.
;; let-bound window-state-change-functions locally won't affect the global value.
;; it only supresses winnie save in window state change functions.
(defun winnie-restore (confs winnie-position)
  (cl-letf ((window-state-change-functions (remove 'winnie-window-state-change window-state-change-functions)))
    (let* ((winnie-length (length confs))
           fallback-buf)
      (when (window-dedicated-p)
        (setq fallback-buf (get-buffer-create "*scratch*"))
        (pop-to-buffer fallback-buf))
      (delete-other-windows)
      (winnie-list-to-tree (nth winnie-position confs)
                           (selected-window)
                           fallback-buf)
      (if (equal winnie-position 0)
          (setq winnie-traverse-destination-conf nil)
        (setq winnie-traverse-destination-conf (nth winnie-position confs)))
      (message "Winnie %s / %s" winnie-position winnie-length))))

(defun winnie-tree-to-list (tree selected)
  "TREE is the output of `window-tree' except `minibuffer'.
Return a list containing the current window tree info. The format looks like:
('h(orizontal)/'v(ertical) size-of-left-window xxx xxx ...)
where the first element is the split direction, second is the size used as the
argument of `split-winow'. The rest is a list of win-info, or another list with the given format."
  (if (windowp tree)
      (with-selected-window tree
        (let ((buf-name (if (eq major-mode 'exwm-mode)
                            exwm--id
                          (buffer-name)))
              (file-name (or buffer-file-name
                             (and (equal major-mode 'dired-mode)
                                  default-directory)))
              (dedicated (window-dedicated-p)))
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
                  (equal tree selected)
                  dedicated))))
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

(defun winnie-buffer-p (buf-name)
  (or (stringp buf-name)
      ;; exwm--id
      (numberp buf-name)))

(defsubst winnie-equal (conf-a conf-b)
  (if (and (winnie-buffer-p (car conf-a))
           (winnie-buffer-p (car conf-b)))
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
  (when confs
    (catch 'found
      (dotimes (ind (length confs))
        (when (winnie-equal conf (nth ind confs))
          (throw 'found ind))))))

(defun nth-delq (n list-in)
  (delq (setcar (nthcdr n list-in) (gensym)) list-in))

(defun winnie-region-active ()
  (declare (gv-setter (lambda (store) `(if ,store (activate-mark) (deactivate-mark)))))
  (region-active-p))

;; winnie saves at traverse-beginning and window-state-change-functions that triggers not by traversing
(defun winnie-save (&optional conf)
  (interactive)
  (when (or (not (active-minibuffer-window))
            ;; conf spicified only on saving winnie-traverse-destination-conf, it might be triggered by a minibuffer popup
            conf)
    (let* ((frame (selected-frame))
           (win (selected-window))
           (conf (or conf (winnie-tree-to-list (car (window-tree)) (selected-window))))
           (confs (cdr (assoc frame winnie-alist)))
           (ind (winnie-find-index-for-conf confs conf)))
      (if ind
          (setq confs (nth-delq ind confs))
        (when (> (length confs) winnie-max-conf-num)
          (setq confs (butlast confs))))
      ;; save current conf at slot 0
      (push conf confs)
      (alist-set 'winnie-alist frame confs))))

(defvar winnie-traverse-destination-conf nil)

;; in window-state-change-functions this-command has become real-last-command, this-command is nil
(defun winnie-window-state-change (&optional frame-or-window)
  (interactive)
  (unless (winnie-command-p winnie-real-this-command)
    (when winnie-traverse-destination-conf
      (winnie-save winnie-traverse-destination-conf)
      (setq winnie-traverse-destination-conf nil))
    (winnie-save))
  ;; (notify (format "win-change last-cmd %s this-cmd %s" winnie-real-last-command winnie-real-this-command)
  ;;         (let (result)
  ;;           (dolist (conf (cdr (assoc (selected-frame) winnie-alist)))
  ;;             (setq result (concat result "\n" (format "%s" conf))))
  ;;           result)
  ;;         :timeout 25000)
  )

(defun alist-set (alist-symbol key value)
  "Set KEY to VALUE in alist ALIST-SYMBOL."
  (set alist-symbol
       (cons (cons key value)
             (assoc-delete-all key (eval alist-symbol)))))

(defun winnie-restore-relative (step)
  (let* ((confs (cdr (assoc (selected-frame) winnie-alist)))
         (winnie-length (length confs)))
    (when confs
      (setq winnie-traverse-position
            (mod (cl-incf winnie-traverse-position step) winnie-length))
      (winnie-restore confs winnie-traverse-position))))

(defun winnie-command-p (cmd)
  (or (equal cmd 'winnie-undo)
      (equal cmd 'winnie-redo)))

(defun winnie-undo ()
  (interactive)
  (when winnie-alist
    (winnie-clean-confs)
    (unless (winnie-command-p winnie-real-last-command)
      (when winnie-traverse-destination-conf
        ;; update so the current is always 0
        (winnie-save winnie-traverse-destination-conf)
        (setq winnie-traverse-destination-conf nil))
      (setq winnie-traverse-position 0))
    (winnie-restore-relative 1)))

(defun winnie-redo ()
  (interactive)
  (if (winnie-command-p winnie-real-last-command)
      (winnie-restore-relative -1)
    (message "Winnie undo not started.")))

(defvar winnie-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(control c) left] #'winnie-undo)
    (define-key map [(control c) right] #'winnie-redo)
    map)
  "Keymap for winnie mode.")

(defun winnie-keyboard-quit (&rest arg)
  "Abort window conf jumping, and go back to conf before jump."
  (when (winnie-command-p real-last-command)
    (winnie-restore (cdr (assoc (selected-frame) winnie-alist)) 0)))

(defun winnie-record-command ()
  (setq winnie-real-last-command real-last-command)
  (setq winnie-real-this-command real-this-command))

;;;###autoload
(define-minor-mode winnie-mode
  "Toggle winnie mode on or off.

winnie mode is a global minor mode that records the changes in the window configuration,
i.e. how the frames are partitioned into windows)."
  :global t
  (if winnie-mode
      (progn
        (winnie-save)
        ;; hook can not be added to window-state-change-hook via winnie-mode enabled in after-init-hook
        (add-hook 'window-state-change-functions 'winnie-window-state-change)
        (add-hook 'pre-command-hook 'winnie-record-command)
        (advice-add #'keyboard-quit :before #'winnie-keyboard-quit))
    (remove-hook 'window-state-change-functions 'winnie-window-state-change)
    (remove-hook 'pre-command-hook 'winnie-record-command)
    (advice-remove #'keyboard-quit #'winnie-keyboard-quit)))

(provide 'winnie)
;;; winnie.el ends here
