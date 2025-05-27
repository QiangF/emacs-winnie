;;; beacon.el --- Highlight the cursor whenever the window scrolls  -*- lexical-binding: t; -*-
;; URL: https://github.com/Malabarba/beacon

(require 'cl-lib)
(require 'seq)
(require 'faces)

(defgroup beacon nil
  "Customization group for beacon."
  :group 'emacs
  :prefix "beacon-")

(defvar beacon-timer nil)

(defcustom beacon-blink-duration 0.3
  "Time, in seconds, that the blink should last."
  :type 'number)

(defcustom beacon-blink-delay 0.3
  "Time, in seconds, before starting to fade the beacon."
  :type 'number)

(defcustom beacon-size 40
  "Size of the beacon in characters."
  :type 'number)

(defcustom beacon-color 0.5
  "Color of the beacon.
This can be a string or a number.

If it is a number, the color is taken to be white or
black (depending on the current theme's background) and this
number is a float between 0 and 1 specifing the brightness.

If it is a string, it is a color name or specification,
e.g. \"#666600\"."
  :type '(choice number color))

(defface beacon-fallback-background
  '((((class color) (background light)) (:background "black"))
    (((class color) (background dark)) (:background "white")))
  "Fallback beacon background color.
Used in cases where the color can't be determined by Emacs.
Only the background of this face is used.")

(defvar beacon-dont-blink-predicates nil
  "A list of predicates that prevent the beacon blink.
These predicate functions are called in order, with no
arguments, before blinking the beacon.  If any returns
non-nil, the beacon will not blink.

For instance, if you want to disable beacon on buffers where
`hl-line-mode' is on, you can do:

    (add-hook \\='beacon-dont-blink-predicates
              (lambda () (bound-and-true-p hl-line-mode)))")

(defun beacon--compilation-mode-p ()
  "Non-nil if this is some form of compilation mode."
  (or (derived-mode-p 'compilation-mode)
      (bound-and-true-p compilation-minor-mode)))

(add-hook 'beacon-dont-blink-predicates #'window-minibuffer-p)
(add-hook 'beacon-dont-blink-predicates #'beacon--compilation-mode-p)

(defcustom beacon-dont-blink-major-modes '(t magit-status-mode magit-popup-mode
                                             inf-ruby-mode
                                             mu4e-headers-mode
                                             gnus-summary-mode gnus-group-mode)
  "A list of major-modes where the beacon won't blink.
Whenever the current buffer satisfies `derived-mode-p' for
one of the major-modes on this list, the beacon will not
blink."
  :type '(repeat symbol))

(defcustom beacon-dont-blink-commands '(next-line previous-line forward-line)
  "A list of commands that should not make the beacon blink.
Use this for commands that scroll the window in very
predictable ways, when the blink would be more distracting
than helpful.."
  :type '(repeat symbol))

(defcustom beacon-before-blink-hook nil
  "Hook run immediately before blinking the beacon."
  :type 'hook)

;;; Overlays
(defvar beacon-ovs nil)

(defconst beacon-overlay-priority (/ most-positive-fixnum 2)
  "Priotiy used on all of our overlays.")

(defun beacon--make-overlay (length &rest properties)
  "Put an overlay at point over LENGTH columns.

Specify background color in PROPERTIES."
  (let ((ov (make-overlay (point) (+ length (point)))))
    (overlay-put ov 'beacon t)
    ;; Our overlay is very temporary, so we take the liberty of giving
    ;; it a high priority.
    (overlay-put ov 'priority beacon-overlay-priority)
    (overlay-put ov 'window (selected-window))
    (while properties
      (overlay-put ov (pop properties) (pop properties)))
    (push ov beacon-ovs)
    ov))

(defun beacon--colored-overlay (color)
  "Put an overlay at point with background COLOR."
  (beacon--make-overlay 1 'face (list :background color)))

(defun beacon--ov-put-after-string (overlay colors)
  "Add an after-string property to OVERLAY.
The property's value is a string of spaces with background
COLORS applied to each one.
If COLORS is nil, OVERLAY is deleted!"
  (if (not colors)
      (when (overlayp overlay)
        (delete-overlay overlay))
    (overlay-put overlay 'beacon-colors colors)
    (overlay-put overlay 'after-string
                 (propertize
                  (mapconcat (lambda (c) (propertize " " 'face (list :background c)))
                             colors
                             "")
                  'cursor 1000))))

(defun beacon--visual-current-column ()
  "Get the visual column we are at.

Take long lines and visual line mode into account."
  (save-excursion
    (let ((current (point)))
      (beginning-of-visual-line)
      (- current (point)))))

(defun beacon--after-string-overlay (colors)
  "Put an overlay at point with an after-string property.
The property's value is a string of spaces with background
COLORS applied to each one."
  ;; The after-string must not be longer than the remaining columns
  ;; from point to right window-end else it will be wrapped around.
  (let ((colors (seq-take colors (- (window-width) (beacon--visual-current-column) 1))))
    (beacon--ov-put-after-string (beacon--make-overlay 0) colors)))

(defun beacon--ov-at-point ()
  "Return beacon overlay at current point."
  (car (or (seq-filter (lambda (o) (overlay-get o 'beacon))
                       (overlays-in (point) (point)))
           (seq-filter (lambda (o) (overlay-get o 'beacon))
                       (overlays-at (point))))))

(defun beacon--vanish (&rest _)
  "Turn off the beacon."
  (when (get-buffer-window)
    (mapc #'delete-overlay beacon-ovs)
    (setq beacon-ovs nil)))

;;; Colors
(defun beacon--int-range (a b)
  "Return a list of integers between A inclusive and B exclusive.
Only returns `beacon-size' elements."
  (let ((d (/ (- b a) beacon-size))
        (out (list a)))
    (dotimes (_ (1- beacon-size))
      (push (+ (car out) d) out))
    (nreverse out)))

(defun beacon--color-range ()
  "Return a list of background colors for the beacon."
  (let* ((default-bg (or (save-excursion
                           (unless (eobp)
                             (forward-line 1)
                             (unless (or (bobp) (not (bolp)))
                               (forward-char -1)))
                           (background-color-at-point))
                         (face-background 'default)))
         (bg (color-values (if (or (not (stringp default-bg))
                                   (string-match "\\`unspecified-" default-bg))
                               (face-attribute 'beacon-fallback-background :background)
                             default-bg)))
         (fg (cond
              ((stringp beacon-color) (color-values beacon-color))
              ((and (stringp bg)
                    (< (color-distance "black" bg)
                       (color-distance "white" bg)))
               (make-list 3 (* beacon-color 65535)))
              (t (make-list 3 (* (- 1 beacon-color) 65535))))))
    (when bg
      (apply #'seq-mapn (lambda (r g b) (format "#%04x%04x%04x" r g b))
             (mapcar (lambda (n) (butlast (beacon--int-range (elt fg n) (elt bg n))))
                     [0 1 2])))))

;;; Blinking
(defun beacon--shine ()
  "Shine a beacon at point."
  (let ((colors (beacon--color-range)))
    (save-excursion
      (while colors
        (if (looking-at "$")
            (progn
              (beacon--after-string-overlay colors)
              (setq colors nil))
          (beacon--colored-overlay (pop colors))
          (forward-char 1))))))

(defun beacon--dec ()
  "Decrease the beacon brightness by one."
  (pcase (beacon--ov-at-point)
    (`nil (beacon--vanish))
    ((and o (let c (overlay-get o 'beacon-colors)) (guard c))
     (beacon--ov-put-after-string o (cdr c)))
    (o
     (delete-overlay o)
     (save-excursion
       (while (and (condition-case nil
                       (progn (forward-char 1) t)
                     (end-of-buffer nil))
                   (setq o (beacon--ov-at-point)))
         (let ((colors (overlay-get o 'beacon-colors)))
           (if (not colors)
               (move-overlay o (1- (point)) (point))
             (forward-char -1)
             (beacon--colored-overlay (pop colors))
             (beacon--ov-put-after-string o colors)
             (forward-char 1))))))))

;;;###autoload
(defun beacon-blink ()
  "Blink the beacon at the location of the cursor.
Unlike `beacon--blink-automated', the beacon will blink
unconditionally (even if `beacon-mode' is disabled), and this can
be invoked as a user command or called from Lisp code."
  (interactive)
  (run-hooks 'beacon-before-blink-hook)
  (when (timerp beacon-timer)
    (cancel-timer beacon-timer))
  (beacon--vanish)
  (beacon--shine)
  (setq beacon-timer
        (run-at-time beacon-blink-delay
                     (/ beacon-blink-duration 1.0 beacon-size)
                     #'beacon--dec)))

(defun beacon--blink-automated ()
  "If appropriate, blink the beacon at the location of the cursor.
Unlike `beacon-blink', the blinking is conditioned on a series of
variables: `beacon-mode', `beacon-dont-blink-commands',
`beacon-dont-blink-major-modes', and
`beacon-dont-blink-predicates'."
  ;; Record vars here in case something is blinking outside the
  ;; command loop.
  (unless (or (run-hook-with-args-until-success 'beacon-dont-blink-predicates)
              (seq-find #'derived-mode-p beacon-dont-blink-major-modes)
              (memq (or this-command last-command) beacon-dont-blink-commands))
    (beacon-blink)))

;;; Movement detection
(defun beacon--movement-> (delta-y &optional delta-x)
  "Return non-nil if latest vertical movement is > DELTA-Y.
If DELTA-Y is nil, return nil.
The same is true for DELTA-X and horizonta movement."
  (and ;; Quick check that prevents running the code below in very
       ;; short movements (like typing).
       (if (> (abs (- (point) beacon-pre-command-point-marker)) 2) t
         (beacon--debug "beacon omit movement saving: movement too short" t))
       ;; Col movement.
       (or (and delta-x
                (> (abs (- (current-column)
                           (save-excursion
                             (goto-char beacon-pre-command-point-marker)
                             (current-column))))
                   delta-x))
           ;; Check if the movement was >= DELTA lines by moving DELTA
           ;; lines. `count-screen-lines' is too slow if the movement had
           ;; thousands of lines.
           (save-excursion
             (let ((p (point)))
               (goto-char (min beacon-pre-command-point-marker p))
               (vertical-motion delta-y)
               (> (max p beacon-pre-command-point-marker)
                  (line-beginning-position)))))))

;;; Beacon Marker
(defvar beacon-scrolled-window nil)
(defvar beacon-pre-command-point-marker nil)
;; (defvar beacon-pre-command-mark-list-head nil)
(defvar beacon-pre-command-window nil)
(defvar beacon-pre-command-window-start 0)
(defvar beacon-markers nil)
(make-variable-buffer-local 'beacon-markers)

(defvar beacon-markers-max-num 15
  "Maximum size of overall mark list.  Start discarding off end if gets this big.")

(defvar beacon-mark-traversal-position 0
  "Stores the traversal location within the beacon-mark-list.")

(defvar beacon-last-mark-before-jump nil)

(defcustom beacon-push-mark-threshold 3
  "Should the mark be pushed before long movements?
If nil, `beacon' will not push the mark.
Otherwise this should be a number, and `beacon' will push the
mark whenever point moves more than that many lines."
  :type '(choice integer (const nil)))

(defvar beacon-debug-on nil)
(defun beacon--debug (msg &optional supress)
  (when beacon-debug-on
    (unless supress
      (message "%s" msg))))

(defun beacon--push-mark (&optional location nomsg activate)
  "Handles mark-tracking work for backward-forward.
ignores its arguments LOCATION, NOMSG, ACTIVATE
Uses following steps:
pushes the just-created mark by `push-mark' onto beacon-markers
\(If we exceed beacon-markers-max-num then old marks are pushed off\)

note that perhaps this should establish one mark list per window in the future"
  (when (and (not mark-active)
             (not (beacon--backward-forward-command-p this-command)))
    (let* ((marker (copy-marker (or location (point))))
           (ind (beacon--find-index-for-mark marker beacon-markers))
           (history-delete-duplicates nil))
      (if ind
          (setq beacon-markers (beacon--nth-delq ind beacon-markers))
        (when (> (length beacon-markers) beacon-markers-max-num)
          ;;purge excess entries from the end of the list
          ;; (when (> (length beacon-markers) beacon-markers-max-num)
          ;;   (move-marker (car (nthcdr beacon-markers-max-num beacon-markers)) nil)
          ;;   (setcdr (nthcdr (1- beacon-markers-max-num) beacon-markers) nil))
          (set-marker (car (last beacon-markers)) nil)
          (setq beacon-markers (butlast beacon-markers))))
      (beacon--debug (format "beacon: save marker %s" marker))
      ;; (setq beacon-markers (cons (copy-marker marker) beacon-markers))
      ;; (add-to-history 'beacon-markers (copy-marker marker) beacon-markers-max-num)
      (push marker beacon-markers))))

(defun beacon--nth-delq (n list-in)
  (delq (setcar (nthcdr n list-in) (gensym)) list-in))

(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun beacon-increase-mark-position (&optional step)
  "Used to navigate to the previous location on beacon-mark-list.
1. Increments beacon-mark-traversal-position
2. Jumps to the mark at that position
Borrows code from `pop-global-mark'."
  (interactive)
  (when beacon-markers
    (let ((message-log-max nil)
          (point-before-jump (point)))
      (cl-incf beacon-mark-traversal-position step)
      (setq beacon-mark-traversal-position (mod beacon-mark-traversal-position (length beacon-markers)))
      (goto-char (elt beacon-markers beacon-mark-traversal-position))
      (when (equal point-before-jump (point))
        (cl-incf beacon-mark-traversal-position step)
        (setq beacon-mark-traversal-position (mod beacon-mark-traversal-position (length beacon-markers)))
        (goto-char (elt beacon-markers beacon-mark-traversal-position)))
      (message "beacon-mark-position: %s" beacon-mark-traversal-position))))

(defun beacon--find-index-for-mark (marker1 marker-list)
  (catch 'found
    (dotimes (ind (length marker-list))
      (let ((marker2 (nth ind marker-list)))
        (when (equal (marker-position marker1)
                     (marker-position marker2))
          (throw 'found ind))))))

(defun beacon--backward-forward-command-p (cmd)
  (or (equal cmd 'beacon-backward-forward-previous)
      (equal cmd 'beacon-backward-forward-next)))

(defun beacon-backward-forward-next ()
  "A `beacon-increase-mark-position' wrap for skip invalid locations."
  (interactive)
  ;; (message "this command %s" this-command)
  (when (not (beacon--backward-forward-command-p last-command))
    (setq beacon-mark-traversal-position 0))
  (beacon-increase-mark-position -1))

(defun beacon-backward-forward-previous ()
  "A `beacon-increase-mark-position' wrap for skip invalid locations."
  (interactive)
  (when (not (beacon--backward-forward-command-p last-command))
    (setq beacon-mark-traversal-position -1))
  (beacon-increase-mark-position 1))

(defun beacon--scroll-command-p (cmd)
  (or (equal cmd 'scroll-up-command)
      (equal cmd 'scroll-down-command)))

(defun beacon--pre-command ()
  "Record some variables for interal use."
  (setq beacon-pre-command-point-marker (point-marker))
  ;; (setq beacon-pre-command-mark-list-head (car mark-ring))
  (setq beacon-pre-command-window (selected-window))
  (setq beacon-pre-command-window-start (window-start))
  (when (and (beacon--backward-forward-command-p this-command)
             (not (beacon--backward-forward-command-p last-command)))
    ;; first invocation of beacon-backward-forward, save point-marker, but not in mark ring
    (setq beacon-last-mark-before-jump beacon-pre-command-point-marker)))

(defun beacon--post-command ()
  "Blink if point moved very far."
  (unless (minibufferp)
    ;; (message "beacon--push-mark %S" marker)
    ;; based on push-mark
    (if (and (equal this-command 'keyboard-quit)
             (beacon--backward-forward-command-p last-command)
             beacon-last-mark-before-jump)
        ;; quick way to get back
        (progn (goto-char beacon-last-mark-before-jump)
               (beacon--debug "beacon: quit")
               (beacon-blink))
      (cond
       ;; Blink for switching buffers.
       ((not (eq (marker-buffer beacon-pre-command-point-marker)
                 (current-buffer)))
        (beacon--debug "beacon: switching buffer")
        (beacon--blink-automated)
        (beacon--push-mark))
       ;; Blink for switching windows.
       ((not (eq beacon-pre-command-window (selected-window)))
        (beacon--debug "beacon: switching window")
        (beacon--blink-automated)
        (beacon--push-mark))
       ;; Blink for movement, same window, same buffer
       ((beacon--movement-> beacon-push-mark-threshold)
        (if (and beacon-scrolled-window
                 (equal beacon-scrolled-window (selected-window)))
            (progn
              (beacon--debug "beacon: window scroll")
              ;; Blink for scrolling
              (beacon--blink-automated)
              (unless (beacon--scroll-command-p last-command)
                (beacon--push-mark)))
          (beacon--debug "beacon: row movement")
          (beacon--blink-automated)
          (beacon--push-mark)))))
    (setq beacon-scrolled-window nil)))

(defun beacon--window-scroll-function (window start-pos)
  "Blink the beacon or record that WINDOW has been scrolled.
If invoked during the command loop, record the current window so
that it may be blinked on post-command.  This is because the
scrolled window might not be active, but we only know that at
`post-command-hook'.

If invoked outside the command loop, `post-command-hook' would be
unreliable, so just blink immediately."
  (unless (and (equal beacon-pre-command-window-start start-pos)
               (equal beacon-pre-command-window window))
    (if this-command
        (setq beacon-scrolled-window window)
      (setq beacon-scrolled-window nil))))

;;; Minor-mode
;;;###autoload
(define-minor-mode beacon-mode
  nil :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "<C-left>") #'beacon-backward-forward-previous)
            (define-key map (kbd "<C-right>") #'beacon-backward-forward-next)
            map)
  (if beacon-mode
      (progn
        (beacon--pre-command)
        ;; push-mark might be called several times in a command
        (advice-add 'push-mark :after #'beacon--push-mark)
        (add-function :after after-focus-change-function #'beacon--blink-automated)
        (add-hook 'window-scroll-functions #'beacon--window-scroll-function)
        (add-hook 'pre-command-hook #'beacon--pre-command)
        (add-hook 'post-command-hook #'beacon--post-command)
        (add-hook 'before-change-functions #'beacon--vanish))

    (advice-remove 'push-mark #'beacon--push-mark)
    (remove-function after-focus-change-function #'beacon--blink-automated)
    (remove-hook 'window-scroll-functions #'beacon--window-scroll-function)
    (remove-hook 'post-command-hook #'beacon--post-command)
    (remove-hook 'pre-command-hook #'beacon--pre-command)
    (remove-hook 'before-change-functions #'beacon--vanish)))

(provide 'beacon)
;;; beacon.el ends here
