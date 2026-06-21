;;; shackle.el --- Enforce rules for popups  -*- lexical-binding: t; -*-
;; Author: Vasilij Schneidermann <mail@vasilij.de>
;; URL: https://depp.brause.cc/shackle

(require 'cl-lib)
(require 'cl-extra)

(defgroup shackle nil
  "Enforce rules for popups"
  :group 'convenience
  :prefix "shackle-")

(defcustom shackle-select-reused-windows nil
  "Make Emacs select reused windows by default?
When t, select every window that is already displaying the buffer
after attempting to display its buffer again by default,
otherwise only do that if the :select keyword is present."
  :type 'boolean
  :group 'shackle)

(defcustom shackle-inhibit-window-quit-on-same-windows nil
  "Make Emacs inhibit quitting same windows by default?
When t, a buffer that has been displayed by switching to it in
the same window is exempt from `quit-window' closing its window,
otherwise only do that if the :inhibit-window-quit keyword is
present."
  :type 'boolean
  :group 'shackle)

(defvar shackle-default-alignment 'below
  "Default alignment of window relative to the selected window.
It may be one of the following values: 'above, 'below, 'left, 'right.
Or use a <function> with no arguments to determine side, must return one of the above four values.")

(defcustom shackle-default-size 0.5
  "Default size of aligned windows.
A floating point number between 0 and 1 is interpreted as a
ratio.  An integer equal or greater than 1 is interpreted as a
number of lines. If a function is specified, it is called with
zero arguments and must return a number of the above two types."
  :type '(choice (integer :tag "Number of lines")
                 (float :tag "Number of lines (ratio)")
                 (function :tag "Custom"))
  :group 'shackle)

;; hide *Warnings* buffer
;; (add-to-list 'display-buffer-alist '("*Warnings*" . nil))
;; if mutiple match, rule at the front takes precedence
(defvar shackle-default-rules
  '(;; embark buffer doens't have fixed buffer name or major mode
    ;; embark--verbose-indicator-buffer
    ("\\*Embark Actions\\*" :override-shackle)
    ("\\`\\*Warnings.*?\\*\\'" :ignore t)
    (" \\*LV\\*" :override-shackle)
    ("\\*Ediff Control Panel\\*" :override-shackle)
    ("^\\*Ilist\\*$" :override-shackle)
    (dired-mode :custom shackle--custom-display-dired
                :align left :select t))
  "Association list of rules what to do with windows.
Each rule consists of a condition and a property list.  The
condition can be a symbol, a string or a list of either type.  If
it's a symbol, match the buffer's major mode.  If it's a string,
match the name of the buffer.  A list of symbols or strings
requires a match of any element as described earlier for its
type.  Use the following option in the property list to use
regular expression matching on a buffer name:

:regexp and t

As a special case, a list of the (:custom function) form
will call the supplied predicate with the buffer to be displayed
as value and be interpreted as a match for a non-nil return value.

A default rule can be set up with `shackle-default-rule'.
To make an exception to `shackle-default-rule', use the condition
you want to exclude and either not use the key in question, use a
different value or use a placeholder as key.

The property list accepts the following keys and values:

:select and t

Make sure the window that popped up is selected afterwards.
Customize `shackle-select-reused-windows' to make this the
default for windows already displaying the buffer.

:custom and a function name or lambda

Override with a custom action.  Takes a function as argument
which is called with BUFFER-OR-NAME, ALIST and PLIST as argument
and must return the window to be displayed or nil to inhibit its
display.  This mode of operation allows you to pick one of the
existing actions, but by your own conditions.

:inhibit-window-quit and t

Modify the behavior of `quit-window' to not delete the window.
This option is recommended in combination with :same, but can be
used with other keys like :other as well.  Customize
`shackle-inhibit-window-quit-on-same-windows' to make this the
default for every buffer that was displayed by switching to it in
the same window.

:ignore and t

Ignore the request of displaying a buffer completely.  Note that
this does *not* inhibit preceding actions such as creation or
update of the buffer in question.

:other and t

Reuse the other window if there's more than one window open,
otherwise pop up a new window.  Can be used with :popup-frame to do the
equivalent with the other frame and a new frame.

:popup and t

Pop up a new window instead of reusing the current one.

:same and t

Don't pop up any window and reuse the currently active one.

:align and t or either of 'above, 'below, 'left and 'right

Align the popped up window at any of the specified sides or the
default size (see `shackle-default-alignment') by splitting the
root window.

Additionally to that, one can use a function called with zero
arguments that must return any of the above alignments.

:size and a number greater than zero

Use this option to specify a different size than the default
value of 0.5 (see `shackle-default-size').

:popup-frame and t

Pop to a frame instead of window.")
(defvar shackle-default-rule '(:align right :select t :default t))
(defvar shackle-rules shackle-default-rules)
(setq pop-up-windows nil)

(defcustom shackle-display-buffer-popup-frame-function
  'shackle--display-buffer-popup-frame
  "Handler function for `:popup-frame t'.

This function will receive the same (BUFFER ALIST PLIST) as
`shackle-display-buffer', which see.  It should return the window
displaying BUFFER, or 'fail if it hasn't displayed it."
  :type 'function
  :group 'shackle)

(defun shackle-match (buffer-or-name)
  "Check whether BUFFER-OR-NAME is any rule match.
If there is a match, it returns a property list which
`shackle-display-buffer-action' use."
  (let* ((buffer (get-buffer buffer-or-name)))
    (cl-loop for (condition . plist) in shackle-rules
             when (cond ((symbolp condition)
                         (eq condition (buffer-local-value 'major-mode buffer)))
                        ((stringp condition)
                         (string-match condition (buffer-name buffer)))
                        (t nil))
             return plist
             finally return shackle-default-rule)))

;; dired-kill-when-opening-new-dired-buffer happens before shackle
;; dired: O to open in other window, Enter in the same window
(setq dired-kill-when-opening-new-dired-buffer nil)

(defun shackle--custom-display-dired (buffer-or-name alist plist)
  "Act like dired-kill-when-opening-new-dired-buffer is t.
not using dired-kill-when-opening-new-dired-buffer, because it kills window
before shackle get into action."
  (let ((old-buffer (current-buffer))
        window)
    (setq window
          ;; pop-up-windows is used inswitch-to-buffer-other-window
          (cond ((or pop-up-windows
                     (cdr (assoc 'inhibit-same-window alist))
                     current-prefix-arg)
                 (shackle--display-buffer-aligned-window buffer-or-name alist plist))
                (t (shackle--display-buffer-same buffer-or-name alist))))
    ;; (if (or revert-buffer-in-progress-p
    ;;         (and dired-kill-old-on-new-buffer (equal major-mode 'dired-mode)
    ;;              (not (string-equal "*Fd*" (buffer-name)))))
    ;;     (progn
    ;;       (setq window (shackle--display-buffer-same buffer-or-name alist))
    ;;       (remove-hook 'kill-buffer-hook 'shackle--delete-window t)
    ;;       (unless (eq (get-buffer buffer-or-name) old-buffer)
    ;;         (kill-buffer old-buffer)))
    ;;   (setq window
    ;;         (shackle--display-buffer-aligned-window buffer-or-name alist plist)))

    (and window
         (unless jit-lock-defer-buffers
           (when (< (window-total-width window)
                    (window-size (frame-root-window) t))
             (with-selected-window window
               (call-interactively 'move-end-of-line)))))
    window))

;; shackle plist key can be put in the action argument of pop-to-buffer etc.
;; eg.
;; (action '(shackle-display-buffer-action
;;           (align . right)
;;           (inhibit-same-window . t)))

;; shackle action is the user-action that is handled first in display-buffer
;; if shackle action fails to provide a window, display-buffer moves on to next action
(defvar shackle--bypass-shackle-keys
  '(window window-width window-height same-window
           side slot dedicated direction
           some-window same-window same-frame
           previous-window lru-frames lru-time bump-use-time allow-no-window))

;; 3 ways to override: 1. let override-shackle to t, 2. ((override-shackle . t)) alist 3. (:override-shackle) in shackle-rules
(defvar override-shackle nil
  "flag variable for overriding shackle in a function. use plist for overriding shackle rules.")

(defun shackle-override-advice (orig-fun &rest args)
  (let ((override-shackle t))
    (apply orig-fun args)))

(defun shackle-switch-to-buffer-advice (orig-fun &rest args)
  (if (called-interactively-p 'any)
      (let ((switch-to-buffer-obey-display-actions nil))
        (apply orig-fun args))
    (apply orig-fun args)))

;; other alist keys:
;; window-min-height window-min-width inhibit-switch-frame
;; mode pop-up-frame-parameters child-frame-parameters inhibit-same-window

;; ref:
;; display-buffer-in-atom-window special-display-popup-frame window--pop-up-frames
;; display-buffer-use-some-frame display-buffer-reuse-window display-buffer-reuse-mode-window
;; display-buffer-pop-up-frame display-buffer-in-child-frame
;; window--try-to-split-window-in-direction display-buffer-in-previous-window
;; display-buffer--lru-window display-buffer-use-some-window display-buffer-use-least-recent-window
;; display-buffer-no-window

;; inhibit-same-window is set in display-buffer when action is t
;; override-shackle example:
;; (display-buffer-use-some-window buffer '((lambda (buffer alist) nil) . ((override-shackle . t))))
;; (cdr (assoc 'override-shackle alist))
;; dired find file alist is ((inhibit-same-window))
(defun shackle-display-buffer-action (buffer alist)
  "Execute an action for BUFFER according to `shackle-rules'.
This uses `shackle-display-buffer' internally, BUFFER and ALIST
take the form `display-buffer-alist' specifies."
  (let ((shackle--display-plist (shackle-match buffer)))
    (unless (or override-shackle
                (plist-member shackle--display-plist :override-shackle)
                (plist-get shackle--display-plist :ignore)
                (cl-some (lambda (key) (assoc key alist)) shackle--bypass-shackle-keys))
      (let* ((shackle-previous-window (selected-window))
             (window (shackle-display-buffer buffer alist shackle--display-plist)))
        ;; (message "shackle buffer %s\n alist: %s\n plist: %s" buffer alist shackle--display-plist)
        (if (and (or (plist-get shackle--display-plist :select)
                     (alist-get 'select alist))
                 (window-live-p window))
            (select-window window)
          (when (window-live-p shackle-previous-window)
            (select-window shackle-previous-window)))
        window))))

(defun shackle--frame-splittable-p (frame)
  "Return FRAME if it is splittable."
  (when (and (window--frame-usable-p frame)
             (not (frame-parameter frame 'unsplittable)))
    frame))

(defun shackle--splittable-frame (&optional frame)
  "Return a splittable frame to work on.
This can be either the selected frame or the last frame that's
not displaying a lone minibuffer."
  (if frame
      (shackle--frame-splittable-p frame)
    (or (shackle--frame-splittable-p (selected-frame))
        (shackle--frame-splittable-p (last-nonminibuffer-frame)))))

(defun shackle--split-some-window (frame alist)
  "Return a window if splitting any window was successful.
This function tries using the largest window on FRAME for
splitting, if all windows are the same size, the selected one is
taken, in case this fails, the least recently used window is used
for splitting.  ALIST is passed to `window--try-to-split-window'
internally."
  (or (window--try-to-split-window (get-largest-window frame t) alist)
      (window--try-to-split-window (get-lru-window frame t) alist)))

(defun shackle--inhibit-window-quit (window)
  "Keep `quit-window' in WINDOW from deleting the window."
  (set-window-parameter window 'quit-restore nil))

(defun shackle--window-display-buffer (buffer window type alist)
  "Compatibility wrapper for `window--display-buffer'.
Displays BUFFER in WINDOW, considering TYPE and ALIST. This
accounts for the changed meaning of the former DEDICATED argument
which has been dropped in Emacs 27.  Considering that this
package never supported marking a window as dedicated and earlier
Emacsen just passed `display-buffer-mark-dedicated' for its
value, it's safe to just omit that argument if not necessary."
  (if (version< emacs-version "27")
      (window--display-buffer buffer window type alist
                              display-buffer-mark-dedicated)
    (window--display-buffer buffer window type alist)))

(defun shackle--display-buffer-reuse (buffer alist)
  "Attempt reusing a window BUFFER is already displayed in.
ALIST is passed to `display-buffer-reuse-window' internally.  If
`shackle-select-reused-windows' is t, select the window
afterwards."
  (let ((window (display-buffer-reuse-window buffer alist)))
    (prog1 window
      (when (and window (window-live-p window)
                 shackle-select-reused-windows)
        (select-window window)))))

(defun shackle--display-buffer-same (buffer alist)
  "Display BUFFER in the currently selected window.
ALIST is passed to `shackle--window-display-buffer' internally."
  (unless (window-minibuffer-p)
    (let ((window (shackle--window-display-buffer
                   buffer (selected-window) 'reuse alist)))
      (prog1 window
        (when shackle-inhibit-window-quit-on-same-windows
          (shackle--inhibit-window-quit window))))))

(defun shackle--display-buffer-popup-frame (buffer alist plist)
  "Display BUFFER in a popped up frame.
ALIST is passed to `shackle--window-display-buffer' internally.
If PLIST contains the :other key with t as value, reuse the next
available frame if possible, otherwise pop up a new frame."
  (let* ((params (cdr (assq 'pop-up-frame-parameters alist)))
         (pop-up-frame-alist (append params pop-up-frame-alist))
         (fun pop-up-frame-function))
    (when fun
      (let* ((frame (if (and (plist-get plist :other)
                             (> (length (frames-on-display-list)) 1))
                        (next-frame nil 'visible)
                      (funcall fun)))
             (window (frame-selected-window frame)))
        (prog1 (shackle--window-display-buffer
                buffer window 'frame alist)
          (unless (cdr (assq 'inhibit-switch-frame alist))
            (window--maybe-raise-frame frame)))))))

(defvar shackle-last-buffer nil
  "Last buffer displayed with shackle.")

(defvar shackle-last-window nil
  "Last window displayed with shackle.")

(defun shackle--display-buffer-popup-window (buffer alist plist)
  "Display BUFFER in a popped up window.
ALIST is passed to `shackle--window-display-buffer' internally.
If PLIST contains the :other key with t as value, reuse the next
available window if possible."
  (let ((frame (shackle--splittable-frame)))
    (when frame
      (let ((window (if (and (plist-get plist :other) (not (one-window-p)))
                        (next-window nil 'nominibuf)
                      (shackle--split-some-window frame alist))))
        (prog1 (shackle--window-display-buffer
                buffer window 'window alist)
          (when window
            (setq shackle-last-window window
                  shackle-last-buffer buffer))
          (unless (cdr (assq 'inhibit-switch-frame alist))
            (window--maybe-raise-frame (window-frame window))))))))

(defun shackle--delete-window ()
  (when (> (count-windows) 1)
    (delete-window)))

(defun shackle--display-buffer-aligned-window (buffer alist plist)
  "Display BUFFER in an aligned window.
ALIST is passed to `shackle--window-display-buffer' internally.
Optionally use a different alignment and/or size if PLIST
contains the :alignment key with an alignment different than the
default one in `shackle-default-alignment' and/or PLIST contains
the :size key with a number value."
  (let ((frame (shackle--splittable-frame))
        (prefer-same (plist-get plist :prefer-same))
        window type)
    (when frame
      (if (and prefer-same
               ;; dired-find-file-other-window
               (not (cdr (assoc 'inhibit-same-window alist)))
               (equal frame (selected-frame)))
          (setq window (selected-window)
                type 'reuse)
        (let* ((alignment-argument (or (plist-get plist :align)
                                       (alist-get 'align alist)))
               (alignments '(above below left right))
               (alignment (cond
                           ((functionp alignment-argument)
                            (funcall alignment-argument))
                           ((memq alignment-argument alignments)
                            alignment-argument)
                           ((functionp shackle-default-alignment)
                            (funcall shackle-default-alignment))
                           (t shackle-default-alignment)))
               (horizontal (when (memq alignment '(left right)) t))
               (frame-size (window-size (frame-root-window) horizontal))
               ;; (window-min-size (if horizontal window-min-width window-min-height))
               (window-max-size (round (* 0.7 frame-size)))
               (selected-window-size (if horizontal (window-total-width (selected-window))
                                       (window-total-height (selected-window))))
               (size (or (plist-get plist :size)
                         (if (functionp shackle-default-size)
                             (funcall shackle-default-size)
                           (round (* shackle-default-size frame-size))))))
          (when (plist-get plist :clear)
            (ignore-errors (delete-other-windows)))
          ;; if a split already exist
          (if (< selected-window-size frame-size)
              (progn
                (if (> selected-window-size window-max-size)
                    (setq window (selected-window)
                          type 'reuse)
                  (setq window (next-window window 0)))
                (when (window-dedicated-p window)
                  (setq window (next-window window 0))))
            (setq window (split-window (frame-root-window frame) size alignment)
                  type 'window))))
      (when window
        (prog1 (shackle--window-display-buffer buffer window type alist)
          (setq shackle-last-window window
                shackle-last-buffer buffer)
          (when (plist-get plist :kill-window)
            (with-selected-window window
              (add-hook 'kill-buffer-hook 'shackle--delete-window 0 t)))
          (unless (cdr (assq 'inhibit-switch-frame alist))
            (window--maybe-raise-frame frame)))))))

(defun shackle--display-buffer (buffer alist plist)
  "Internal function for `shackle-display-buffer'.
Displays BUFFER according to ALIST and PLIST."
  (cond
   ((plist-get plist :custom)
    (let* ((action (plist-get plist :custom))
           (window (funcall action buffer alist plist)))
      (when (and window (not (windowp window)))
        (user-error "Custom action didn't return window: %S %S" window action))
      window))
   ((shackle--display-buffer-reuse buffer alist))
   ((or (plist-get plist :align)
        (alist-get 'align alist))
    (shackle--display-buffer-aligned-window buffer alist plist))
   ((plist-get plist :maximize)
    (display-buffer-full-frame buffer alist))
   ((or (plist-get plist :same)
        ;; there is `display-buffer--same-window-action' which things
        ;; like `info' use to reuse the currently selected window, it
        ;; happens to be of the (inhibit-same-window . nil) form and
        ;; should be permitted unless a popup is requested
        (and (not (plist-get plist :popup))
             (and (assq 'inhibit-same-window alist)
                  (not (cdr (assq 'inhibit-same-window alist))))))
    (shackle--display-buffer-same buffer alist))
   ((plist-get plist :popup-frame)
    (funcall shackle-display-buffer-popup-frame-function buffer alist plist))
   (t (shackle--display-buffer-popup-window buffer alist plist))))

(defun shackle-display-buffer (buffer alist plist)
  "Display BUFFER according to ALIST and PLIST.
See `display-buffer-pop-up-window' and
`display-buffer-pop-up-frame' for the basic functionality the
majority of code was lifted from.  Additionally to BUFFER and
ALIST this function takes an optional PLIST argument which allows
it to do useful things such as selecting the popped up window
afterwards and/or inhibiting `quit-window' from deleting the
window."
  (save-excursion
  (let* ((ignore-window-parameters t)
         (window (shackle--display-buffer buffer alist plist)))
    (when (plist-get plist :inhibit-window-quit)
      (shackle--inhibit-window-quit window))
    window)))

;; intercept all display-buffer call in case display-buffer is called with action, as in display-warning
(defun shackle--display-buffer-advice (orig-fun buffer-or-name &optional action frame)
  (let ((alist (cdr action)))
        (or (shackle-display-buffer-action buffer-or-name alist)
            (funcall orig-fun buffer-or-name action frame))))

;; (setq display-buffer-alist
;;       (cons '("*" shackle-display-buffer-action)
;;             display-buffer-alist))
;;;###autoload
(define-minor-mode shackle-mode
  "Toggle `shackle-mode'.
This global minor mode allows you to easily set up rules for
popups in Emacs."
  :global t
  (if shackle-mode
      (progn
        (with-eval-after-load 'consult
          (advice-add 'consult-buffer :around #'shackle-switch-to-buffer-advice))
        (with-eval-after-load 'exwm-workspace
          (advice-add 'exwm-workspace-switch-to-buffer :around #'shackle-switch-to-buffer-advice))
        (advice-add 'switch-to-buffer :around #'shackle-switch-to-buffer-advice)
        (with-eval-after-load 'button
          (advice-add 'button-activate :around 'shackle-override-advice))
        (advice-add 'display-buffer :around 'shackle--display-buffer-advice))
    (with-eval-after-load 'consult
      (advice-remove 'consult-buffer #'shackle-switch-to-buffer-advice))
    (with-eval-after-load 'exwm-workspace
      (advice-remove 'exwm-workspace-switch-to-buffer #'shackle-switch-to-buffer-advice))
    (advice-remove 'switch-to-buffer #'shackle-switch-to-buffer-advice)
    (with-eval-after-load 'button
      (advice-remove 'button-activate 'shackle-override-advice))
    (advice-remove 'display-buffer 'shackle--display-buffer-advice)))

;; debugging support
(require 'trace)

(defcustom shackle-trace-buffer "*shackle trace*"
  "Name of the buffer for tracing `shackle-traced-functions'."
  :type 'string
  :group 'shackle)

(defcustom shackle-traced-functions
  '(display-buffer
    pop-to-buffer
    pop-to-buffer-same-window
    switch-to-buffer-other-window
    switch-to-buffer-other-frame)
  "List of `display-buffer'-style functions to trace."
  :type '(list function))

(defun shackle-trace-functions ()
  "Enable tracing `shackle-traced-functions'."
  (interactive)
  (dolist (function shackle-traced-functions)
    (trace-function-background function shackle-trace-buffer)))

(defun shackle-untrace-functions ()
  "Enable tracing `shackle-traced-functions'."
  (interactive)
  (dolist (function shackle-traced-functions)
    (untrace-function function)))

(provide 'shackle)
;;; shackle.el ends here
