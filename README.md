Emacs winner-mode conceals complicated manipulation of window configurations, and has unpredictable behaviors. This package saves window configuration in plain text, and a lot simpler.

Put the file on your load-path. Example config:

```
(use-package winnie
  :ensure nil
  :bind* (("C-9" . winnie-redo)
          ("C-8" . winnie-undo))
  :hook after-init)
```

You can also try winnie's sister package. I modified beacon to save point history when it blinks, and bookmark-view is more explicit than winnie-mode, and easier to use than bookmarks or registers.

```
(use-package beacon
  :ensure nil
  :bind* (("<C-i>" . beacon-backward-forward-previous)
          ("C-o" . beacon-backward-forward-next)
          ("M-\"" . beacon-blink))
  :hook after-init
  :config
  (setq beacon-color "#00f600"
        beacon-blink-delay 0.4)
  (advice-add 'pop-global-mark :around
              (defun my/pop-global-mark-display-buffer (pgm)
                (interactive)
                (cl-letf (((symbol-function 'switch-to-buffer)
                           #'pop-to-buffer))
                  (funcall pgm)))))
```

```
(use-package bookmark-view
  :ensure nil
  :commands my-bookmark-view-create
  :bind*
  (("M-8" . my-bookmark-view-previous)
   ("M-9" . my-bookmark-view-next)
   ("M-0" . my-bookmark-view-dwim)
   ("M-1" . my-bookmark-view)
   ("M-2" . my-bookmark-view)
   ("M-3" . my-bookmark-view)
   ("M-4" . my-bookmark-view)
   ("M-5" . my-bookmark-view)
   ("M-6" . my-bookmark-view))
  :config
  (setq bookmark-view-name-format "<count> <buffers>"
        bookmark-view-name-regexp "\\`[0-9]+ ")

  (defun bookmark-view-buffer-names ()
    (string-join (sort (mapcar #'buffer-name (bookmark-view--buffers nil))
                       #'string-lessp) " "))

  (defun my-bookmark-view-dwim (&optional arg)
    (interactive "P")
    (if arg (bookmark-view-delete)
      (if (my-bookmark-view-after-hopping-p)
          (my-bookmark-view "0")
        (call-interactively 'bookmark-view))))

  (defun my-bookmark-rotate (forward)
    ;; Two functions append to a list: append and nconc, both would want a list as the append value
    ;; which I create with (cons 'value ())
    (if forward
        (let ((target (pop bookmark-view-history)))
          (add-to-list 'bookmark-view-history target t)
          target)
      (let ((target (last bookmark-view-history)))
        (setq bookmark-view-history
              (nconc target (butlast bookmark-view-history)))
        (car target))))

  (defun my-bookmark-view-after-hopping-p ()
    (or (equal last-command 'my-bookmark-view-previous)
        (equal last-command 'my-bookmark-view-next)))

  (defun my-bookmark-view-previous ()
    (interactive)
    (unless (my-bookmark-view-after-hopping-p)
      ;; save place-before-hopping to slot 0
      (my-bookmark-view-create "0" 'omit-history))
    (when bookmark-view-history
      (when (equal last-command 'my-bookmark-view-next)
        (my-bookmark-rotate t))
      (bookmark-view-open (my-bookmark-rotate t))))

  (defun my-bookmark-view-next ()
    (interactive)
    (unless (my-bookmark-view-after-hopping-p)
      ;; save place-before-hopping to slot 0
      (my-bookmark-view-create "0" 'omit-history))
    (when bookmark-view-history
      (when (equal last-command 'my-bookmark-view-previous)
        (my-bookmark-rotate nil))
      (bookmark-view-open (my-bookmark-rotate nil))))

  (defun bookmark-view--get-slot (slot &optional delete)
    (let ((slot-name (seq-find (apply-partially #'string-match-p (format "\\`%s:" slot))
                               (bookmark-view-names))))
      (and slot-name delete
           (progn (bookmark-view-delete slot-name)
                  (setq bookmark-view-history
                        (delete slot-name bookmark-view-history))))
      slot-name))

  (defun my-bookmark-view (&optional slot)
    (interactive)
    (let ((slot (or slot (substring (key-description (this-command-keys-vector)) -1))))
      (let ((slot-name (bookmark-view--get-slot slot)))
        (if slot-name (bookmark-view slot-name)
          (message "No view in slot %s" slot)))))

  (defun my-bookmark-view-create (&optional slot omit-history)
    (interactive)
    (let* ((slot (or slot (substring (key-description (this-command-keys-vector)) -1)))
           (name (format "%s:%s" slot (bookmark-view-buffer-names))))
      (bookmark-view--get-slot slot 'delete)
      (bookmark-view-save name nil)
      (unless omit-history
        (add-to-history 'bookmark-view-history name))
      ;; (add-to-ordered-list 'bookmark-view-history name 0)
      (message "View saved to slot %s" slot))))
```

Lastly, there's this heavily modified shackle.

```
(use-package shackle
  :ensure nil
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-rule nil)
  :config
  (setq split-width-threshold nil
        split-height-threshold nil)
  (defun shackle-display-buffer-default-action (buffer alist)
    (let ((shackle--display-plist '(:align right :select t :size 0.5)))
      (shackle-display-buffer-action buffer alist)))
  (setq display-buffer-base-action '((shackle-display-buffer-default-action)))
  (setq switch-to-buffer-obey-display-actions nil)
  (setq shackle-rules
        '(("\\`\\*EKG .*" :regexp t :align right :select t :kill-window t)
          ("\\`\\*Warnings.*?\\*\\'" :regexp t)
          ("*Repeat Commands*" :align below :size 0.2)
          (helpful-mode :align right :select t)
          (mistty-mode :align right :select t)
          (dired-mode :custom shackle--custom-display-dired
                      :align left :select t)
          ("*rg*" :align right :select t)
          ("*Fd*" :align right :select t)
          ("\\*Man.*?\\*" :regexp t :align right :select t)
          ("\\*lispy-python-.*?\\*" :align right :size 0.5 :regexp t :select t :kill-window t)
          ("*lispy-message*" :align below :size 0.2)
          (emms-browser-mode :select t :align left :clear)
          (emms-playlist-mode :align right)
          (lyrics-view-mode :select t :align right)
          ("\\`\\*image-dired.*?\\*\\'" :regexp t :align nil)
          (image-dired-thumbnail-mode :align nil)
          ;; image-dired
          (image-dired-image-display-mode :align nil)
          (rst-toc-mode :select t :align left :size 0.3)
          (lyrics-fetcher-view-mode :select t :align right)
          (TeX-errors-mode :select t)
          (compilation-mode :select t)
          (debugger-mode :same t :select t)
          (lyrics-fetcher-view-mode :same t :select t)
          (wl-folder-mode :select t :maximize t)
          (help-mode :align right :select t))))
```

Enjoy!

Todo:

The current version hooks into window-state-change-functions, in future, use function similar to cl-letf to override it in winnie-save.
