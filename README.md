Emacs winner-mode conceals complicated manipulation of window configurations, and has unpredictable behaviors. This package saves window configuration in plain text, and a lot simpler.

Put the file on your load-path. Example config:

```
(use-package winnie
  :ensure nil
  :bind* (("C-9" . winnie-next)
          ("C-8" . winnie-previous))
  :hook after-init)
```

Enjoy!

Todo:

The current version hooks into window-state-change-functions, in future, use function similar to cl-letf to override it in winnie-save.
