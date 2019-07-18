;;; macos -*- lexical-binding: t -*-
(setq mac-pass-control-to-system nil
      mac-pass-command-to-system nil
      mac-command-modifier 'meta
      mac-option-modifier 'control
      mac-control-modifier 'control
      mac-function-modifier 'meta
      mac-mouse-wheel-smooth-scroll nil
      browse-url-browser-function 'browse-url-default-macosx-browser)

(unless (version< "27.0" emacs-version)
    (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

(provide 'macos)
