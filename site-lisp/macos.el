;;; macos -*- lexical-binding: t -*-
(setq mac-pass-option-to-system nil
      mac-pass-control-to-system nil
      mac-pass-command-to-system nil
      mac-command-key-is-meta t
      mac-option-key-is-meta nil
      mac-command-modifier 'meta
      mac-option-modifier 'control
      mac-control-modifier 'control
      mac-function-modifier 'meta
      browse-url-browser-function 'browse-url-default-macosx-browser
      ns-use-native-fullscreen nil)

(provide 'macos)
