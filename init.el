(package-initialize t)
(setq package-enable-at-startup nil)

(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/emacs.org")
