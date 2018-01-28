;;; -*- lexical-binding: t; -*-
;; Disable package initialize at startup. The commented line below is
;; required for really disabling it.

;; (package-initialize)
(setq package-enable-at-startup nil)

(defun vj--file-modified-date (filename)
  (if (file-exists-p filename)
      (file-attribute-modification-time (file-attributes filename))
    '(0 0 0 0)))

(setq vj--emacs-org "~/.emacs.d/emacs.org"
      vj--emacs-elc (concat (file-name-sans-extension vj--emacs-org) ".elc"))

(defun vj--emacs-elc-is-old ()
  (time-less-p (vj--file-modified-date vj--emacs-elc)
               (vj--file-modified-date vj--emacs-org)))

(defun recompile-emacs-org ()
  (interactive)
  (let ((old-init-file-debug init-file-debug)
        (init-file-debug nil))
    (require 'ob-tangle)
    (org-babel-load-file vj--emacs-org t)
    (setq init-file-debug old-init-file-debug)))

;; If emacs.org is newer than emacs.elc, then load .org and show a
;; message that elc is out of date. Don't load elc anyway if in
;; init-file-debug mode
(if (or init-file-debug (vj--emacs-elc-is-old))
    (progn
      (require 'ob-tangle)
      (org-babel-load-file vj--emacs-org)
      (message "emacs.elc older than emacs.org. Update with M-x recompile-emacs-org"))
  (load-file vj--emacs-elc))
