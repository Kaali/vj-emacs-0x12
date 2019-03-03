;;; -*- lexical-binding: t; -*-
;; Disable package initialize at startup. The commented line below is
;; required for really disabling it.
;; (package-initialize)

(when (< emacs-major-version 26)
  (error "Requires emacs >= 26"))


;;; Early init settings

(setq package-enable-at-startup nil)

;; Print time it took to init
;; Code from https://github.com/jwiegley/dot-emacs/blob/master/init.el
(defconst emacs-start-time (current-time))

;; Relax GC on init to speed it up
(defvar original-gc-cons-threshold gc-cons-threshold)
(defvar original-gc-cons-percentage gc-cons-percentage)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda () (setq gc-cons-threshold original-gc-cons-threshold
                            gc-cons-percentage original-gc-cons-percentage)) t)

;; Make startup faster by removing all file handlers for the duration of init
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'after-init-hook
          `(lambda () (setq file-name-handler-alist file-name-handler-alist-old)) t)

(defun display-startup-echo-area-message ())
(setq initial-frame-alist '((fullscreen . maximized)))


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)
(load (expand-file-name "private.el" user-emacs-directory))

;; Make sure that TLS trust is checked
(require 'tls)
(setq tls-checktrust t)


;;; Packaging

(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir (expand-file-name "elpa" user-emacs-directory)
        package--init-file-ensured t
        package-enable-at-startup nil)

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))

  (setq load-path (append `(,(expand-file-name "site-lisp" user-emacs-directory))
                          (directory-files package-user-dir t "^[^.]" t)
                          load-path))

  ;; Add packages to info, required because we haven't run
  ;; package-initialize
  (with-eval-after-load "info"
    (info-initialize)
    (dolist (dir (directory-files package-user-dir))
      (let ((fdir (concat (file-name-as-directory package-user-dir) dir)))
        (unless (or (member dir '("." ".." "archives" "gnupg"))
                    (not (file-directory-p fdir))
                    (not (file-exists-p (concat (file-name-as-directory fdir) "dir"))))
          (add-to-list 'Info-directory-list fdir))))))

(defun vj--package-installed-p (orig-fn &rest args)
  (if (eq (car args) 'org)
      (if (file-expand-wildcards (concat package-user-dir "/org-[0-9]*")) t nil)
    (apply orig-fn args)))

(defun vj--setup-package-el (&optional _)
  (require 'package)
  ;; Make package-installed-p only check the user package dir for org-mode
  ;; to make it skip the bundled org-mode.
  (advice-add 'package-installed-p :around #'vj--package-installed-p)

  (let* ((proto (if (gnutls-available-p) "https" "http")))
    (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
    (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
    (setq package-archive-priorities '(("org" . 3)
                                       ("melpa" . 2)
                                       ("gnu" . 1)))))

(advice-add #'package-initialize :before #'vj--setup-package-el)

(eval-when-compile
  (package-initialize 'noactivate)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; In init-file-debug mode, enable verbosity and statistics for use-package.
(eval-and-compile
  (setq use-package-always-ensure t)
  (if init-file-debug
      (setq use-package-verbose t
            use-package-expand-minimally nil
            use-package-compute-statistics t)
    (setq use-package-verbose nil
          use-package-expand-minimally t
          use-package-compute-statistics nil)))

(eval-when-compile
  (require 'use-package))

(unless (featurep 'use-package)
  (require 'package)
  (autoload 'use-package "use-package")
  (eval-after-load "use-package" #'package-initialize))

;; Early packages with global effects
(use-package diminish)
(use-package bind-key)
(use-package no-littering)


;;; Emacs settings

(defconst is-windows (eq system-type 'windows-nt))
(defconst is-macos (eq system-type 'darwin))

(eval-when-compile (require 'compile))
(eval-when-compile (require 'cc-vars))

(setq inhibit-startup-screen t
      confirm-kill-emacs 'y-or-n-p

      sentence-end-double-space nil
      require-final-newline t

      kill-do-not-save-duplicates t
      set-mark-command-repeat-pop t
      mouse-yank-at-point t
      save-interprogram-paste-before-kill t

      scroll-conservatively 1000
      scroll-margin 0
      scroll-preserve-screen-position t
      hscroll-margin 1
      hscroll-step 1

      visible-bell t
      window-combination-resize t

      auto-save-timeout 30
      auto-save-interval 0
      auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
      make-backup-files t
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      create-lockfiles nil

      compilation-scroll-output 'first-error

      ;; Improve performance, especially with long lines or large buffers
      bidi-display-reordering nil
      jit-lock-stealth-nice 0.1
      jit-lock-stealth-time 0.2

      ;; Clean up visual cruft
      indicate-buffer-boundaries nil
      indicate-empty-lines nil

      uniquify-buffer-name-style 'forward
      uniquify-min-dir-content 2)

(customize-set-variable 'help-at-pt-display-when-idle t)
(customize-set-variable 'help-at-pt-timer-delay 0.1)

(setq-default indent-tabs-mode nil
              c-basic-offset 4
              c-default-style '((java-mode . "java") (awk-mode . "awk") (other . "bsd")))

(when is-macos
  (require 'macos))

(when is-windows
  (require 'windows))

(put 'narrow-to-region 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(global-auto-revert-mode t)
(diminish 'auto-revert-mode)
(delete-selection-mode t)
(auto-save-visited-mode t)
(winner-mode t)

(require 'seq)
(defun font-candidate (&rest fonts)
  "Return the first available font."
  (seq-find (lambda (x) (find-font (font-spec :name x))) fonts))

(if is-windows
    (progn
      (setq inhibit-compacting-font-caches t)
      (set-frame-font (font-candidate "Noto Mono-11" "Input-13" "DejaVu Sans Mono-9" "Consolas-9") nil t))
  (set-frame-font (font-candidate "Noto Mono-16" "Input-13" "DejaVu Sans Mono-9" "Consolas-9") nil t))
(set-face-attribute 'default (selected-frame))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(unless is-macos
  (menu-bar-mode -1))

(column-number-mode t)
(size-indication-mode t)

(add-to-list
 'display-buffer-alist
 '("\\*compilation\\*" display-buffer-reuse-window
   (reusable-frames . t)))

;; Save some buffers without prompt when emacs goes out of focus
(defun save-some-buffers-without-prompt ()
  (save-some-buffers
   :no-prompt
   (lambda ()
     (not (and buffer-auto-save-file-name
               auto-save-visited-file-name)))))
(add-hook 'focus-out-hook #'save-some-buffers-without-prompt)

(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort
                        abort-recursive-edit
                        exit-minibuffer
                        keyboard-quit))
          (ding))))

(defun vj--modeline-justify (left right)
  (let* ((l (format-mode-line left))
         (r (format-mode-line right))
         (width (max 0 (- (window-total-width) (string-width l) (string-width r))))
         (fill (make-string width ? )))
    (concat l fill r)))

(defvar vj--projectile-mode-line)
(put 'vj--projectile-mode-line 'risky-local-variable t)
(with-eval-after-load 'projectile
  (setq vj--projectile-mode-line
        '(:propertize
          (:eval (when (projectile-project-p)
                   (concat " [" (projectile-project-name) "]"))))))

(setq-default mode-line-format
              '((:eval (vj--modeline-justify
                        '("%e"
                          mode-line-front-space
                          mode-line-modified
                          "%4l:%c"
                          vj--projectile-mode-line
                          " %[" mode-line-buffer-identification "%]")
                        '(" "
                          mode-line-modes
                          mode-line-misc-info
                          mode-line-end-spaces)))))


(defun vj/zap-up-to-char (arg char)
  "Zap up to a character."
  (interactive "p\ncZap up to char: ")
  (zap-to-char arg char)
  (insert char)
  (forward-char -1))

(define-key input-decode-map [?\C-m] [C-m])
(windmove-default-keybindings 'meta)
(bind-key "M-`" 'other-frame)
(bind-key "RET" 'newline-and-indent)
(bind-key "M-%" 'query-replace-regexp)
(bind-key "C-M-%" 'query-replace)
(bind-key "C-x k" 'kill-this-buffer)
(bind-key "C-x K" 'kill-buffer)
(bind-key "C-z" 'undo)
(bind-key "M-j" 'join-line)
(bind-key "M-z" 'vj/zap-up-to-char)
(bind-key* "C-M-SPC" 'cycle-spacing)

;;; Packages

(use-package savehist
  :ensure nil
  :config
  (setq history-length t
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring))
  (savehist-mode t))

(use-package exec-path-from-shell
  :unless is-windows
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package color-theme-sanityinc-tomorrow
  :demand t
  :config
  (load-theme 'sanityinc-tomorrow-day t))

(use-package el-patch
  :demand t
  :commands (el-patch-defmacro el-patch-defsubst el-patch-defvar
             el-patch-defconst el-patch-defcustom el-patch-defun
             el-patch-define-minor-mode el-patch-feature))

(use-package async
  :config
  (setq async-bytecomp-package-mode t))

(use-package magit
  :commands magit-status
  :preface
  (defun vj--demand-magit-if-git-registered ()
    (when (vc-git-registered (buffer-file-name))
      (require 'magit)
      (remove-hook 'find-file-hook #'vj--demand-magit-if-git-registered)))
  (add-to-list 'find-file-hook #'vj--demand-magit-if-git-registered)
  :config
  (with-eval-after-load 'ivy
    (setq magit-completing-read-function 'ivy-completing-read))
  (setq magit-save-repository-buffers 'dontask
        magit-wip-after-save-local-mode-lighter nil
        magit-wip-after-apply-mode-lighter nil
        magit-wip-before-change-mode-lighter nil
        magit-no-message '("Turning on magit-auto-revert-mode...")
        git-commit-summary-max-length 50)
  (magit-wip-after-save-mode)
  (magit-wip-after-apply-mode)
  (magit-wip-before-change-mode)
  :bind ("C-x g" . magit-status))

(use-package gitconfig-mode
  :mode ("/\\.gitconfig\\'"
         "/\\.git/config\\'"
         "/modules/.*/config\\'"
         "/git/config\\'"
         "/\\.gitmodules\\'"
         "/etc/gitconfig\\'"))

(use-package gitattributes-mode
  :mode ("/\\.gitattributes\\'"
         "/info/attributes\\'"
         "/git/attributes\\'"))

(use-package gitignore-mode
  :mode ("/\\.gitignore\\'"
         "/info/exclude\\'"
         "/git/ignore\\'"))

(use-package ssh-agency
  :after magit
  :if is-windows)

(use-package yaml-mode
  :mode "\\.ya?ml$")

(use-package jinja2-mode
  :mode ("\\.j2\\'" . jinja2-mode))

(use-package markdown-mode
  :mode "\\.md$")

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-load-hook
            (lambda ()
              (defun ediff-arrange-autosave-in-merge-jobs (_))
              (defvar ediff-saved-window-configuration)
              (add-hook 'ediff-before-setup-hook
                        (lambda ()
                          (setq ediff-saved-window-configuration (current-window-configuration))))
              (let ((restore-window-configuration
                     (lambda ()
                       (set-window-configuration ediff-saved-window-configuration))))
                (add-hook 'ediff-quit-hook restore-window-configuration 'append)
                (add-hook 'ediff-suspend-hook restore-window-configuration 'append)))))

(use-package eshell
  :preface
  (defun vj--new-eshell () (interactive) (eshell t))

  ;; isearch things copied from jwiegley's emacs config
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
        (goto-char end)
        (when (looking-back "&!" beg)
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char beg)
          (insert "spawn "))))

    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    (use-package em-unix
      :defer t
      :ensure nil
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil)))

  ;; Replace default completion with a version that ivy is hooked to.
  ;; Can't use company-mode here as it doesn't really work well with eshell.
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>")
                (lambda () (interactive) (pcomplete-std-complete)))))
  :hook (eshell-first-time-mode-hook . eshell-initialize)
  :bind (("C-x m" . eshell)
         ("C-x M" . vj--new-eshell)))

(use-package dired
  :ensure nil
  :commands dired
  :bind (:map dired-mode-map
              ("M-s f"))
  :config
  (setq dired-auto-revert-buffer t))

(use-package dired-x
  :ensure nil
  :after dired
  :config
  (setq-default dired-omit-files-p t))

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package dired-collapse
  :after dired
  :hook (dired-mode . dired-collapse-mode))

(use-package org
  :functions (org-mode org-agenda-mode)
  :bind (("C-c a" . org-agenda)
         :map org-mode-map
         ("C-'"))
  :mode ("\\.org$" . org-mode)
  :hook (org-mode . auto-fill-mode)
  :config
  (setq org-startup-folded t
        org-startup-with-inline-images t
        org-startup-truncated t
        org-directory "~/Documents/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-replace-disputed-keys t
        org-hide-emphasis-markers t))

(use-package projectile
  :demand t
  :diminish
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching nil
        projectile-indexing-method 'alien)
  (projectile-mode))

(use-package project
  :ensure nil
  :demand t
  :config
  (defun projectile-project-find-function (dir)
    (let* ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (with-eval-after-load 'projectile
    (add-to-list 'project-find-functions 'projectile-project-find-function)))

(use-package counsel-projectile
  :demand t
  :after (counsel projectile)
  :bind (("C-;" . counsel-projectile))
  :config
  (counsel-projectile-mode t))

(use-package python
  :mode ("\\.py$" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (when is-macos
    (setq python-shell-interpreter "python3")))

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\(\\.h|\\.c\\)" . c-mode))
  :preface
  (defun my-c-mode-setup ()
    (subword-mode 1)
    (c-set-offset 'innamespace 4))
  :config
  (add-hook 'c-mode-common-hook 'my-c-mode-setup))

(use-package cmake-mode
  :mode "CMakeFiles.txt")

(use-package js2-mode
  :mode "\\.js$"
  :config (setq js2-basic-offset 2))

(use-package js2-refactor
  :after js2-mode
  :hook (js2-mode . js2-refactor-mode))

(use-package edebug
  :ensure nil
  :config
  (setq edebug-trace t))

(use-package eldoc
  :diminish eldoc-mode
  :hook ((c-mode-common emacs-lisp-mode lisp-interaction-mode-hook) . eldoc-mode))

(use-package lua-mode
  :mode "\\.lua$"
  :interpreter "lua")

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (use-package ibuffer-vc
    :config
    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini " "
                  (name 18 18 :left :elide)
                  " "
                  (size 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process))
          ibuffer-show-empty-filter-groups nil)
    :hook (ibuffer . (lambda ()
                       (setq ibuffer-filter-groups
                             (append
                              '(
                                ("Emacs"
                                 (or
                                  (name . "^\\*scratch\\*$")
                                  (name . "^\\*Messages\\*$")))
                                ("Special buffers"
                                 (name . "^\\*.*\\*$")))
                              (ibuffer-vc-generate-filter-groups-by-vc-root)))
                       (unless (eq ibuffer-sorting-mode 'alphabetic)
                         (ibuffer-do-sort-by-filename/process))
                       (ibuffer-update nil t)))))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode)

(use-package json-mode
  :mode "\\.json$")

(use-package json-snatcher
  :commands jsons-print-path)

(use-package nix-mode
  :mode "\\.nix$")

(use-package text-mode
  :ensure nil
  :hook (text-mode . auto-fill-mode))

(use-package deadgrep
  :bind ("M-s d" . deadgrep))

(use-package treemacs
  :commands treemacs)

(use-package treemacs-projectile
  :after treemacs
  :commands treemacs-projectile)

(use-package amx
  :demand t
  :bind ("M-X" . amx-major-mode-commands))

(use-package ivy
  :diminish ""
  :demand t
  :init
  (setq ivy-use-virtual-buffers t
        ivy-height 20
        ivy-fixed-height-minibuffer t
        ivy-count-format "%d/%d "
        ; don't use an initial input for ivy
        ivy-initial-inputs-alist nil
        ; allow regexp in any order
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-dynamic-exhibit-delay-ms 200
        ivy-use-selectable-prompt t
        ivy-format-function #'ivy-format-function-line
        ivy-magic-tilde nil)
  :bind (
         ("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         :map ivy-minibuffer-map
              ("<up>" . ivy-previous-line-or-history))
  :config
  (ivy-mode 1))

(use-package ivy-hydra
  :after ivy)

(use-package swiper
  :after ivy
  :bind (
         ("M-s s" . counsel-grep-or-swiper)
         :map isearch-mode-map
         ("C-o" . swiper-from-isearch)
         ("C-M-s" . swiper-from-isearch)
         :map swiper-map
         ("C-'" . swiper-avy)
         ("M-c" . haba/swiper-mc-fixed))
  :config
  ;; https://github.com/abo-abo/swiper/issues/1304
  (defun haba/swiper-mc-fixed ()
    (interactive)
    (setq swiper--current-window-start nil)
    (swiper-mc)))

(use-package counsel
  :demand t
  :diminish
  :after ivy
  :config
  (setq counsel-find-file-at-point t)
  (counsel-mode t)
  :bind (("C-h C-h" . counsel-M-x)
         ("M-s a" . counsel-ag)
         ("M-s g" . counsel-git-grep)
         ("M-s f" . counsel-file-jump)
         ("M-i" . counsel-imenu)))

(use-package my-counsel
  :after counsel
  :load-path "site-lisp/counsel/"
  :bind ("C-c i" . counsel-all-mark-ring))

(use-package ivy-rich
  :after (ivy)
  :config
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev
        ivy-format-function #'ivy-format-function-line)
  (ivy-rich-mode 1))

(use-package ivy-xref
  :after (ivy xref)
  :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-historian
  :after ivy
  :config (ivy-historian-mode t))

(use-package avy
  :commands avy-goto-char-timer
  :config (avy-setup-default)
  :bind ("C-'" . avy-goto-char-timer))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (sp-use-smartparens-bindings)
  ;; Don't use Emacs matching paren blink as smartparens does something
  ;; similar.
  (setq blink-matching-paren nil)

  (defun vj--sp-strict-kill-region-or-whole-line ()
    (interactive
     (if mark-active
         (sp-kill-region (region-beginning) (region-end))
       ;; Could not use sp-kill-whole-line here as it leaves an empty
       ;; line in the kill-ring
       (progn
         (beginning-of-line)
         (sp-kill-hybrid-sexp nil)))))
  (define-key smartparens-strict-mode-map
    [remap kill-region] #'vj--sp-strict-kill-region-or-whole-line)
  (define-key smartparens-mode-map (kbd "M-<backspace>") nil)

  (dolist (mode sp-lisp-modes)
    (let ((mode-hook (intern (format "%S-hook" mode))))
      (add-hook mode-hook #'smartparens-strict-mode)))

  ;; Disable smartparens in org-stuff
  (with-eval-after-load 'org
    (add-to-list 'sp-ignore-modes-list #'org-mode))

  (with-eval-after-load 'org-agenda
    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))

  (defun radian-enter-and-indent-sexp (&rest _ignored)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (defun vj--sp-enter-and-indent-pair (mode open close)
    (sp-local-pair mode open close :post-handlers
                   '((radian-enter-and-indent-sexp "RET")
                     (radian-enter-and-indent-sexp "<return>"))))

  (dolist (mode '(c-mode c++-mode css-mode objc-mode java-mode
                         js2-mode json-mode
                         python-mode sh-mode web-mode))
    (vj--sp-enter-and-indent-pair mode "{" nil))

  (dolist (mode '(js2-mode json-mode python-mode web-mode))
    (vj--sp-enter-and-indent-pair mode "[" nil))

  (dolist (mode '(python-mode))
    (vj--sp-enter-and-indent-pair mode "(" nil)
    (vj--sp-enter-and-indent-pair mode "\"\"\"" "\"\"\"")))

(use-package diff-hl
  :after magit
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package expand-region
  :bind (("M-[" . er/contract-region)
         ("M-]" . er/expand-region)))

(use-package selected
  :diminish selected-minor-mode
  :demand t
  :defines selected-keymap
  :bind (:map selected-keymap
              ("q" . selected-off)
              ("<tab>" . indent-region)
              ("m" . apply-macro-to-region-lines))
  :config
  (selected-global-mode t))

(use-package multiple-cursors
  :after (selected phi-search)
  :demand t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-*" . mc/mark-all-like-this)
         ("S-<down-mouse-1>")
         ("S-<mouse-1>" . mc/add-cursor-on-click))
  :bind (:map mc/keymap
              ("C-s" . phi-search)
              ("C-r" . phi-search-backward))
  :bind (:map selected-keymap
              ("c"   . mc/edit-lines)
              ("*"   . mc/mark-all-like-this)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this)
              ("?"   . mc-hide-unmatched-lines-mode)))

(use-package phi-search)

(use-package phi-search-mc
  :after (phi-search multiple-cursors)
  :config
  (phi-search-mc/setup-keys)
  (add-hook 'isearch-mode-mode #'phi-search-from-isearch-mc/setup-keys))

(use-package undo-tree
  :disabled
  :diminish undo-tree-mode
  :demand t
  :bind (:map undo-tree-map
              ("C-/" . nil))
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history nil))

(use-package recentf
  :config
  (setq recentf-exclude (append recentf-exclude
                                `(,no-littering-var-directory)
                                `(,no-littering-etc-directory)
                                `(,package-user-dir)))
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        recentf-auto-cleanup 'never)
  (recentf-mode t))

(use-package company
  :diminish company-mode
  :bind (("M-/" . company-complete))
  :config
  (setq company-backends
        '(company-bbdb
          company-cmake
          company-css
          company-nxml
          company-elisp
          company-capf
          company-files
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse company-dabbrev))
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-require-match nil)
  (global-company-mode t))

(use-package company-dabbrev
  :ensure nil
  :after company
  :config
  (setq company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil))

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map
              ("C-c h" . company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode))

(use-package flycheck
  :diminish global-flycheck-mode
  :hook ((emacs-lisp-mode . flycheck-mode)
         (prog-mode-hook . flycheck-mode)
         (c++-mode-hook . (lambda () (setq flycheck-clang-language-standard "c++11"))))
  :commands (flycheck-mode flycheck-next-error flycheck-previous-error)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (setq whitespace-style '(face indentation:space trailing tabs tab-mark))
  (global-whitespace-mode t))

(use-package which-key
  :diminish ""
  :config (which-key-mode))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package switch-window
  :bind (("C-x o" . switch-window)
         ("C-\\" . switch-window)
         ("C-1" . switch-window-then-maximize)
         ("C-2" . switch-window-then-split-below)
         ("C-3" . switch-window-then-split-right)
         ("C-0" . switch-window-then-delete))
  :config
  (setq switch-window-shortcut-style 'qwerty
        switch-window-qwerty-shortcuts '("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")
        switch-window-threshold 2
        switch-window-minibuffer-shortcut ?z))

(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)
         ("C-c m" . vr/mc-mark)))

(use-package eglot
  :commands eglot
  :preface
  ;; Patch to shutdown eglot, because of the yes-no advice is not enough
  (with-eval-after-load 'projectile
    (with-eval-after-load 'eglot
      (el-patch-defun projectile-kill-buffers ()
        "Kill project buffers.

The buffer are killed according to the value of
`projectile-kill-buffers-filter'."
        (interactive)
        (let* ((project (projectile-ensure-project (projectile-project-root)))
               (project-name (projectile-project-name project))
               (buffers (projectile-project-buffers project)))
          (when (yes-or-no-p
                 (format "Are you sure you want to kill %s buffers for '%s'? "
                         (length buffers) project-name))
            (dolist (buffer buffers)
              (when (and
                     ;; we take care not to kill indirect buffers directly
                     ;; as we might encounter them after their base buffers are killed
                     (not (buffer-base-buffer buffer))
                     (if (functionp projectile-kill-buffers-filter)
                         (funcall projectile-kill-buffers-filter buffer)
                       (pcase projectile-kill-buffers-filter
                         ('kill-all t)
                         ('kill-only-files (buffer-file-name buffer))
                         (_ (user-error "Invalid projectile-kill-buffers-filter value: %S" projectile-kill-buffers-filter)))))
                (el-patch-swap
                  (kill-buffer buffer)
                  (progn
                    (ignore-errors
                          (when-let* ((server (eglot--current-server)))
                            (eglot-shutdown server)))
                    (kill-buffer buffer))))))))))
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(typescript-mode . ("javascript-typescript-stdio"))))
  :config
  (require 'company))

(use-package dumb-jump
  :after smart-jump
  :commands (dumb-jump-go
             dumb-jump-quick-look
             dumb-jump-go-other-window
             dumb-jump-go-prefer-external
             dumb-jump-go-prefer-external-other-window
             dumb-jump-go-prompt)
  :config
  (setq dumb-jump-selector 'ivy))

(use-package smart-jump
  :commands smart-jump-go
  :config
  (smart-jump-setup-default-registers)
  ;; Replace the default elisp registration as I don't have slime and it doesn't
  ;; work without it
  (smart-jump-register :modes '(emacs-lisp-mode lisp-interaction-mode))
  ;; Also add js2-mode mode, as the defaults requires rjsx-mode?
  (with-eval-after-load 'js2-mode
    (smart-jump-register :modes '(js2-mode))))

(use-package try
  :commands try
  :init
  (eval-after-load "try" #'package-initialize))

(use-package free-keys
  :commands free-keys)

(use-package imenu-list
  :commands imenu-list-minor-mode)

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-minor-mode-on yas-expand
             yas-expand-snippet yas-lookup-snippet
             yas-insert-snippet yas-new-snippet
             yas-visit-snippet-file snippet-mode)
  :preface
  (defvar yas-minor-mode-map (make-sparse-keymap))
  :init
  (defun vj--yas-reload-all-hook ()
    (yas-reload-all)
    (remove-hook 'yas-minor-mode-hook 'vj--yas-reload-all-hook))
  (add-hook 'yas-minor-mode-hook 'vj--yas-reload-all-hook)
  :hook ((text-mode . yas-minor-mode-on)
         (prog-mode . yas-minor-mode-on)
         (snippet-mode . yas-minor-mode-on))
  :config
  (setq yas-triggers-in-field t)
  ;; Fix smartparens conflict
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package auto-yasnippet
  :after yasnippet
  :bind (("C-c y a" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line)))

(use-package crux
  :bind* (("C-o" . crux-smart-open-line)
          ("C-S-o" . crux-smart-open-line-above)
          ("C-a" . crux-move-beginning-of-line)))

(use-package phi-rectangle
  :after multiple-cursors
  :demand t
  :config
  (phi-rectangle-mode 1))

(use-package symbol-overlay
  :diminish symbol-overlay-mode
  :hook ((prog-mode-hook . symbol-overlay-mode)
         (html-mode-hook . symbol-overlay-mode)
         (css-mode-hook . symbol-overlay-mode))
  :bind (("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("C-c o" . symbol-overlay-put)))

(use-package shackle
  :demand t
  :config
  (setq shackle-default-rule '(:select t)
        shackle-rules '(("\\`\\*Flycheck" :regexp t :size 0.2 :noselect t :align bottom)
                        ("\\`\\*Flymake" :regexp t :size 0.2 :noselect t :align bottom)
                        ("*Warnings*" :size 0.2 :noselect t :align bottom)
                        ("\\`\\*COMMIT_EDITMSG" :regexp t :size 0.5 :align right)
                        (magit-diff-mode :noselect t :align bottom :size 0.5)
                        (magit-commit-mode :ignore t)
                        ("*edebug-trace*" :size 0.2 :align bottom)
                        ("*Messages*" :select t :size 0.4 :align t)
                        (compilation-mode :noselect t :size 0.4 :align t)))
  (shackle-mode))

(use-package eyebrowse
  :demand t
  :config
  (setq eyebrowse-mode-line-separator ""
        eyebrowse-new-workspace t)
  (eyebrowse-mode t))

(use-package macrostep
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

(use-package isearch
  :ensure nil
  :config
  (setq lazy-highlight-initial-delay 0)
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-r" . isearch-backward)))

(use-package xterm-color
  :demand t
  :config
  (with-eval-after-load 'comint
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions)))
  (with-eval-after-load 'shell
    (add-hook 'shell-mode-hook
              (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))

  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setenv "TERM" "xterm-256color")
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  (with-eval-after-load 'compile
    (setq compilation-environment '("TERM=xterm-256color"))
    (add-hook 'compilation-start-hook
              (lambda (proc)
                ;; We need to differentiate between compilation-mode buffers
                ;; and running as part of comint (which at this point we assume
                ;; has been configured separately for xterm-color)
                (when (eq (process-filter proc) 'compilation-filter)
                  ;; This is a process associated with a compilation-mode buffer.
                  ;; We may call `xterm-color-filter' before its own filter function.
                  (set-process-filter
                   proc
                   (lambda (proc string)
                     (funcall 'compilation-filter proc
                              (xterm-color-filter string))))))))
  )

(use-package dtrt-indent
  :diminish ""
  :hook (prog-mode . dtrt-indent-mode)
  :config
  (add-to-list 'dtrt-indent-hook-mapping-list '(typescript-mode javascript typescript-indent-level)))

(use-package direnv
  :unless is-windows
  :diminish
  :config
  (direnv-mode))

(use-package pos-tip
  :config
  (when is-macos
    (setq pos-tip-use-relative-coordinates t)))

(use-package server
  :unless (or noninteractive is-windows)
  :no-require
  :hook (after-init . server-start))

(let ((elapsed
       (float-time
        (time-subtract (current-time) emacs-start-time))))
  (message "Loading %s...done (%.3fs)"
           load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
