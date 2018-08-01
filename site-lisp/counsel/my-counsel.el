;;; my-counsel.el --- My Counsel extensions -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018  Free Software Foundation, Inc.

;; Author: Väinö Järvelä <vaino@jarve.la>
;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (swiper "0.9.0"))
;; Keywords: convenience, matching, tools

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A bunch of the marking code has been copied from Counsel, which is
;; why the license differs from the main LICENSE in this repository.
;; The code fixes problems with counsel-mark-ring (such as sorting,
;; deduping). There is also a global-mark-ring support, and support
;; for viewing both rings. I might make a PR of this some day, but I
;; haven't signed Emacs contributor agreement.

(require 'ivy)
(require 'seq)

(defun counsel-local-mark-candidates ()
  (save-excursion
    (save-restriction
      ;; Widen, both to save `line-number-at-pos' the trouble
      ;; and for `buffer-substring' to work.
      (widen)
      (let ((fmt (format "%%%dd %%s"
                         (length (number-to-string
                                  (line-number-at-pos (point-max)))))))
        (seq-uniq
         (mapcar (lambda (mark)
                   (when (marker-position mark)
                     (goto-char (marker-position mark))
                     (let ((linum (line-number-at-pos))
                           (line  (buffer-substring
                                   (line-beginning-position)
                                   (line-end-position))))
                       (cons (format fmt linum line) (cons (point) (current-buffer))))))
                 (if (mark t)
                     (cons (mark-marker) mark-ring)
                   mark-ring))
         (lambda (a b) (equal (car a) (car b))))))))

(defun counsel-global-mark-candidates ()
  (seq-uniq
   (mapcar (lambda (mark)
             (when (marker-buffer mark)
               (with-current-buffer (marker-buffer mark)
                 (goto-char (marker-position mark))
                 (forward-line 0)
                 (let ((linum (line-number-at-pos))
                       (line  (buffer-substring
                               (line-beginning-position)
                               (line-end-position))))
                   (cons
                    (format "%7d %s:   %s" linum (marker-buffer mark) line)
                    (cons (point) (current-buffer)))))))
           global-mark-ring)
   (lambda (a b) (equal (car a) (car b)))))

(defun counsel-mark-ring-action (cand)
  (let ((pos (cadr cand))
        (buffer (cddr cand)))
    (when (and pos buffer)
      (set-buffer buffer)
      (unless (<= (point-min) pos (point-max))
        (if widen-automatically
            (widen)
          (error "\
Position of selected mark outside accessible part of buffer")))
      (goto-char pos)
      (switch-to-buffer buffer))))

(defun counsel-global-mark-ring ()
  "Browse `global-mark-ring' interactively."
  (interactive)
  (let ((cands (counsel-global-mark-candidates)))
    (if cands
        (ivy-read "Mark: " cands
                  :require-match t
                  :action 'counsel-mark-ring-action
                  :caller 'counsel-global-mark-ring)
      (message "Mark ring is empty"))))

(defun counsel-local-mark-ring ()
  "Browse `mark-ring' interactively.
Obeys `widen-automatically', which see."
  (interactive)
  (let ((cands (counsel-local-mark-candidates)))
    (if cands
        (ivy-read "Mark: " cands
                  :require-match t
                  :action 'counsel-mark-ring-action
                  :caller 'counsel-local-mark-ring)
      (message "Mark ring is empty"))))

(defun counsel-all-mark-ring ()
  "Browse all mark rings interactively.
Obeys `widen-automatically', which see."
  (interactive)
  (let ((cands (append (counsel-local-mark-candidates) (counsel-global-mark-candidates))))
    (if cands
        (ivy-read "Mark: " cands
                  :require-match t
                  :action 'counsel-mark-ring-action
                  :caller 'counsel-all-mark-ring)
      (message "Mark ring is empty"))))

(provide 'my-counsel)
