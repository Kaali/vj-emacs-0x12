;;; windows -*- lexical-binding: t -*-
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))

(provide 'windows)
