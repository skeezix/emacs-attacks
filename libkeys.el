
(defun emx/point-right ()
  (interactive)
  (forward-char)
  (message "right")
)

(when t
  (defvar emattacks-mode-map nil "keymap for Emacs Attacks!")
  (setq emattacks-mode-map (make-sparse-keymap))
  (set-keymap-parent emattacks-mode-map special-mode-map)
  (define-key emattacks-mode-map (kbd "j") 'next-line)
  (define-key emattacks-mode-map (kbd "k") 'previous-line)
  (define-key emattacks-mode-map (kbd "l") 'emx/point-right)
  (define-key emattacks-mode-map (kbd "h") 'backward-char)
)
