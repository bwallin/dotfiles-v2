
(defun bw/load-system-settings ()
  (interactive)
  (load-file "~/.dotfiles/.emacs.d/per-system-settings.el"))

(defun bw/system-settings-get (setting)
  (alist-get setting dw/system-settings))

(provide 'bw-settings)
