(require 'org)
(load-file "~/.dotfiles/.emacs.d/lisp/bw-settings.el")

;; Don't ask when evaluating code blocks
(setq org-confirm-babel-evaluate nil)

(let* ((dotfiles-path (expand-file-name "~/.dotfiles"))
 (org-files (directory-files dotfiles-path nil "\\.org$")))

  (defun bw/tangle-org-file (org-file)
    (message "\n\033[1;32mUpdating %s\033[0m\n" org-file)
    (org-babel-tangle-file (expand-file-name org-file dotfiles-path)))

  ;; Tangle Systems.org first
  (bw/tangle-org-file "Systems.org")

  (dolist (org-file org-files)
    (unless (member org-file '("README.org" "Systems.org"))
      (bw/tangle-org-file org-file))))
