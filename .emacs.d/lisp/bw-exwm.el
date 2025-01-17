(defvar bw/exwm--floating-pinned-windows '()
  "Holds the set of pinned window IDs.")

(defun bw/next-telega-buffer ()
  (interactive)
  (let ((buffers (-filter (lambda (b)
                            (with-current-buffer b
                              (eq major-mode "telega-chat-mode")))
                         (buffer-list))))
    (message "%s" buffers)))

;; This enables a keybinding to be created for a specific workspace ID which
;; will switch to the workspace or invoke the buffer cycling function if we're
;; already on that workspace.
(defun bw/exwm-switch-or-cycle-workspace (id cycle-func &optional reverse)
  (interactive)
  (if (eq exwm-workspace-current-index id)
      (funcall cycle-func reverse)
    (exwm-workspace-switch id)))

;;;###autoload
(defun bw/exwm-floating-toggle-pinned (&optional id)
  (interactive)
  (when-let ((exwm--floating-frame)
             (window-id (or id exwm--id)))
    ;; This approach is more reliable for now even with the workspace snap-back
    (setq exwm--desktop 0xffffffff)))
    ;; (if (seq-contains bw/exwm--floating-pinned-windows window-id)
    ;;   (setq bw/exwm--floating-pinned-windows (remq window-id bw/exwm--floating-pinned-windows))
    ;;   (push window-id bw/exwm--floating-pinned-windows))))

(defun bw/exwm-floating--on-workspace-switch ()
  (let ((current-monitor (frame-parameter exwm--frame 'exwm-randr-monitor)))
    (dolist (id bw/exwm--floating-pinned-windows)
      (when-let ((buffer (exwm--id->buffer id)))
        (with-current-buffer buffer
          (when (equal current-monitor (frame-parameter exwm--frame 'exwm-randr-monitor))
            (exwm-workspace-move-window exwm-workspace-current-index id)))))))

(defun bw/exwm-floating--on-buffer-killed ()
  (when (derived-mode-p 'exwm-mode)
    (setq bw/exwm--floating-pinned-windows (remq exwm--id bw/exwm--floating-pinned-windows))))

(add-hook 'exwm-workspace-switch-hook #'bw/exwm-floating--on-workspace-switch)
(add-hook 'kill-buffer-hook #'bw/exwm-floating--on-buffer-killed)

(provide 'bw-exwm)
