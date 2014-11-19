
(setq flow_binary "$TOPDIR/facebook/flow/flow")

(defun column-number-at-pos (pos)
  "column number at pos"
  (save-excursion (goto-char pos) (current-column))
)

(defun string-of-region ()
  "string of region"
  (if (use-region-p)
      (let ((begin (region-beginning))
            (end (region-end)))
        (format ":%d:%d,%d:%d"
                (line-number-at-pos begin)
                (column-number-at-pos begin)
                (line-number-at-pos end)
                (column-number-at-pos end)))
    "")
)

(defun flow_status (s)
  "Run flow"
  (interactive "sRoot directory: ")
  (compile (format "%s status %s --from emacs; exit 0" flow_binary s))
)

;;(setq compilation-scroll-output t)
(global-set-key "\C-c\C-c" 'flow_status)

(defun show-type ()
  "show type"
  (interactive)
  (let ((file (buffer-file-name))
        (line (line-number-at-pos))
        (col (current-column))
        (buffer (current-buffer)))
    (switch-to-buffer-other-window "*Shell Command Output*")
    (shell-command
     (format "%s type-at-pos %s %d %d"
             flow_binary
             file
             line
             col))
    (compilation-mode)
    (switch-to-buffer-other-window buffer))
)

(global-set-key (kbd "M-t") 'show-type)

(defun fill-types ()
  "fill types"
  (interactive)
  (let ((file (buffer-file-name))
        (region (string-of-region))
        (buffer (current-buffer)))
    (switch-to-buffer-other-window "*Shell Command Output*")
    (shell-command
     (format "%s suggest %s%s"
             flow_binary
             file
             region))
    (compilation-mode)
    (switch-to-buffer-other-window buffer))
  ;;(revert-buffer)
)

(global-set-key (kbd "C-t") 'fill-types)
