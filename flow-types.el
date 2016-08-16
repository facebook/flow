;;; flow-types -- A minor mode to interface with facebook flow

;;; Commentary:
;;
;; flow is a static typechecker for javascript
;; more information can be found at flowtype.org
;;
;; Enable flow-types for all buffers with
;; M-x global-flow-types-mode

;;; Code:
(require 'cl-lib)
(require 'ring)

(defgroup flow-types nil
  "Flow types group"
  :group 'editing
  :group 'convenience
  :group 'external)

(defcustom flow-types-binary
  (or (executable-find "flow") "flow")
  "Path to the flow binary."
  :group 'flow-types
  :type 'string)

(defcustom flow-types-keymap-prefix (kbd "C-c f")
  "Flow Types keymap prefix."
  :group 'flow-types
  :type 'string)

;; (defcustom flow-company-meta-face '((t (:bold t)))
;;   "Meta information face for company suggestions."
;;   :type 'face
;;   :group 'flow-types)

;; (defcustom flow-types-after-start-hook nil
;;   "Run after the flow server is started."
;;   :type 'hook
;;   :group 'flow-types)

;; (defcustom flow-types-before-kill-hook nil
;;   "Run before the flow server is killed."
;;   :group 'flow-types
;;   :type 'hook)

;; list of instances that were started by emacs
;; this is used to cleanup on emacs exit
(defvar flow-types--instances '())

(defvar flow-types-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'flow-types-type-at-pos)
    (define-key map (kbd "j") #'flow-types-jump-def)
    (define-key map (kbd "f") #'flow-types-fill-types)
    map))


(define-minor-mode flow-types-mode
  "\"flow-types\" is a minor mode that interfaces
with the flow static typechecker from facebook"
  ;; Inital Value
  nil
  ;; Mode line indicator
  " Flow"
  ;; keymap to be used
  ;; TODO: This needs to be changed to have a prefix
  :keymap flow-types-mode-map
  :group 'flow-types
  (local-set-key flow-types-keymap-prefix flow-types-mode-map))

(define-globalized-minor-mode global-flow-types-mode flow-types-mode
  (lambda () (flow-types-mode 1)))

(add-hook 'kill-emacs-hook 'flow-types-kill-all)

(defun flow-types-kill-all ()
  "Kill all known flow instances."
  (mapc 'flow-types-kill flow-types--instances))

(defun flow-types-kill (dir)
  "Kill flow instance on DIR if it exists."
  (interactive "DRoot directory: ")
  (when (member dir flow-types--instances)
    (flow-types--run-command-to-message (format "%s stop %s" flow-types-binary dir))
    (setq flow-types--instances (remove dir flow-types--instances))))

;; the ring-insert method we use is deprecated on EMACS 25.1
;; and is replaced by xref-push-marker-stack however
;; it doesen't exist in EMACS 24 (current version) so, in order
;; to not cause unnecessary dependencies we use this
(defun flow-types--push-tag-mark (&optional mark)
  "Pushes MARK to the `find-tag-marker-ring`.
\\[pop-tag-mark] can be used to come back to current position,
if no mark is given the current position position is used instead."
  (if mark
      (ring-insert find-tag-marker-ring mark)
    (ring-insert find-tag-marker-ring (point-marker))))

(defun flow-types--handle-error (command exitcode)
  "Handle COMMAND EXITCODE error."
  nil)

(defun flow-types--command (command &rest args)
  "Execute auto built flow COMMAND with optional ARGS.
ARGS must all be strings
errors are handled by flow-types--handle-error"
  (with-temp-buffer
    (let ((flow-command (flow-types--get-command command args)))
      (let ((ret-code (call-process-shell-command flow-command nil (current-buffer) nil)))
	(if (not (eq 0 ret-code))
	    (buffer-string)
	  (flow-types--handle-error command ret-code)
	  (buffer-string))))))

(defun flow-types--get-command (command &rest args)
  "Builds a flow command with some extra default arguments.
Use COMMAND as a flow command and ARGS as optional arguments, all ARGS must be strings"
  (let ((base (format "%s %s --json --no-auto-start" flow-types-binary command))
	(extra (mapconcat 'identity args " ")))
    (concat base " " extra)))

(defun flow-types--run-command-to-message (cmd)
  "Execute CMD in a shell subprocess, print output to the message buffer and return the exit code."
  (with-temp-buffer
    (let ((ret-code (call-process-shell-command cmd nil (current-buffer) nil)))
      (message "%s" (buffer-string))
      ret-code)))

(defun flow-types--goto-file-char (file line char)
  "Open FILE in current buffer on LINE at CHAR."
  (find-file file)
  (goto-char (point-min))
  (forward-line (1- line))
  (forward-char char))


(defun flow-types--server-is-running ()
  "Verifies if the server on the current directory is running."
  (let ((ret-code (call-process-shell-command (flow-types--get-command "status" default-directory) nil nil nil)))
    (if (not (eq 6 ret-code)) t nil))) ;; 6 is the exit code returned when there is no server running


(defun flow-types-init (dir)
  "Initialize flow server on DIR."
  (interactive "DRoot directory: ")
  (if (member dir flow-types--instances)
      (message "A flow server for this directory already exists")
    (when (eq 0 (flow-types--run-command-to-message (format "%s start %s" flow-types-binary dir)))
      (add-to-list 'flow-types--instances dir))))

(defun flow-types-type-at-pos ()
  (interactive)
  (let ((file (buffer-file-name))
	(line (number-to-string (line-number-at-pos)))
	(col (number-to-string (+ 1 (current-column)))))
    (let ((cmd (format "%s type-at-pos --json --no-auto-start %s %s %s" flow-types-binary file line col)))
      (with-temp-buffer
	(call-process-shell-command cmd nil (current-buffer) nil)
	(let ((json (json-read-from-string (buffer-string))))
	  (message (cdr (elt json 0)))
	  )))))

;; TODO: parse the JSON in a non position dependent way
(defun flow-types-jump-def ()
  "Jump to definition."
  (interactive)
  (let ((file (buffer-file-name))
	(line (number-to-string (line-number-at-pos)))
	(col (number-to-string (+ 1 (current-column)))))
    (let ((cmd (format "%s get-def --json --no-auto-start %s %s %s" flow-types-binary file line col))
	  (mark (point-marker)))
      (with-temp-buffer
	(call-process-shell-command cmd nil (current-buffer) nil)
	(let ((json (json-read-from-string (buffer-string))))
	  (message "%s" (current-buffer))
	  (let ((dest-file (cdr (car json)))
		(dest-line (cdr (elt json 1)))
		(dest-col  (cdr (elt json 3))))
	    (unless (string-equal dest-file "")
	      (flow-types--push-tag-mark mark)
	      (flow-types--goto-file-char dest-file dest-line dest-col))))))))

(defun company-flow-types (command &optional arg &rest ignored)
  "Company backend for flow-types."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-flow-types))
    (prefix (company-grab-symbol-cons "\\.\\|->" 2))
    (candidates (flow-types--get-auto-complete))
    (meta (format "This value is named %s" arg))))

;; (setq company-backends '(company-flow-types))


;; TODO: parse the JSON in a non position dependent way
(defun flow-types--get-auto-complete ()
  "Return a list of completion candidates."
  (let ((file (buffer-file-name))
	(line (number-to-string (line-number-at-pos)))
	(col (number-to-string (+ 1 (current-column))))
	(tmpfile (make-temp-file "fltypes")))
    ;; the 2 is just so that a "Wrote /tmp/flt..." doesen't appear
    ;; if you are going to fix this, the best way is to create an async process
    ;; and send the buffer to the process, that way we avoid creating a tmp file
    (write-region nil nil tmpfile nil 2)
    (let ((cmd (format "%s autocomplete --json --no-auto-start %s %s" flow-types-binary line col)))
      (with-temp-buffer
	(call-process-shell-command cmd tmpfile (current-buffer) nil)
	(let ((suggestions (cdr (car (json-read-from-string (buffer-string))))))
	  (mapcar (lambda (c) (cdr (elt c 0))) suggestions))))))

(defun flow-types-auto-complete ()
  "Provide auto completion candidates on a separate buffer."
  (interactive)
  (let ((file (buffer-file-name))
	(line (line-number-at-pos))
	(col (current-column))
	(buffer (current-buffer)))
    (switch-to-buffer-other-window "*Shell Command Output*")
    (shell-command
     (format "%s autocomplete %s %d %d < %s" flow-types-binary file line (1+ col) file))
    (compilation-mode)
    (switch-to-buffer-other-window buffer)))



(defun flow-types-fill-types ()
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


(provide 'flow-types)

;;; flow-types ends here
