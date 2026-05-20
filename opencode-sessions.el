;;; opencode-sessions.el --- Code for managing opencode sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Code for managing opencode sessions

;;; Code:

(require 'comint)
(require 'magit)
(require 'mailcap)
(require 'markdown-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-format-tool-calls)
(require 'project)
(require 'vtable)
(require 'yank-media)

(defvar opencode-session-control-mode-map
  (define-keymap
    "r" 'opencode-sessions-redisplay
    "g" nil
    "SPC" nil
    "n" 'opencode-new-session
    "M" 'opencode-toggle-mcp
    "U" 'opencode-unshare-all-sessions
    "v" 'opencode-session-control-toggle-verbose))

(defvar-local opencode-session-control-verbose nil
  "Toggle whether to display subagents in session control buffer.")

(define-derived-mode opencode-session-control-mode special-mode "Sessions"
  "Opencode session control panel mode.")

(defun opencode--tab-dispatch ()
  "Cycle session agent, or move to next button if point is on one."
  (interactive)
  (if (button-at (point))
      (forward-button 1)
    (call-interactively #'opencode-cycle-session-agent)))

(defvar opencode-session-mode-map
  (define-keymap
    "C-c C-y" 'opencode-yank-code-block
    "C-c C-c" 'opencode-abort-session
    "C-c C-p" 'opencode-respond-permission
    "C-c x" 'opencode-kill-session
    "<backtab>" 'backward-button
    "TAB" 'opencode--tab-dispatch
    "C-c r" 'opencode-rename-session
    "C-c n" 'opencode-new-session
    "C-c l" 'opencode-select-session
    "C-c c" 'opencode-select-child-session
    "C-c p" 'opencode-open-parent
    "C-c f" 'opencode-add-file
    "C-c b" 'opencode-add-buffer-dwim
    "C-c s" 'opencode-share-session
    "C-c u" 'opencode-unshare-session
    "C-c U" 'opencode-unshare-all-sessions
    "C-c m" 'opencode-select-model
    "C-c v" 'opencode-select-variant
    "C-c M" 'opencode-toggle-mcp
    "C-c F" 'opencode-fork-session
    "C-c D" 'opencode-delete-message
    "C-c R" 'opencode-revert-message
    "/" 'opencode-insert-slash-command
    "@" 'opencode-add-subagent))

(with-eval-after-load 'evil
  (declare-function evil-define-key* "evil-core")
  (evil-define-key* 'normal opencode-session-control-mode-map
    "r" 'opencode-sessions-redisplay
    "n" 'opencode-new-session
    "gv" 'opencode-session-control-toggle-verbose)
  (evil-define-key* 'insert opencode-session-mode-map
    "@" 'opencode-add-subagent
    "/" 'opencode-insert-slash-command))

(defvar-local opencode-session-id nil
  "Session id for the current opencode session buffer.")

(defvar-local opencode-session-tokens 0
  "Tokens consumed by the current session.")

(defvar-local opencode-session-status "idle"
  "Status of the current opencode session (busy or idle).")

(defvar-local opencode-session-agent nil
  "Currently active agent for this buffer's session.")

(defvar-local opencode-session-agents nil
  "List of agents for the current session.
Buffer local so we can configure models and variants per agent per session.")

(defvar-local opencode-session-pending-questions nil
  "Pending questions for this session buffer, awaiting user response.
When non-nil, contains a cons cell (QUESTION-ID . QUESTIONS-VECTOR).")

(defvar-local opencode-session-pending-permission nil
  "Pending permission requests for this session buffer, as a list of plists.
Each plist has keys :id, :session-id, :type, :title, and :marker.
Requests are processed FIFO.")

(defvar-local opencode--temp-files nil
  "Temporary files to delete after sending or killing a session buffer.")

(defvar-local opencode--files-edited-this-turn nil
  "Files edited by opencode during the current turn.")

(defvar opencode-session-buffers
  (make-hash-table :test 'equal)
  "A mapping of session ids to Emacs buffers.")

(defvar-local opencode--tool-calls-displayed nil
  "A hash table containing all callID that have already been displayed.")

(defvar opencode-assistant-messages
  nil
  "An alist mapping all currently updating assistant message ids, to start pos.")

(defun opencode-cycle-session-agent ()
  "Switch to the next agent in `opencode-session-agents'."
  (interactive)
  (let* ((agent-name (alist-get 'name opencode-session-agent))
         (agent-cell (and agent-name
                          (cl-loop for cell on opencode-session-agents
                                   when (string= agent-name
                                                 (alist-get 'name (car cell)))
                                   return cell))))
    (when agent-cell
      (setcar agent-cell opencode-session-agent)))
  (let* ((pos (cl-position-if (lambda (agent)
                                (string= (alist-get 'name agent)
                                         (alist-get 'name opencode-session-agent)))
                              opencode-session-agents))
         (next (nth (1+ pos) opencode-session-agents)))
    (setf opencode-session-agent (or next (car opencode-session-agents)))
    (unless (string= "primary" (alist-get 'mode opencode-session-agent))
      (opencode-cycle-session-agent)))
  (force-mode-line-update))

(defun opencode--session-buffer-p (buf)
  "Return non-nil if BUF is a opencode session buffer.
Accepts (\"name\" . buffer) to work as a read-buffer predicate."
  (with-current-buffer (cl-etypecase buf
                         ((or string buffer) buf)
                         (list (cdr buf)))
    opencode-session-id))

(defun opencode-select-open-session ()
  "Select among open session buffers."
  (interactive)
  (switch-to-buffer
   (read-buffer "Switch to: " nil t 'opencode--session-buffer-p)))

(defun opencode-consult-sessions ()
  "Run `consult-line-multi' across all open opencode sessions."
  (interactive)
  (if (require 'consult nil t)
      (progn
        (declare-function consult-line-multi "consult")
        (consult-line-multi '(:predicate opencode--session-buffer-p)))
    (user-error "This requires consult")))

(defun opencode-visit-last-idle ()
  "Open the most recent session to notify as idle."
  (interactive)
  (if opencode-alerted-sessions
      (opencode-open-session (pop opencode-alerted-sessions))
    (message "No idle sessions")))

(defun opencode-select-session ()
  "Select and open a session from the current project using completion."
  (interactive)
  (opencode-api-sessions sessions
    (opencode--select-sessions "Session: "
                               (seq-remove (lambda (session)
                                             (alist-get 'parentID session))
                                           sessions)
                               (format "No sessions in %s" default-directory)
                               :pop-to-buffer nil)))

(cl-defun opencode--select-sessions (prompt sessions no-sessions-message &key (pop-to-buffer t))
  "Select a session from SESSIONS to open.
Use PROMPT and display NO-SESSIONS-MESSAGE if SESSIONS is empty.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer."
  (if sessions
      (opencode-open-session
       (opencode--annotated-completion
        prompt
        (cl-loop for session in sessions
                 collect (let-alist session
                           (list .title
                                 session
                                 (opencode--format-time-ago
                                  (opencode--time-ago
                                   session 'updated))
                                 (opencode--time-ago
                                  session 'updated)))))
       :pop-to-buffer pop-to-buffer)
    (message no-sessions-message)))

(defun opencode-select-idle ()
  "Select a session that hasn't been visited since it went idle."
  (interactive)
  (opencode--select-sessions "Session: " opencode-alerted-sessions "No idle sessions"))

(defun opencode--collect-all-models ()
  "Collect all models from `opencode-providers' as a list.
Each element is (display-name . (provider-id provider-name model-id))."
  (let (result)
    (dolist (provider opencode-providers)
      (let-alist provider
        (dolist (model-entry .models)
          (let ((model (cdr model-entry)))
            (push (list (alist-get 'name model)
                        `((providerID . ,.id)
                          (modelID . ,(alist-get 'id model)))
                        .name)
                  result)))))
    (nreverse result)))

(defun opencode-select-model ()
  "Select a model for the current session and agent using completion."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (when-let ((model (opencode--annotated-completion
                     "Model: "
                     (opencode--collect-all-models))))
    (setf (alist-get 'model opencode-session-agent) model
          opencode-last-model model)
    (when-let ((variant (alist-get 'variant opencode-session-agent)))
      (unless (alist-get variant
                         (alist-get 'variants (opencode--current-model)))
        (setq opencode-session-agent
              (assq-delete-all 'variant opencode-session-agent))))))

(defun opencode--current-model ()
  "Return the active model for this session."
  (let-alist opencode-session-agent
    (map-nested-elt
     (seq-find (lambda (provider)
                 (string= .model.providerID (alist-get 'id provider)))
               opencode-providers)
     `(models ,(intern .model.modelID)))))

(defun opencode-select-variant ()
  "Select a variant for the current model."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (let ((variants (alist-get 'variants (opencode--current-model))))
    (if variants
        (setf (alist-get 'variant opencode-session-agent)
              (opencode--annotated-completion
               "Variant: "
               (cl-loop for (variant . options) in
                        variants
                        collect (list (symbol-name variant)
                                      variant
                                      (format "%s" options)))))
      (message "No variants"))))

(defun opencode-select-child-session ()
  "Open a child (subagent) session of the current session."
  (interactive)
  (opencode-api-session-children (opencode-session-id)
      children
    (opencode--select-sessions "Subagent: " children "No children")))

(defun opencode-open-parent ()
  "Open the parent of the current session."
  (interactive)
  (opencode-api-session (opencode-session-id)
      session
    (opencode-api-session ((alist-get 'parentID session))
        parent
      (opencode-open-session parent))))

(defun opencode-share-session (&optional session)
  "Share SESSION or the current session."
  (interactive)
  (opencode-api-share-session ((or (alist-get 'id session)
                                   opencode-session-id))
      session
    (let ((url (map-nested-elt session '(share url))))
      (gui-select-text url)
      (message "Copied to clipboard: %s" url))))

(defun opencode-unshare-session (&optional session)
  "Unshare SESSION or the current session."
  (interactive)
  (opencode-api-unshare-session ((or (alist-get 'id session)
                                     opencode-session-id))
      _session
    (message "Session no longer shared")))

(defun opencode-unshare-all-sessions ()
  "Unshare all sessions across all projects."
  (interactive)
  (opencode-api-projects projects
    (dolist (project projects)
      (let ((default-directory (alist-get 'worktree project)))
        (opencode-api-sessions sessions
          (dolist (session sessions)
            (when (alist-get 'share session)
              (opencode-unshare-session session))))))))

(defun opencode-insert-slash-command ()
  "Insert an opencode slash command."
  (interactive)
  (if (= (point) (cdr comint-last-prompt))
      (let* ((directory (opencode--normalize-directory default-directory))
             (commands (alist-get directory opencode--slash-commands-by-directory
                                  nil nil #'string=))
             (command (opencode--annotated-completion
                       "Slash command: "
                       (cl-loop for command in commands
                                collect (let-alist command
                                          (list
                                           .name
                                           .name
                                           .description))))))
        (when command
          (insert (concat "/" command))))
    (call-interactively #'self-insert-command)))

(defun opencode--session-status-indicator ()
  "Return mode line indicator for session status."
  (let-alist opencode-session-agent
    (let* ((agent (pcase .name
                    ("Planner-Sisyphus" "Planner")
                    (name name)))
           (model (opencode--current-model))
           (status (pcase opencode-session-status
                     ("busy" "⏳")
                     ("idle" "🚀")
                     (_ "")))
           (context-used (* 100 (/ (float opencode-session-tokens)
                                   (map-nested-elt model '(limit context))))))
      (if (< (window-width) 115)
          (format "[🤖 %s] %.0f%%%% %s  " agent context-used status)
        (format "[🤖 %s - %s] %.0f%%%% %s  "
                agent
                (concat (alist-get 'name model)
                        (when .variant
                          (propertize (format " %s" .variant)
                                      'face '(bold opencode-request-margin-highlight))))
                context-used status)))))

(defun opencode-session--set-status (session-id status)
  "Set STATUS for the session with SESSION-ID and update modeline."
  (when-let (buffer (gethash session-id opencode-session-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq opencode-session-status status)
        (force-mode-line-update)))))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender 'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  (visual-line-mode)
  (font-lock-mode -1)
  (cursor-intangible-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image)
  (add-hook 'comint-input-filter-functions 'opencode--render-input-markdown nil t))

(defun opencode-yank-code-block ()
  "Yank the markdown code block under point."
  (interactive)
  (save-excursion
    (markdown-backward-block)
    (copy-region-as-kill (point)
                         (progn (markdown-forward-block)
                                (point)))))

(defun opencode-kill-session (&optional session)
  "Kill SESSION."
  (interactive)
  (opencode-api-delete-session ((or (alist-get 'id session)
                                    opencode-session-id))
      result
    (unless result
      (error "Unable to delete session"))
    ;; if called from session control buffer, don't kill buffer
    ;; but if called from a session buffer then kill this buffer
    (unless session
      (kill-this-buffer))))

(defun opencode--highlight-input (&optional _proc _string)
  "Highlight last prompt input."
  (opencode--add-margin comint-last-input-start
                        comint-last-input-end
                        'opencode-request-margin-highlight))

(defun opencode--mimetype (file)
  "Return guess of mimetype for FILE."
  (pcase (file-name-extension file)
    ((or "org" "ts" (pred null)) "text/plain")
    (ext (or (mailcap-extension-to-mime ext)
             "text/plain"))))

(defun opencode--remove-label (overlay after _beg _end &optional _len)
  "Called AFTER deleting OVERLAY, remove the associated part from context."
  (when (and after
             (not (eq this-command 'comint-send-input)))
    (let ((file-url (overlay-get overlay 'file-url))
          (buffer-name (overlay-get overlay 'buffer-name))
          (region-id (overlay-get overlay 'region-id))
          (agent-name (overlay-get overlay 'agent-name))
          (ov-start (overlay-start overlay))
          (ov-end (overlay-end overlay)))
      (setq opencode--extra-parts
            (seq-remove (lambda (part)
                          (let-alist part
                            (cond
                             (file-url (string= file-url .url))
                             (buffer-name (string= buffer-name .metadata.buffer-name))
                             (region-id (eq region-id .metadata.region-id))
                             (agent-name (string= agent-name .name)))))
                        opencode--extra-parts))
      (delete-region ov-start ov-end)
      (delete-overlay overlay))))

(defun opencode--in-label-p ()
  "Return non-nil if point is inside (or at the edge of) a label overlay."
  (cl-loop for ov in (overlays-at (point))
           thereis (overlay-get ov 'opencode-label)))

(defun opencode--insert-intangible (name extra-prop extra-value)
  "Insert an intangible label with NAME (buffer or filename).
Assign the overlay EXTRA-PROP with EXTRA-VALUE."
  (let* ((start (point))
         (end (progn (insert "`")
                     (insert (propertize name 'cursor-intangible t))
                     (insert (propertize "`" 'rear-nonsticky t))
                     (point)))
         (ov (make-overlay start end)))
    (overlay-put ov 'opencode-label t)
    (overlay-put ov extra-prop extra-value)
    (overlay-put ov 'display (propertize name 'face 'markdown-inline-code-face))
    (overlay-put ov 'modification-hooks '(opencode--remove-label))
    ov))

(defun opencode--add-file-to-context (file &optional mime display-name)
  "Add FILE to context with optional MIME and DISPLAY-NAME."
  (let* ((display-name (or display-name
                           (opencode--relative-path-for-display file)))
         (mime (or mime (opencode--mimetype file)))
         (url (concat "file://" file)))
    (push `((type . file)
            (filename . ,display-name)
            (mime . ,mime)
            (url . ,url))
          opencode--extra-parts)
    (opencode--insert-intangible display-name 'file-url url)
    (insert " ")))

(defun opencode--image-extension (mime)
  "Return a file extension for image MIME."
  (or (car (split-string (or (cadr (split-string mime "/")) "")
                         "[;+]" t))
      "img"))

(defun opencode--yank-image (type data)
  "Add pasted image DATA of MIME TYPE to the next prompt."
  (unless (comint-after-pmark-p)
    (user-error "Images can only be pasted into the current prompt"))
  (let* ((mime (symbol-name type))
         (extension (opencode--image-extension mime))
         (file (make-temp-file "opencode-image-" nil
                               (concat "." extension))))
    (condition-case error
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region data nil file nil 'silent))
          (push file opencode--temp-files)
          (opencode--add-file-to-context file mime (file-name-nondirectory file)))
      (error
       (ignore-errors (delete-file file))
       (signal (car error) (cdr error))))))

(defun opencode--delete-temp-files (files)
  "Delete temporary FILES created by opencode."
  (dolist (file files)
    (when (and file (file-exists-p file))
      (ignore-errors (delete-file file)))))

(defun opencode--session-cleanup ()
  "Clean up resources for the current session buffer."
  (when-let ((process (get-buffer-process (current-buffer))))
    (delete-process process))
  (opencode--delete-temp-files opencode--temp-files)
  (setq opencode--temp-files nil))

(defmacro with-last-opencode-session (&rest body)
  "Run BODY with the last used opencode session buffer active."
  `(if opencode-last-session-buffer
       (if (buffer-live-p opencode-last-session-buffer)
           (with-current-buffer opencode-last-session-buffer
             (unless (comint-after-pmark-p)
               (goto-char (point-max)))
             ,@body)
         (user-error "Last selected OpenCode session buffer is no longer active"))
     (user-error "Open an OpenCode session first")))

(defun opencode-add-file (&optional file)
  "Add a FILE to context, or prompt for file in current project."
  (interactive)
  (with-last-opencode-session
   (let* ((project (project-current t))
          (file (or file
                    (project--read-file-name project
                                             "Add to context"
                                             (project-files project)
                                             nil
                                             'file-name-history))))
     (opencode--add-file-to-context file))))

(defun opencode-add-file-dwim ()
  "If in Dired, add all marked files, or file at point if none marked.
Otherwise add the current buffer's file.
Otherwise prompt for file in current project."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (if-let (files (dired-get-marked-files))
          (mapc #'opencode-add-file files)
        (if-let (file (dired-get-filename nil t))
            (opencode-add-file file)
          (user-error "No file at point, and no marked files")))
    (opencode-add-file (buffer-file-name))))

(defun opencode-add-buffer-dwim ()
  "Add current buffer or prompt for one to add."
  (interactive)
  (let ((buffer (if opencode-session-id
                    (read-buffer "Add to context: ")
                  (buffer-name))))
    (with-last-opencode-session
     (push `((type . text)
             (text . ,(concat
                       (format "<buffer name=\"%s\">" buffer)
                       (with-current-buffer buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                       "</buffer>"))
             (synthetic . t)
             (metadata . ((buffer-name . ,buffer))))
           opencode--extra-parts)
     (opencode--insert-intangible buffer 'buffer-name buffer))))

(defun opencode-add-region ()
  "Add the active region to context."
  (interactive)
  (if (use-region-p)
      (let ((region (buffer-substring-no-properties (region-beginning)
                                                    (region-end)))
            (region-id (gensym)))
        (with-last-opencode-session
         (push `((type . text)
                 (text . ,(concat "<region>" region "</region>"))
                 (synthetic . t)
                 (metadata . ((region-id . ,region-id))))
               opencode--extra-parts)
         (opencode--insert-intangible
          (format "region: %s"
                  (truncate-string-to-width region 24 0 nil (truncate-string-ellipsis)))
          'region-id region-id)
         (insert " ")))
    (user-error "No active region")))

(defun opencode-add-subagent ()
  "Insert a subagent mention into the current input."
  (interactive)
  (if (comint-after-pmark-p)
      (let ((subagents (cl-loop for agent in opencode-agents
                                when (string= "subagent" (alist-get 'mode agent))
                                collect agent)))
        (unless subagents
          (user-error "No subagents available"))
        (let ((name (opencode--annotated-completion
                     "Subagent: "
                     (cl-loop for agent in subagents
                              collect (let-alist agent
                                        (list .name .name .description))))))
          (push `((type . "agent")
                  (name . ,name))
                opencode--extra-parts)
          (opencode--insert-intangible (concat "@" name) 'agent-name name)))
    (call-interactively #'self-insert-command)))

(defun opencode--send-input (_proc string)
  "Send STRING as input to current opencode session."
  (opencode--highlight-input)
  (opencode--output "\n")
  (let ((extra-parts opencode--extra-parts)
        (temp-files opencode--temp-files)
        sent-message)
    (setf opencode--extra-parts nil
          opencode--temp-files nil)
    (let-alist opencode-session-agent
      (cond
       ((string-prefix-p "/" string)
        (let ((space-pos (seq-position string ?\s)))
          (opencode-api-execute-command (opencode-session-id)
              `((agent . ,.name)
                (model . ,(concat .model.providerID "/" .model.modelID))
                (command . ,(substring string 1 space-pos))
                (arguments . ,(if space-pos
                                  (substring string (1+ space-pos))
                                "")))
              _response)))
       ((string-prefix-p "!" string)
        (opencode-api-execute-shell (opencode-session-id)
            `((agent . ,.name)
              (command . ,(substring string 1)))
            _response))
       (t
        (setq sent-message t)
        (opencode-api-send-message (opencode-session-id)
            `((agent . ,.name)
              ,(assoc 'model opencode-session-agent)
              ,@(when .variant
                  `((variant . ,.variant)))
              (parts . ,(nreverse
                         (cons `((type . text) (text . ,string))
                               extra-parts))))
            _result
          (opencode--delete-temp-files temp-files)))))
    (unless sent-message
      (opencode--delete-temp-files temp-files))))

(defun opencode-session--send-synthetic-input (string)
  "Send STRING to the current opencode session.
Preserve any pending input context while sending STRING as a plain prompt."
  (let* ((process (get-buffer-process (current-buffer)))
         (pending-prompt (copy-marker (process-mark process) t))
         (original-point (copy-marker (point) t))
         (prompt-start (opencode--session-process-position))
         start
         end)
    (unwind-protect
        (progn
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char prompt-start)
              (setq start (point))
              (insert string)
              (setq end (point))))
          (set-marker (process-mark process) end)
          (let ((comint-last-input-start start)
                (comint-last-input-end end)
                (opencode--extra-parts nil)
                (opencode--temp-files nil))
            (opencode--send-input process string)
            (opencode--maybe-insert-block-spacing)))
      (set-marker (process-mark process) pending-prompt)
      (goto-char original-point)
      (set-marker pending-prompt nil)
      (set-marker original-point nil))))

(defun opencode-session--message-updated (info)
  "Handle message.updated event with INFO."
  (let-alist info
    (pcase .role
      ("assistant"
       (when .time.completed
         (when-let (buffer (map-elt opencode-session-buffers .sessionID))
           (with-current-buffer buffer
             (setq opencode-session-tokens
                   (+ .tokens.input .tokens.output .tokens.reasoning
                      .tokens.cache.read .tokens.cache.write))
             (force-mode-line-update))))
       (if (or .finish .error)
           (setf opencode-assistant-messages
                 (assoc-delete-all .id opencode-assistant-messages))
         (push (cons .id nil) opencode-assistant-messages))))))

(defun opencode--render-region (type start &optional end)
  "Render TYPE markdown from START to END.
END defaults to the process mark."
  (setq end (or end (opencode--session-process-position)))
  (when (and (seq-contains-p '(reasoning text) type)
             (< start end))
    (let ((inhibit-read-only t)
          (text (buffer-substring-no-properties start end)))
      (ignore-errors
        (setf text (opencode--render-markdown text)))
      (delete-region start end)
      (cl-case type
        (reasoning (opencode--insert-reasoning-block text))
        (text (opencode--output text))))))

(defun opencode--stream-line-start ()
  "Return the current line start at process mark."
  (save-excursion
    (goto-char (opencode--session-process-position))
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defun opencode--maybe-insert-block-spacing ()
  "Ensure \n\n before block."
  (let ((pos (opencode--session-process-position)))
    (opencode--output
     (cond
      ((not (eq ?\n (char-before pos))) "\n\n")
      ((and (eq ?\n (char-before pos))
            (not (eq ?\n (char-before (1- pos)))))
       "\n")
      (t "")))))

(defun opencode--output (string)
  "Output STRING as comint output."
  (comint-output-filter (get-buffer-process (current-buffer))
                        (opencode--buttonize-file-references string)))

(defun opencode-insert-logo ()
  "Insert the opencode logo."
  (let ((logo-left '("                   " "█▀▀█ █▀▀█ █▀▀█ █▀▀▄"
                     "█░░█ █░░█ █▀▀▀ █░░█" "▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀  ▀"))
        (logo-right '("             ▄     " "█▀▀▀ █▀▀█ █▀▀█ █▀▀█"
                      "█░░░ █░░█ █░░█ █▀▀▀" "▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀")))
    (cl-loop for line in logo-left
             for index from 0
             do
             (opencode--output (propertize line 'face 'shadow))
             (opencode--output " ")
             (opencode--output (propertize (nth index logo-right) 'face 'bold))
             (opencode--output "\n"))
    (opencode--output "\n")))

(defun opencode--show-prompt ()
  "Highlight the prompt after displaying output."
  (opencode--output (propertize "> " 'invisible t))
  (opencode--add-margin (car comint-last-prompt)
                        (cdr comint-last-prompt)
                        'opencode-request-margin-highlight)
  (goto-char (point-max)))

(defun opencode-session--update-part (part delta type)
  "Display PART, partial message output. DELTA is new text since last update.
TYPE is text|reasoning|tool|step-finish"
  (let-alist part
    (if .time.end
        (remhash .id opencode-part-type)
      ;; only will follow up with message.part.delta updates when it has "" as text
      (when (string-empty-p .text)
        (puthash .id type opencode-part-type)))
    (when-let ((buffer (gethash .sessionID opencode-session-buffers))
               (process (get-buffer-process buffer))
               (message-parts (assoc-string .messageID opencode-assistant-messages)))
      (with-current-buffer buffer
        (let ((last-type (cadr message-parts))
              (last-start (cddr message-parts)))
          (cl-flet ((maybe-render-last-and-update-message-parts
                      (type)
                      (unless (eq type last-type)
                        (when last-start
                          (opencode--render-region last-type last-start)
                          (opencode--maybe-insert-block-spacing))
                        (setf (cdr message-parts) (cons type
                                                        (marker-position (process-mark process)))))))
            ;; don't display margins on extra whitespace
            (when (and delta
                       (not (string= type last-type)))
              (setf delta (string-trim-left delta)))
            (pcase type
              ((and "reasoning" (guard delta))
               (maybe-render-last-and-update-message-parts 'reasoning)
               (let ((start (opencode--stream-line-start)))
                 (opencode--insert-reasoning-block delta)
                 (opencode--render-region 'reasoning start)))
              ((and "text" (guard delta))
               (maybe-render-last-and-update-message-parts 'text)
               (let ((start (opencode--stream-line-start)))
                 (opencode--output delta)
                 (opencode--render-region 'text start)))
              ("tool" (maybe-render-last-and-update-message-parts 'tool)
               (when (and
                      ;; only when it first starts running
                      (string= .state.status "running")
                      ;; avoid duplicate display
                      (not (gethash .callID opencode--tool-calls-displayed))
                      ;; skip the question tool, handled by question.asked event
                      (not (string= .tool "question")))
                 (puthash .callID t opencode--tool-calls-displayed)
                 (opencode--insert-tool-block .tool .state.input))
               (opencode--maybe-insert-tool-output part))
              ("step-finish"
               (when (string= "stop" .reason)
                 (opencode--render-region last-type last-start)
                 (opencode--maybe-insert-block-spacing))))))))))

(defface opencode-request-margin-highlight
  '((t :inherit outline-1 :height reset))
  "OpenCode margin face to apply to user requests."
  :group 'opencode-faces)

(defface opencode-reasoning-margin-highlight
  '((t :inherit outline-2 :height reset))
  "OpenCode margin face to apply to reasoning blocks."
  :group 'opencode-faces)

(defface opencode-tool-margin-highlight
  '((t :inherit outline-5 :height reset))
  "OpenCode margin face to apply to tool call blocks."
  :group 'opencode-faces)

(defun opencode--margin (face)
  "Return margin string for FACE."
  (propertize ">" 'display
              `((margin left-margin)
                ,(propertize "▎" 'face
                             face))))

(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  (let ((ov (make-overlay start (1- end)))
        (margin (opencode--margin face)))
    (overlay-put ov 'line-prefix margin)
    (overlay-put ov 'wrap-prefix margin)))

(defun opencode--render-input-markdown (input)
  "Rerender comint INPUT as markdown."
  (let ((inhibit-read-only t))
    (delete-region (opencode--session-process-position)
                   (point))
    (insert (opencode--render-markdown input))))

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (opencode--show-prompt)
  (let-alist message
    (dolist (part .parts)
      (let-alist part
        (unless .synthetic
          (pcase .type
            ("text" (insert .text))))))
    (insert "\n")
    (let ((comint-input-sender #'opencode--highlight-input))
      (comint-send-input))
    (let ((agent (seq-find (lambda (agent)
                             (string= (alist-get 'name agent)
                                      .info.agent))
                           opencode-session-agents)))
      (setq opencode-session-agent agent)
      (setf (alist-get 'model opencode-session-agent) .info.model)
      (setf (alist-get 'variant opencode-session-agent) .info.model.variant))))

(defun opencode--insert-block-with-margin (text face)
  "Insert TEXT with FACE margin highlight."
  (unless (string-empty-p text)
    (let ((beginning (opencode--session-process-position)))
      (opencode--output text)
      (opencode--add-margin beginning (save-excursion
                                        (goto-char (opencode--session-process-position))
                                        (skip-chars-backward "\r\n[:blank:]")
                                        (point))
                            face))))

(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (opencode--insert-block-with-margin text 'opencode-reasoning-margin-highlight))

(defun opencode--refine-diff-hunks (start)
  "Refine all diff hunks between START and process marker."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (opencode--session-process-position))
      (condition-case nil
          (while (>= (point) start)
            (diff-refine-hunk)
            (diff-hunk-prev)
            ;; Hide the diff hunk headers
            (add-text-properties (line-beginning-position)
                                 (min (point-max)
                                      (1+ (line-end-position)))
                                 '(invisible t)))
        (error nil)))))

(defun opencode--session-process-position ()
  "Return position of process marker."
  (marker-position
   (process-mark
    (get-buffer-process
     (current-buffer)))))

(defun opencode--insert-tool-block (tool input)
  "Insert TOOL call with INPUT as margin-highlighted block."
  (let ((start (opencode--session-process-position)))
    (opencode--insert-block-with-margin
     (opencode--format-tool-call tool input)
     'opencode-tool-margin-highlight)
    ;; For diff-like tools, apply diff hunk refinement after insertion.
    (when (member tool '("edit" "apply_patch"))
      (opencode--refine-diff-hunks start))))

(defun opencode--maybe-insert-tool-output (message)
  "Maybe insert the output from tool call in MESSAGE."
  (let-alist message
    (when (and (string= .state.status "completed")
               (or opencode-show-tool-output
                   ;; show output for user run shell commands
                   (and (string= .tool "bash")
                        (not .state.input.description)))
               .state.output)
      (opencode--output .state.output)
      (opencode--output "\n"))))

(defun opencode-open-session-same-window (session)
  "Open SESSION using the current window."
  (opencode-open-session session :pop-to-buffer nil))

(cl-defun opencode-open-session (session &key (pop-to-buffer t))
  "Open comint based shell for SESSION.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer.
Returns the buffer."
  (let-alist session
    (let ((old-buffer (gethash .id opencode-session-buffers)))
      (if (buffer-live-p old-buffer)
          (if pop-to-buffer
              (pop-to-buffer old-buffer)
            (switch-to-buffer old-buffer))
        (let ((buffer (generate-new-buffer (format "*OpenCode: %s*" .title)))
              (agent (copy-tree opencode-session-agent))
              (agents (copy-tree opencode-session-agents)))
          (with-current-buffer buffer
            (opencode-session-mode)
            (setq opencode-session-id .id
                  opencode-last-session-buffer buffer
                  default-directory (file-name-as-directory .directory)
                  opencode--tool-calls-displayed (make-hash-table :test 'equal)
                  opencode-session-agents (mapcar (lambda (agent)
                                                    (unless (alist-get 'model agent)
                                                      (setf (alist-get 'model agent)
                                                            opencode-last-model))
                                                    agent)
                                                  (or agents
                                                      (copy-tree opencode-agents)))
                  opencode-session-agent (or agent (car opencode-session-agents))
                  mode-line-process '(:eval (opencode--session-status-indicator)))
            (hack-dir-local-variables-non-file-buffer)
            (add-hook 'fill-nobreak-predicate #'opencode--in-label-p nil t)
            (puthash .id buffer opencode-session-buffers)
            (let ((proc (start-process "dummy" buffer nil)))
              (set-process-query-on-exit-flag proc nil)
              (add-hook 'kill-buffer-hook #'opencode--session-cleanup nil t)
              (opencode-insert-logo)
              (opencode-api-session-messages (.id)
                  messages
                (dolist (message messages)
                  (let-alist (alist-get 'info message)
                    (pcase .role
                      ("user" (opencode--replay-user-request message))
                      ("assistant"
                       (dolist (part (alist-get 'parts message))
                         (let-alist part
                           (if (string= .type "tool")
                               (progn
                                 (opencode--insert-tool-block .tool .state.input)
                                 (opencode--maybe-insert-tool-output message))
                             (when .text
                               (let ((text (opencode--render-markdown (string-trim .text))))
                                 (unless (string-empty-p text)
                                   (pcase .type
                                     ("text" (opencode--output text))
                                     ("reasoning"
                                      (opencode--insert-reasoning-block
                                       text)))
                                   (opencode--output "\n\n")))))))
                       (setq opencode-session-tokens
                             (+ .tokens.input .tokens.output .tokens.reasoning
                                .tokens.cache.read .tokens.cache.write))))))
                (opencode--show-prompt)))
            (if pop-to-buffer
                (pop-to-buffer buffer)
              (switch-to-buffer buffer))))))))

(defun opencode--current-message-number ()
  "Return the 0-indexed message number at point.
Counts prompts from the beginning of the buffer to the current position.
Returns nil if point is before the first prompt."
  (save-excursion
    (end-of-line)
    (comint-previous-prompt 1)
    (let ((target-point (point)))
      (goto-char (point-min))
      (cl-loop do (comint-next-prompt 1)
               while (< (point) target-point)
               count t))))

(defmacro opencode--current-message-id (result &rest body)
  "Run BODY with RESULT as the message id of the user message at point."
  (declare (indent defun))
  `(opencode--current-message-exchange (,result _assistant-id)
     ,@body))

(defmacro opencode--current-message-exchange (bindings &rest body)
  "Run BODY with BINDINGS (user-id assistant-id)
bound to the exchange ids at point."
  (declare (indent defun))
  (let ((user-id (car bindings))
        (assistant-id (cadr bindings)))
    `(when-let (message-number (opencode--current-message-number))
       (opencode-api-session-messages (opencode-session-id)
           messages
         (let* ((user-message-index
                 (cl-loop for message in messages
                          for index from 0
                          when (string= "user" (map-nested-elt message '(info role)))
                          count t into count
                          when (= (1- count) message-number)
                          return index))
                (user-message (and user-message-index
                                   (nth user-message-index messages)))
                (assistant-message (and user-message-index
                                        (let ((next-message (nth (1+ user-message-index)
                                                                 messages)))
                                          (when (string= "assistant"
                                                         (map-nested-elt next-message
                                                                         '(info role)))
                                            next-message))))
                (,user-id (map-nested-elt user-message '(info id)))
                (,assistant-id (map-nested-elt assistant-message '(info id))))
           ,@body)))))

(defun opencode--delete-message-at-point ()
  "Delete the prompt at point and its output from the session buffer."
  (let ((start (save-excursion
                 (end-of-line)
                 (comint-previous-prompt 1)
                 (line-beginning-position)))
        (end (save-excursion
               (end-of-line)
               (comint-previous-prompt 1)
               (goto-char (line-beginning-position 2))
               (if (ignore-errors (comint-next-prompt 1) t)
                   (line-beginning-position)
                 (point-max)))))
    (let ((inhibit-read-only t))
      (remove-overlays start end)
      (delete-region start end))))

(defun opencode-rename-session (&optional session)
  "Rename SESSION. If in a session buffer, rename that session."
  (interactive)
  (let ((title (read-string "Title: ")))
    (opencode-api-rename-session ((or (alist-get 'id session)
                                      opencode-session-id))
        `((title . ,title))
        _res
      (unless session
        (rename-buffer
         (generate-new-buffer-name (format "*OpenCode: %s*" title)))))))

(defun opencode-session--display-error (session-id message)
  "Display error MESSAGE in SESSION-ID and then new prompt."
  (when-let (buffer (gethash session-id opencode-session-buffers))
    (with-current-buffer buffer
      (opencode--output (propertize message 'face 'error))
      (opencode--output "\n\n")
      (opencode--show-prompt))))

(defun opencode-abort-session ()
  "Abort a busy session and go back to prompt."
  (interactive)
  (opencode-api-abort-session (opencode-session-id)
      success-p
    (unless success-p
      (message "Failed to abort session."))))

(defun opencode-session-control-toggle-verbose ()
  "Toggle verbose mode in session control buffer."
  (interactive)
  (setq opencode-session-control-verbose
        (not opencode-session-control-verbose))
  (opencode-sessions-redisplay))

(defun opencode-sessions-redisplay ()
  "Refresh the session display table for DIRECTORY."
  (interactive)
  (opencode-api-sessions sessions
    (let ((inhibit-read-only t)
          (point (point))
          (sessions (if opencode-session-control-verbose
                        sessions
                      (seq-remove (lambda (session)
                                    (alist-get 'parentID session))
                                  sessions)))
          cache)
      (erase-buffer)
      (if sessions
          (make-vtable
           :columns '("Title"
                      (:name "Branch" :min-width 6)
                      (:name "Last Updated" :width 12
                       :formatter opencode--format-time-ago
                       :primary ascend)
                      (:name "Files changed" :width 13 :align right)
                      (:name "Created at" :width 10
                       :formatter opencode--format-time-ago))
           :objects sessions
           :actions '("x" opencode-kill-session
                      "R" opencode-rename-session
                      "s" opencode-share-session
                      "u" opencode-unshare-session
                      "RET" opencode-open-session-same-window
                      "o" opencode-open-session-same-window)
           :getter (lambda (object column vtable)
                     (let-alist object
                       (pcase (vtable-column vtable column)
                         ("Title" (if .share
                                      (concat (propertize "shared " 'face
                                                          '(bold opencode-request-margin-highlight))
                                              .title)
                                    .title))
                         ("Branch" (if (and .directory (file-exists-p .directory))
                                       (let ((default-directory .directory))
                                         (with-memoization
                                             (map-elt cache .directory)
                                           (magit-get-current-branch)))
                                     "-"))
                         ("Last Updated" (opencode--time-ago object 'updated))
                         ("Files changed" (let-alist .summary
                                            (if (opencode--json-falsy .files)
                                                "none"
                                              (format "%d  +%d-%d"
                                                      .files
                                                      (if (opencode--json-falsy .additions)
                                                          0 .additions)
                                                      (if (opencode--json-falsy .deletions)
                                                          0 .deletions)))))
                         ("Created at" (opencode--time-ago object 'created)))))
           :separator-width 3
           :keymap opencode-session-control-mode-map)
        (insert "No sessions in " (or default-directory "unknown directory")))
      (goto-char point))))

(provide 'opencode-sessions)
;;; opencode-sessions.el ends here
