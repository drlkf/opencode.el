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
(require 'markdown-ts-mode)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-convert)
(require 'opencode-diff-parser)
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
    "C-c Y" 'opencode-copy-conversation
    "C-c C-c" 'opencode-abort-session
    "C-c C-p" 'opencode-respond-permission
    "C-c x" 'opencode-kill-session
    "<backtab>" 'backward-button
    "TAB" 'opencode--tab-dispatch
    "C-c r" 'opencode-rename-session
    "C-c n" 'opencode-new-session
    "C-c l" 'opencode-select-session
    "C-c c" 'opencode-select-child-session
    "C-c C" 'opencode-compact-session
    "C-c t" 'opencode-scroll-to-last-response-start
    "C-c p" 'opencode-open-parent
    "C-c f" 'opencode-add-file
    "C-c b" 'opencode-add-buffer-dwim
    "C-c m" 'opencode-select-model
    "C-c v" 'opencode-select-variant
    "C-c M" 'opencode-toggle-mcp
    "C-c F" 'opencode-fork-session
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

(defvar-local opencode--table-scan-marker nil
  "Marker at the unprocessed Markdown in the current assistant text part.")

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

(defvar-local opencode--skip-files-finished-editing-hook-on-next-idle nil
  "Non-nil means the next session idle should not run finished-editing hooks.")

(defvar opencode-session-buffers
  (make-hash-table :test 'equal)
  "A mapping of session ids to Emacs buffers.")

(defvar-local opencode--tool-calls-displayed nil
  "A hash table containing all callID that have already been displayed.")

(defvar-local opencode--tool-call-info nil
  "A hash table mapping in-flight tool callID to (NAME . INPUT).")

(defvar-local opencode-last-response-start nil
  "Position of the start of the latest assistant text block.")

(defvar-local opencode-session--last-block nil
  "Cons (KIND . KEY) of the most recently opened streaming block.
KIND is \\='text or \\='reasoning; KEY is (ASSISTANT-MESSAGE-ID . ORDINAL).
Used to detect new blocks for spacing, margins, and table-scan setup.")

(defvar-local opencode-session--synced-agent nil
  "Name of the agent most recently confirmed set on the server for this session.")

(defvar-local opencode-session--synced-model nil
  "Model.Ref most recently confirmed set on the server for this session.")

(defmacro opencode--with-session-buffer (session-id &rest body)
  "Run BODY in the live session buffer for SESSION-ID."
  (declare (indent 1))
  `(when-let ((buffer (gethash ,session-id opencode-session-buffers)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         ,@body))))

(defun opencode-cycle-session-agent ()
  "Switch to the next agent in `opencode-session-agents'."
  (interactive)
  (let* ((name (alist-get 'name opencode-session-agent))
         (agents opencode-session-agents)
         (pos (and name (cl-position-if
                         (lambda (agent)
                           (string= name (alist-get 'name agent)))
                         agents))))
    (when pos
      (setcar (nthcdr pos agents) opencode-session-agent)
      (when-let ((next (seq-find
                        (lambda (agent)
                          (string= "primary" (alist-get 'mode agent)))
                        (append (nthcdr (1+ pos) agents)
                                (seq-take agents (1+ pos))))))
        (setq opencode-session-agent next))))
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

(declare-function opencode-autoconnect "opencode")

(defun opencode-select-session ()
  "Select and open a session from the current project using completion."
  (interactive)
  (opencode-autoconnect
   (lambda ()
     (opencode-api-sessions sessions
       (opencode--select-sessions "Session: "
                                  (seq-remove (lambda (session)
                                                (alist-get 'parentID session))
                                              sessions)
                                  (format "No sessions in %s" default-directory)
                                  :pop-to-buffer nil)))))

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
  "Collect all models from `opencode-models' as a list.
Each element is (display-name . model-ref), where model-ref is a Model.Ref
alist of (id . model-id) (providerID . provider-id)."
  (mapcar (lambda (model)
            (let-alist model
              (list .name `((id . ,.id) (providerID . ,.providerID)) .providerID)))
          opencode-models))

(defun opencode-select-model ()
  "Select a model for the current session and agent using completion."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (when-let ((model (opencode--annotated-completion
                     "Model: "
                     (opencode--collect-all-models))))
    (setf (alist-get 'model opencode-session-agent) model
          opencode-last-model model)))

(defun opencode--current-model ()
  "Return the active Model.Info for this session's agent."
  (let-alist opencode-session-agent
    (seq-find (lambda (model)
                (and (equal .model.id (alist-get 'id model))
                     (equal .model.providerID (alist-get 'providerID model))))
              opencode-models)))

(defun opencode-select-variant ()
  "Select a variant for the current model."
  (interactive)
  (unless opencode-session-agent
    (user-error "not in a session"))
  (let ((variants (alist-get 'variants (opencode--current-model))))
    (if variants
        (setf (alist-get 'variant (alist-get 'model opencode-session-agent))
              (opencode--annotated-completion
               "Variant: "
               (cl-loop for variant in variants
                        for id = (alist-get 'id variant)
                        collect (list id id (format "%s" (alist-get 'settings variant))))))
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
                        (when .model.variant
                          (propertize (format " %s" .model.variant)
                                      'face '(bold opencode-request-margin-highlight))))
                context-used status)))))

(defun opencode-session--set-status (session-id status)
  "Set STATUS for the session with SESSION-ID and update modeline."
  (opencode--with-session-buffer session-id
    (setq opencode-session-status status)
    (force-mode-line-update)))

(defun opencode--defer-files-finished-editing-hook (session-id)
  "Return and clear SESSION-ID's next-idle finished-editing hook skip flag."
  (opencode--with-session-buffer session-id
    (prog1 opencode--skip-files-finished-editing-hook-on-next-idle
      (setq opencode--skip-files-finished-editing-hook-on-next-idle nil))))

(defun opencode--fontify-code-block (mode beg end)
  "Collect MODE faces for BEG..END without sharing session text properties."
  (let ((text (buffer-substring-no-properties beg end))
        faces)
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (funcall mode))
      (font-lock-ensure)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change pos 'face nil (point-max)))
                (face (get-text-property pos 'face)))
            (when face
              (push (list (+ beg (1- pos)) (+ beg (1- next)) face) faces))
            (setq pos next)))))
    (nreverse faces)))

(defun opencode--fontify-markdown (start end &optional loudly)
  "Fontify Markdown between START and END, optionally LOUDLY.
Isolate conventional code-block fontification from session properties."
  (let ((collect (symbol-function 'markdown-ts--fontify-non-ts-collect-faces)))
    (cl-letf (((symbol-function 'markdown-ts--fontify-non-ts-collect-faces)
               (lambda (mode beg finish)
                 (if (memq mode markdown-ts-code-block-force-conventional-modes)
                     (funcall collect mode beg finish)
                   (opencode--fontify-code-block mode beg finish)))))
      (treesit-font-lock-fontify-region start end loudly))))

(define-derived-mode opencode-session-mode comint-mode "OpenCode"
  "Major mode for interacting with an opencode session."
  (setq-local comint-use-prompt-regexp nil
              comint-input-sender 'opencode--send-input
              comint-highlight-input nil
              left-margin-width (1+ left-margin-width))
  (setq-local markdown-ts-hide-markup t
              markdown-ts-enable-code-block-context-mode nil
              markdown-ts-enable-table-mode nil)
  ;; Set up Markdown's parser and font-lock without replacing comint as the
  ;; major mode (and losing its process, prompt, and input handling).
  (unless (and (treesit-language-available-p 'markdown)
               (treesit-language-available-p 'markdown-inline))
    (error "OpenCode sessions require the markdown and markdown-inline Tree-sitter grammars"))
  (markdown-ts-mode--initialize)
  ;; Markdown's conventional-mode helper uses an indirect buffer sharing all
  ;; session properties.  Mode initialization/cleanup there can interfere with
  ;; already-fontified Markdown.  Collect code faces in an isolated buffer.
  (setq-local font-lock-defaults
              '(nil nil nil nil
                (font-lock-fontify-syntactically-function
                 . opencode--fontify-markdown)))
  (setq-local font-lock-fontify-syntactically-function #'opencode--fontify-markdown)
  ;; File-reference buttons are created by comint output, not font-lock.
  (setq-local font-lock-extra-managed-props
              (cl-set-difference font-lock-extra-managed-props
                                 '(button category action help-echo)))
  (visual-line-mode)
  (cursor-intangible-mode)
  (yank-media-handler "\\`image/" #'opencode--yank-image))

(defun opencode-yank-code-block ()
  "Yank the code inside the markdown code block under point."
  (interactive)
  (let* ((node (treesit-parent-until (treesit-node-at (point) 'markdown)
                                     "fenced_code_block" t))
         (content (and node (treesit-search-subtree node "code_fence_content"))))
    (unless node
      (user-error "Not in a fenced code block"))
    (if content
        (copy-region-as-kill (treesit-node-start content) (treesit-node-end content))
      (kill-new ""))))

(defun opencode-copy-conversation ()
  "Copy the current conversation as a Markdown transcript.
The transcript is built from raw session messages, so hidden tool output is
included regardless of `opencode-show-tool-output'.

Useful if you have separate usage limits for opencode vs webchat, so you can let
it gather context and start a plan in opencode, and then iterate on the plan
in webchat."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an OpenCode session"))
  (message "Fetching conversation...")
  (opencode-api-session-messages (opencode-session-id)
      messages
    (let ((transcript (opencode--conversation-to-markdown (reverse messages))))
      (kill-new transcript)
      (message "Copied conversation (%d messages, %d characters)"
               (length messages)
               (length transcript)))))

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
      (kill-buffer (current-buffer)))))

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
    (overlay-put ov 'display (propertize name 'face 'markdown-ts-code))
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
  (or (cadr (split-string mime "[/;+]" t))
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

(defun opencode-add-file (&optional file prompt-directories)
  "Add a FILE to context, or prompt for file in current project.
With a prefix argument, show project directories instead of files."
  (interactive (list nil current-prefix-arg))
  (with-last-opencode-session
   (let* ((project (project-current t))
          (root (project-root project))
          (all-files (project-files project)))
     (opencode--add-file-to-context
      (or file
          (project--read-file-name
           project
           "Add to context"
           (if prompt-directories
               (delete root
                       (delete-dups
                        (cons
                         (concat root "./")
                         (mapcar #'file-name-directory all-files))))
             all-files)
           nil
           'file-name-history))))))

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
  (call-interactively #'self-insert-command)
  (when (comint-after-pmark-p)
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
        (delete-char -1)
        (opencode--insert-intangible (concat "@" name) 'agent-name name)))))

(defun opencode-session--sync-agent-and-model (callback)
  "Ensure the server's session agent and model match the buffer's, then
call CALLBACK with no arguments."
  (let-alist opencode-session-agent
    (cl-flet ((sync-model ()
                (if (equal opencode-session--synced-model .model)
                    (funcall callback)
                  (opencode-api-set-session-model (opencode-session-id)
                      `((model . ,.model))
                      _response
                    (setq opencode-session--synced-model .model)
                    (funcall callback)))))
      (if (equal opencode-session--synced-agent .name)
          (sync-model)
        (opencode-api-set-session-agent (opencode-session-id)
            `((agent . ,.name))
            _response
          (setq opencode-session--synced-agent .name)
          (sync-model))))))

(defun opencode-session--prompt-fields (text extra-parts)
  "Return a plist (:text TEXT :files FILES :agents AGENTS) to send TEXT with
accumulated EXTRA-PARTS context (file attachments, agent mentions, and
synthetic text such as added buffers or regions)."
  (let (files agents texts)
    (dolist (part extra-parts)
      (let-alist part
        (pcase .type
          ("file" (push `((uri . ,.url) (name . ,.filename)) files))
          ("agent" (push `((name . ,.name)) agents))
          ("text" (push .text texts)))))
    (list :text (string-join (append texts (list text)) "\n\n")
          :files files
          :agents agents)))

(defun opencode--send-input (_proc string)
  "Send STRING as input to current opencode session."
  (opencode--highlight-input)
  (opencode--output "\n")
  (let ((extra-parts opencode--extra-parts)
        (temp-files opencode--temp-files)
        sent-message)
    (setf opencode--extra-parts nil
          opencode--temp-files nil)
    (cond
     ((string-prefix-p "/" string)
      (let* ((space-pos (seq-position string ?\s))
             (name (substring string 1 space-pos)))
        (opencode-api-execute-command (opencode-session-id)
            `((name . ,name)
              (text . ,(if space-pos (substring string (1+ space-pos)) "")))
            _response)))
     ((string-prefix-p "!" string)
      (opencode-api-execute-shell (opencode-session-id)
          `((command . ,(substring string 1)))
          _response))
     (t
      (setq sent-message t)
      (opencode-session--sync-agent-and-model
       (lambda ()
         (let ((fields (opencode-session--prompt-fields string extra-parts)))
           (opencode-api-send-message (opencode-session-id)
               `((text . ,(plist-get fields :text))
                 ,@(when (plist-get fields :files)
                     `((files . ,(plist-get fields :files))))
                 ,@(when (plist-get fields :agents)
                     `((agents . ,(plist-get fields :agents)))))
               _result
             (opencode--delete-temp-files temp-files)))))))
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

(defun opencode--maybe-insert-block-spacing ()
  "Ensure \n\n before block."
  (let ((pos (opencode--session-process-position)))
    (opencode--output
     (cond
      ((not (eq ?\n (char-before pos))) "\n\n")
      ((eq ?\n (char-before pos))
       (if (eq ?\n (char-before (1- pos))) "" "\n"))
      (t "")))))

(defun opencode--output (string)
  "Output STRING as comint output."
  (let ((start (save-excursion
                 (goto-char (opencode--session-process-position))
                 (let ((inhibit-field-text-motion t))
                   (line-beginning-position)))))
    (comint-output-filter (get-buffer-process (current-buffer)) string)
    (opencode--buttonize-file-references
     start (opencode--session-process-position))))

(defun opencode--align-output-table (beg end)
  "Align the completed Markdown table between BEG and END.
Use Markdown's table command in an isolated buffer, then apply only its
text changes so session properties and markers survive on unchanged text."
  (let ((source (current-buffer))
        (text (buffer-substring-no-properties beg end))
        (output-properties
         (cl-loop for property in '(field read-only insert-in-front-hooks
                                    inhibit-line-move-field-capture front-sticky)
                  for value = (get-text-property beg property)
                  when value append (list property value)))
        (inhibit-read-only t))
    (save-excursion
      (with-temp-buffer
        ;; Automatic context modes schedule timers which run in whichever
        ;; buffer is current later, potentially overriding the session's RET.
        ;; Enable only the table context needed by the alignment command.
        (let ((markdown-ts-enable-table-mode nil)
              (markdown-ts-enable-code-block-context-mode nil))
          (delay-mode-hooks (markdown-ts-mode)))
        (insert text)
        (goto-char (point-min))
        (markdown-ts-in-table-mode 1)
        (markdown-ts-table-align-table t)
        ;; The command always adds a newline.  Do not introduce one if the
        ;; final text part ended at the last cell instead.
        (unless (string-suffix-p "\n" text)
          (when (eq (char-before (point-max)) ?\n)
            (delete-region (1- (point-max)) (point-max))))
        (let ((aligned (current-buffer)))
          (with-current-buffer source
            (save-restriction
              (narrow-to-region beg end)
              (replace-buffer-contents aligned)
              ;; Diff insertion preserves existing cells, but newly inserted
              ;; padding also needs comint's output field and protection.
              (when output-properties
                (let ((pos (point-min)))
                  (while (< pos (point-max))
                    (let ((next (next-single-property-change pos 'field nil (point-max))))
                      (unless (get-text-property pos 'field)
                        (add-text-properties pos next output-properties))
                      (setq pos next)))))
              (opencode--buttonize-file-references (point-min) (point-max)))))))))

(defun opencode--align-finished-tables (&optional finished)
  "Align tables whose end is confirmed by subsequent complete output lines.
If FINISHED is non-nil, also align a table at the end of the text part.
Retain one line of context for headers whose delimiter has yet to arrive."
  (when opencode--table-scan-marker
    (save-excursion
      (save-restriction
        ;; Pending comint input is not part of the assistant's Markdown.
        (narrow-to-region (point-min) (opencode--session-process-position))
        (let* ((inhibit-field-text-motion t)
               (limit (copy-marker (opencode--session-process-position)))
               (complete-end
                (copy-marker
                 (if finished
                     limit
                   (goto-char limit)
                   (line-beginning-position))))
               waiting)
          (unwind-protect
              (progn
                (goto-char opencode--table-scan-marker)
                (while (and (< (point) complete-end) (not waiting))
                  (let ((node (treesit-node-at (point) 'markdown)))
                    (while (and node (not (equal (treesit-node-type node) "pipe_table")))
                      (setq node (treesit-node-parent node)))
                    ;; At whitespace, `treesit-node-at' can return the table
                    ;; immediately before point.  Never revisit that table.
                    (if (and node (< (point) (treesit-node-end node)))
                        (let ((beg (treesit-node-start node))
                              (end (treesit-node-end node)))
                          (if (and (<= end limit)
                                   (or finished
                                       (< (save-excursion
                                            (goto-char end)
                                            (if (bolp) (point)
                                              (line-beginning-position 2)))
                                          complete-end)))
                              (let ((next (copy-marker end t)))
                                (unwind-protect
                                    (progn
                                      (opencode--align-output-table beg end)
                                      (goto-char next)
                                      (set-marker opencode--table-scan-marker next))
                                  (set-marker next nil)))
                            (set-marker opencode--table-scan-marker beg)
                            (setq waiting t)))
                      (set-marker opencode--table-scan-marker (point))
                      (forward-line 1))))
                (when finished
                  (set-marker opencode--table-scan-marker nil)
                  (setq opencode--table-scan-marker nil)))
            (set-marker limit nil)
            (set-marker complete-end nil)))))))

(defun opencode-insert-logo ()
  "Insert the opencode logo."
  (let ((logo-left '("                   " "█▀▀█ █▀▀█ █▀▀█ █▀▀▄"
                     "█░░█ █░░█ █▀▀▀ █░░█" "▀▀▀▀ █▀▀▀ ▀▀▀▀ ▀  ▀"))
        (logo-right '("             ▄     " "█▀▀▀ █▀▀█ █▀▀█ █▀▀█"
                      "█░░░ █░░█ █░░█ █▀▀▀" "▀▀▀▀ ▀▀▀▀ ▀▀▀▀ ▀▀▀▀")))
    (cl-loop for left in logo-left
             for right in logo-right
             do (opencode--output (propertize left 'face 'shadow))
             (opencode--output " ")
             (opencode--output (propertize right 'face 'bold))
             (opencode--output "\n"))
    (opencode--output "\n")))

(defun opencode--show-prompt ()
  "Highlight the prompt after displaying output."
  (let ((start (opencode--session-process-position)))
    (opencode--output "> ")
    (let ((ov (make-overlay start (opencode--session-process-position))))
      (overlay-put ov 'invisible t)))
  (opencode--add-margin (car comint-last-prompt)
                        (cdr comint-last-prompt)
                        'opencode-request-margin-highlight))

(defun opencode-session--stream-delta (session-id kind key delta)
  "Stream DELTA text of KIND (\\='text or \\='reasoning) for block KEY.
SESSION-ID identifies the owning session buffer.  KEY uniquely identifies the
streaming block, normally (ASSISTANT-MESSAGE-ID . ORDINAL)."
  (opencode--with-session-buffer session-id
    (let ((new-block-p (not (equal key (cdr opencode-session--last-block)))))
      ;; don't display margins on extra whitespace
      (when (and delta new-block-p)
        (setq delta (string-trim-left delta)))
      ;; text and reasoning blocks start once they have visible content, so
      ;; leading whitespace deltas stay trimmed with no block opened yet
      (when (and new-block-p delta (not (string-empty-p delta)))
        (opencode--align-finished-tables t)
        (when (and (eq kind 'text) (not (eq (car opencode-session--last-block) 'text)))
          (setf opencode-last-response-start (opencode--session-process-position)))
        (when opencode-session--last-block
          (opencode--maybe-insert-block-spacing))
        (when (eq kind 'text)
          (setq opencode--table-scan-marker (copy-marker (opencode--session-process-position))))
        (setq opencode-session--last-block (cons kind key)))
      (when (and delta (not (string-empty-p delta)))
        (if (eq kind 'reasoning)
            (opencode--insert-reasoning-block delta)
          (opencode--output delta)))
      (when (and (eq kind 'text) delta (string-search "\n" delta))
        (opencode--align-finished-tables nil)))))

(defun opencode-session--stream-ended (session-id kind key text)
  "Finish the streaming block KEY of KIND for SESSION-ID.
TEXT is the block's final full text, used as a fallback when no delta events
arrived for it (non-streaming providers)."
  (opencode--with-session-buffer session-id
    (unless (equal key (cdr opencode-session--last-block))
      (when (and text (not (string-empty-p text)))
        (opencode-session--stream-delta session-id kind key text)))
    (when (eq kind 'text)
      (opencode--align-finished-tables t))))

(defun opencode-session--tool-called (session-id call-id input)
  "Render the CALL-ID tool call with its final INPUT once known.
SESSION-ID identifies the owning session buffer.  The tool's name was
recorded earlier from the matching tool.input.started event."
  (opencode--with-session-buffer session-id
    (let ((name (car (gethash call-id opencode--tool-call-info))))
      (puthash call-id (cons name input) opencode--tool-call-info)
      (when (and name
                 ;; skip the question tool, handled by form.created
                 (not (string= name "question"))
                 (not (gethash call-id opencode--tool-calls-displayed)))
        (puthash call-id t opencode--tool-calls-displayed)
        (opencode--insert-tool-block name input)))))

(defun opencode-session--maybe-track-edited-file (name input metadata)
  "Record files touched by a completed write/edit/apply_patch NAME tool call.
INPUT is the tool call's input alist; METADATA is its success metadata.
Runs `opencode-file-edited-functions' once per edited file."
  (dolist (file-ranges
           (pcase name
             ("write"
              (when-let ((path (alist-get 'path input)))
                (list (cons path nil))))
             ((or "edit" "apply_patch")
              (let-alist metadata
                (unless .files
                  (error "Missing files metadata for %s tool" name))
                (mapcan (lambda (file-entry)
                          (opencode-diff->source-line-ranges (alist-get 'patch file-entry)))
                        .files)))))
    (when-let ((file (opencode--resolve-file-reference (car file-ranges))))
      (cl-pushnew file opencode--files-edited-this-turn :test #'equal)
      (run-hook-with-args 'opencode-file-edited-functions file (cdr file-ranges)))))

(defun opencode-session--tool-finished (session-id call-id content metadata &optional error)
  "Handle completion of tool call CALL-ID with CONTENT, METADATA, and ERROR.
ERROR is non-nil only when the call failed."
  (opencode--with-session-buffer session-id
    (let* ((entry (gethash call-id opencode--tool-call-info))
           (name (car entry))
           (input (cdr entry)))
      (when name
        (unless error
          (opencode-session--maybe-track-edited-file name input metadata))
        (opencode--maybe-insert-tool-output
         name `((status . "completed") (input . ,input) (content . ,content))))
      (remhash call-id opencode--tool-call-info))))

(defun opencode-session--handle-event (type data)
  "Handle streaming session event TYPE with DATA.
TYPE is the event's dotted symbol (e.g. \\='session.text.delta); DATA is its
alist payload.  Handles everything not already intercepted by opencode.el's
dispatch: streaming text/reasoning/tool rendering, usage updates, and
files-edited tracking."
  (let-alist data
    (pcase type
      ('session.text.delta
       (opencode-session--stream-delta .sessionID 'text (cons .assistantMessageID .ordinal) .delta))
      ('session.reasoning.delta
       (opencode-session--stream-delta .sessionID 'reasoning (cons .assistantMessageID .ordinal) .delta))
      ('session.text.ended
       (opencode-session--stream-ended .sessionID 'text (cons .assistantMessageID .ordinal) .text))
      ('session.reasoning.ended
       (opencode-session--stream-ended .sessionID 'reasoning (cons .assistantMessageID .ordinal) .text))
      ('session.tool.input.started
       (opencode--with-session-buffer .sessionID
         (puthash .id (cons .name nil) opencode--tool-call-info)))
      ('session.tool.called
       (opencode-session--tool-called .sessionID .id .input))
      ('session.tool.success
       (opencode-session--tool-finished .sessionID .id .content .metadata))
      ('session.tool.failed
       (opencode-session--tool-finished .sessionID .id .content .metadata .error))
      ('session.step.ended
       (opencode--with-session-buffer .sessionID
         (when (equal .finish "stop")
           (opencode--maybe-insert-block-spacing))))
      ('session.usage.updated
       (opencode--with-session-buffer .sessionID
         (setq opencode-session-tokens
               (+ .tokens.input .tokens.output .tokens.reasoning
                  .tokens.cache.read .tokens.cache.write))
         (force-mode-line-update)))
      ('session.compaction.started
       (opencode--with-session-buffer .sessionID
         (opencode--maybe-insert-block-spacing)
         (opencode--output (propertize "⟳ Compacting conversation…" 'face 'shadow))
         (opencode--output "\n\n")))
      ('session.compaction.failed
       (opencode--with-session-buffer .sessionID
         (opencode--maybe-insert-block-spacing)
         (opencode--output
          (propertize (format "Compaction failed: %s" (or .error.message "unknown error"))
                      'face 'error))
         (opencode--output "\n\n")))
      (_ nil))))

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

(defun opencode--add-margin (start end face)
  "Display margin from START (inclusive) to END (exclusive) with FACE."
  ;; a reversed range would highlight the lines before START
  (when (> end start)
    (let ((ov (make-overlay start (1- end)))
          (margin (propertize ">" 'display
                              `((margin left-margin)
                                ,(propertize "▎" 'face face)))))
      (overlay-put ov 'line-prefix margin)
      (overlay-put ov 'wrap-prefix margin))))

(defun opencode--replay-user-request (message)
  "Replay a user request MESSAGE."
  (opencode--show-prompt)
  (let-alist message
    (when .text (insert .text)))
  (insert "\n")
  (let ((comint-input-sender #'opencode--highlight-input))
    (comint-send-input)))

(defun opencode-session--sync-agent-from-message (message)
  "Set the buffer's active agent/model from assistant MESSAGE, when known.
Also marks that agent/model as already synced with the server, so the next
send does not needlessly re-select them."
  (let-alist message
    (when-let ((agent (seq-find (lambda (agent)
                                  (string= (alist-get 'name agent) .agent))
                                opencode-session-agents)))
      (setq opencode-session-agent agent)
      (setf (alist-get 'model opencode-session-agent) .model)
      (setq opencode-session--synced-agent .agent
            opencode-session--synced-model .model))))

(defun opencode--insert-block-with-margin (text face)
  "Insert TEXT with FACE margin highlight."
  (unless (string-empty-p text)
    (let ((beginning (opencode--session-process-position)))
      (opencode--output text)
      (opencode--add-margin beginning (save-excursion
                                        (goto-char (opencode--session-process-position))
                                        (skip-chars-backward "\r\n[:blank:]")
                                        ;; Markdown hides closing fences, so
                                        ;; their otherwise visible margin would
                                        ;; appear as an extra highlighted line.
                                        (let ((inhibit-field-text-motion t))
                                          (beginning-of-line))
                                        (unless (looking-at-p "[`~]\\{3,\\}[[:blank:]]*$")
                                          (goto-char (opencode--session-process-position))
                                          (skip-chars-backward "\r\n[:blank:]"))
                                        (point))
                            face))))

(defun opencode--insert-reasoning-block (text)
  "Insert TEXT as reasoning block."
  (opencode--insert-block-with-margin text 'opencode-reasoning-margin-highlight))

(defun opencode--session-process-position ()
  "Return position of process marker."
  (marker-position
   (process-mark
    (get-buffer-process
     (current-buffer)))))

(defun opencode--refine-diff-hunks (start)
  "Refine and hide diff hunk headers from START to the process marker."
  (let ((inhibit-read-only t)
        (inhibit-field-text-motion t))
    (save-excursion
      (save-restriction
        (narrow-to-region start (opencode--session-process-position))
        (goto-char (point-min))
        (while (re-search-forward diff-hunk-header-re nil t)
          (let* ((beg (match-beginning 0))
                 (header-end (line-beginning-position 2))
                 (end (progn
                        (goto-char beg)
                        ;; apply_patch headers have synthetic line counts.
                        (diff-end-of-hunk nil t)
                        (point))))
            (when diff-refine
              (diff--refine-hunk beg end t))
            ;; Font-lock can clear invisible text properties on refontification.
            (let ((overlay (make-overlay beg header-end)))
              (overlay-put overlay 'invisible t)
              (overlay-put overlay 'evaporate t))
            (goto-char (max header-end end))))))))

(defun opencode--insert-tool-block (tool input)
  "Insert TOOL call with INPUT as margin-highlighted block."
  (let ((start (opencode--session-process-position)))
    (opencode--insert-block-with-margin
     (opencode--format-tool-call tool input)
     'opencode-tool-margin-highlight)
    (opencode--maybe-insert-block-spacing)
    (when (member tool '("edit" "apply_patch"))
      (opencode--refine-diff-hunks start))))

(defun opencode--maybe-insert-tool-output (name state)
  "Maybe insert tool output for a completed NAME tool call with STATE.
STATE is a Session.Message.ToolState.* alist (status, input, content, ...)."
  (let-alist state
    (when (and (string= .status "completed")
               (or opencode-show-tool-output
                   ;; show output for user run shell commands
                   (and (string= name "shell")
                        (not .input.description)))
               .content)
      (let ((output (opencode--tool-content-text .content)))
        (unless (string-empty-p output)
          (opencode--output (opencode--fenced-block output "text"))
          (opencode--output "\n"))))))

(defun opencode-open-session-same-window (session)
  "Open SESSION using the current window."
  (opencode-open-session session :pop-to-buffer nil))

(defun opencode--replay-assistant-message (message)
  "Replay assistant MESSAGE's content blocks."
  (let-alist message
    (setq opencode-session-tokens
          (+ (or .tokens.input 0) (or .tokens.output 0) (or .tokens.reasoning 0)
             (or .tokens.cache.read 0) (or .tokens.cache.write 0)))
    (let (last-type)
      (dolist (item .content)
        (let-alist item
          (pcase .type
            ("tool"
             (unless (string= .name "question")
               (opencode--insert-tool-block .name .state.input)
               (opencode--maybe-insert-tool-output .name .state))
             (setq last-type "tool"))
            ("text"
             (let ((text (string-trim (or .text ""))))
               (unless (string-empty-p text)
                 (unless (equal last-type "text")
                   (setf opencode-last-response-start
                         (opencode--session-process-position)))
                 (setq opencode--table-scan-marker
                       (copy-marker (opencode--session-process-position)))
                 (opencode--output text)
                 (opencode--align-finished-tables t)
                 (opencode--output "\n\n")
                 (setq last-type "text"))))
            ("reasoning"
             (let ((text (string-trim (or .text ""))))
               (unless (string-empty-p text)
                 (opencode--insert-reasoning-block text)
                 (opencode--output "\n\n")
                 (setq last-type "reasoning"))))))))))

(defun opencode--replay-session-messages (messages)
  "Display past session MESSAGES, oldest first."
  (let ((inhibit-redisplay t)
        (comint-output-filter-functions
         (remove #'comint-postoutput-scroll-to-bottom
                 comint-output-filter-functions))
        last-assistant)
    (dolist (message messages)
      (pcase (alist-get 'type message)
        ("user" (opencode--replay-user-request message))
        ("assistant"
         (opencode--replay-assistant-message message)
         (setq last-assistant message))
        (_ nil)))
    (when last-assistant
      (opencode-session--sync-agent-from-message last-assistant))))

(cl-defun opencode-open-session (session &key (pop-to-buffer t) callback)
  "Open comint based shell for SESSION.
POP-TO-BUFFER controls whether to pop to or switch to the session buffer.
Returns the buffer.
If CALLBACK is given, it will be called with the session after it is initialized."
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
                  default-directory (file-name-as-directory .location.directory)
                  opencode--tool-calls-displayed (make-hash-table :test 'equal)
                  opencode--tool-call-info (make-hash-table :test 'equal)
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
                (opencode--replay-session-messages (reverse messages))
                (opencode-api-sessions-active
                  busy-sessions
                  (if (assoc-string .id busy-sessions)
                      (opencode-session--set-status .id "busy")
                    (opencode--show-prompt))
                  (when callback
                    (funcall callback session)))))
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
         (let* ((messages (reverse messages))
                (user-message-index
                 (cl-loop for message in messages
                          for index from 0
                          when (equal "user" (alist-get 'type message))
                          count t into count
                          when (= (1- count) message-number)
                          return index))
                (user-message (and user-message-index
                                   (nth user-message-index messages)))
                (assistant-message (and user-message-index
                                        (let ((next-message (nth (1+ user-message-index)
                                                                 messages)))
                                          (when (equal "assistant"
                                                       (alist-get 'type next-message))
                                            next-message))))
                (,user-id (alist-get 'id user-message))
                (,assistant-id (alist-get 'id assistant-message)))
           ,@body)))))

(defun opencode-rename-session (&optional session)
  "Rename SESSION. If in a session buffer, rename that session."
  (interactive)
  (let* ((old-title (or (alist-get 'title session)
                        (string-remove-prefix
                         "*OpenCode: "
                         (string-trim-right (buffer-name)
                                            "\\*\\(<[0-9]+>\\)?"))))
         (title (read-string "Title: " old-title)))
    (opencode-api-rename-session ((or (alist-get 'id session)
                                      opencode-session-id))
        `((title . ,title))
        _res)))

(defun opencode-session--display-error (session-id message)
  "Display error MESSAGE in SESSION-ID and then new prompt."
  (opencode--with-session-buffer session-id
    (opencode--output (propertize message 'face 'error))
    (opencode--output "\n\n")
    (opencode--show-prompt)))

(defun opencode-abort-session ()
  "Abort a busy session and go back to prompt."
  (interactive)
  (setq opencode--skip-files-finished-editing-hook-on-next-idle t)
  (opencode-api-interrupt-session (opencode-session-id)
      success-p
    (unless success-p
      (setq opencode--skip-files-finished-editing-hook-on-next-idle nil)
      (message "Failed to abort session."))))

(defun opencode-session-control-toggle-verbose ()
  "Toggle verbose mode in session control buffer."
  (interactive)
  (setq opencode-session-control-verbose
        (not opencode-session-control-verbose))
  (opencode-sessions-redisplay))

(defun opencode-compact-session ()
  "Compact the current session."
  (interactive)
  (if opencode-session-id
      (opencode-api-compact-session (opencode-session-id)
          result
        (unless result
          (error "Unable to compact session")))
    (user-error "Not in an OpenCode session")))

(defun opencode-scroll-to-last-response-start ()
  "Scroll to the top of the last text part of assistant response."
  (interactive)
  (unless opencode-last-response-start
    (user-error "No last response"))
  (goto-char opencode-last-response-start)
  (forward-word 1)
  (backward-word 1)
  (recenter-top-bottom 0))

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
                      "RET" opencode-open-session-same-window
                      "o" opencode-open-session-same-window)
           :getter (lambda (object column vtable)
                     (let-alist object
                       (pcase (vtable-column vtable column)
                         ("Title" .title)
                         ("Branch" (if (and .location.directory (file-exists-p .location.directory))
                                       (let ((default-directory .location.directory))
                                         (with-memoization
                                             (map-elt cache .location.directory)
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
