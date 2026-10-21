;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; byte-compile-warnings: (not docstrings-wide); -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 1.1.8
;; Package-Requires: ((emacs "30.1") (markdown-mode "2.6") (magit "4.0") (plz "0.9") (plz-media-type "0.2.4") (plz-event-source "0.1.3"))
;; URL: https://codeberg.org/sczi/opencode.el/

;;; Commentary:

;; Emacs interface to opencode.
;; Provides a comint-based mode for interacting with an opencode server.

;;; Code:

(require 'json)
(require 'magit)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-diff-parser)
(require 'opencode-permission)
(require 'opencode-question)
(require 'opencode-sessions)
(require 'plz-media-type)
(require 'plz-event-source)
(require 'project)

;; pending fix upstream at https://github.com/r0man/plz-event-source/pull/15
;; see also: https://codeberg.org/sczi/opencode.el/issues/12
(setq plz-event-source-parser--line-regexp
      (rx (*? not-newline) (or "\r\n" "\n" "\r")))

(defgroup opencode nil
  "Emacs interface to opencode."
  :group 'applications)

(defgroup opencode-faces nil
  "Faces for opencode interface."
  :group 'opencode)

(defcustom opencode-command "opencode"
  "Base command for the opencode executable.
Used to start the shared background service via `opencode service start'
when it isn't already running."
  :type 'string
  :group 'opencode)

(defcustom opencode-session-deleted-functions nil
  "Hook run when an opencode session is deleted.
This is an abnormal hook. Each function receives one argument, SESSION-ID."
  :type 'hook
  :group 'opencode)

;; so invisible prompt "> " doesn't make whole prompt invisible
(add-to-list 'comint--prompt-rear-nonsticky 'invisible)

(defun opencode--service-json-path ()
  "Return the path to the opencode service descriptor file."
  (expand-file-name "~/.local/state/opencode/service.json"))

(defun opencode--read-service-json ()
  "Return the parsed opencode service descriptor, or nil if unavailable."
  (let ((path (opencode--service-json-path)))
    (when (file-readable-p path)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents path)
          (json-parse-buffer :object-type 'alist))))))

(defun opencode--service-healthy-p (service)
  "Return non-nil if SERVICE responds to GET /api/info."
  (when-let ((url (alist-get 'url service))
             (password (alist-get 'password service)))
    (ignore-errors
      (plz 'get (concat url "/api/info")
        :headers (list (cons "Authorization"
                             (concat "Basic "
                                     (base64-encode-string (concat "opencode:" password)))))
        :timeout 2)
      t)))

(defun opencode--ensure-service ()
  "Return a healthy opencode service descriptor, starting the service if needed."
  (let ((service (opencode--read-service-json)))
    (unless (opencode--service-healthy-p service)
      (unless (zerop (call-process opencode-command nil nil nil "service" "start"))
        (error "Failed to start the opencode service with `%s service start'"
               opencode-command))
      (setq service (opencode--read-service-json))
      (unless (opencode--service-healthy-p service)
        (error "Could not connect to the opencode service after starting it")))
    service))

(defun opencode-autoconnect (on-connect)
  "Connect to opencode if not already connected, then run ON-CONNECT."
  (if opencode--event-subscription
      (funcall on-connect)
    (opencode-connect on-connect)))

;;;###autoload
(defun opencode ()
  "Open opencode sessions control buffer for the current project directory.
Connects to the shared opencode background service, starting it if necessary."
  (interactive)
  (let ((project-dir (expand-file-name
                      (if-let (proj (project-current))
                          (project-root proj)
                        default-directory))))
    (opencode-autoconnect (lambda () (opencode-open-project project-dir)))))

(defun opencode-connect (&optional on-connect)
  "Connect to the shared opencode background service.
Starts it via `opencode-command' if it isn't already running.
Runs ON-CONNECT once connected, if given."
  (interactive)
  (when opencode--event-subscription
    (user-error "Already connected"))
  (let ((service (opencode--ensure-service)))
    (setq opencode-api-url (alist-get 'url service)
          opencode-api-password (alist-get 'password service)))
  (setq opencode--slash-commands-by-directory nil)
  (opencode--subscribe-global-events)
  (opencode-api-models models
    (setq opencode-models (seq-filter (lambda (model) (alist-get 'enabled model)) models))
    (opencode--fetch-agents (lambda () (when on-connect (funcall on-connect)))))
  (message "Connected to %s" opencode-api-url))

(defun opencode-open-project (directory)
  "Open sessions control buffer for DIRECTORY."
  (opencode--download-slash-commands directory)
  (let ((buffer-name (format "*OpenCode Sessions in %s*" directory)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (opencode-session-control-mode)
        (setq default-directory directory)
        (opencode-api-location location
          (let-alist location
            (setf (map-elt opencode--session-control-buffers .project.id)
                  (cons (current-buffer)
                        (seq-filter #'buffer-live-p
                                    (map-elt opencode--session-control-buffers
                                             .project.id))))))
        (opencode-sessions-redisplay)))
    (pop-to-buffer buffer-name)))

(defvar opencode-worktree-directory (expand-file-name "~/opencode_worktrees/")
  "Directory to store worktrees created for opencode.")

(defun opencode-new-worktree ()
  "Create a new git branch, and worktree prompting for a name.
Then open an opencode session in it."
  (interactive)
  (let* ((name (read-string "Worktree and branch name: "))
         (directory (file-name-concat opencode-worktree-directory name)))
    (when (magit-worktree-branch directory name "HEAD")
      (let ((default-directory directory))
        (opencode-new-session)))))

(defun opencode-select-project ()
  "Completing read to prompt which project to select."
  (interactive)
  (opencode-autoconnect
   (lambda ()
     (opencode-api-projects projects
       (opencode-open-project
        (opencode--annotated-completion
         "Project: "
         (cl-loop for project in projects
                  for directory = (alist-get 'canonical project)
                  collect (list (string-remove-prefix
                                 (expand-file-name "~/")
                                 directory)
                                directory
                                (opencode--format-time-ago
                                 (opencode--time-ago
                                  project 'updated))))))))))

(defvar opencode-event-log-max-lines nil
  "Maximum number of lines to log in the opencode event log buffer.
Or nil to disable logging.")

(defun opencode--log-event (type event)
  "Log EVENT of TYPE to the opencode log buffer."
  (when opencode-event-log-max-lines
    (with-current-buffer (get-buffer-create "*opencode-event-log*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "[%s] %s: %s\n"
                        (format-time-string "%Y-%m-%d %H:%M:%S")
                        type
                        event))
        (opencode--truncate-at-max-lines opencode-event-log-max-lines)))))

(defvar opencode--files-finished-editing-current nil
  "Current `opencode-files-finished-editing-functions' run context.")

(defvar opencode--files-finished-editing-queue nil
  "Queued `opencode-files-finished-editing-functions' run contexts.")

(defun opencode-report-diagnostic (message)
  "Queue diagnostic MESSAGE for the active finished-editing hook."
  (unless opencode--files-finished-editing-current
    (error "`opencode-report-diagnostic' called outside a finished-editing hook"))
  (when (and message (not (string= message "")))
    (push message
          (plist-get opencode--files-finished-editing-current :diagnostics))))

(defun opencode-hook-finished ()
  "Mark the active finished-editing hook as complete."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "`opencode-hook-finished' called outside a finished-editing hook"))
    (unless (plist-get context :active-hook)
      (error "`opencode-hook-finished' called without an active hook"))
    (plist-put context :active-hook nil)
    (opencode--run-next-files-finished-editing-hook)))

(defun opencode--files-finished-editing-diagnostic-message (diagnostics)
  "Return a synthetic input message for DIAGNOSTICS."
  (concat
   "Diagnostics were reported after editing files. "
   "Please fix any problems caused by your changes.\n\n"
   (mapconcat #'identity diagnostics "\n\n")))

(defun opencode--maybe-start-files-finished-editing-hook ()
  "Start the next queued finished-editing hook run if none is active."
  (unless opencode--files-finished-editing-current
    (when opencode--files-finished-editing-queue
      (let ((context (pop opencode--files-finished-editing-queue)))
        (setq opencode--files-finished-editing-current context)
        (opencode--run-next-files-finished-editing-hook)))))

(defun opencode--finish-files-finished-editing-hook-run (context)
  "Finish finished-editing hook run CONTEXT and send queued diagnostics."
  (let ((diagnostics (nreverse (plist-get context :diagnostics)))
        (buffer (plist-get context :session-buffer)))
    (setq opencode--files-finished-editing-current nil)
    (when (and diagnostics (buffer-live-p buffer))
      (with-current-buffer buffer
        (opencode-session--send-synthetic-input
         (opencode--files-finished-editing-diagnostic-message diagnostics)))))
  (opencode--maybe-start-files-finished-editing-hook))

(defun opencode--run-next-files-finished-editing-hook ()
  "Run the next hook in the active finished-editing context."
  (let ((context opencode--files-finished-editing-current))
    (unless context
      (error "No active finished-editing hook context"))
    (when-let ((buffer (plist-get context :session-buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (if-let ((hook (car (plist-get context :hooks))))
              (let (result errored)
                (plist-put context :hooks (cdr (plist-get context :hooks)))
                (plist-put context :active-hook hook)
                (condition-case err
                    (setq result (funcall hook (plist-get context :files)))
                  (error
                   (setq errored t)
                   (opencode--log-event
                    "WARNING FINISHED EDITING HOOK"
                    (format "%S failed: %s" hook (error-message-string err)))
                   (when (and (eq opencode--files-finished-editing-current context)
                              (eq (plist-get context :active-hook) hook))
                     (plist-put context :active-hook nil)
                     (opencode--run-next-files-finished-editing-hook))))
                (when (and (not errored)
                           (not (eq result :opencode-async))
                           (eq opencode--files-finished-editing-current context)
                           (eq (plist-get context :active-hook) hook))
                  (plist-put context :active-hook nil)
                  (opencode--run-next-files-finished-editing-hook)))
            (opencode--finish-files-finished-editing-hook-run context)))))))

(defun opencode--maybe-run-files-finished-editing-hook (session-id)
  "Run `opencode-files-finished-editing-functions' for SESSION-ID's pending files."
  (opencode--with-session-buffer session-id
    (when opencode--files-edited-this-turn
      (let ((files opencode--files-edited-this-turn))
        (setq opencode--files-edited-this-turn nil)
        (when-let ((hooks (append opencode-files-finished-editing-functions
                                  opencode-project-files-finished-editing-functions)))
          (setq opencode--files-finished-editing-queue
                (append opencode--files-finished-editing-queue
                        (list (list :session-buffer (current-buffer)
                                    :files files
                                    :hooks hooks
                                    :diagnostics nil
                                    :active-hook nil))))
          (opencode--maybe-start-files-finished-editing-hook))))))

(defun opencode--indent-files (files)
  "Indent FILES with Emacs."
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (unless (opencode--indent-sensitive-buffer-p)
        (let ((inhibit-message t))
          (revert-buffer t t t)
          (indent-region (point-min) (point-max)))
        (when (buffer-modified-p)
          (message "opencode indented %s" file)
          (save-buffer))))))

(defun opencode-run-command-diagnostic (command)
  "Run COMMAND asynchronously and report diagnostics.
For use within `opencode-files-finished-editing-functions' or
`opencode-project-files-finished-editing-functions'."
  (let* ((command-name (car command))
         (buffer (generate-new-buffer (format "*%s*" command-name)))
         (command-string (string-join command " ")))
    (condition-case err
        (progn
          (make-process
           :name command-name
           :buffer buffer
           :command command
           :connection-type 'pipe
           :noquery t
           :sentinel
           (lambda (process _event)
             (when (memq (process-status process) '(exit signal))
               (unwind-protect
                   (unless (zerop (process-exit-status process))
                     (let ((output (with-current-buffer (process-buffer process)
                                     (string-trim (buffer-string)))))
                       (opencode-report-diagnostic
                        (if (equal output "")
                            (format "`%s` failed" command-string)
                          (format "`%s` failed:\n\n%s"
                                  command-string
                                  output)))))
                 (kill-buffer buffer)
                 (opencode-hook-finished)))))
          :opencode-async)
      (error
       (kill-buffer buffer)
       (opencode-report-diagnostic
        (format "Failed to start `%s`: %s"
                command-string
                (error-message-string err)))
       nil))))

(defun opencode--emacs-ert-command ()
  "Return the batch Emacs command used to test with ERT."
  (append
   (list (concat invocation-directory invocation-name) "--batch")
   (cl-loop for path in (delete-dups (append (list default-directory) load-path nil))
            when (and (stringp path) (file-directory-p path))
            append (list "-L" (expand-file-name path)))
   (cl-loop for file in (directory-files-recursively default-directory
                                                     "\\(?:-test\\|-tests\\)\\.el\\'")
            append (list "-l" file))
   (list "-f" "ert-run-tests-batch-and-exit")))

(defun opencode-uv-pytest (_files)
  "Run pytest with uv and report diagnostics."
  (opencode-run-command-diagnostic
   '("uv" "run" "python3" "-m" "pytest")))

(defun opencode-emacs-ert (_files)
  "Run ERT and report diagnostics."
  (opencode-run-command-diagnostic
   (opencode--emacs-ert-command)))

(defun opencode--selection-change-hook (&optional _frame)
  "Hook to remove session from the alerted sessions list when it's visited.
Also prompts for pending questions or permissions if any."
  (when opencode-session-id
    (setf opencode-alerted-sessions
          (cl-delete-if (lambda (session)
                          (string= opencode-session-id
                                   (alist-get 'id session)))
                        opencode-alerted-sessions)
          opencode-last-session-buffer (current-buffer))
    ;; Handle pending questions when buffer becomes active
    (when opencode-session-pending-questions
      (let ((pending opencode-session-pending-questions))
        (setq opencode-session-pending-questions nil)
        (opencode--prompt-questions (car pending) (cdr pending))))
    ;; Handle pending permissions when buffer becomes active
    (when opencode-session-pending-permission
      (run-at-time 0 nil #'opencode-respond-permission))))

(add-hook 'window-selection-change-functions 'opencode--selection-change-hook)

;; Handles the case where the session buffer was already selected when the
;; request arrived but Emacs did not have OS focus at the time.
(add-function :after after-focus-change-function #'opencode--selection-change-hook)

(declare-function opencode-session--handle-event "opencode-sessions" (type data))
(declare-function opencode--permission-request "opencode-permission" (data))
(declare-function opencode--permission-settled "opencode-permission" (data))
(declare-function opencode--form-request "opencode-question" (data))
(declare-function opencode--form-settled "opencode-question" (data))

(defun opencode--handle-session-lifecycle-event (type-string data)
  "Handle session lifecycle event of TYPE-STRING with DATA.
Covers session.created/updated/renamed/deleted/moved/forked/archived."
  (let ((session-id (or (alist-get 'id data) (alist-get 'sessionID data)))
        (project-id (alist-get 'projectID data)))
    (when (equal type-string "session.deleted")
      (run-hook-with-args 'opencode-session-deleted-functions session-id))
    (if project-id
        (dolist (buffer (map-elt opencode--session-control-buffers project-id))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (opencode-sessions-redisplay))))
      (dolist (buffers (map-values opencode--session-control-buffers))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (opencode-sessions-redisplay))))))
    (opencode--with-session-buffer session-id
      (cond
       ((and (member type-string '("session.renamed" "session.updated"))
             (alist-get 'title data))
        (let ((name (format "*OpenCode: %s*" (alist-get 'title data))))
          (unless (string-prefix-p name (buffer-name))
            (rename-buffer name t))))
       ((equal type-string "session.deleted")
        (delete-process))))))

(defun opencode--handle-session-execution-finished-event (type-string session-id data)
  "Handle execution-finished event of TYPE-STRING for SESSION-ID with DATA."
  (unless (opencode--defer-files-finished-editing-hook session-id)
    (opencode--maybe-run-files-finished-editing-hook session-id))
  (opencode--with-session-buffer session-id
    (opencode-api-session (session-id)
        session
      (opencode--show-prompt)
      (unless (or (opencode--buffer-active-p (current-buffer))
                  ;; don't show alert for subagent sessions
                  (alist-get 'parentID session))
        (opencode--toast-show `((title . "OpenCode Finished")
                                (message . ,(alist-get 'title session))
                                (variant . "success")))
        (push session opencode-alerted-sessions))))
  (opencode-session--set-status session-id "idle")
  (when (equal type-string "session.execution.failed")
    (opencode-session--display-error
     session-id
     (or (alist-get 'message data) (map-nested-elt data '(error message))
         "execution failed"))))

(defun opencode--dispatch-event (type-string data)
  "Dispatch an opencode event of TYPE-STRING with DATA (an alist)."
  (let ((session-id (alist-get 'sessionID data)))
    (cond
     ((member type-string '("session.created" "session.updated" "session.renamed"
                            "session.deleted" "session.moved" "session.forked"
                            "session.archived"))
      (opencode--handle-session-lifecycle-event type-string data))
     ((equal type-string "session.execution.started")
      (opencode-session--set-status session-id "busy"))
     ((member type-string '("session.execution.succeeded" "session.execution.failed"
                            "session.execution.interrupted"))
      (opencode--handle-session-execution-finished-event type-string session-id data))
     ((equal type-string "session.error")
      (opencode-session--display-error
       session-id (or (alist-get 'message data) (format "%s" data))))
     ((equal type-string "session.retry.scheduled")
      (opencode--toast-show `((title . "OpenCode retrying")
                              (message . ,(format "%s" (or (alist-get 'message data) "")))
                              (variant . "warning"))))
     ((equal type-string "permission.asked") (opencode--permission-request data))
     ((member type-string '("permission.replied" "permission.rejected"))
      (opencode--permission-settled data))
     ((equal type-string "form.created") (opencode--form-request data))
     ((member type-string '("form.replied" "form.cancelled")) (opencode--form-settled data))
     ((string-prefix-p "session." type-string)
      (opencode-session--handle-event (intern type-string) data)))))

(defun opencode--handle-event (event)
  "Handle decoded EVENT from the opencode SSE stream."
  (let* ((envelope (json-read-from-string (plz-event-source-event-data event)))
         (type-string (alist-get 'type envelope))
         (directory (map-nested-elt envelope '(location directory)))
         (data (alist-get 'data envelope)))
    (with-current-buffer (process-buffer opencode--event-subscription)
      (let ((default-directory (if directory
                                   (opencode--normalize-directory directory)
                                 default-directory)))
        (opencode--log-event type-string envelope)
        (opencode--dispatch-event type-string data)))))

(defun opencode--subscribe-global-events ()
  "Subscribe to the opencode event stream."
  (setq opencode--event-subscription
        (plz-media-type-request
          'get (concat opencode-api-url "/api/event")
          :as `(media-types
                ((text/event-stream
                  . ,(plz-event-source:text/event-stream
                      :events `((open . ,(lambda (event)
                                           (opencode--log-event "OPEN" event)))
                                (message . opencode--handle-event)
                                (close . opencode-disconnect))))))
          :headers (delq nil (list (opencode--auth-header)))
          :then 'opencode-disconnect
          :else 'opencode-disconnect))
  (set-process-query-on-exit-flag opencode--event-subscription nil))

(defun opencode-disconnect (&optional event)
  "Disconnect from the opencode event stream, optionally log EVENT."
  (interactive)
  (opencode--log-event "DISCONNECT" event)
  (when (process-live-p opencode--event-subscription)
    (kill-process opencode--event-subscription))
  (setq opencode--event-subscription nil
        opencode--slash-commands-by-directory nil))

(defun opencode--fetch-agents (callback)
  "Fetch available agents from server and filter out hidden agents.
Run CALLBACK once fetched."
  (opencode-api-agents agents
    (setq opencode-agents
          (seq-filter (lambda (agent)
                        (opencode--json-falsy (alist-get 'hidden agent)))
                      agents))
    (funcall callback)))

(cl-defun opencode-new-session (&key title callback)
  "Create a new session. With a prefix argument it will ask for TITLE.
Without it will use a default title and then automatically generate one.
If CALLBACK is given, it will be called with the session after it is created."
  (interactive
   (when current-prefix-arg
     (list :title (read-string "Title: "))))
  (opencode-autoconnect
   (lambda ()
     (opencode--download-slash-commands default-directory)
     (opencode-api-create-session
         `((location . ((directory . ,(expand-file-name default-directory))))
           ,@(when title `((title . ,title))))
         session
       (opencode-open-session session :callback callback)))))

(defun opencode-toggle-mcp ()
  "Completing read to select an MCP to toggle."
  (interactive)
  (opencode-api-mcps mcps
    (let* ((mcp (opencode--annotated-completion
                 "MCP: "
                 (cl-loop for server in mcps
                          for name = (alist-get 'name server)
                          for status = (map-nested-elt server '(status status))
                          collect (list name name
                                        (pcase status
                                          ("connected" "🟢 connected")
                                          ("disabled" "🔴 disabled")
                                          (_ status))))))
           (status (map-nested-elt
                    (seq-find (lambda (server) (equal (alist-get 'name server) mcp)) mcps)
                    '(status status))))
      (if (equal status "connected")
          (opencode-api-disconnect-mcp (mcp)
              _res
            (message "Disconnected %s" mcp))
        (opencode-api-connect-mcp (mcp)
            _res
          (message "Connected %s" mcp))))))

(defun opencode-fork-session ()
  "Fork the current session from the message at point.
Creates a new session starting from the current user message.
If point is before the first prompt, creates a new session instead."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-fork-session (opencode-session-id)
            `((before . ,message-id))
            session
          (opencode-open-session-same-window session))
      ;; if before the first prompt just open a new session
      (opencode-new-session))))

(defun opencode-revert-message ()
  "Select a message to revert in the current session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-revert-stage (opencode-session-id)
            `((messageID . ,message-id))
            result
          (message
           (if result
               "Reverted edits after message"
             "Failed to revert message")))
      (user-error "No user message at point"))))

(defun opencode-unrevert-all ()
  "Unrevert all reverted messages in the current session."
  (interactive)
  (opencode-api-revert-clear (opencode-session-id)
      _result
    (message "Restored all edits in session")))

(defun opencode--download-slash-commands (directory)
  "Download slash commands for DIRECTORY."
  (setf directory (opencode--normalize-directory directory))
  (unless (assoc directory opencode--slash-commands-by-directory)
    (let ((default-directory directory))
      (opencode-api-commands commands
        (setf (alist-get directory opencode--slash-commands-by-directory
                         nil nil #'string=)
              commands)))))

(provide 'opencode)
;;; opencode.el ends here
