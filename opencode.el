;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (markdown-mode "2.6") (plz "0.9") (plz-media-type "0.2.4") (plz-event-source "0.1.3"))
;; URL: https://codeberg.org/sczi/opencode.el/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs interface to opencode.
;; Provides a comint-based mode for interacting with an opencode server.

;;; Code:

(require 'json)
(require 'magit)
(require 'notifications)
(require 'opencode-api)
(require 'opencode-common)
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

(defcustom opencode-host "localhost"
  "Hostname for the opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-port 4096
  "Port for the opencode server."
  :type 'integer
  :group 'opencode)

(defcustom opencode-command "opencode"
  "Base command for the opencode executable.
Used when `opencode-serve-command' is nil to construct the serve command."
  :type 'string
  :group 'opencode)

(defcustom opencode-serve-command nil
  "Full command to start the opencode server.
When nil, the command is constructed from `opencode-command',
`opencode-host', and `opencode-port'.

Set this to a custom command for special cases like nix:
  \"nix run github:numtide/nix-ai-tools#opencode -- serve --port 4096 --hostname localhost\""
  :type '(choice (const :tag "Construct from opencode-command" nil)
                 (string :tag "Custom command"))
  :group 'opencode)

(defcustom opencode-auto-start-server t
  "Whether to automatically start a server if none is running.
When nil, `opencode' will only connect to an already running server."
  :type 'boolean
  :group 'opencode)

(defvar opencode--process nil
  "Opencode server process when started by Emacs.")

;; so invisible prompt "> " doesn't make whole prompt invisible
(add-to-list 'comint--prompt-rear-nonsticky 'invisible)

(defun opencode--server-running-p ()
  "Return non-nil if an opencode server is running at configured host and port."
  (condition-case err
      (plz 'get (format "http://%s:%d/global/health" opencode-host opencode-port)
        :headers (list (opencode--auth-header))
        :timeout 1)
    (plz-http-error
     (when-let (plz-error (cl-third err))
       (when (= 401 (plz-response-status (plz-error-response plz-error)))
         (user-error "OpenCode server is already running but password protected: \
set `opencode-server-password' to connect to it"))))
    (error nil)))

(defun opencode--serve-command ()
  "Return the command to start the opencode server."
  (or opencode-serve-command
      (format "%s serve --port %d --hostname %s"
              opencode-command opencode-port opencode-host)))

(defun opencode--start-server (on-connect)
  "Start an opencode server and run ON-CONNECT when ready."
  (setf opencode-server-password
        (or opencode-server-password
            (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
              (apply #'string
                     (cl-loop repeat 32
                              collect (aref chars (random (length chars))))))))
  (setf opencode--process
        (let ((process-environment (cl-list*
                                    (format "OPENCODE_SERVER_USERNAME=%s"
                                            opencode-server-username)
                                    (format "OPENCODE_SERVER_PASSWORD=%s"
                                            opencode-server-password)
                                    process-environment)))
          (start-process-shell-command
           "opencode" "*opencode-serve*"
           (opencode--serve-command))))

  (set-process-filter
   opencode--process
   (lambda (process output)
     (with-current-buffer "*opencode-serve*"
       (goto-char (process-mark process))
       (insert output))
     (when (string-prefix-p "opencode server listening on" output)
       ;; Remove filter to avoid repeated callbacks
       (set-process-filter process nil)
       (opencode-connect opencode-host opencode-port)
       (funcall on-connect))))

  (set-process-sentinel
   opencode--process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (unless (zerop (process-exit-status process))
         (pop-to-buffer (process-buffer process))
         (error "OpenCode failed, %s" (string-trim-right event "\n")))))))

(defun opencode-autoconnect (on-connect)
  "Connect to an existing server if one is running, otherwise start a new one.
Only starts new one if `opencode-auto-start-server' is non-nil.
Run ON-CONNECT after connected."
  (cond
   ;; Already connected
   (opencode--event-subscriptions
    (funcall on-connect))
   ;; We started a server process that's still alive
   ((process-live-p opencode--process)
    (funcall on-connect))
   ;; Server already running externally
   ((opencode--server-running-p)
    (opencode-connect opencode-host opencode-port)
    (funcall on-connect))
   ;; Need to start a new server
   (opencode-auto-start-server
    (opencode--start-server on-connect))
   ;; Auto-start disabled, no server running
   (t
    (user-error "No opencode server running at %s:%d (auto-start disabled)"
                opencode-host opencode-port))))


;;;###autoload
(defun opencode ()
  "Open opencode sessions control buffer for the current project directory.
Connects to an existing server if one is running, otherwise starts a new one
if `opencode-auto-start-server' is non-nil."
  (interactive)
  (let ((project-dir (expand-file-name
                      (if-let (proj (project-current))
                          (project-root proj)
                        default-directory))))
    (opencode-autoconnect (lambda () (opencode-open-project project-dir)))))

(defun opencode-connect (host port)
  "Connect to opencode server, prompting for HOST and PORT."
  (interactive
   (list (read-string "Host: " opencode-host)
         (read-number "Port: " opencode-port)))
  (when opencode--event-subscriptions
    (user-error "Already connected"))
  (setq opencode-api-url (format "http://%s:%d" host port))
  (opencode--fetch-agents)
  (opencode-api-commands commands
    (setq opencode-slash-commands commands))
  (opencode-api-configured-providers result
    (setq opencode-providers (alist-get 'providers result)))
  (message "Connected to %s" opencode-api-url))

(defun opencode-open-project (directory)
  "Open sessions control buffer for DIRECTORY."
  (setq directory (file-name-as-directory directory))
  (opencode-process-events directory)
  (let ((buffer-name (format "*OpenCode Sessions in %s*" directory)))
    (unless (get-buffer buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (opencode-session-control-mode)
        (setq default-directory directory)
        (opencode-api-current-project project
          (let-alist project
            (setf (map-elt opencode--session-control-buffers .id)
                  (cons (current-buffer)
                        (seq-filter #'buffer-live-p
                                    (map-elt opencode--session-control-buffers
                                             .id))))))
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
                  for worktree = (alist-get 'worktree project)
                  when worktree
                  collect (list (string-remove-prefix
                                 (expand-file-name "~/")
                                 worktree)
                                worktree
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

(defun opencode--toast-show (properties)
  "Show toast notification with PROPERTIES from opencode."
  (funcall opencode-toast-function properties))

(defun opencode--default-toast-show (properties)
  "Default notifier to show notification with PROPERTIES from opencode."
  (let-alist properties
    (cond
     ((featurep 'dbusbind)
      (notifications-notify
       :title .title
       :body .message
       :urgency (pcase .variant
                  ((or "error" "warning" )'critical)
                  ((or "info" "success") 'normal))
       :timeout .duration
       :replaces-id 5647
       :app-icon 'none))
     ((eq system-type 'darwin)
      (let ((title (concat (pcase .variant
                             ("error" "❌")
                             ("warning" "⚠️")
                             ((or "info" "success") "ℹ️"))
                           " " .title)))
        (if (executable-find "terminal-notifier")
            (start-process "opencode-notification" nil "terminal-notifier"
                           "-title" title
                           "-message" .message
                           "-group" "opencode-toast"
                           "-activate" "org.gnu.Emacs"
                           "-sound" (pcase .variant
                                      ((or "error" "warning") "Basso")
                                      (_ "default")))
          (start-process "opencode-notification" nil "osascript" "-e"
                         (format "display notification %S with title %S"
                                 .message
                                 title))))))))

(defun opencode--buffer-active-p (buffer)
  "Return non-nil if BUFFER is visible in the selected window of a focused frame."
  (and buffer
       (eq buffer (window-buffer (selected-window)))
       (frame-focus-state (window-frame (selected-window)))))

(defun opencode--output-questions (buffer questions)
  "Output QUESTIONS to BUFFER with ❓ prefix for each question."
  (when (and (buffer-live-p buffer)
             (get-buffer-process buffer))
    (with-current-buffer buffer
      (opencode--maybe-insert-block-spacing)
      (opencode--output (opencode--format-questions questions))
      (opencode--output "\n"))))

(defun opencode--window-selection-change-hook (_frame)
  "Hook to remove session from the alerted sessions list when it's visited.
Also prompts for pending questions if any."
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
        (opencode--prompt-questions (car pending) (cdr pending))))))

(add-hook 'window-selection-change-functions 'opencode--window-selection-change-hook)

(defun opencode--prompt-questions (question-id questions)
  "Prompt user to answer QUESTIONS, then reply or reject to QUESTION-ID.
QUESTIONS is a vector of question objects with `question', `header', and
`options' fields.  Each option has `label' and `description' fields.
If user selects \"Other\", prompts for custom input using the description.
If user quits, rejects the question via API."
  (condition-case nil
      (let ((answers
             (cl-loop for q across questions
                      for question = (alist-get 'question q)
                      for options = (alist-get 'options q)
                      for candidates = (cl-loop for opt across options
                                                for label = (alist-get 'label opt)
                                                for desc = (alist-get 'description opt)
                                                collect (list label label desc))
                      for answer = (opencode--annotated-completion
                                    (concat question " ")
                                    candidates)
                      collect (list answer))))
        (opencode-api-reply-questions (question-id)
            `((answers . ,(vconcat answers)))
            _result
          nil))
    (quit
     (opencode-api-reject-questions (question-id)
         _result
       nil))))

(defun opencode--permission-request (permission-id session-id type title)
  "Respond to permission request from opencode.
Args are PERMISSION-ID, SESSION-ID, and the TYPE and TITLE of the request."
  (opencode-api-respond-permission-request (session-id permission-id)
      `((response . ,(x-popup-dialog
                      t
                      `(,(format "OpenCode Permission Request\n%s: %s"
                                 type title)
                        ("Accept" . "once")
                        ("Accept Always" . "always")
                        ("Deny" . "reject")))))
      response
    (unless response
      (user-error "Response to permission request failed"))))

(defun opencode--handle-message (event)
  "Handle a message EVENT from opencode server."
  (let* ((data (json-read-from-string (plz-event-source-event-data event)))
         (msg-type (intern (alist-get 'type data)))
         (properties (alist-get 'properties data)))
    (unless (memq msg-type '(file.watcher.updated server.heartbeat session.diff))
      (opencode--log-event "MESSAGE" data)
      (let-alist properties
        (cl-case msg-type
          (tui.toast.show (opencode--toast-show properties))
          (session.idle (opencode-api-session (.sessionID)
                            session
                          (let ((buffer (gethash .sessionID opencode-session-buffers)))
                            (with-current-buffer buffer
                              (opencode--show-prompt))
                            (unless (or
                                     (opencode--buffer-active-p buffer)
                                     ;; don't show alert for subagent sessions
                                     (alist-get 'parentID session))
                              (opencode--toast-show `((title . "OpenCode Finished")
                                                      (message . ,(alist-get 'title session))
                                                      (variant . "success")
                                                      (timeout . 1000)))
                              (push session opencode-alerted-sessions)))))
          (session.status (opencode-session--set-status .sessionID .status.type))
          ((session.created session.updated session.deleted)
           (dolist (buffer (map-elt opencode--session-control-buffers .info.projectID))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (opencode-sessions-redisplay))))
           (when-let (buffer (map-elt opencode-session-buffers .info.id))
             (with-current-buffer buffer
               (cl-case msg-type
                 (session.updated (rename-buffer (format "*OpenCode: %s*" .info.title) t))
                 (session.deleted (delete-process))))))
          (session.error (opencode-session--display-error .sessionID .error.data.message))
          (message.part.updated (opencode-session--update-part .part .delta .part.type))
          (message.part.delta (opencode-session--update-part properties .delta
                                                             (gethash .partID opencode-part-type)))
          (message.updated (opencode-session--message-updated .info))
          (permission.updated (opencode--permission-request .id .sessionID .type .title))
          (question.asked
           (let ((buffer (gethash .sessionID opencode-session-buffers)))
             ;; Output questions to the buffer
             (opencode--output-questions buffer .questions)
             (if (opencode--buffer-active-p buffer)
                 ;; Buffer is active, prompt immediately
                 (opencode--prompt-questions .id .questions)
               ;; Buffer not active, show notification and store pending
               (opencode--toast-show `((title . "OpenCode Questions")
                                       (message . ,(alist-get
                                                    'question
                                                    (aref .questions 0)))
                                       (variant . "info")
                                       (timeout . 1000)))
               (opencode-api-session (.sessionID)
                   session
                 (push session opencode-alerted-sessions))
               (when buffer
                 (with-current-buffer buffer
                   (setq opencode-session-pending-questions
                         (cons .id .questions)))))))
          (otherwise (opencode--log-event "WARNING" "unhandled message type")))))))

(defun opencode-disconnect (&optional event)
  "Disconnect from opencode server, optionally log EVENT."
  (interactive)
  (opencode--log-event "DISCONNECT" event)
  (cl-loop for (process) in opencode--event-subscriptions
           when (process-live-p process)
           do (kill-process process))
  (when (process-live-p opencode--process)
    (set-process-sentinel opencode--process nil)
    (kill-process opencode--process))
  (setq opencode--event-subscriptions nil))

(defun opencode--fetch-agents ()
  "Fetch available agents from server and filter out subagents or hidden agents."
  (opencode-api-agents agents
    (setq opencode-agents
          (seq-filter (lambda (agent)
                        (and (string= "primary" (alist-get 'mode agent))
                             (not (alist-get 'hidden agent))))
                      agents))))

(defun opencode-new-session (&optional title)
  "Create a new session. With a prefix argument it will ask for TITLE.
Without it will use a default title and then automatically generate one."
  (interactive
   (list (when current-prefix-arg
           (read-string "Title: "))))
  (setq default-directory (expand-file-name default-directory))
  (opencode-autoconnect
   (lambda ()
     (opencode-process-events default-directory)
     (opencode-api-create-session (if title
                                      `((title . ,title))
                                    (make-hash-table))
         session
       (opencode-open-session session)))))

(defun opencode-toggle-mcp ()
  "Completing read to select an MCP to toggle."
  (interactive)
  (opencode-api-mcps mcps
    (let ((mcp (opencode--annotated-completion
                "MCP: "
                (cl-loop for mcp in mcps
                         for (mcp-name . mcp-info) = mcp
                         collect (list (symbol-name mcp-name)
                                       mcp-name
                                       (pcase (alist-get 'status mcp-info)
                                         ("connected" "🟢 connected")
                                         ("disabled" "🔴 disabled")))))))
      (pcase (map-nested-elt mcps `(,mcp status))
        ("connected" (opencode-api-disable-mcp (mcp)
                         _res
                       (message "Disabled %s" mcp)))
        ("disabled" (opencode-api-enable-mcp (mcp)
                        _res
                      (message "Enabled %s" mcp)))))))

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
            `((messageID . ,message-id))
            session
          (opencode-open-session session :pop-to-buffer nil))
      ;; if before the first prompt just open a new session
      (opencode-new-session))))

(defun opencode-revert-message ()
  "Select a message to revert in the current session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-id message-id
    (if message-id
        (opencode-api-revert-message (opencode-session-id)
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
  (opencode-api-unrevert-all (opencode-session-id)
      result
    (message
     (if result
         "Restored all edits in session"
       "Failed to restore edits"))))

(defun opencode-process-events (directory)
  "Connect to the opencode event stream and process all events for DIRECTORY."
  (unless (rassoc directory opencode--event-subscriptions)
    (let ((process
           (plz-media-type-request
             'get (concat opencode-api-url "/event")
             :as `(media-types
                   ((text/event-stream
                     . ,(plz-event-source:text/event-stream
                         :events `((open . ,(lambda (event)
                                              (opencode--log-event "OPEN" event)))
                                   (message . ,(lambda (event)
                                                 (let ((default-directory directory))
                                                   (opencode--handle-message event))))
                                   (close . opencode-disconnect))))))
             :headers `(("x-opencode-directory" . ,directory)
                        ,(opencode--auth-header))
             :then 'opencode-disconnect
             :else 'opencode-disconnect)))
      (set-process-query-on-exit-flag process nil)
      (push (cons process directory) opencode--event-subscriptions))))

(provide 'opencode)
;;; opencode.el ends here
