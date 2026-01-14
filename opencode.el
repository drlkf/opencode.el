;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
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

(defvar opencode--process nil
  "Opencode server process when started by Emacs.")

;;;###autoload
(defun opencode ()
  "Open opencode sessions control buffer for the current project directory."
  (interactive)
  (let ((project-dir (when-let (proj (project-current))
                       (directory-file-name (project-root proj)))))
    (if (or opencode--event-subscriptions
            (process-live-p opencode--process))
        (opencode-open-project project-dir)
      (setf opencode--process (start-process "opencode" "*opencode-serve*"
                                             "opencode" "serve"
                                             "--port" (number-to-string opencode-port)
                                             "--hostname" opencode-host))
      (set-process-filter
       opencode--process
       (lambda (process output)
         (when (string-prefix-p "opencode server listening on" output)
           ;; Remove filter to avoid repeated callbacks
           (set-process-filter process nil)
           (opencode-connect opencode-host opencode-port)
           (opencode-open-project project-dir)))))))

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
                               project 'updated))))))))

(defvar opencode-event-log-max-lines nil
  "Maximum number of lines to log in the opencode event log buffer.
Or nil to disable logging.")

(defun opencode--truncate-at-max-lines (max-lines)
  "Delete the first half of the buffer if we've reached MAX-LINES."
  (when (> (line-number-at-pos) max-lines)
    (goto-char (point-max))
    (forward-line (- (/ max-lines 2)))
    (delete-region (point-min) (point))))

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
  (let-alist properties
    (notifications-notify
     :title .title
     :body .message
     :urgency (pcase .variant
                ((or "error" "warning" )'critical)
                ((or "info" "success") 'normal))
     :timeout .duration
     :replaces-id 5647
     :app-icon 'none)))

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
         (msg-type (intern (alist-get 'type data))))
    (opencode--log-event "MESSAGE" data)
    (let-alist (alist-get 'properties data)
      (cl-case msg-type
        (tui.toast.show (opencode--toast-show (alist-get 'properties data)))
        (session.idle (opencode-api-session (.sessionID)
                          session
                        (let ((buffer (gethash .sessionID opencode-session-buffers)))
                          (unless (or
                                   ;; don't show alert if the buffer and frame is active
                                   (and (eq buffer (window-buffer (selected-window)))
                                        (frame-focus-state (window-frame (selected-window))))
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
        (message.part.updated (opencode-session--update-part .part .delta))
        (message.updated (opencode-session--message-updated .info))
        (permission.updated (opencode--permission-request .id .sessionID .type .title))
        (otherwise (opencode--log-event "WARNING" "unhandled message type"))))))

(defun opencode-disconnect (&optional event)
  "Disconnect from opencode server, optionally log EVENT."
  (interactive)
  (opencode--log-event "DISCONNECT" event)
  (cl-loop for (process) in opencode--event-subscriptions
           when (process-live-p process)
           do (kill-process process))
  (when (process-live-p opencode--process)
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
  (opencode-process-events default-directory)
  (opencode-api-create-session (if title
                                   `((title . ,title))
                                 (make-hash-table))
      session
    (opencode-open-session session)))

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
                                         ("disabled" "🔴 disabled"
                                          )))))))
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
          (opencode-open-session session))
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
             :headers `(("x-opencode-directory" . ,directory))
             :then 'opencode-disconnect
             :else 'opencode-disconnect)))
      (set-process-query-on-exit-flag process nil)
      (push (cons process directory) opencode--event-subscriptions))))

(provide 'opencode)
;;; opencode.el ends here
