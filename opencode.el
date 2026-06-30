;;; opencode.el --- Emacs interface to opencode -*- lexical-binding: t; byte-compile-warnings: (not docstrings-wide); -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (markdown-mode "2.6") (plz "0.9") (plz-media-type "0.2.4") (plz-event-source "0.1.3"))
;; URL: https://codeberg.org/sczi/opencode.el/

;;; Commentary:

;; Emacs interface to opencode.
;; Provides a comint-based mode for interacting with an opencode server.

;;; Code:

(require 'json)
(require 'magit)
(require 'plz-media-type)
(require 'plz-event-source)
(require 'project)

(require 'opencode-common)
(require 'opencode-api)
(require 'opencode-diff-parser)
(require 'opencode-permission)
(require 'opencode-question)
(require 'opencode-sessions)

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

(defcustom opencode-servers '((default))
  "Alist of pre-configured OpenCode server connections.
Each entry has the form:
\(NAME :host HOST :port PORT :base-url PATH :username USER :password PASS).

NAME is a symbol used as the connection name; the remaining elements
form a plist of field overrides.  Fields omitted from the plist fall
back to legacy defcustoms:
  :host       -> `opencode-host'
  :port       -> `opencode-port'
  :username   -> `opencode-server-username'
  :password   -> `opencode-server-password'"
  :type 'sexp
  :group 'opencode)

(defcustom opencode-find-server-function nil
  "Function to pick the OpenCode server name for a given directory.
Called with one argument, the directory (a string), it must return a
server name (a symbol) that appears as the NAME in some entry of
`opencode-servers', or nil to fall through to the default resolution."
  :type '(choice (const nil) function)
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

(defcustom opencode-session-deleted-functions nil
  "Hook run when an opencode session is deleted.
This is an abnormal hook. Each function receives one argument, SESSION-ID."
  :type 'hook
  :group 'opencode)

(defvar opencode--current-connection)

(defvar opencode--connection-registry nil
  "Alist of (KEY . CONNECTION) for known OpenCode connections.")

(defvar opencode--project-connections nil
  "Alist of (PROJECT-ROOT . CONNECTIONS).")

(defun opencode--normalize-base-url (base-url)
  (let ((s (or base-url "")))
    (cond
     ((string= s "") "")
     ((string-prefix-p "/" s)
      (if (string-suffix-p "/" s) (substring s 0 -1) s))
     (t
      (concat "/" (if (string-suffix-p "/" s) (substring s 0 -1) s))))))

(defun opencode--server-from-entry (entry)
  "Build an `opencode--connection' from an `opencode-servers' ENTRY."
  (let* ((plist (cdr entry))
         (host (or (plist-get plist :host) opencode-host))
         (port (or (plist-get plist :port) opencode-port))
         (base-url (opencode--normalize-base-url (or (plist-get plist :base-url) "")))
         (auth-username (or (plist-get plist :username) opencode-server-username))
         (auth-password (or (plist-get plist :password) opencode-server-password))
         (name (car entry)))
    (make-opencode--connection
     :name name
     :host host
     :port port
     :base-url base-url
     :url (format "http://%s:%d%s" host port base-url)
     :auth-username auth-username
     :auth-password auth-password)))

(defun opencode--connection-key (connection)
  "Return the registry key for CONNECTION."
  (intern (format "%s@%s:%d%s"
                  (opencode--connection-auth-username connection)
                  (opencode--connection-host connection)
                  (opencode--connection-port connection)
                  (opencode--connection-base-url connection))))

(defun opencode--format-connection (connection)
  "Format CONNECTION for non-chooser display."
  (let* ((user (opencode--connection-auth-username connection))
         (host (opencode--connection-host connection))
         (port (opencode--connection-port connection))
         (base-url (opencode--connection-base-url connection)))
    (format "%s%s:%d%s"
            (if (string= user "opencode") "" (format "%s@" user))
            (if (string= host "localhost") "" host)
            port
            base-url)))

(defun opencode--format-connection-verbose (connection)
  "Format CONNECTION for chooser display."
  (let* ((user (opencode--connection-auth-username connection))
         (host (opencode--connection-host connection))
         (port (opencode--connection-port connection))
         (base-url (opencode--connection-base-url connection)))
    (format "%s%s:%d%s%s"
            (if (string= user "opencode") "" (format "%s@" user))
            (if (string= host "localhost") "" host)
            port
            base-url
            (if-let ((name (opencode--connection-name connection)))
                (format " (%s)" name)
              ""))))

(defun opencode--existing-connection (connection)
  "Return the connection registered under CONNECTION's key, if any."
  (alist-get (opencode--connection-key connection) opencode--connection-registry))

(defun opencode--refresh-last-use (connection)
  "Record CONNECTION as most-recently-used by stamping its `last-used' timestamp."
  (setf (opencode--connection-last-used connection) (float-time)))

(defun opencode--register-connection (connection)
  "Register CONNECTION in `opencode--connection-registry' and `opencode--project-connections'."
  (let ((key (opencode--connection-key connection)))
    (setq opencode--connection-registry
          (cons (cons key connection)
                (assq-delete-all key opencode--connection-registry)))
    (when-let ((project (project-current)))
      (let* ((root (expand-file-name (project-root project)))
             (conns (cdr (assoc root opencode--project-connections))))
        (setq opencode--project-connections
              (cons (cons root (cons connection (delete connection conns)))
                    (assoc-delete-all root opencode--project-connections))))))
  (opencode--refresh-last-use connection)
  (opencode--adopt-orphaned-buffers connection))

(defun opencode--teardown-connection (connection)
  "Tear down CONNECTION's live resources and nil their slots."
  (let ((sub (opencode--connection-event-subscription connection)))
    (when (process-live-p sub)
      (kill-process sub)))
  (let ((proc (opencode--connection-process connection)))
    (when (process-live-p proc)
      (set-process-sentinel proc nil)
      (kill-process proc)))
  (setf (opencode--connection-event-subscription connection) nil
        (opencode--connection-process connection) nil)
  (opencode--refresh-connection-state connection))

(defun opencode--forget-connection (connection)
  "Remove CONNECTION from `opencode--connection-registry' and the project map."
  (cl-assert (not (opencode--connection-live-p connection)))
  (setf (opencode--connection-dead connection) t)
  (let ((key (opencode--connection-key connection)))
    (setq opencode--connection-registry
          (assq-delete-all key opencode--connection-registry)
          opencode--project-connections
          (cl-loop for (root . val) in opencode--project-connections
                   collect (cons root (delq connection val))))))

(defun opencode--connection-for-project (&optional directory)
  "Return the most-recently-used connection for DIRECTORY's project."
  (let* ((directory (or directory default-directory))
         (root (if-let ((proj (project-current nil directory)))
                   (expand-file-name (project-root proj))
                 (expand-file-name directory))))
    (when-let ((conns (cdr (assoc root opencode--project-connections))))
      (cl-reduce (lambda (a b)
                   (if (> (opencode--connection-last-used a)
                          (opencode--connection-last-used b))
                       a b))
                 conns))))

(defun opencode--resolve-connection (on-connect)
  "Resolve a connection via DWIM and pass it to ON-CONNECT."
  (let* ((connection (opencode--buffer-connection)))
    (cond
     (connection (funcall on-connect connection))
     (opencode--connection-registry
      (funcall on-connect (if (null (cdr opencode--connection-registry))
                              (cdar opencode--connection-registry)
                            (opencode--choose-live-connection))))
     (t (opencode-autoconnect on-connect)))))

(defun opencode--connection-live-p (connection)
  "Return non-nil if CONNECTION has any live backing process."
  (or (process-live-p (opencode--connection-event-subscription connection))
      (process-live-p (opencode--connection-process connection))))

(defun opencode--choose-connection-matching (predicate &optional prompt default)
  "Prompt for a connection in `opencode--connection-registry' matching PREDICATE."
  (let* ((entries (cl-remove-if-not predicate (mapcar #'cdr opencode--connection-registry)))
         (candidates (mapcar #'opencode--format-connection-verbose entries)))
    (unless candidates
      (user-error "opencode: no matching connections"))
    (let* ((choice (completing-read (or prompt "Connection: ")
                                    candidates nil t nil nil
                                    (or default (car candidates))))
           (hit (cl-find choice entries
                         :key #'opencode--format-connection-verbose
                         :test #'string=)))
      (or hit (user-error "No matching connection named %s" choice)))))

(defun opencode--choose-live-connection (&optional prompt default)
  (opencode--choose-connection-matching #'opencode--connection-live-p
                                        (or prompt "Connection: ")
                                        default))

(defun opencode--choose-connection (&optional prompt default)
  (opencode--choose-connection-matching #'identity (or prompt "Connection:") default))

(defun opencode--choose-server (on-connect)
  "Prompt for a server (known connection or unconnected entry), then call ON-CONNECT."
  (unless (or opencode-servers opencode--connection-registry)
    (user-error "opencode: no servers in `opencode-servers' and no known connections"))
  (let* ((known-conns (mapcar #'cdr opencode--connection-registry))
         (known-strs (mapcar #'opencode--format-connection-verbose known-conns))
         (configured-strs (mapcar (lambda (e)
                                    (opencode--format-connection-verbose
                                     (opencode--server-from-entry e)))
                                  opencode-servers))
         (candidates (cl-remove-duplicates
                      (append known-strs configured-strs)
                      :test #'string=))
         (choice (completing-read "Server: " candidates nil t nil nil
                                  (car candidates)))
         (known-index (cl-position choice known-strs :test #'string=)))
    (if known-index
        (funcall on-connect (nth known-index known-conns))
      (let ((entry (cl-find choice opencode-servers
                            :key (lambda (e)
                                   (opencode--format-connection-verbose
                                    (opencode--server-from-entry e)))
                            :test #'string=)))
        (unless entry
          (user-error "Server %s not in `opencode-servers'" choice))
        (opencode-autoconnect on-connect entry)))))

;; so invisible prompt "> " doesn't make whole prompt invisible
(add-to-list 'comint--prompt-rear-nonsticky 'invisible)

(defun opencode--server-running-p (connection)
  "Return non-nil if an opencode server is running at CONNECTION's host and port."
  (condition-case err
      (plz 'get (concat (opencode--connection-url connection) "/global/health")
        :headers (list (opencode--auth-header connection))
        :timeout 1)
    (plz-http-error
     (when-let (plz-error (cl-third err))
       (when (= 401 (plz-response-status (plz-error-response plz-error)))
         (user-error "OpenCode server is already running but password protected: \
set `opencode-server-password' to connect to it"))))
    (error nil)))

(defun opencode--serve-command (connection)
  "Return the command to start the opencode server for CONNECTION."
  (or opencode-serve-command
      (format "%s serve --port %d --hostname %s"
              opencode-command
              (opencode--connection-port connection)
              (opencode--connection-host connection))))

(defun opencode--start-server (connection on-connect)
  "Start an opencode server for CONNECTION and run ON-CONNECT when ready."
  (setf (opencode--connection-auth-password connection)
        (or (opencode--connection-auth-password connection)
            (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
              (apply #'string
                     (cl-loop repeat 32
                              collect (aref chars (random (length chars))))))))
  (setf (opencode--connection-process connection)
        (let ((process-environment (cl-list*
                                    (format "OPENCODE_SERVER_USERNAME=%s"
                                            (opencode--connection-auth-username connection))
                                    (format "OPENCODE_SERVER_PASSWORD=%s"
                                            (opencode--connection-auth-password connection))
                                    process-environment)))
          (start-process-shell-command
           "opencode" "*opencode-serve*"
           (opencode--serve-command connection))))

  (set-process-filter
   (opencode--connection-process connection)
   (lambda (process output)
     (with-current-buffer "*opencode-serve*"
       (goto-char (process-mark process))
       (insert output))
     (when (string-prefix-p "opencode server listening on" output)
       ;; Remove filter to avoid repeated callbacks
       (set-process-filter process nil)
       (opencode-connect connection on-connect))))

  (set-process-sentinel
   (opencode--connection-process connection)
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (unless (zerop (process-exit-status process))
         (pop-to-buffer (process-buffer process))
         (error "OpenCode failed, %s" (string-trim-right event "\n")))))))

(defun opencode-autoconnect (on-connect &optional entry)
  "Connect via ENTRY (default: first of `opencode-servers'), or restart as needed.
Only starts a new server if `opencode-auto-start-server' is non-nil.
Run ON-CONNECT with the connection as its single argument after connected."
  (let* ((entry (or entry
                    (car opencode-servers)
                    (user-error "No servers in `opencode-servers'")))
         (new (opencode--server-from-entry entry))
         (existing (opencode--existing-connection new))
         (connection (or existing new)))
    (cond
     ;; Already connected
     ((and existing (opencode--connection-event-subscription connection))
      (funcall on-connect connection)
      (opencode--refresh-connection-state connection))
     ;; We started a server process that's still alive
     ((and existing (process-live-p (opencode--connection-process connection)))
      (funcall on-connect connection)
      (opencode--refresh-connection-state connection))
     ;; Server already running externally
     ((opencode--server-running-p connection)
      (opencode-connect connection on-connect))
     ;; Need to start a new server
     (opencode-auto-start-server
      (opencode--start-server connection on-connect))
     ;; Auto-start disabled, no server running
     (t
      (user-error "No opencode server running at %s (auto-start disabled)"
                  (opencode--format-connection connection))))))


;;;###autoload
(defun opencode (&optional arg)
  "Open opencode sessions control buffer for the current project directory.
Connects to an existing server if one is running, otherwise starts a new one
if `opencode-auto-start-server' is non-nil.

With prefix ARG, prompt for a live connection or an unconnected server
from `opencode-servers'."
  (interactive "P")
  (let* ((project-dir (expand-file-name
                       (if-let (proj (project-current))
                           (project-root proj)
                         default-directory)))
         (on-connect (lambda (connection) (opencode-open-project project-dir connection)))
         (dwim-name (when (functionp opencode-find-server-function)
                      (ignore-errors
                        (funcall opencode-find-server-function
                                 project-dir))))
         (dwim-entry (when dwim-name
                       (assq dwim-name opencode-servers))))
    (cond
     ((and (not arg) dwim-entry)
      (opencode-autoconnect on-connect dwim-entry))
     (arg
      (opencode--choose-server on-connect))
     (t
      (opencode--resolve-connection on-connect)))))

(cl-defun opencode--do-connect (connection on-connect)
  (when-let ((existing (opencode--existing-connection connection)))
    (when (not (eq existing connection))
      (if (process-live-p (opencode--connection-event-subscription existing))
          (progn
            (funcall on-connect existing)
            (cl-return-from opencode--do-connect))
        (opencode--teardown-connection existing)
        (setq connection existing))))
  (opencode--register-connection connection)
  (opencode--with-connection connection
    (setf (opencode--connection-slash-commands-by-directory connection) nil)
    (opencode--subscribe-global-events connection)
    (opencode-api-configured-providers result
      (setf (opencode--connection-providers connection) (alist-get 'providers result))
      (opencode--fetch-agents connection
                              (lambda (connection)
                                (funcall on-connect connection)
                                (opencode--refresh-connection-state connection))))
    (message "Connected to %s" (opencode--format-connection connection))))

(defun opencode-connect (connection on-connect)
  "Connect to opencode server specified by CONNECTION, then run ON-CONNECT.
Interactively, prompt for which entry of `opencode-servers' to use, With
prefix arg, prompt for ad-hoc host/port/creds."
  (interactive
   (if current-prefix-arg
       (let* ((host (read-string "Host: " opencode-host))
              (port (read-number "Port: " opencode-port))
              (base-url (let ((s (read-string "Base URL (empty for none): " "")))
                          (opencode--normalize-base-url s)))
              (entry (list (intern (format "%s:%d" host port))
                           :host host :port port :base-url base-url)))
         (opencode-autoconnect #'ignore entry)
         (list nil nil))
     (let* ((entries opencode-servers)
            (names (mapcar (lambda (e) (symbol-name (car e))) entries))
            (default-name
             (or (and (functionp opencode-find-server-function)
                      (let ((picked (ignore-errors
                                      (funcall opencode-find-server-function
                                               (expand-file-name
                                                (if-let (proj (project-current))
                                                    (project-root proj)
                                                  default-directory))))))
                        (and picked
                             (cl-find picked entries :key #'car)
                             (symbol-name picked))))
                 (and entries (symbol-name (car (car entries))))))
            (name-str (completing-read "Server: " names nil t
                                       nil nil default-name))
            (entry (or (cl-find (intern name-str) entries :key #'car)
                       (user-error "No server named %s" name-str))))
       (opencode-autoconnect #'ignore entry)
       (list nil nil))))
  (when connection
    (opencode--do-connect connection on-connect)))

(defun opencode-open-project (directory connection)
  "Open sessions control buffer for DIRECTORY, routing API calls via CONNECTION."
  (opencode--with-connection connection
    (opencode--download-slash-commands directory)
    (let ((buffer-name (format "*OpenCode[%s] Sessions in %s*"
                               (opencode--format-connection connection)
                               directory)))
      (unless (get-buffer buffer-name)
        (with-current-buffer (get-buffer-create buffer-name)
          (opencode-session-control-mode)
          (setq opencode--buffer-connection connection
                default-directory directory)
          (opencode--register-connection connection)
          (opencode-api-current-project project
            (let-alist project
              (setf (map-elt (opencode--connection-session-control-buffers connection) .id)
                    (cons (current-buffer)
                          (seq-filter #'buffer-live-p
                                      (map-elt (opencode--connection-session-control-buffers connection)
                                               .id))))))
          (opencode-sessions-redisplay)))
      (pop-to-buffer buffer-name))))

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
  (opencode--resolve-connection
   (lambda (connection)
     (opencode--with-connection connection
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
                                    project 'updated)))))
          connection))))))

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

(defun opencode--run-file-edited-hook (tool state)
  "Run `opencode-file-edited-functions' for completed TOOL with STATE."
  (let-alist state
    (dolist (file-ranges (pcase tool
                           ("write"
                            (when .input.filePath
                              (list (cons .input.filePath nil))))
                           ((or "edit" "apply_patch")
                            (unless .metadata.diff
                              (error "Missing diff metadata for %s tool" tool))
                            (opencode-diff->source-line-ranges .metadata.diff))))
      (run-hook-with-args 'opencode-file-edited-functions
                          (car file-ranges)
                          (cdr file-ranges)))))

(defun opencode-report-diagnostic (connection message)
  "Queue diagnostic MESSAGE for the active finished-editing hook on CONNECTION."
  (let ((active (and connection (opencode--connection-files-finished-editing-current connection))))
    (unless (and active (plist-get active :active-hook))
      (error "`opencode-report-diagnostic' called outside a finished-editing hook"))
    (when (and message (not (string= message "")))
      (push message (plist-get active :diagnostics)))))

(defun opencode-hook-finished (connection)
  "Mark the active finished-editing hook on CONNECTION as complete."
  (let ((active (and connection (opencode--connection-files-finished-editing-current connection))))
    (unless (and active (plist-get active :active-hook))
      (error "`opencode-hook-finished' called outside a finished-editing hook"))
    (plist-put active :active-hook nil)
    (opencode--with-connection connection
      (opencode--run-next-files-finished-editing-hook))))

(defun opencode--files-finished-editing-diagnostic-message (diagnostics)
  "Return a synthetic input message for DIAGNOSTICS."
  (concat
   "Diagnostics were reported after editing files. "
   "Please fix any problems caused by your changes.\n\n"
   (mapconcat #'identity diagnostics "\n\n")))

(defun opencode--maybe-start-files-finished-editing-hook ()
  "Start the next queued finished-editing hook run if none is active."
  (unless (opencode--connection-files-finished-editing-current opencode--current-connection)
    (let ((queue (opencode--connection-files-finished-editing-queue opencode--current-connection)))
      (when queue
        (let ((context (pop queue)))
          (setf (opencode--connection-files-finished-editing-current opencode--current-connection) context)
          (opencode--run-next-files-finished-editing-hook))))))

(defun opencode--finish-files-finished-editing-hook-run (context)
  "Finish finished-editing hook run CONTEXT and send queued diagnostics."
  (let ((diagnostics (nreverse (plist-get context :diagnostics)))
        (buffer (plist-get context :session-buffer)))
    (setf (opencode--connection-files-finished-editing-current opencode--current-connection) nil)
    (when (and diagnostics (buffer-live-p buffer))
      (with-current-buffer buffer
        (opencode-session--send-synthetic-input
         (opencode--files-finished-editing-diagnostic-message diagnostics)))))
  (opencode--maybe-start-files-finished-editing-hook))

(defun opencode--run-next-files-finished-editing-hook ()
  "Run the next hook in the active finished-editing context."
  (let ((context (opencode--connection-files-finished-editing-current opencode--current-connection)))
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
                    (setq result (funcall hook opencode--current-connection (plist-get context :files)))
                  (error
                   (setq errored t)
                   (opencode--log-event
                    "WARNING FINISHED EDITING HOOK"
                    (format "%S failed: %s" hook (error-message-string err)))
                   (when (and (eq (opencode--connection-files-finished-editing-current opencode--current-connection) context)
                              (eq (plist-get context :active-hook) hook))
                     (plist-put context :active-hook nil)
                     (opencode--run-next-files-finished-editing-hook))))
                (when (and (not errored)
                           (not (eq result :opencode-async))
                           (eq (opencode--connection-files-finished-editing-current opencode--current-connection) context)
                           (eq (plist-get context :active-hook) hook))
                  (plist-put context :active-hook nil)
                  (opencode--run-next-files-finished-editing-hook)))
            (opencode--finish-files-finished-editing-hook-run context)))))))

(defun opencode--maybe-run-file-edited-hook (part)
  "Run `opencode-file-edited-functions' if PART completed a file edit."
  (let-alist part
    (when (and (equal .type "tool")
               (equal .state.status "completed"))
      (opencode--run-file-edited-hook .tool .state))))

(defun opencode--message-summary-diff-files (info)
  "Return file names from INFO summary diffs."
  (when-let ((diffs (map-nested-elt info '(summary diffs))))
    (delq nil
          (mapcar (lambda (diff)
                    (opencode--resolve-file-reference
                     (alist-get 'file diff)))
                  (seq-into diffs 'list)))))

(defun opencode--record-files-edited-this-turn (info)
  "Record INFO summary diff files for the session's current turn."
  (let-alist info
    (when (and .sessionID (map-nested-elt info '(summary diffs)))
      (let ((files (opencode--message-summary-diff-files info)))
        (opencode--with-session-buffer .sessionID
          (setq opencode--files-edited-this-turn
                (delete-dups
                 (append opencode--files-edited-this-turn files))))))))

(defun opencode--maybe-run-files-finished-editing-hook (session-id)
  "Run `opencode-files-finished-editing-functions' for SESSION-ID's pending files."
  (opencode--with-session-buffer session-id
    (when opencode--files-edited-this-turn
      (let ((files opencode--files-edited-this-turn))
        (setq opencode--files-edited-this-turn nil)
        (when-let ((hooks (append opencode-files-finished-editing-functions
                                  opencode-project-files-finished-editing-functions)))
          (setf (opencode--connection-files-finished-editing-queue opencode--current-connection)
                (append (opencode--connection-files-finished-editing-queue opencode--current-connection)
                        (list (list :session-buffer (current-buffer)
                                    :files files
                                    :hooks hooks
                                    :diagnostics nil
                                    :active-hook nil))))
          (opencode--maybe-start-files-finished-editing-hook))))))

(defun opencode--indent-files (_ files)
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

(defun opencode-run-command-diagnostic (connection command)
  "Run COMMAND asynchronously and report diagnostics for the hook on CONNECTION.
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
               (opencode--with-connection connection
                 (unwind-protect
                     (unless (zerop (process-exit-status process))
                       (let ((output (with-current-buffer (process-buffer process)
                                       (string-trim (buffer-string)))))
                         (opencode-report-diagnostic
                          connection
                          (if (equal output "")
                              (format "`%s` failed" command-string)
                            (format "`%s` failed:\n\n%s"
                                    command-string
                                    output)))))
                   (kill-buffer buffer)
                   (opencode-hook-finished connection))))))
          :opencode-async)
      (error
       (kill-buffer buffer)
       (opencode-report-diagnostic
        connection
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

(defun opencode-uv-pytest (connection _)
  "Run pytest with uv and report diagnostics on CONNECTION."
  (opencode-run-command-diagnostic
   connection
   '("uv" "run" "python3" "-m" "pytest")))

(defun opencode-emacs-ert (connection _)
  "Run ERT and report diagnostics on CONNECTION."
  (opencode-run-command-diagnostic
   connection
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

(defun opencode--handle-message (data)
  "Handle decoded message DATA from opencode server."
  (let* ((msg-type (intern (alist-get 'type data)))
         (properties (alist-get 'properties data)))
    (unless (memq msg-type '(file.watcher.updated session.diff sync))
      (opencode--log-event "MESSAGE" data)
      (let-alist properties
        (cl-case msg-type
          (tui.toast.show (opencode--toast-show properties))
          (session.idle
           (unless (opencode--defer-files-finished-editing-hook
                    .sessionID)
             (opencode--maybe-run-files-finished-editing-hook .sessionID))
           (opencode-api-session (.sessionID)
               session
             (opencode--with-session-buffer .sessionID
               (opencode--show-prompt)
               (unless (or
                        (opencode--buffer-active-p (current-buffer))
                        ;; don't show alert for subagent sessions
                        (alist-get 'parentID session))
                 (opencode--toast-show `((title . "OpenCode Finished")
                                         (message . ,(alist-get 'title session))
                                         (variant . "success")))
                 (push session opencode-alerted-sessions)))))
          (session.status (pcase .status.type
                            ((or "busy" "idle")
                             (opencode-session--set-status .sessionID .status.type))
                            ("retry"
                             (opencode-api-session (.sessionID)
                                 session
                               (opencode--toast-show `((title . ,(concat "OpenCode: "
                                                                         (alist-get 'title session)))
                                                       (message . ,(format "%s\n\nRetry #%d"
                                                                           .status.message
                                                                           .status.attempt))
                                                       (variant . "warning")))))))
          ((session.created session.updated session.deleted)
           (when (eq 'session.deleted msg-type)
             (run-hook-with-args 'opencode-session-deleted-functions .info.id))
           (dolist (buffer (map-elt (opencode--connection-session-control-buffers opencode--current-connection) .info.projectID))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (opencode-sessions-redisplay))))
           (opencode--with-session-buffer .info.id
             (cl-case msg-type
               (session.updated (rename-buffer (format "*OpenCode: %s*" .info.title) t))
               (session.deleted (delete-process)))))
          (session.error (opencode-session--display-error .sessionID .error.data.message))
          (message.part.updated
           (opencode--maybe-run-file-edited-hook .part)
           (opencode-session--update-part .part.id .part .delta .part.type))
          (message.part.delta (opencode-session--update-part
                               .partID properties .delta
                               (gethash .partID opencode-part-type)))
          (message.updated
           (opencode--record-files-edited-this-turn .info)
           (opencode-session--message-updated .info))
          (permission.asked
           (opencode--permission-request
            .id .sessionID .permission
            .metadata
            (seq-into .patterns 'list)
            (seq-into .always 'list)))
          (question.asked
           (opencode--question-request .id .sessionID .questions))
          (otherwise (opencode--log-event "WARNING" "unhandled message type")))))))

(defun opencode--handle-global-event (event)
  "Handle a global wrapper EVENT from opencode server."
  (let* ((data (json-read-from-string (plz-event-source-event-data event)))
         (directory (alist-get 'directory data))
         (payload (alist-get 'payload data)))
    (if (and directory payload)
        (let ((default-directory (opencode--normalize-directory directory)))
          (opencode--handle-message payload))
      (unless (string= "server.heartbeat" (alist-get 'type payload))
        (opencode--log-event "WARNING GLOBAL EVENT" data)))))

(defun opencode--subscribe-global-events (connection)
  "Subscribe to the global opencode event stream for CONNECTION."
  (setf (opencode--connection-event-subscription connection)
        (plz-media-type-request
          'get (concat (opencode--connection-url connection) "/global/event")
          :as `(media-types
                ((text/event-stream
                  . ,(plz-event-source:text/event-stream
                      :events `((open . ,(lambda (event)
                                           (opencode--log-event "OPEN" event)))
                                (message . ,(lambda (event)
                                              (opencode--with-connection connection
                                                (opencode--handle-global-event event))))
                                (close . ,(lambda (event)
                                            (opencode--with-connection connection
                                              (opencode-disconnect event)))))))))
          :headers (delq nil (list (opencode--auth-header connection)))
          :then (lambda (response)
                  (opencode--with-connection connection
                    (opencode-disconnect (format "HTTP %s" (plz-response-status response)))))
          :else (lambda (response)
                  (opencode--with-connection connection
                    (opencode-disconnect (format "ERROR %s" response))))))
  (set-process-query-on-exit-flag (opencode--connection-event-subscription connection) nil))

(defun opencode-disconnect (&optional event)
  "Disconnect from opencode server, optionally log EVENT.
With prefix arg, disconnect all live connections."
  (interactive "P")
  (opencode--log-event "DISCONNECT" event)
  (let* ((ambient (opencode--buffer-connection))
         (live (cl-remove-if-not #'opencode--connection-live-p
                                 (mapcar #'cdr opencode--connection-registry))))
    (cond
     (ambient
      (opencode--teardown-connection ambient))
     ((null live)
      (user-error "opencode: not connected"))
     ;; Prefix arg: disconnect all live.
     (current-prefix-arg
      (dolist (connection live)
        (opencode--teardown-connection connection)))
     ;; Exactly one live: disconnect it.
     ((null (cdr live))
      (opencode--teardown-connection (car live)))
     ;; Multiple live: prompt.
     (t
      (opencode--teardown-connection (opencode--choose-live-connection))))))

;;;###autoload
(defun opencode-forget-connection (&optional arg)
  "Forget a disconnected connection.
With prefix ARG: prompt for a connection to forget."
  (interactive "P")
  (let* ((ambient (opencode--buffer-connection))
         (ambient-disconnected-p (and ambient (not (opencode--connection-live-p ambient))))
         (all-disconnected (cl-remove-if #'opencode--connection-live-p (mapcar #'cdr opencode--connection-registry)))
         (target (cond
                  ((and (not arg) ambient-disconnected-p) ambient)
                  (all-disconnected
                   (opencode--choose-connection-matching
                    (lambda (c) (not (opencode--connection-live-p c)))
                    "Forget connection: "
                    (and ambient-disconnected-p ambient)))
                  (t (user-error "opencode: nothing to forget")))))
    (opencode--forget-connection target)
    (message "Forgot connection %s" (opencode--format-connection target))))

(defun opencode--fetch-agents (connection callback)
  "Fetch available agents from server via CONNECTION and filter out hidden agents.
Run CALLBACK with CONNECTION once fetched."
  (opencode--with-connection connection
    (opencode-api-agents agents
      (setf (opencode--connection-agents connection)
            (seq-filter (lambda (agent)
                          (opencode--json-falsy (alist-get 'hidden agent)))
                        agents))
      (funcall callback connection))))

(cl-defun opencode-new-session (&key title callback)
  "Create a new session. With a prefix argument it will ask for TITLE.
Without it will use a default title and then automatically generate one.
If CALLBACK is given, it will be called with the session after it is created."
  (interactive
   (when current-prefix-arg
     (list :title (read-string "Title: "))))
  (opencode--resolve-connection
   (lambda (connection)
     (opencode--with-connection connection
       (opencode--download-slash-commands default-directory)
       (opencode-api-create-session (if title
                                        `((title . ,title))
                                      (make-hash-table))
           session
         (opencode-open-session session :callback callback))))))

(defun opencode-toggle-mcp ()
  "Completing read to select an MCP to toggle."
  (interactive)
  (opencode--with-connection (opencode--buffer-connection)
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
                        (message "Enabled %s" mcp))))))))

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
        (opencode-api-revert-message (opencode-session-id)
            `((messageID . ,message-id))
            result
          (message
           (if result
               "Reverted edits after message"
             "Failed to revert message")))
      (user-error "No user message at point"))))

(defun opencode-delete-message ()
  "Delete the message at point from the current session."
  (interactive)
  (unless opencode-session-id
    (user-error "Not in an opencode session buffer"))
  (opencode--current-message-exchange (user-id assistant-id)
    (if (and user-id assistant-id)
        (opencode-api-delete-message (opencode-session-id assistant-id)
            assistant-result
          (unless assistant-result
            (error "Failed to delete assistant message"))
          (opencode-api-delete-message (opencode-session-id user-id)
              user-result
            (unless user-result
              (error "Failed to delete user message"))
            (opencode--delete-message-at-point)))
      (user-error "No message with assistant response at point"))))

(defun opencode-unrevert-all ()
  "Unrevert all reverted messages in the current session."
  (interactive)
  (opencode--with-connection (opencode--buffer-connection)
    (opencode-api-unrevert-all (opencode-session-id)
        result
      (message
       (if result
           "Restored all edits in session"
         "Failed to restore edits")))))

(defun opencode--download-slash-commands (directory)
  "Download slash commands for DIRECTORY on the current connection."
  (setf directory (opencode--normalize-directory directory))
  (let* ((connection opencode--current-connection))
    (unless (assoc directory (opencode--connection-slash-commands-by-directory connection))
      (let ((default-directory directory))
        (opencode-api-commands commands
          (setf (alist-get directory (opencode--connection-slash-commands-by-directory connection)
                           nil nil #'string=)
                commands))))))

(provide 'opencode)
;;; opencode.el ends here
