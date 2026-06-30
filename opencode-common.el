;;; opencode-common.el --- Common code shared through the package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Common code shared through the package

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'map)
(require 'markdown-mode)
(require 'notifications)
(require 'project)
(require 'savehist)

(require 'opencode-flycheck)

(let (;; Have to set this to make slot `setf` methods usable in other
      ;; files.  The alternative would be to farm out the
      ;; `cl-defstruct` into its own file and to `require` that file
      ;; everywhere under `eval-and-compile`
      (cl--struct-inline nil))
  (cl-defstruct opencode--connection
    "An OpenCode server connection."
    name
    host
    port
    (base-url "")
    auth-username
    auth-password

    url
    process

    ;; SSE event process for the global event stream
    event-subscription
    ;; List of available providers and models
    providers
    ;; List of available agents excluding hidden
    agents
    ;; Alist mapping normalized project directories to available slash commands
    slash-commands-by-directory
    ;; An alist mapping of projectIDs to session control buffers for that project
    session-control-buffers

    ;; Current `opencode-files-finished-editing-functions' run context
    files-finished-editing-current
    ;; Queued `opencode-files-finished-editing-functions' run contexts
    files-finished-editing-queue

    ;; An alist mapping currently updating assistant message ids to render state
    assistant-messages

    ;; Time of most recent use
    (last-used 0)
    ;; Whether explicitly forgotten
    dead))

(defvar opencode-last-model '((providerID . "opencode")
                              (modelID . "big-pickle"))
 "Last model used.")
(add-to-list 'savehist-additional-variables 'opencode-last-model)

(defvar opencode-alerted-sessions nil
  "List of unvisited idle sessions.")

(defvar opencode-last-session-buffer nil
  "Last active opencode session buffer.")

(defvar opencode-part-type (make-hash-table :test 'equal)
  "Mapping of part id's to their type.")

(defvar-local opencode--extra-parts nil
  "An list containing extra parts to send with the next input.")

(defmacro opencode--with-connection (connection &rest body)
  "Run BODY with `opencode--current-connection' bound to CONNECTION."
  (declare (indent 1))
  `(let ((opencode--current-connection ,connection))
     (ignore opencode--current-connection)
     (when (and ,connection (called-interactively-p 'any))
       (opencode--refresh-last-use ,connection))
     ,@body))

(defvar opencode--connection-registry)

(defvar-local opencode--buffer-connection)
(defun opencode--buffer-connection ()
  (or (bound-and-true-p opencode--buffer-connection)
      (opencode--connection-for-project)))

(defun opencode--connection-live-p (connection)
  (and connection
       (or (process-live-p (opencode--connection-event-subscription connection))
           (process-live-p (opencode--connection-process connection)))))

(defun opencode--connection-status-string (connection)
  (cond
   ((opencode--connection-live-p connection) nil)
   ((opencode--connection-dead connection)
    (propertize (format " ⚠ DEAD %s " (opencode--format-connection connection))
                'face 'warning))
   (t
    (propertize (format " ⚠ DISCONNECTED %s " (opencode--format-connection connection))
                'face 'warning))))

(define-minor-mode opencode--connection-dead-mode
  "Indicate when buffer's connection is disconnected or dead."
  :lighter (:eval (when (bound-and-true-p opencode--buffer-connection)
                    (opencode--connection-status-string opencode--buffer-connection)))
  :interactive nil
  (when (derived-mode-p 'opencode-session-mode)
    (read-only-mode (if opencode--connection-dead-mode 1 0))))

(defun opencode--refresh-connection-state (connection)
  "Update modeline + read-only for all opencode buffers bound to CONNECTION."
  (let ((target (if (opencode--connection-live-p connection) 0 1)))
    (dolist (entry (opencode--connection-session-control-buffers connection))
      (dolist (buf (cdr entry))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (opencode--connection-dead-mode target)))))
    (maphash (lambda (_ buf)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (when (eq opencode--buffer-connection connection)
                     (opencode--connection-dead-mode target)))))
             opencode-session-buffers)
    (force-mode-line-update t)))

(defun opencode--normalize-directory (directory)
  "Return DIRECTORY as a canonical directory name."
  (file-name-as-directory (file-truename directory)))

(defcustom opencode-toast-function 'opencode--default-toast-show
  "Function to use to show toast notifications.
It receives a single argument, PROPERTIES, which is an alist with string keys
for title, message, variant (error/warning/info/success), and duration"
  :type 'function
  :group 'opencode)

(defcustom opencode-server-username "opencode"
  "Username to connect to opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-server-password nil
  "Password to connect to opencode server."
  :type 'string
  :group 'opencode)

(defcustom opencode-show-tool-output nil
  "Show output of tool calls."
  :type 'boolean
  :group 'opencode)

(defcustom opencode-file-edited-functions
  '(opencode--fix-parens
    opencode--reload-elisp-file)
  "Hook run after opencode edits a file.
This is an abnormal hook.  Each function receives two arguments, FILE and
RANGES, after a completed write, edit, or apply_patch tool call.  FILE is the
edited file path.  RANGES is a list of (START-LINE . END-LINE) cons cells in
the post-edit file, or nil when the whole file was written.  For deletion-only
edits, END-LINE is nil."
  :type 'hook
  :group 'opencode)

(defcustom opencode-files-finished-editing-functions
  '(opencode--indent-files
    opencode--flycheck-edited-files)
  "Run hooks after opencode finishes editing files.
This is an abnormal hook.  Each entry receives two arguments, CONTEXT
and FILES, the list of file paths from a message summary diff.  Hooks
that finish synchronously can just return.  Hooks that finish
asynchronously must return `:opencode-async' and call
`opencode-hook-finished' with CONTEXT when done.  While a hook is
active, it may call `opencode-report-diagnostic' with CONTEXT and a
message to queue diagnostic messages; all queued diagnostics are sent to
the session together after all hooks finish."
  :type 'hook
  :group 'opencode)

(defcustom opencode-project-files-finished-editing-functions
  nil
  "Like `opencode-files-finished-editing-functions', but per-project.
Designed to be set in `.dir-locals.el'"
  :local t
  :type 'hook
  :group 'opencode)

(defconst opencode--apply-patch-file-header-regexp
  "^\\*\\*\\* \\(Add File\\|Update File\\|Delete File\\): \\(.*\\)$"
  "Regexp matching file operation headers in apply_patch input.")

(defconst opencode--apply-patch-move-to-regexp
  "^\\*\\*\\* Move to: \\(.*\\)$"
  "Regexp matching move destination headers in apply_patch input.")

(defun opencode--time-ago (opencode-object type)
  "Return .time.TYPE value from OPENCODE-OBJECT, as seconds ago.
Returns nil if the timestamp is not present."
  (when-let ((timestamp (map-nested-elt opencode-object `(time ,type))))
    (- (float-time) (/ timestamp 1000))))

(defun opencode--format-time-ago (seconds)
  "Format SECONDS as a human-readable duration string.
Returns \"-\" if SECONDS is nil."
  (if seconds
      (seconds-to-string seconds)
    "-"))

(defun opencode--json-falsy (value)
  "Return t if VALUE is null, :null, or 0."
  (cl-case value
    ((nil :null 0) t)
    (t nil)))

(defun opencode--apply-patch-edited-files (patch-text)
  "Return files added or updated by PATCH-TEXT."
  (when patch-text
    (with-temp-buffer
      (insert patch-text)
      (goto-char (point-min))
      (cl-loop while (re-search-forward opencode--apply-patch-file-header-regexp nil t)
               for operation = (match-string 1)
               for file = (match-string 2)
               when (member operation '("Add File" "Update File"))
               collect (opencode--resolve-file-reference
                        (or (and (equal operation "Update File")
                                 (save-excursion
                                   (forward-line 1)
                                   (when (looking-at opencode--apply-patch-move-to-regexp)
                                     (match-string 1))))
                            file))))))

(defun opencode--truncate-at-max-lines (max-lines)
  "Delete the first half of the buffer if we've reached MAX-LINES."
  (when (> (line-number-at-pos) max-lines)
    (goto-char (point-max))
    (forward-line (- (/ max-lines 2)))
    (delete-region (point-min) (point))))

(cl-defun opencode--annotated-completion (prompt candidates)
  "Read a candidate with PROMPT and annotations.
CANDIDATES is a list of lists, where the first element is the string to show,
the second is the value to return, the third is the annotation to show, and
the optional fourth element is a number to sort by."
  (if candidates
      (let* ((max-length (seq-max
                          (mapcar (lambda (candidate)
                                    (length (car candidate)))
                                  candidates)))
             (candidate-results (make-hash-table :test 'equal))
             (has-sort-values nil)
             (candidates (cl-loop for (name return-value maybe-annotation sort-value) in candidates
                                  for annotation = (or maybe-annotation "")
                                  for candidate = (concat name
                                                          (propertize annotation 'invisible t))
                                  when sort-value do (setf has-sort-values t)
                                  do (puthash candidate return-value candidate-results)
                                  collect (propertize candidate
                                                      'opencode-annotation
                                                      (concat (make-string (+ 5 (- max-length
                                                                                   (length name)))
                                                                           ?\s)
                                                              annotation)
                                                      'opencode-sort-value sort-value)))
             (completion-extra-properties
              `(:annotation-function
                ,(lambda (candidate)
                   (get-text-property 0 'opencode-annotation candidate))
                :display-sort-function
                ,(when has-sort-values
                   (lambda (candidates)
                     (sort candidates
                           (lambda (a b)
                             (< (get-text-property 0 'opencode-sort-value a)
                                (get-text-property 0 'opencode-sort-value b)))))))))
        (gethash
         (completing-read
          prompt
          candidates
          nil t)
         candidate-results))
    (message "%sNo completion candidates" prompt)))

(defun opencode--format-questions (questions)
  "Format QUESTIONS showing ❓ prefix for each question."
  (string-join
   (cl-loop for q being the elements of questions
            for question-text = (alist-get 'question q)
            collect (concat "❓ " question-text "\n"))))

(defun opencode--relative-path-for-display (file)
  "Return FILE relative to project root when possible."
  (when file
    (let* ((project (project-current))
           (root (if project
                     (project-root project)
                   default-directory)))
      (if (and (file-name-absolute-p file)
               root
               (file-in-directory-p file root))
          (file-relative-name file root)
        file))))

(defun opencode--resolve-file-reference (file)
  "Return absolute path for FILE, or nil if no file exists."
  (let ((project (project-current)))
    (seq-find (lambda (candidate)
                (and candidate
                     (file-exists-p candidate)
                     (not (file-directory-p candidate))))
              (delete-dups
               (delq nil
                     (list (and (file-name-absolute-p file) file)
                           (when project
                             (expand-file-name file (project-root project)))
                           (expand-file-name file default-directory)))))))

(defun opencode--visit-file-location (location)
  "Open LOCATION in another window."
  (let ((file (car location))
        (line (cdr location)))
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (1- line))
    (recenter)))

(defun opencode--buttonize-file-references (string)
  "Return STRING with backticked file references turned into buttons."
  (let ((start 0)
        (regexp
         (rx "`"
             (group (any alnum "._~/")
                    (* (any alnum "@%_./~+-")))
             (optional ":"
                       (group (+ digit))
                       (optional "-" (+ digit)))
             "`"))
        parts)
    (while (string-match regexp string start)
      (let* ((match-beg (match-beginning 0))
             (match-end (match-end 0))
             (file (match-string 1 string))
             (line-string (match-string 2 string))
             (line (if line-string
                       (string-to-number line-string)
                     1))
             (path (opencode--resolve-file-reference file)))
        (push (substring string start match-beg) parts)
        (push (if path
                  (make-text-button
                   (substring string match-beg match-end)
                   nil
                   'action #'opencode--visit-file-location
                   'button-data (cons path line)
                   'follow-link t
                   'help-echo (if line-string
                                  (format "Open %s:%d in another window" file line)
                                (format "Open %s in another window" file))
                   'mouse-face 'highlight)
                (substring string match-beg match-end))
              parts)
        (setq start match-end)))
    (push (substring string start) parts)
    (apply #'concat (nreverse parts))))

(defun opencode--render-markdown (string)
  "Render STRING in `gfm-view-mode'."
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks (gfm-view-mode))
    (font-lock-ensure)
    (buffer-string)))

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
       :timeout (or .duration 5000)
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

(defun opencode--toast-show (properties)
  "Show toast notification with PROPERTIES from opencode."
  (funcall opencode-toast-function properties))

(defun opencode--buffer-active-p (buffer)
  "Return non-nil if BUFFER is visible in the selected window of a focused frame."
  (and buffer
       (eq buffer (window-buffer (selected-window)))
       (frame-focus-state (window-frame (selected-window)))))

(defun opencode--reload-elisp-file (file &optional _ranges)
  "Reload elisp FILE after LLM edits."
  (when (string-suffix-p ".el" file)
    (condition-case err
        (load-file file)
      (error
       (message "Error reloading %s after LLM edit: %s" file err)))))

(defvar flycheck-checkers)

(defun opencode--fix-parens (file &optional _ranges)
  "Use parinfer to fix parens in FILE after LLM edits."
  (when (and (or (string-suffix-p ".el" file)
                 (string-suffix-p ".lisp" file))
             (require 'parinfer-rust-mode nil t))
    (declare-function parinfer-rust--set-default-state "parinfer-rust-mode")
    (declare-function parinfer-rust--execute "parinfer-rust-mode")
    (defvar parinfer-rust--mode)
    (with-current-buffer (find-file-noselect file)
      (revert-buffer t t t)
      (condition-case nil
          (check-parens)
        (error
         (let ((flycheck-checkers (delq 'parinfer-rust
                                        (and (boundp 'flycheck-checkers)
                                             flycheck-checkers))))
           (parinfer-rust--set-default-state)
           (let ((parinfer-rust--mode "indent"))
             (parinfer-rust--execute)))
         (save-buffer))))))

(defun opencode--indent-sensitive-buffer-p ()
  "Return non-nil if the current buffer is indentation-sensitive."
  (derived-mode-p '(python-base-mode
                    haskell-mode
                    haskell-ts-mode
                    fsharp-mode
                    fsharp-ts-mode
                    yaml-mode
                    yaml-ts-mode
                    nim-mode
                    nim-ts-mode)))

(provide 'opencode-common)
;;; opencode-common.el ends here
