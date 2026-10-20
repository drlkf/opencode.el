;;; opencode-sessions.el --- Convert opencode session to markdown  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: internal

;;; Commentary:

;; Convert opencode session to markdown

;;; Code:

(require 'json)
(require 'opencode-format-tool-calls)

(defun opencode--markdown-fence (text &optional language)
  "Return TEXT in a Markdown fenced block labeled LANGUAGE."
  (let ((text (or text ""))
        (start 0)
        (longest-backticks 0))
    (while (string-match "`+" text start)
      (setq longest-backticks
            (max longest-backticks
                 (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (let ((fence (make-string (max 3 (1+ longest-backticks)) ?`)))
      (concat fence (or language "") "\n"
              text
              (unless (string-suffix-p "\n" text) "\n")
              fence))))

(defun opencode--conversation-json (value)
  "Return VALUE encoded as pretty JSON."
  (let ((json-encoding-pretty-print t))
    (json-encode value)))

(defun opencode--conversation-format-text-part (part)
  "Return Markdown for assistant text content PART."
  (let ((text (alist-get 'text part)))
    (when (and text (not (string-empty-p text)))
      text)))

(defun opencode--conversation-format-reasoning-part (part)
  "Return Markdown for assistant reasoning content PART."
  (when-let ((text (alist-get 'text part)))
    (unless (string-empty-p text)
      (concat "### Reasoning\n\n"
              (opencode--markdown-fence text "text")))))

(defun opencode--conversation-format-tool-part (part)
  "Return Markdown for assistant tool call content PART."
  (let-alist part
    (string-join
     (delq nil
           (list
            (format "### Tool call: %s" (or .name "unknown"))
            (when .id
              (format "Call ID: `%s`" .id))
            (when .state.status
              (format "Status: `%s`" .state.status))
            (when .state.input
              (concat "Input:\n\n"
                      (opencode--markdown-fence
                       (opencode--conversation-json .state.input)
                       "json")))
            (when .state.content
              (concat "Output:\n\n"
                      (opencode--markdown-fence
                       (opencode--tool-content-text .state.content) "text")))))
     "\n\n")))

(defun opencode--conversation-format-file-attachment (file)
  "Return Markdown for user message file attachment FILE."
  (let-alist file
    (string-join
     (delq nil
           (list
            "### Context file"
            (when .name (format "- Name: `%s`" .name))
            (when .uri (format "- URI: `%s`" .uri))
            (when .description (format "- Description: %s" .description))))
     "\n")))

(defun opencode--conversation-format-agent-attachment (agent)
  "Return Markdown for user message agent mention AGENT."
  (when-let ((name (alist-get 'name agent)))
    (format "### Context agent\n\n@%s" name)))

(defun opencode--conversation-format-part (part)
  "Return Markdown for a supported assistant content PART."
  (let-alist part
    (pcase .type
      ("text" (opencode--conversation-format-text-part part))
      ("reasoning" (opencode--conversation-format-reasoning-part part))
      ("tool" (opencode--conversation-format-tool-part part)))))

(defun opencode--conversation-format-simple-message (title text)
  "Return Markdown for a plain TEXT message under TITLE."
  (when (and text (not (string-empty-p text)))
    (format "## %s\n\n%s" title text)))

(defun opencode--conversation-format-user-message (message)
  "Return Markdown for a user MESSAGE, including its file/agent attachments."
  (let-alist message
    (let ((sections
           (delq nil
                 (append
                  (list (unless (string-empty-p (or .text "")) .text))
                  (mapcar #'opencode--conversation-format-file-attachment .files)
                  (mapcar #'opencode--conversation-format-agent-attachment .agents)))))
      (when sections
        (string-join (cons "## User" sections) "\n\n")))))

(defun opencode--conversation-format-message (message)
  "Return Markdown for MESSAGE."
  (let-alist message
    (pcase .type
      ("user" (opencode--conversation-format-user-message message))
      ("system" (opencode--conversation-format-simple-message "System" .text))
      ("synthetic" (opencode--conversation-format-simple-message "Synthetic context" .text))
      ("assistant"
       (when-let ((parts (delq nil (mapcar #'opencode--conversation-format-part .content))))
         (string-join (cons "## Assistant" parts) "\n\n"))))))

(defun opencode--conversation-to-markdown (messages)
  "Return MESSAGES as a Markdown conversation transcript.
MESSAGES must be oldest first."
  (let ((formatted-messages
         (delq nil
               (mapcar #'opencode--conversation-format-message messages))))
    (concat
     (string-join
      (list "# OpenCode Conversation"
            (if formatted-messages
                (string-join formatted-messages "\n\n")
              "_No supported message parts._"))
      "\n\n")
     "\n")))


(provide 'opencode-convert)
;;; opencode-convert.el ends here
