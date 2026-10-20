;;; opencode-question.el --- Form answer UI for opencode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Arthur Heymans

;; Author: Arthur Heymans <arthur@aheymans.xyz>
;; Keywords: internal

;;; Commentary:

;; Provides a transient-based UI for answering forms (question-kind, but
;; handled generically for any form) sent by the opencode server.  Supports
;; single-select, multiple-select, and custom typed answers.
;;
;; Each option is a numbered key in the transient popup.  Press the key
;; to toggle the option.  Confirm with C-c C-c, reject with q.
;;
;; The transient directly calls the server API on confirm or reject,
;; avoiding `recursive-edit' which conflicts with transient key handling.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'opencode-api)
(require 'opencode-common)
(require 'opencode-sessions)

;;; Per-field display state (global, since transient is not buffer-local)

(defvar opencode-question--marked nil
  "Vector of booleans tracking which options are marked.")

(defvar opencode-question--labels nil
  "Vector of label strings for the options.")

(defvar opencode-question--descriptions nil
  "Vector of description strings for the options.")

(defvar opencode-question--values nil
  "Vector of option values (as sent back in the reply) for the options.")

(defvar opencode-question--multiple nil
  "Non-nil if the current field allows multiple selections.")

(defvar opencode-question--custom-enabled nil
  "Non-nil if the current field allows a custom typed answer.")

(defvar opencode-question--custom-text nil
  "Custom text entered by the user, or nil if none.")

(defvar opencode-question--question-text nil
  "The full question text currently being displayed.")

;;; Batch state (tracks the full field sequence and accumulated answers)

(defvar opencode-question--form-id nil
  "The form ID for replying or cancelling via the server API.")

(defvar opencode-question--session-id nil
  "The session ID owning the current form, for the server API.")

(defvar opencode-question--questions nil
  "The full vector of form field objects being asked.")

(defvar opencode-question--current-index 0
  "Index of the current field being displayed.")

(defvar opencode-question--answers nil
  "Accumulated alist of (KEY . VALUE) answers, one per answered field.")

(defvar opencode-question--handled nil
  "Non-nil when confirm or reject has been processed for the current transient.")

;;; Helpers

(defun opencode-question--format-option (idx)
  "Format option IDX as a checkbox string with label and description."
  (let ((marked (aref opencode-question--marked idx))
        (label (aref opencode-question--labels idx))
        (desc (aref opencode-question--descriptions idx)))
    (concat (if marked "[x]" "[ ]") " " label
            (when desc
              (concat "  " (propertize desc 'face 'shadow))))))

(defun opencode-question--toggle-option (idx)
  "Toggle option at IDX, respecting single/multi-select mode."
  (if opencode-question--multiple
      (aset opencode-question--marked idx
            (not (aref opencode-question--marked idx)))
    ;; Single-select: radio button
    (let ((cur (aref opencode-question--marked idx)))
      (dotimes (i (length opencode-question--marked))
        (aset opencode-question--marked i nil))
      (unless cur
        (aset opencode-question--marked idx t)
        (setq opencode-question--custom-text nil)))))

(defun opencode-question--index-to-key (i)
  "Convert numeric index I to a transient key string."
  (cond ((< i 9) (string (+ ?1 i)))
        ((< i 35) (string (+ ?a (- i 9))))
        (t (format "M-%d" (- i 35)))))

(defun opencode-question--collect-values ()
  "Collect currently selected option values and custom text into a list."
  (let ((selected (cl-loop for i below (length opencode-question--values)
                           when (aref opencode-question--marked i)
                           collect (aref opencode-question--values i))))
    (when opencode-question--custom-text
      (setq selected (append selected (list opencode-question--custom-text))))
    selected))

;;; Custom suffix class for checkbox options

(defclass opencode-question-option (transient-suffix)
  ((option-index :initarg :option-index :initform 0)
   (transient :initform t))
  "A togglable checkbox option suffix for opencode form fields.")

(cl-defmethod transient-format-description ((obj opencode-question-option))
  "Format OBJ as a checkbox line."
  (opencode-question--format-option (oref obj option-index)))

;;; Custom suffix class for typed answer

(defclass opencode-question-custom (transient-suffix)
  ((transient :initform t))
  "A custom text input option suffix for opencode form fields.")

(cl-defmethod transient-format-description ((_obj opencode-question-custom))
  "Format the custom answer line with current text."
  (concat (if opencode-question--custom-text "[x]" "[ ]")
          " Type your own answer"
          (when opencode-question--custom-text
            (concat "  "
                    (propertize opencode-question--custom-text
                                'face 'font-lock-string-face)))))

;;; Suffix commands

(defun opencode-question--close-on-multiple ()
  "Exit transient in single-select, stay in multi-select."
  (if opencode-question--multiple
      (transient--do-stay)
    (transient--do-exit)))

(transient-define-suffix opencode-question--do-toggle ()
  "Toggle the checkbox option associated with this suffix."
  :class 'opencode-question-option
  :transient #'opencode-question--close-on-multiple
  (interactive)
  (opencode-question--toggle-option
   (oref (transient-suffix-object) option-index))
  (unless opencode-question--multiple
    (call-interactively #'opencode-question--do-confirm)))

(transient-define-suffix opencode-question--do-custom ()
  "Type a custom answer."
  :class 'opencode-question-custom
  :transient #'opencode-question--close-on-multiple
  (interactive)
  (let ((text (read-string "Custom answer: "
                           opencode-question--custom-text)))
    (if (string-empty-p text)
        (setq opencode-question--custom-text nil)
      (setq opencode-question--custom-text text)
      (unless opencode-question--multiple
        ;; Single-select: clear all option marks
        (dotimes (i (length opencode-question--marked))
          (aset opencode-question--marked i nil))
        (call-interactively #'opencode-question--do-confirm)))))

(defun opencode-question--confirm-pre-command ()
  "Pre-command function for confirm: exit if selection is non-empty, else stay."
  (if (opencode-question--collect-values)
      transient--exit
    transient--stay))

(transient-define-suffix opencode-question--do-confirm ()
  "Confirm the current selection and reply to the server once all fields are answered."
  :transient #'opencode-question--confirm-pre-command
  (interactive)
  (let ((selected (opencode-question--collect-values)))
    (if (not selected)
        (message "No options selected")
      (setq opencode-question--handled t)
      (let ((key (alist-get 'key (aref opencode-question--questions
                                       opencode-question--current-index))))
        (push (cons key (if opencode-question--multiple selected (car selected)))
              opencode-question--answers))
      (if (< (1+ opencode-question--current-index)
             (length opencode-question--questions))
          ;; More fields remain; advance after transient fully exits.
          (progn
            (cl-incf opencode-question--current-index)
            (run-at-time 0 nil #'opencode-question--show-current))
        ;; All fields answered; send reply to server.
        (opencode-api-reply-form (opencode-question--session-id
                                   opencode-question--form-id)
            `((answer . ,(nreverse opencode-question--answers)))
            _result
          nil)))))

(transient-define-suffix opencode-question--do-reject ()
  "Cancel the form and notify the server."
  :transient nil
  (interactive)
  (setq opencode-question--handled t)
  (opencode-api-cancel-form (opencode-question--session-id
                              opencode-question--form-id)
      _result
    nil))

;;; Transient exit hook

(defun opencode-question--on-exit ()
  "Handle transient exit by cancelling the form unless confirmed or rejected."
  (remove-hook 'transient-exit-hook #'opencode-question--on-exit)
  (unless opencode-question--handled
    (opencode-api-cancel-form (opencode-question--session-id
                                opencode-question--form-id)
        _result
      nil)))

;;; Dynamic children setup

(defun opencode-question--setup-options (_children)
  "Generate checkbox suffix specs from current field state.
_CHILDREN is the statically-defined children, which are ignored."
  (transient-parse-suffixes
   'opencode-question--transient
   (append
    (cl-loop for i below (length opencode-question--labels)
             collect `(,(opencode-question--index-to-key i)
                       opencode-question--do-toggle
                       :option-index ,i))
    (when opencode-question--custom-enabled
      '(("!" opencode-question--do-custom))))))

;;; Transient prefix

(transient-define-prefix opencode-question--transient ()
  "Answer a form field from the opencode server."
  [:class transient-column
   :description (lambda () (or opencode-question--question-text "Question"))
   :setup-children opencode-question--setup-options]
  [["Actions"
    ("C-c C-c" "Confirm" opencode-question--do-confirm :if (lambda () opencode-question--multiple))
    ("q"       "Reject"  opencode-question--do-reject)]])

;;; Entry points

(defun opencode-question--show-current ()
  "Set up per-field state and display transient for the current form field."
  (let* ((field (aref opencode-question--questions
                      opencode-question--current-index))
         (options (seq-into (or (alist-get 'options field) []) 'vector))
         (n (length options))
         (title (alist-get 'title field))
         (desc (alist-get 'description field)))
    (setq opencode-question--labels (make-vector n nil)
          opencode-question--descriptions (make-vector n nil)
          opencode-question--values (make-vector n nil)
          opencode-question--marked (make-vector n nil)
          opencode-question--multiple (equal (alist-get 'type field) "multiselect")
          opencode-question--custom-enabled (or (= n 0)
                                                (not (opencode--json-falsy
                                                      (alist-get 'custom field))))
          opencode-question--custom-text nil
          opencode-question--question-text (if (and desc (not (string-empty-p desc)))
                                                (concat title "\n" desc)
                                              title)
          opencode-question--handled nil)
    (dotimes (i n)
      (let ((opt (aref options i)))
        (aset opencode-question--labels i (alist-get 'label opt))
        (aset opencode-question--descriptions i (alist-get 'description opt))
        (aset opencode-question--values i (alist-get 'value opt))))
    (add-hook 'transient-exit-hook #'opencode-question--on-exit)
    (call-interactively #'opencode-question--transient)))

(defun opencode-question--prompt (form-id session-id questions)
  "Prompt user to answer QUESTIONS from form FORM-ID on SESSION-ID.
QUESTIONS is a vector of Form.Field objects with `key', `title',
`description', `type', `options', and `custom' fields.  Each option has
`value', `label', and `description' fields.  Displays a transient popup
for each field.  On confirm, replies to the server.  On reject or
dismiss, cancels the form."
  (setq opencode-question--form-id form-id
        opencode-question--session-id session-id
        opencode-question--questions questions
        opencode-question--current-index 0
        opencode-question--answers nil
        opencode-question--handled nil)
  (opencode-question--show-current))

(defun opencode--prompt-questions (form-id questions)
  "Prompt user to answer QUESTIONS from form FORM-ID, then reply or cancel.
QUESTIONS is a vector of Form.Field objects, see `opencode-question--prompt'.
Must be called with the owning session buffer current, so SESSION-ID can
be read from the buffer-local `opencode-session-id'."
  (opencode-question--prompt form-id opencode-session-id questions))

(defun opencode--format-form-fields (fields)
  "Format FIELDS showing a ❓ prefix for each one."
  (mapconcat (lambda (field)
               (concat "❓ " (alist-get 'title field) "\n"))
             fields))

(defun opencode--output-questions (buffer fields)
  "Output FIELDS to BUFFER with a ❓ prefix for each one."
  (when (and (buffer-live-p buffer)
             (get-buffer-process buffer))
    (with-current-buffer buffer
      (opencode--maybe-insert-block-spacing)
      (opencode--output (opencode--format-form-fields fields))
      (opencode--output "\n"))))

(defun opencode--queue-questions (buffer form-id fields)
  "Queue FORM-ID with FIELDS in BUFFER and prompt if active.
`opencode--prompt-questions' reads the session ID from the buffer-local
`opencode-session-id', so BUFFER must be current when it is called; unlike
`opencode--selection-change-hook', this call site is not otherwise
guaranteed to run with BUFFER current."
  (opencode--output-questions buffer fields)
  (if (opencode--buffer-active-p buffer)
      (with-current-buffer buffer
        (opencode--prompt-questions form-id fields))
    (opencode--toast-show `((title . "OpenCode Questions")
                            (message . ,(alist-get 'title (aref fields 0)))
                            (variant . "info")))
    (with-current-buffer buffer
      (setq opencode-session-pending-questions
            (cons form-id fields)))))

(defun opencode--form-request (data)
  "Handle a form.created event DATA for a new form.
DATA is a V2 Form.Info alist (id, sessionID, title, metadata, fields),
possibly wrapped in a \"form\" key as the live event stream sends it.
Forms are handled generically regardless of metadata.kind.
Ensures the session buffer exists so pending forms are not dropped."
  (let ((form (or (alist-get 'form data) data)))
    (let-alist form
      (let ((fields (seq-into .fields 'vector)))
        (if-let (buffer (gethash .sessionID opencode-session-buffers))
            (opencode--queue-questions buffer .id fields)
          (opencode-api-session (.sessionID)
              session
            (push session opencode-alerted-sessions)
            (opencode-open-session session)
            (when-let (buffer (gethash .sessionID opencode-session-buffers))
              (opencode--queue-questions buffer .id fields))))))))

(defun opencode--form-settled (data)
  "Clear pending state for a form.replied/form.cancelled event DATA.
Clears a matching pending-but-inactive queue entry in the owning session
buffer.  A no-op when the form was answered through this client's own
transient, since that path never populates the pending queue."
  (let-alist data
    (when-let (buffer (gethash .sessionID opencode-session-buffers))
      (with-current-buffer buffer
        (when (equal (car-safe opencode-session-pending-questions) .id)
          (setq opencode-session-pending-questions nil))))))

(provide 'opencode-question)
;;; opencode-question.el ends here
