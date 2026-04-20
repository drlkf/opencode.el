;;; opencode-permission.el --- Permission response UI for opencode  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Scott Zimmermann

;; Author: Scott Zimmermann <sczi@disroot.org>
;; Keywords: tools, llm, opencode

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

;; Provides a transient-based UI for responding to permission requests
;; from the opencode server.  Press o to accept once, a to accept
;; always (showing the patterns that will be permanently approved),
;; or n to deny.
;;
;; Activated when `opencode-transient-permission' is non-nil.
;; When nil, the fallback is a plain `x-popup-dialog'.
;;
;; Dismissing the transient without responding (e.g. q or C-g) leaves
;; the request pending.  Re-open it later with `opencode-respond-permission'.

;;; Code:

(require 'transient)

;;; State

(defvar opencode-permission--callback nil
  "Function to call with the user's choice string.
Called with one of \"once\", \"always\", or \"reject\".")

(defvar opencode-permission--type nil
  "The permission kind (e.g. \"bash\") for display in the transient header.")

(defvar opencode-permission--title nil
  "The permission title/description for display in the transient header.")

(defvar opencode-permission--always-patterns nil
  "The glob patterns approved by accepting always, for display in the transient.")

(defvar opencode-permission--handled nil
  "Non-nil when a response has been sent for the current transient.
Prevents double-handling if the transient exit hook fires after a suffix.")

;;; Suffix commands

(transient-define-suffix opencode-permission--do-accept ()
  "Accept the permission request (once)."
  :transient nil
  (interactive)
  (setq opencode-permission--handled t)
  (funcall opencode-permission--callback "once"))

(transient-define-suffix opencode-permission--do-accept-always ()
  "Accept the permission request permanently."
  :transient nil
  (interactive)
  (setq opencode-permission--handled t)
  (funcall opencode-permission--callback "always"))

(transient-define-suffix opencode-permission--do-deny ()
  "Deny the permission request."
  :transient nil
  (interactive)
  (setq opencode-permission--handled t)
  (funcall opencode-permission--callback "reject"))

;;; Transient exit hook

(defun opencode-permission--on-exit ()
  "Clean up the exit hook when the transient is dismissed.
Dismissing without responding leaves the permission pending so the
user can re-open it later with \\[opencode-respond-permission]."
  (remove-hook 'transient-exit-hook #'opencode-permission--on-exit))

;;; Transient prefix

(transient-define-prefix opencode-permission--transient ()
  "Respond to a permission request from the opencode server."
  [:description
   (lambda ()
     (format "%s: %s"
             (or opencode-permission--type "permission")
             (or opencode-permission--title "")))
   ("o" "Ok"            opencode-permission--do-accept)
   ("a" opencode-permission--do-accept-always
    :description (lambda () (format "Accept always (%s)"
                                    (or opencode-permission--always-patterns
                                        opencode-permission--title ""))))
   ("n" "No"            opencode-permission--do-deny)])

;;; Entry point

(defun opencode-permission--prompt (type title always-patterns callback)
  "Display a transient popup to respond to a permission request.
TYPE is the permission kind, TITLE is the description,
ALWAYS-PATTERNS are the glob patterns for permanent approval.
CALLBACK is called with one of \"once\", \"always\", or \"reject\"."
  (setq opencode-permission--type type
        opencode-permission--title title
        opencode-permission--always-patterns always-patterns
        opencode-permission--callback callback
        opencode-permission--handled nil)
  (add-hook 'transient-exit-hook #'opencode-permission--on-exit)
  (opencode-permission--transient))

(provide 'opencode-permission)
;;; opencode-permission.el ends here
