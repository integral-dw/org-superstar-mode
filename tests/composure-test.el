;;; composure-test.el --- Track how org-superstar primitives modify the buffer.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file purposefully breaks naming conventions to indicate that
;; it is *NOT* part of the main package.

;; Load this file with a minimal setup to confirm that composition
;; works as intended.  You can dismiss all advices applied by calling
;; ‘org-superstar/dismiss-composure’.

;;; Code:

(require 'cl-macs)
(require 'subr-x)

(defvar-local org-superstar/listen nil
  "If t, activate the advice ‘org-superstar/comp-test’.
The idea is to only let ‘org-superstar/comp-test’ listen in on
‘compose-region’ when another function is advised to let-bind
this variable.  You can control which functions are currently
active listeners by calling ‘org-superstar/toggle-listener’.")


(defconst org-superstar/comp-listeners
  '(org-superstar--prettify-ibullets
    org-superstar--prettify-main-hbullet
    org-superstar--prettify-other-hbullet
    org-superstar--prettify-other-lbullet
    org-superstar--prettify-leading-hbullets)
  "List of functions ‘org-superstar/comp-test’ can be applied to.")

;;; Advice definitions

(defun org-superstar/comp-test (start end &optional components mod-func)
  "Advise ‘compose-region’ to log modifications made to buffer.
START, END, COMPONENTS and MOD-FUNC correspond to the arguments
of ‘compose-region’."
  (when org-superstar/listen
    ;; I currently *do not* want to touch more than one character at a
    ;; time.  This test will only fail when I mess up regex grouping,
    ;; but it serves as a reminder that composing a region is not as
    ;; trivial as making the region bigger.
    (should (= 1 (- end start)))
    (let ((line (line-number-at-pos start))
          (col (save-excursion (goto-char start) (current-column)))
          (composed-string (buffer-substring-no-properties start end)))
    (cond
     ((stringp components)
      (message
       "line %s, column %s: composing ‘%s’ using string ‘%s’"
       line col composed-string
       components))
     ((characterp components)
      (message
       "line %s, column %s: composing ‘%s’ using character ‘%c’"
       line col composed-string
       components))
     (t
      (message
       "composing ‘%s’ using a sequence"
       (buffer-substring-no-properties start end)))))))

(defun org-superstar/wrap-prettify (face-function &rest args)
  "Wrap FACE-FUNCTION and call with ARGS.
Ensure the return value is a face or nil.  Also toggle
‘compose-region’ calls to log behavior."
  (let ((org-superstar/listen t)
         (returned-face nil))
    (prog1 (setq returned-face (apply face-function args))
      (should (or (facep returned-face)
                     (null returned-face)))
      (when (facep returned-face)
        (message "Applied face ‘%s’ to group (line %d)"
                 returned-face
                 (line-number-at-pos (match-beginning 0)))))))


;;; Helper functions
(defun org-superstar/read-listener ()
  "Return an argument list for ‘org-superstar/toggle-silence’."
  (let ((answer (completing-read
                 "Toggle silence for: "
                 org-superstar/comp-listeners nil t)))
    (unless (string-empty-p answer)
      (list (read answer)))))

;;; Adding and removing advice

(defun org-superstar/toggle-listener (&optional symbol)
  "Toggle listening to ‘compose-region’ for listener SYMBOL."
  (interactive (org-superstar/read-listener))
  (when symbol
    (let ((is-adviced
           (advice-member-p #'org-superstar/wrap-prettify symbol)))
      (cond (is-adviced
             (message "‘%s’ listening: OFF" symbol)
             (advice-remove symbol #'org-superstar/wrap-prettify))
            (t
             (message "‘%s’ listening: ON" symbol)
             (advice-add symbol
                         :around #'org-superstar/wrap-prettify))))))


(defun org-superstar/start-listening ()
  "Set up all advices provided by composure-test."
  (interactive)
  (advice-add 'compose-region :before #'org-superstar/comp-test)
  (dolist (symbol org-superstar/comp-listeners)
    (org-superstar/toggle-listener symbol)))


(defun org-superstar/dismiss-listening ()
  "Remove all advices added by composure-test."
  (interactive)
  (advice-remove 'compose-region #'org-superstar/comp-test)
  (dolist (symbol org-superstar/comp-listeners)
    (advice-remove symbol #'org-superstar/wrap-prettify)))
