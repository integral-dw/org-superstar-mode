;;; composure-test.el --- Track how org-superstar primitives modify the buffer.  -*- lexical-binding: t; -*-

;;; Commentary:

;; This file purposefully breaks naming conventions to indicate that
;; it is *NOT* part of the main package.

;; WARNING: This testing package is *not* suitable for any purpose
;; other than testing org-superstar-mode.  ONLY use this code on a
;; clean Emacs install ("emacs -Q") and ONLY keep the Emacs session
;; running for as long as you need to.  DO NOT use an Emacs session
;; loading this file for everyday editing.

;; THIS FILE ADVISES EMACS INTERNALS FOR DEBUGGING PURPOSES ONLY.
;; USE AT YOUR OWN RISK.

;;; Code:

(require 'cl-macs)

(defvar-local org-superstar/listen nil
  "If t, activate the advice ‘org-superstar/comp-test’.
The idea is to only let ‘org-superstar/comp-test’ listen in on
‘compose-region’ when another function is advised to let-bind
this variable.")

;;; Advice definitions

(defun org-superstar/comp-test (start end &optional components mod-func)
  "Advise ‘compose-region’ to log modifications made to buffer.
START, END, COMPONENTS and MOD-FUNC correspond to the arguments
of ‘compose-region’."
  (when org-superstar/listen
    (message "compose-region called: from %d to %d" start end)
    ;; I currently *do not* want to touch more than one character at a
    ;; time.  This test will only fail when I mess up regex grouping,
    ;; but it serves as a reminder that composing a region is not as
    ;; trivial as making the region bigger.
    (cl-assert (= 1 (- end start)))
    (cond
     ((stringp components)
      (message
       "composing ‘%s’ using string ‘%s’"
       (buffer-substring-no-properties start end)
       components))
     ((characterp components)
      (message
       "composing ‘%s’ using string ‘%s’"
       (buffer-substring-no-properties start end)
       components))
     (t
      (message
       "composing ‘%s’ using a sequence"
       (buffer-substring-no-properties start end))))))

(defun org-superstar/wrap-prettify (face-function &rest args)
  "Wrap FACE-FUNCTION and call with ARGS.
Ensure the return value is a face or nil.  Also toggle
‘compose-region’ calls to log behavior."
  (let ((org-superstar/listen t)
         (returned-face nil))
    (message "Entered function")
    (prog1 (setq returned-face (apply face-function args))
      (cl-assert (or (facep returned-face)
                     (null returned-face)))
      (when (facep returned-face)
        (message "Applied face ‘%s’ to group."
                 returned-face))
      (message "Exited" face-function))))



;;; Adding advice

;; listen in on compose-region
(advice-add 'compose-region :before #'org-superstar/comp-test)

;; advise prettifyers to snoop

(advice-add 'org-superstar--prettify-ibullets
            :around #'org-superstar/wrap-prettify)
(advice-add 'org-superstar--prettify-main-hbullet
            :around #'org-superstar/wrap-prettify)
(advice-add 'org-superstar--prettify-other-hbullet
            :around #'org-superstar/wrap-prettify)
(advice-add 'org-superstar--prettify-leading-hbullets
            :around #'org-superstar/wrap-prettify)
