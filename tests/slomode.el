;; this is a temporary file for testing slowdown issues.
;; edit the variable test-case to 1 or 2, then run:
;; emacs -Q -l slomode.el <your sample file>
;; start the test with ‘M-x slo-mode’.
(require 'org)

(defvar test-case 1
  "Check one of several plausible test cases.")

(if (= test-case 1)
    (setq how-kw 'prepend)
  (setq how-kw nil))

(setq inhibit-compacting-font-caches nil)

(defface slo-face '((default . nil)) "Sloface")

(defvar slo-keywords
  `(("^\\**\\(?1:\\*\\) "
     (1 'slo-face ,how-kw))))

(defun slo-fontify-buffer ()
  "Fontify the buffer."
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-ensure)
      (font-lock-flush))))

;;; Mode commands
;;;###autoload
(define-minor-mode slo-mode
  "Try and slow down the buffer."
  nil " Slo" nil
  (cond
   (slo-mode
    (font-lock-add-keywords nil slo-keywords
                            'append)
    (slo-fontify-buffer))
   (t
    (font-lock-remove-keywords nil slo-keywords)
    (slo-fontify-buffer))))
