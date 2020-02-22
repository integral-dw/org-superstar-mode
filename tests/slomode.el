;; this is a temporary file for testing slowdown issues.
;; edit the variable test-case to 1,2 or 3, then run:
;; emacs -Q -l slomode-redux-1.el <your sample file>
;; start the test with ‘M-x slo-mode’.
(require 'org)

(defvar test-case 1
  "Check one of several plausible test cases.")

(setq inhibit-compacting-font-caches nil)

(cond ((= test-case 1)
       ;; same as 2 and 3 combined
       (defun slo-compose ()
         (compose-region (match-beginning 1) (match-end 1) ?◉)
         'slo-face))
      ((= test-case 2)
       (defun slo-compose ()
         'slo-face))
      ((= test-case 3)
       (defun slo-compose ()
         (compose-region (match-beginning 1) (match-end 1) ?◉)
         nil)))

(defface slo-face '((default . nil)) "Sloface")

(defvar slo-keywords
  '(("^\\**\\(?1:\\*\\) "
     (1 (slo-compose) prepend))))

(defun slo-fontify-buffer ()
  "Fontify the buffer."
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-ensure)
      (font-lock-flush))))

(defun un-slo ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
      (decompose-region (match-beginning 0) (match-end 0)))))


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
    (un-slo)
    (slo-fontify-buffer))))
