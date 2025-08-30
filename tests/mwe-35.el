;;; MWE for #35
(add-hook 'org-mode-hook
          (lambda ()
            (org-superstar-mode 1)))

(set-face-attribute 'org-superstar-item nil
                    :background "red")
