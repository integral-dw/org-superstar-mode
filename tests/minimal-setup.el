;;; A small init file setting you up for tests.

(add-hook 'org-mode-hook
          (lambda ()
            (org-superstar-mode 1)))
