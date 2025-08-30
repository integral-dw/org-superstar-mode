;;; MWE for #43
(add-hook 'org-mode-hook
          (lambda ()
            (org-superstar-mode 1)))

(setq inhibit-compacting-font-caches t)
