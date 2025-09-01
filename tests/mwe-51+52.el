;;; A setup file for both Issue #51 and #52.

(add-hook 'org-mode-hook
          (lambda ()
            (org-superstar-mode 1)))

(setq org-superstar-remove-leading-stars t)

;; potential solution
;; (setq disable-point-adjustment t
;;       line-move-ignore-invisible t
;;       global-disable-point-adjustment t)
