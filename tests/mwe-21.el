;; the MWE
(require 'package)
(package-initialize t)
(add-to-list 'load-path "~/.emacs.d/elpa/org-superstar-20200818.2257/")
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
(setq org-hide-leading-stars nil)
(setq org-superstar-leading-bullet ?\s)
(global-hl-line-mode 1)
(custom-set-faces
 '(hl-line ((t (:background "dark magenta" :foreground "white")))))

;; this fixes the issue
(setq org-indent-mode-turns-on-hiding-stars nil)
