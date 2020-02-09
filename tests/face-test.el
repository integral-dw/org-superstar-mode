;;; face-test.el --- Visualize how org-superstar faces modify the buffer.  -*- lexical-binding: t; -*-

;;; Commentary:

;;; This file creates a simple "test image" of a buffer to help
;;; visualize the effects and limitations of common face attributes.

;;; Code:

(setq org-superstar-item-bullet-alist
  '((?* . ?⌬)
    (?+ . ?◐)
    (?- . ?⏭)))

(setq org-superstar-leading-bullet ?⁂)

;; Mess with character heights (nicked from LaTeX)

(set-face-attribute
 'default nil
 :height 150)

(set-face-attribute
 'org-document-title nil
 :height 2.074 ;; \huge
 :foreground 'unspecified
 :inherit 'org-level-8)


(set-face-attribute
 'org-level-1 nil
 :height 1.728) ;; \LARGE

(set-face-attribute
 'org-level-2 nil
 :height 1.44) ;; \Large

(set-face-attribute
 'org-level-3 nil
 :height 1.2) ;; \large

(set-face-attribute
 'org-level-4 nil
 :height 1.0
 :foreground "red")

;; Only use the first 4 styles and do not cycle.
(setq org-cycle-level-faces nil)
(setq org-n-level-faces 4)

(set-face-attribute
 'org-superstar-leading nil
 :height 1.2
 :weight 'light
 :slant 'italic
 :inverse-video t
 :box '(:line-width 2 :color "magenta")
 :inherit 'unspecified)

(set-face-attribute
 'org-superstar-header-bullet nil
 :height 1.2
 :background "cyan"
 :weight 'ultra-bold)

(set-face-attribute
 'org-superstar-item nil
 :foreground "lawn green"
 :background "magenta"
 :box t
 :height 1.2)
