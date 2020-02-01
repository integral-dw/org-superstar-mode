;;; org-superstar.el --- Prettify headings and plain lists in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams, sabof

;; Author: D. Williams <d.williams@posteo.net>
;; Maintainer: D. Williams <d.williams@posteo.net>
;; Keywords: faces, outlines
;; Version: 0
;; Homepage: https://github.com/dw-github-mirror/org-superstar-mode
;; Package-Requires: ((org "9.1.9") (emacs "26"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is heavily influenced by (and uses snippets from) the
;; popular package "org-bullets", created by sabof.  It was made with
;; the goal of inheriting features the author liked about org-bullets
;; while being able to introduce compatibility-breaking changes to it.
;; It is largely rewritten, to the point of almost no function being
;; identical to it's org-bullets counterpart.

;; *IMPORTANT*: This package is under construction.  I am still
;; working out the kinks.  Any function and variable defined here is
;; subject to change until version 1.0.0, from which point on I will
;; try my best not to inconvenience people.

;; Here are some Unicode blocks which are generally nifty resources
;; for this package:
;;
;; General Punctuation (U+2000-U+206F): bullets, leaders, asterisms
;; Dingbats (U+2700-U+27BF)
;; Miscellaneous Symbols and Arrows (U+2B00-U+2BFF)

;;; Code:

(require 'org)

(defgroup org-superstar nil
  "Use UTF8 bullets for headlines and plain lists."
  :group 'org-appearance)

;;; Bullet Variables

(defcustom org-sstar-headline-bullets-list
  '(;; Original ones nicked from org-bullets
    "◉"
    "○"
    "✸"
    "✿")
    "List of bullets used in Org headings.
It can contain any number of bullets, the Nth entry usually
corresponding to the bullet used for level N.  The way this list
is cycled through can use fine-tuned by customizing
‘org-sstar-cycle-headline-bullets’.

You should re-enable ‘\\[org-superstar-mode]’ after changing this
variable for your changes to take effect."
    :group 'org-superstar
    :type '(repeat (string :tag "Bullet character")))

(defcustom org-sstar-item-bullet-alist
  '((?* . ?•)
    (?+ . ?➤)
    (?- . ?–))
  "Alist of UTF-8 bullets to be used for plain org lists.
Each key should be a plain list bullet character (*,+,-), and
each value should be the UTF8 character to be displayed.

You should re-enable ‘\\[org-superstar-mode]’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(alist :options ((?* (character))
                          (?+ (character))
                          (?- (character)))))

;;;###autoload
(put 'org-sstar-leading-bullet
     'safe-local-variable
     #'char-or-string-p)

(defcustom org-sstar-leading-bullet " ․"
  "A special bullet used for leading stars.
Normally, this variable is a character replacing the default
stars.  If it’s a string, list, or vector, compose the
replacement according to the rules of ‘compose-region’ for the
COMPONENTS argument.

If ‘org-hide-leading-stars’ is nil, leading stars in a headline
are represented as a sequence of this bullet using the face
‘org-sstar-leading’.  Otherwise, this variable has no effect and
‘org-mode’ covers leading stars using ‘org-hide’.

You should re-enable ‘\\[org-superstar-mode]’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(choice
          (character :tag "Single character to display"
                     :format "\n%t: %v\n"
                     :value ?‥)
          (string :tag "String of characters to compose replacement from"
                  :format "\n%t:\n%v"
                  :value " ․")
          (vector :tag "Vector of chars and composition rules"
           (repeat
            :inline t
            :tag "Composition sequence"
            (list :inline t :tag "Composition pair"
                  (character :tag "alt char" :value ?\s)
                  (sexp :tag "rule"))))
          (repeat
           :tag "Sequence of chars and composition rules"
           (list :inline t :tag "Composition pair"
                 (character :tag "alt char" :value ?\s)
                 (sexp :tag "rule"))))
  :risky t)


;;; Other Custom Variables

(defcustom org-sstar-prettify-leading-stars t
  "Non-nil means prettify leading stars in headlines.

It is a good idea to disable this feature when you run into any
peformance issues because of this package.  You can still hide
leading stars using ‘org-hide-leading-stars’.

You should re-enable ‘\\[org-superstar-mode]’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(choice
          (const :tag "Prettify leading stars." t)
          (const :tag "Don’t prettify leading stars." nil)))

(defcustom org-sstar-cycle-headline-bullets t
  "Non-nil means cycle through all available headline bullets.

The following values are meaningful:

An integer value of N cycles through the first N entries of the
list instead of the whole list.

If otherwise non-nil, cycle through the entirety of the list.
This is the default behavior inherited from org-bullets.

If nil, repeat the final list entry for all successive levels.

You should re-enable ‘\\[org-superstar-mode]’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(choice
          (const :tag "Cycle through the whole list." t)
          (const :tag "Repeat the last element indefinitely." nil)
          (integer :tag "Repeat the first <integer> elements only."
                   :format "Repeat the first %v entries exclusively.\n"
                   :size 8
                   :value 1
                   :validate org-sstar--validate-hcycle)))

(defun org-sstar--validate-hcycle (text-field)
  "Validate integer values of ‘org-sstar-cycle-headline-bullets’ in TEXT-FIELD.
If the integer exceeds the length of ‘org-sstar-headline-bullets-list’,
set it to the length and raise an error."
  (let ((ncycle (widget-value text-field))
        (maxcycle (org-sstar--hbullets)))
    (unless (<= 1 ncycle maxcycle)
      (widget-put
       text-field
       :error (format "Value must be between 1 and %i"
                      maxcycle))
      (widget-value-set text-field maxcycle)
      text-field)))

(defcustom org-sstar-prettify-item-bullets t
  "Non-nil means display plain lists bullets as UTF8 bullets.

Each type of plain list bullet is associated with a
corresponding UTF8 character in ‘org-sstar-item-bullet-alist’.

You should re-enable ‘\\[org-superstar-mode]’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(choice (const :tag "Enable item bullet fontification" t)
                 (const :tag "Disable item bullet fontification" nil)))


;;; Faces

(defface org-sstar-leading
  '((default . (:inherit default :foreground "gray")))
  "Face used to display prettified leading stars in a headline."
  :group 'org-superstar)

(defface org-sstar-header-bullet
  '((default . nil))
  "Face containing distinguishing features headline bullets.
This face is applied to header bullets \"on top of\" existing
fontification provided by org, allowing you to inherit the
default look of a heading line while still being able to make
modifications.  Every specified face property will replace those
currently in place.  Consequently, leaving all face properties
unspecified inherits the org-level-X faces for header bullets."
  :group 'org-superstar)

(defface org-sstar-item
  '((default . (:inherit default)))
  "Face used to display prettified item bullets."
  :group 'org-superstar)

;;; Functions

(defun org-sstar-configure-like-org-bullets ()
  "Configure ‘\\[org-superstar-mode]’ to approximate ‘\\[org-bullets-mode]’.
This function automatically sets various custom variables, and
therefore should only be called *once* per session, before any
other manual customization of this package.

Warning: This function sets a variable outside of this package:
‘org-hide-leading-stars’.

This function is only meant as a small convenience for people who
just want minor depatures from ‘\\[org-bullets-mode]’.  For a more
fine-grained customization, it’s better to just set the variables
you want.

This changes the following variables:
‘org-sstar-prettify-leading-stars’: Disabled.
‘org-sstar-prettify-leading-stars’: Disabled.
‘org-sstar-cycle-headline-bullets’: Enabled.
‘org-hide-leading-stars’: Enabled.

You should re-enable ‘\\[org-superstar-mode]’ after calling this
function for your changes to take effect."
  (setq org-sstar-prettify-leading-stars nil)
  (setq org-sstar-cycle-headline-bullets t)
  (setq org-hide-leading-stars t)
  nil)

;;; Accessor Functions

(defun org-sstar--hbullets ()
    "Return the length of ‘org-sstar-headline-bullets-list’."
  (length org-sstar-headline-bullets-list))

(defun org-sstar--hbullet (level)
  "Return the desired headline bullet replacement for LEVEL N.

See also ‘org-sstar-cycle-headline-bullets’."
  (let ((max-bullets org-sstar-cycle-headline-bullets)
        (n (1- level)))
    (string-to-char
     (cond ((integerp max-bullets)
            (elt org-sstar-headline-bullets-list
                 (% n max-bullets)))
           (max-bullets
            (elt org-sstar-headline-bullets-list
                 (% n (org-sstar--hbullets))))
           (t
            (elt org-sstar-headline-bullets-list
                 (min n (1- (org-sstar--hbullets)))))))))

(defun org-sstar--ibullet (bullet-string)
  "Return BULLET-STRINGs desired UTF-8 replacement.

Each of the three regular plain list bullets +, - and * will be
replaced by their corresponding entry in ‘org-sstar-item-bullet-alist’."
  (or (cdr (assq (string-to-char bullet-string)
                 org-sstar-item-bullet-alist))
      (string-to-char bullet-string)))

;;; Fontification

(defun org-sstar--prettify-ibullets ()
  "Prettify plain list bullets.

This function uses ‘org-list-in-valid-context-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-list-in-valid-context-p)
    (let* ((current-bullet (match-string 1)))
      (compose-region (match-beginning 1)
                      (match-end 1)
                      (org-sstar--ibullet current-bullet)))

    'org-sstar-item))

(defun org-sstar--unprettify-ibullets ()
  "Revert all changes made to item bullets."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]+\\([-+*]\\) " nil t)
        (decompose-region (match-beginning 1) (match-end 1)))))


(defun org-sstar--prettify-main-hbullet ()
  "Prettify the trailing star in a headline.

This function uses ‘org-list-in-valid-context-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-list-in-valid-context-p)
    (let ((level (- (match-end 0) (match-beginning 0) 1)))
      (compose-region (match-beginning 1) (match-end 1)
                      (org-sstar--hbullet level))))
  'org-sstar-header-bullet)

(defun org-sstar--prettify-other-hbullet ()
  "Prettify the second last star in a headline.
This is only done if the particular title’s level is part of an
inline task, see ‘org-inlinetask-min-level’.  Otherwise, this
block is formatted like the leading asterisks, see
‘org-sstar--prettify-leading-hbullets’.

This function uses ‘org-list-in-valid-context-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-list-in-valid-context-p)
    (let* ((level (- (match-end 0) (match-beginning 0) 1))
           (is-inline-task
            (and (boundp 'org-inlinetask-min-level)
                 (>= level org-inlinetask-min-level)))
           (compose-star (or is-inline-task
                             (and (not org-hide-leading-stars)
                                  org-sstar-prettify-leading-stars)))
           (bullet-char (if is-inline-task
                            (org-sstar--hbullet level)
                            org-sstar-leading-bullet)))
      (when compose-star
        (compose-region (match-beginning 2) (match-end 2)
                        bullet-char))
      (cond (is-inline-task 'org-sstar-header-bullet)
            (org-sstar-prettify-leading-stars 'org-sstar-leading)
            (t 'custom-invalid)))))


(defun org-sstar--prettify-leading-hbullets ()
  "Prettify the leading bullets of a header line.
Unless ‘org-hide-leading-stars’ is non-nil, each leading star is
visually replaced by ‘org-sstar-leading-bullet-char’ and inherits
face properties from ‘org-sstar-leading’.

This function uses ‘org-list-in-valid-context-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-list-in-valid-context-p)
    (unless org-hide-leading-stars
      (let ((star-beg (match-beginning 3))
            (lead-end (match-end 3)))
        (while (< star-beg lead-end)
          (compose-region star-beg (setq star-beg (1+ star-beg))
                          org-sstar-leading-bullet))
        'org-sstar-leading))))



(defun org-sstar--unprettify-hbullets ()
  "Revert all changes made to header bullets."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
        (decompose-region (match-beginning 0) (match-end 0)))))

;;; Font Lock

(defvar-local org-sstar--font-lock-keywords nil)

(defun org-sstar--update-font-lock-keywords ()
  "Set ‘org-sstar--font-lock-keywords’ to reflect current settings.
You should not call this function to avoid confusing the cleanup
routines of ‘\\[org-superstar-mode]’."
  ;; The below regex is nicked from ‘org-list-full-item-re’, but
  ;; reduced to only match simple lists.  Replaced [ \t]* by [ \t]+ to
  ;; avoid confusion with title bullets.
  (setq org-sstar--font-lock-keywords
        `(,@(when org-sstar-prettify-item-bullets
              '(("^[ \t]+\\([-+*]\\) "
                 (1 (org-sstar--prettify-ibullets)))))
          ("^\\(?3:\\**?\\)\\(?2:\\*?\\)\\(?1:\\*\\) "
           (1 (org-sstar--prettify-main-hbullet) prepend)
           ,@(when org-sstar-prettify-leading-stars
               '((3 (org-sstar--prettify-leading-hbullets)
                    t)))
           (2 (org-sstar--prettify-other-hbullet) prepend))
          ;; If requested, put another function here that formats the
          ;; first two stars of an inline as a bullet.
          )))

(defun org-sstar--fontify-buffer ()
  "Fontify the buffer."
  (when font-lock-mode
    (if (and (fboundp 'font-lock-flush)
             (fboundp 'font-lock-ensure))
        (save-restriction
          (widen)
          (font-lock-ensure)
          (font-lock-flush)))))

;;;###autoload
(define-minor-mode org-superstar-mode
  "Use UTF8 bullets for headlines and plain lists."
  nil " ✨" nil
  :group 'org-superstar
  :require 'org
  (cond
   (org-superstar-mode
    (org-sstar--update-font-lock-keywords)
    (font-lock-add-keywords nil org-sstar--font-lock-keywords
                            'append)
    (org-sstar--fontify-buffer))
   (t
    (font-lock-remove-keywords nil org-sstar--font-lock-keywords)
    (org-sstar--unprettify-ibullets)
    (org-sstar--unprettify-hbullets)
    (org-sstar--fontify-buffer))))

(provide 'org-superstar)
;;; org-superstar.el ends here
