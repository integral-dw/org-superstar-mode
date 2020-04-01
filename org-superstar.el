;;; org-superstar.el --- Prettify headings and plain lists in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020  D. Williams, sabof

;; Author: D. Williams <d.williams@posteo.net>
;; Maintainer: D. Williams <d.williams@posteo.net>
;; Keywords: faces, outlines
;; Version: 1.1.0
;; Homepage: https://github.com/integral-dw/org-superstar-mode
;; Package-Requires: ((org "9.1.9") (emacs "26.1"))

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

;;  Prettify headings and plain lists in org-mode.  This package is a
;;  direct descendant of ‘org-bullets’, with most of the code base
;;  completely rewritten (See https://github.com/sabof/org-bullets).
;;  Currently, this package supports:

;; * Prettifying org heading lines by:
;;   + replacing trailing bullets by UTF-8 bullets
;;   + hiding leading stars, customizing their look or removing them
;;     from vision
;;   + applying a custom face to the header bullet
;;   + applying a custom face to the leading bullets
;;   + using double-bullets for inline tasks (see org-inlinetask.el)
;;   + (optional) using special bullets for TODO keywords
;; * Prettifying org plain list bullets by:
;;   + replacing each bullet type (*, + and -) with UTF-8 bullets
;;   + applying a custom face to item bullets
;; * Gracefully degrading features when viewed from terminal

;; This package is heavily influenced by (and uses snippets from) the
;; popular package "org-bullets", created by sabof.  It was made with
;; the goal of inheriting features the author liked about org-bullets
;; while being able to introduce compatibility-breaking changes to it.
;; It is largely rewritten, to the point of almost no function being
;; identical to it's org-bullets counterpart.

;; This package is versioned using (the author's understanding of)
;; semantic versioning: "<major>.<minor>.<patch>".
;; <major> version increments signify backward incompatible changes.
;; <minor> version increments signify backward compatible but
;;         significant changes.
;; <patch> version increments signify changes not affecting the API.

;; Here are some Unicode blocks which are generally nifty resources
;; for this package:
;;
;; General Punctuation (U+2000-U+206F): Bullets, leaders, asterisms.
;; Dingbats (U+2700-U+27BF)
;; Miscellaneous Symbols and Arrows (U+2B00-U+2BFF):
;;     Further stars and arrowheads.
;; Miscellaneous Symbols (U+2600–U+26FF): Smileys and card suits.
;; Supplemental Arrows-C (U+1F800-U+1F8FF)
;; Geometric Shapes (U+25A0-U+25FF): Circles, shapes within shapes.
;; Geometric Shapes Extended (U+1F780-U+1F7FF):
;;     More of the above, and stars.
;;

;;; Code:

(require 'org)
(require 'org-element)
(require 'wid-edit)

(defgroup org-superstar nil
  "Use UTF8 bullets for headlines and plain lists."
  :group 'org-appearance)

;;; Bullet Variables

(defcustom org-superstar-headline-bullets-list
  '(;; Original ones nicked from org-bullets
    "◉"
    "○"
    "✸"
    "✿") ;; "◉" "🞛" "○" "▷"
  "List of bullets used in Org headings.
It can contain any number of bullets, the Nth entry usually
corresponding to the bullet used for level N.  The way this list
is cycled through can use fine-tuned by customizing
‘org-superstar-cycle-headline-bullets’.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(repeat (string :tag "Bullet character")))

(defcustom org-superstar-item-bullet-alist
  '((?* . ?•)
    (?+ . ?➤)
    (?- . ?–))
  "Alist of UTF-8 bullets to be used for plain org lists.
Each key should be a plain list bullet character (*,+,-), and
each value should be the UTF8 character to be displayed.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(alist :options ((?* (character))
                          (?+ (character))
                          (?- (character)))))

(defcustom org-superstar-todo-bullet-alist
  '(("TODO" . ?☐)
    ("DONE" . ?☑))
  "Alist of UTF-8 bullets for TODO items.

Each key should be a TODO keyword, and each value should the UTF8
character to be displayed.  Keywords that are not included in the
alist are handled like normal headings.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(alist :key-type (string :format "TODO keyword: %v")
                :value-type (character :value ?◉
                                       :format "Bullet character: %v\n")))

;;;###autoload
(put 'org-superstar-leading-bullet
     'safe-local-variable
     #'char-or-string-p)

(defun org-superstar--set-lbullet (symbol value)
  "Set SYMBOL ‘org-superstar-leading-bullet’ to VALUE.
If set to a character, also set ‘org-superstar-leading-fallback’."
  (set-default symbol value)
  (when (characterp value)
    (set-default 'org-superstar-leading-fallback value)))

(defcustom org-superstar-leading-bullet " ․"
  "A special bullet used for leading stars.
Normally, this variable is a character replacing the default
stars.  If it’s a string, list, or vector, compose the
replacement according to the rules of ‘compose-region’ for the
COMPONENTS argument.

If ‘org-hide-leading-stars’ is nil, leading stars in a headline
are represented as a sequence of this bullet using the face
‘org-superstar-leading’.  Otherwise, this variable has no effect and
‘org-mode’ covers leading stars using ‘org-hide’.

This variable is only used for graphical displays.
‘org-superstar-leading-fallback’ is used for terminal displays
instead.

You should call ‘org-superstar-restart’ after changing this
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
  :risky t
  :set #'org-superstar--set-lbullet)

(defcustom org-superstar-leading-fallback
  (cond ((characterp org-superstar-leading-bullet)
         org-superstar-leading-bullet)
        (t ?‥))
  "A special bullet used for leading stars.
This variable is a character replacing the default stars in
terminal displays instead of ‘org-superstar-leading-bullet’.

If the leading bullet is set to a character before the package is
loaded, this variable’s default value is set to that character as
well.  Setting the leading bullet to a character using the custom
interface also automatically sets this variable.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(character :tag "Single character to display"
                    :format "\n%t: %v\n"
                    :value ?‥))


;;; Other Custom Variables

(defcustom org-superstar-cycle-headline-bullets t
  "Non-nil means cycle through all available headline bullets.

The following values are meaningful:

An integer value of N cycles through the first N entries of the
list instead of the whole list.

If otherwise non-nil, cycle through the entirety of the list.
This is the default behavior inherited from org-bullets.

If nil, repeat the final list entry for all successive levels.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(choice
          (const :tag "Cycle through the whole list." t)
          (const :tag "Repeat the last element indefinitely." nil)
          (integer :tag "Repeat the first <integer> elements only."
                   :format "Repeat the first %v entries exclusively.\n"
                   :size 8
                   :value 1
                   :validate org-superstar--validate-hcycle)))

(defun org-superstar--validate-hcycle (text-field)
  "Raise an error if TEXT-FIELD’s value is an invalid hbullet number.
This function is used for ‘org-superstar-cycle-headline-bullets’.
If the integer exceeds the length of
‘org-superstar-headline-bullets-list’, set it to the length and
raise an error."
  (let ((ncycle (widget-value text-field))
        (maxcycle (org-superstar--hbullets-length)))
    (unless (<= 1 ncycle maxcycle)
      (widget-put
       text-field
       :error (format "Value must be between 1 and %i"
                      maxcycle))
      (widget-value-set text-field maxcycle)
      text-field)))

(defcustom org-superstar-prettify-item-bullets t
  "Non-nil means display plain lists bullets as UTF8 bullets.

Each type of plain list bullet is associated with a
corresponding UTF8 character in ‘org-superstar-item-bullet-alist’.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(choice (const :tag "Enable item bullet fontification" t)
                 (const :tag "Disable item bullet fontification" nil)))

(defcustom org-superstar-special-todo-items nil
  "Non-nil means use special bullets for TODO items.

Instead of displaying bullets corresponding to TODO items
according to ‘org-superstar-headline-bullets-list’ (dependent on
the headline’s level), display a bullet according to
‘org-superstar-todo-bullet-alist’ (dependent on the TODO
keyword)."
  :group 'org-superstar
  :type 'boolean)

(defvar-local org-superstar-lightweight-lists nil
  "Non-nil means circumvent expensive calls to ‘org-superstar-plain-list-p’.

There is usually no need to use this variable directly; instead,
use the command ‘org-superstar-toggle-lightweight-lists’.")


;;; Faces

(defface org-superstar-leading
  '((default . (:inherit default :foreground "gray")))
  "Face used to display prettified leading stars in a headline."
  :group 'org-superstar)
;; REVIEW: I read that it's generally discouraged to :inherit while
;; overriding certain properties.  Does that also apply to inheriting
;; default?

(defface org-superstar-header-bullet
  '((default . nil))
  "Face containing distinguishing features headline bullets.
This face is applied to header bullets \"on top of\" existing
fontification provided by org, allowing you to inherit the
default look of a heading line while still being able to make
modifications.  Every specified face property will replace those
currently in place.  Consequently, leaving all face properties
unspecified inherits the org-level-X faces for header bullets."
  :group 'org-superstar)

(defface org-superstar-item
  '((default . (:inherit default)))
  "Face used to display prettified item bullets."
  :group 'org-superstar)

(defcustom org-superstar-remove-leading-stars nil
  "Non-nil means font-lock should hide leading star characters.

A more radical version of ‘org-hide-leading-stars’, where the
indentation caused by leading stars is completely removed.  It
works similar to ‘org-hide-emphasis-markers’.

If Non-nil, this variable takes precedence over
‘org-hide-leading-stars’.

This variable only eliminates indentation caused directly by
leading stars, meaning additional indentation should be
preserved.  For an example of this, see the minor-mode command
‘org-indent-mode’.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type 'boolean)


;;; Functions intended for users

(defun org-superstar-configure-like-org-bullets ()
  "Configure Superstar mode to approximate ‘org-bullets-mode’.
This function automatically sets various custom variables, and
therefore should only be called *once* per session, before any
other manual customization of this package.

Warning: This function sets a variable outside of this package:
‘org-hide-leading-stars’.

This function is only meant as a small convenience for people who
just want minor departures from ‘org-bullets-mode’.  For a more
fine-grained customization, it’s better to just set the variables
you want.

This changes the following variables:
‘org-superstar-cycle-headline-bullets’: Enabled.
‘org-hide-leading-stars’: Enabled.
‘org-superstar-special-todo-items’: Disabled.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  (setq org-superstar-cycle-headline-bullets t)
  (setq org-hide-leading-stars t)
  (setq org-superstar-special-todo-items nil)
  nil)

;;;###autoload
(defun org-superstar-toggle-lightweight-lists ()
  "Toggle syntax checking for plain list items.

Disabling syntax checking will cause Org Superstar to display
lines looking like plain lists (for example in code) like plain
lists.  However, this may cause significant speedup for org files
containing several hundred list items."
  (interactive)
  (setq org-superstar-lightweight-lists
        (not org-superstar-lightweight-lists)))


;;; Accessor Functions

(defun org-superstar--get-todo (pom)
  "Return the TODO keyword at point or marker POM.
If no TODO property is found, return nil."
  (save-match-data
    (let ((todo-property
           (cdar (org-entry-properties pom "TODO"))))
      (when (stringp todo-property)
        todo-property))))

(defun org-superstar--todo-bullet ()
  "Return the desired TODO item bullet, if defined.
If no entry can be found in ‘org-superstar-todo-bullet-alist’ for
the current keyword, return nil."
  (cdr (assoc-string
        (org-superstar--get-todo (match-beginning 0))
        org-superstar-todo-bullet-alist)))

(defun org-superstar--hbullets-length ()
  "Return the length of ‘org-superstar-headline-bullets-list’."
  (length org-superstar-headline-bullets-list))

(defun org-superstar--hbullet (level)
  "Return the desired headline bullet replacement for LEVEL N.

If the headline is also a TODO item, you can override the usually
displayed bullet depending on the TODO keyword by setting
‘org-superstar-special-todo-items’ to t and adding relevant
TODO keyword entries to ‘org-superstar-todo-bullet-alist’.

See also ‘org-superstar-cycle-headline-bullets’."
  (let ((max-bullets org-superstar-cycle-headline-bullets)
        (n (if org-odd-levels-only (/ (1- level) 2) (1- level)))
        (todo-bullet (when org-superstar-special-todo-items
                       (org-superstar--todo-bullet))))
    (cond (todo-bullet)
          ((integerp max-bullets)
           (string-to-char
            (elt org-superstar-headline-bullets-list
                 (% n max-bullets))))
          (max-bullets
           (string-to-char
            (elt org-superstar-headline-bullets-list
                 (% n (org-superstar--hbullets-length)))))
          (t
           (string-to-char
            (elt org-superstar-headline-bullets-list
                 (min n (1- (org-superstar--hbullets-length)))))))))

(defun org-superstar--ibullet (bullet-string)
  "Return BULLET-STRINGs desired UTF-8 replacement.

Each of the three regular plain list bullets +, - and * will be
replaced by their corresponding entry in ‘org-superstar-item-bullet-alist’."
  (or (cdr (assq (string-to-char bullet-string)
                 org-superstar-item-bullet-alist))
      (string-to-char bullet-string)))

(defun org-superstar--lbullet ()
  "Return the correct leading bullet for the current display."
  (if (org-superstar-graphic-p)
      org-superstar-leading-bullet
    org-superstar-leading-fallback))

(defun org-superstar--heading-level ()
  "Return the heading level of the currently matched headline."
  (- (match-end 0) (match-beginning 0) 1))


;;; Predicates
;; ‘org-list-in-valid-context-p’ is currently not working.

;; Explicitly returning t is redundant, but does not leak information
;; about how the predicate is implemented.
(defun org-superstar-plain-list-p ()
  "Return t if the current match is a proper plain list.

This function may be expensive for files with very large plain
lists; consider using ‘org-superstar-toggle-lightweight-lists’ in
such cases to avoid slowdown."
  (or org-superstar-lightweight-lists
      (save-match-data
        (org-element-lineage (org-element-at-point)
                             '(plain-list) t))
      t))

(defun org-superstar-headline-or-inlinetask-p ()
  "Return t if the current match is a proper headline or inlinetask."
  (save-match-data
    (and (org-at-heading-p) t)))

(defun org-superstar-headline-p ()
  "Return t if the current match is a proper headline."
  (interactive)
  (save-match-data
    (org-with-limited-levels
     (and (org-at-heading-p) t))))

(defun org-superstar-inlinetask-p ()
  "Return t if the current match is a proper inlinetask."
  (and (featurep 'org-inlinetask)
       (org-superstar-headline-or-inlinetask-p)
       (not (org-superstar-headline-p))))

(defun org-superstar-graphic-p ()
  "Return t if the current display supports proper composing."
  (display-graphic-p))


;;; Fontification

(defun org-superstar--prettify-ibullets ()
  "Prettify plain list bullets.

This function uses ‘org-superstar-plain-list-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-plain-list-p)
    (let* ((current-bullet (match-string 1)))
      (compose-region (match-beginning 1)
                      (match-end 1)
                      (org-superstar--ibullet current-bullet)))
    'org-superstar-item))

(defun org-superstar--unprettify-ibullets ()
  "Revert visual tweaks made to item bullets in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]+\\([-+*]\\) " nil t)
      (decompose-region (match-beginning 1) (match-end 1)))))

(defun org-superstar--prettify-main-hbullet ()
  "Prettify the trailing star in a headline.

This function uses ‘org-superstar-headline-or-inlinetask-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-headline-or-inlinetask-p)
    (let ((level (org-superstar--heading-level)))
      (compose-region (match-beginning 1) (match-end 1)
                      (org-superstar--hbullet level))))
  'org-superstar-header-bullet)

(defun org-superstar--prettify-other-hbullet ()
  "Prettify the second last star in a headline.
This is only done if the particular title’s level is part of an
inline task, see ‘org-inlinetask-min-level’.

This function uses ‘org-superstar-inlinetask-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-inlinetask-p)
    (let ((level (org-superstar--heading-level)))
      (compose-region (match-beginning 2) (match-end 2)
                      (org-superstar--hbullet level))
      'org-superstar-header-bullet)))

(defun org-superstar--prettify-other-lbullet ()
  "Prettify the first leading bullet after the headline bullet.
This function serves as an extension of
‘org-superstar--prettify-leading-hbullets’.
This function uses ‘org-superstar-headline-p’ to avoid
prettifying bullets in (for example) source blocks."
  (cond ((org-superstar-headline-p)
         'org-superstar-leading)
        ((org-superstar-inlinetask-p)
         'org-inlinetask)))

(defun org-superstar--prettify-leading-hbullets ()
  "Prettify the leading bullets of a header line.
Unless ‘org-hide-leading-stars’ is non-nil, each leading star is
visually replaced by ‘org-superstar-leading-bullet’ and inherits
face properties from ‘org-superstar-leading’.

If viewed from a terminal, ‘org-superstar-leading-fallback’ is
used instead of the regular leading bullet to avoid errors.

This function uses ‘org-superstar-headline-or-inlinetask-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-headline-or-inlinetask-p)
    (let ((star-beg (match-beginning 3))
          (lead-end (if (org-superstar-headline-p)
                        (match-end 2) (match-end 3))))
      (while (< star-beg lead-end)
        (compose-region star-beg (setq star-beg (1+ star-beg))
                        (org-superstar--lbullet)))
      'org-superstar-leading)))

(defun org-superstar--make-invisible (subexp)
  "Make part of the text matched by the last search invisible.
SUBEXP, a number, specifies which parenthesized expression in the
last regexp.  If there is no SUBEXPth pair, do nothing."
  ;; REVIEW: Do you think when-let would be nicer here?
  (let ((start (match-beginning subexp))
        (end (match-end subexp)))
    (when start
      (add-text-properties
       start end '(invisible org-superstar-hide)))))

(defun org-superstar--unprettify-hbullets ()
  "Revert visual tweaks made to header bullets in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*+ " nil t)
      (decompose-region (match-beginning 0) (match-end 0)))))


;;; Font Lock

(defvar-local org-superstar--font-lock-keywords nil)

(defun org-superstar--update-font-lock-keywords ()
  "Set ‘org-superstar--font-lock-keywords’ to reflect current settings.
You should not call this function to avoid confusing this mode’s
cleanup routines."
  ;; The below regex is nicked from ‘org-list-full-item-re’, but
  ;; reduced to only match simple lists.  Changes were made to enforce
  ;; a leading space before asterisks to avoid confusion with title
  ;; bullets.
  (setq org-superstar--font-lock-keywords
        `(,@(when org-superstar-prettify-item-bullets
              '(("^[ \t]*?\\(?:\\(?1:[-+]\\)\\|[ \t]\\(?1:\\*\\)\\) "
                 (1 (org-superstar--prettify-ibullets)))))
          ("^\\(?3:\\**?\\)\\(?2:\\*?\\)\\(?1:\\*\\) "
           (1 (org-superstar--prettify-main-hbullet) prepend)
           ,@(unless (or org-hide-leading-stars
                         org-superstar-remove-leading-stars)
               '((3 (org-superstar--prettify-leading-hbullets)
                    t)
                 (2 (org-superstar--prettify-other-lbullet)
                    t)))
           ,@(when org-superstar-remove-leading-stars
               '((3 (org-superstar--make-invisible 3))
                 (2 (org-superstar--make-invisible 2))))
           ,@(when (featurep 'org-inlinetask)
               '((2 (org-superstar--prettify-other-hbullet)
                    prepend)))
           ;; If requested, put another function here that formats the
           ;; first star of an inlinetask as a bullet.
           ))))

(defun org-superstar--fontify-buffer ()
  "Fontify the buffer."
  (when font-lock-mode
    (save-restriction
      (widen)
      (font-lock-ensure)
      (font-lock-flush))))

;;; Mode commands
;;;###autoload
(define-minor-mode org-superstar-mode
  "Use UTF8 bullets for headlines and plain lists."
  nil nil nil
  :group 'org-superstar
  :require 'org
  (cond
   ;; Bail if Org is not enabled.
   ((and org-superstar-mode
         (not (derived-mode-p 'org-mode)))
    (message "Org mode is not enabled in this buffer.")
    (org-superstar-mode 0))
   ;; Set up Superstar.
   (org-superstar-mode
    (font-lock-remove-keywords nil org-superstar--font-lock-keywords)
    (org-superstar--update-font-lock-keywords)
    (font-lock-add-keywords nil org-superstar--font-lock-keywords
                            'append)
    (org-superstar--fontify-buffer)
    (add-to-invisibility-spec '(org-superstar-hide)))
   ;; Clean up nd exit.
   (t
    (remove-from-invisibility-spec '(org-superstar-hide))
    (font-lock-remove-keywords nil org-superstar--font-lock-keywords)
    (setq org-superstar--font-lock-keywords
          (default-value 'org-superstar--font-lock-keywords))
    (org-superstar--unprettify-ibullets)
    (org-superstar--unprettify-hbullets)
    (org-superstar--fontify-buffer))))

(defun org-superstar-restart ()
  "Re-enable Org Superstar mode, if the mode is enabled."
  (interactive)
  (when org-superstar-mode
    (org-superstar-mode 0)
    (org-superstar-mode 1)))


(provide 'org-superstar)
;;; org-superstar.el ends here
