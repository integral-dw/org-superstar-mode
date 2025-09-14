;;; org-superstar.el --- Prettify headings and plain lists in Org mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025  D. Williams, sabof

;; Author: D. Williams <d.williams@posteo.net>
;; Maintainer: D. Williams <d.williams@posteo.net>
;; Keywords: faces, outlines
;; Version: 1.7.0
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
;;   + making inline tasks (see org-inlinetask.el) more fancy by:
;;     - using double-bullets for inline tasks
;;     - applying a custom face to the marker star of inline tasks
;;     - using a special bullet for the marker star
;;     - introducing an independent face for marker stars
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
;; identical to its org-bullets counterpart.

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

(require 'org-element)
(require 'wid-edit)

(declare-function org-indent-mode "org-indent" (arg))
(defvar org-inlinetask-show-first-star)
(defvar org-indent-inlinetask-first-star)
(defvar org-indent-mode)


(defgroup org-superstar nil
  "Use UTF8 bullets for headlines and plain lists."
  :group 'org-appearance)

;;; Bullet Variables

(defcustom org-superstar-headline-bullets-list
  '(;; Original ones nicked from org-bullets
    ?◉
    ?○
    ?✸
    ?✿) ;; My default: ?◉ ?🞛 ?○ ?▷
  "List of bullets used in Org headings.
It can contain any number of bullets, the Nth entry usually
corresponding to the bullet used for level N.  The way this list
is cycled through can use fine-tuned by customizing
‘org-superstar-cycle-headline-bullets’.

Every entry in this list can either be a string, a character, a cons
cell or nil.  Characters and strings are used as simple, verbatim
replacements of the asterisk for every display (be it graphical or
terminal).  In the case of strings, everything past the first character
is ignored.  If an element is nil, the bullet is hidden from view
entirely, including indentation.

If the list element is a cons cell, it should be a proper list of the
form
\(COMPOSE-STRING CHARACTER [REST...])

where COMPOSE-STRING should be a string according to the rules of the
third argument of ‘compose-region’.  It will be used to compose the
specific headline bullet.  CHARACTER is the fallback character used in
terminal displays, where composing characters cannot be relied upon.

The syntax of the above list will change with version 2.0.0 of this
package, where compose support will be replaced with display properties,
see Info node ‘(elisp) Display Property’.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(repeat (choice
                  (character :value ?◉
                             :format "Bullet character: %v\n"
                             :tag "Simple bullet character")
                  (string :value "◉"
                          :tag "Bullet character (legacy method)")
                  (const :value nil
                         :tag "Hide bullet entirely.")
                  (list :tag "Advanced string and fallback"
                        (string :value "◉"
                                :format "String of characters to compose: %v")
                        (character :value ?◉
                                   :format "Fallback character for terminal: %v\n")))))

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

In the simplest case each key should be a TODO keyword, and each
value should the UTF8 character to be displayed.  Keywords that
are not included in the alist are handled like normal headings.

Alternatively, each alist element may be a proper list of the form
\(KEYWORD COMPOSE-STRING CHARACTER [REST...])

where KEYWORD should be a TODO keyword (a string), and COMPOSE-STRING
should be a string according to the rules of the third argument of
‘compose-region’.  It will be used to compose the specific TODO item
bullet.  CHARACTER is the fallback character used in terminal displays,
where composing characters cannot be relied upon.  See also
‘org-superstar-leading-fallback’.

The syntax of the above list will change with version 2.0.0 of this
package, where compose support will be replaced with display properties,
see Info node ‘(elisp) Display Property’.

KEYWORD may also be the symbol ‘default’ instead of a string.  In
this case, this bullet is used for all TODO unspecified keywords.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(alist :key-type
                (choice :format "%[Toggle%] %v\n"
                        (string :tag "Bullet for (custom) TODO keyword"
                                :format "TODO keyword: %v")
                        (const :tag "Default TODO keyword"
                               :format "Default TODO keyword: %v"
                               default))
                :value-type
                (choice
                 (character :value ?◉
                            :format "Bullet character: %v\n"
                            :tag "Simple bullet character")
                 (list :tag "Advanced string and fallback"
                       (string :value "◉"
                               :format "String of characters to compose: %v")
                       (character :value ?◉
                                  :format "Fallback character for terminal: %v\n")))))

(defun org-superstar--set-fbullet (symbol value)
  "Set SYMBOL ‘org-superstar-first-inlinetask-bullet’ to VALUE.
If set to a character, also set ‘org-superstar-first-inlinetask-fallback’."
  (set-default symbol value)
  (when (characterp value)
    (set-default 'org-superstar-first-inlinetask-fallback value)))

(defcustom org-superstar-first-inlinetask-bullet ?▶
  "A special bullet used for the first star of an inline task.
Normally, this variable is a character replacing the default
star.  If it’s a string, compose the replacement according to the
rules of ‘compose-region’ for the COMPONENTS argument.

The syntax of this variable will change with version 2.0.0 of this
package, where compose support will be replaced with display properties,
see Info node ‘(elisp) Display Property’.

This bullet is displayed using the dedicated face
‘org-superstar-first’.

This variable is only used for graphical displays.
‘org-superstar-first-inlinetask-fallback’ is used for terminal
displays instead.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(choice
          (character :tag "Single character to display"
                     :format "\n%t: %v\n"
                     :value ?▶)
          (string :tag "String of characters to compose replacement from"
                  :format "\n%t:\n%v"
                  :value "*"))
  :set #'org-superstar--set-fbullet)

(defcustom org-superstar-first-inlinetask-fallback
  (cond ((characterp org-superstar-first-inlinetask-bullet)
         org-superstar-first-inlinetask-bullet)
        (t ?*))
  "A special bullet used for the first star of an inline task.
This variable is a character replacing the default star in
terminal displays instead of ‘org-superstar-first-inlinetask-bullet’.

If the leading bullet is set to a character before the package is
loaded, this variable’s default value is set to that character as
well.  Setting the leading bullet to a character using the custom
interface also automatically sets this variable.

You should call ‘org-superstar-restart’ after changing this
variable for your changes to take effect."
  :group 'org-superstar
  :type '(character :tag "Single character to display"
                    :format "\n%t: %v\n"
                    :value ?*))

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

The syntax of the above variable will change with version 2.0.0 of this
package, where compose support will be replaced with display properties,
see Info node ‘(elisp) Display Property’.

If ‘org-hide-leading-stars’ is nil, leading stars in a headline
are represented as a sequence of this bullet using the face
‘org-superstar-leading’.  Otherwise, this variable has no effect
and ‘org-mode’ covers leading stars using ‘org-hide’.  See also
‘org-indent-mode-turns-on-hiding-stars’.

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
  "Non-nil means cycle through available headline bullets.

The following values are meaningful:

An integer value of N cycles through the first N entries of the list
instead of the whole list.  If N is negative, cycle through the last -N
entries instead.

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
                   :match (lambda (_ x) (and (integerp x) (> x 0)))
                   :validate org-superstar--validate-hcycle)
          (integer :tag "Repeat the last -<integer> elements only."
                   :format "Repeat the last -(%v) entries exclusively.\n"
                   :size 8
                   :value -1
                   :match (lambda (_ x) (and (integerp x) (< x 0)))
                   :validate (lambda (x) (org-superstar--validate-hcycle x t)))))

(defun org-superstar--validate-hcycle (text-field &optional negative)
  "Raise an error if TEXT-FIELD’s value is an invalid hbullet number.

If the optional argument NEGATIVE is given, flip the sign of the value
read from TEXT-FIELD.

This function is used for ‘org-superstar-cycle-headline-bullets’.
If the integer exceeds the length of
‘org-superstar-headline-bullets-list’, set it to the length and
raise an error."
  (let* ((sign (if negative -1 1))
         (ncycle (* sign (widget-value text-field)))
         (maxcycle (org-superstar--hbullets-length)))
    (unless (<= 1 ncycle maxcycle)
      (widget-put
       text-field
       :error (format "Value must be between %i and %i"
                      sign (* sign maxcycle)))
      (widget-value-set text-field (* sign maxcycle))
      text-field)))

(defcustom org-superstar-prettify-item-bullets t
  "Non-nil means display plain lists bullets as UTF8 bullets.

Each type of plain list bullet is associated with a
corresponding UTF8 character in ‘org-superstar-item-bullet-alist’.

If set to the symbol ‘only’, disable fontifying headlines entirely.
This takes precedence over all other customizations.

Ordered plain list bullets are *not* fontified, but their appearance can
be changed by customizing the face ‘org-superstar-ordered-item’.
Alphabetic bullets are recognized depending on the value of
‘org-list-allow-alphabetical’.

You should call ‘org-superstar-restart’ after changing this variable
\(or ‘org-list-allow-alphabetical’) for your changes to take effect."
  :group 'org-superstar
  :type '(choice (const :tag "Enable item bullet fontification" t)
                 (const :tag "Disable item bullet fontification" nil)
                 (const :tag "Exclusively fontify item bullets" only)))

(defcustom org-superstar-special-todo-items nil
  "Non-nil means use special bullets for TODO items.

Instead of displaying bullets corresponding to TODO items
according to ‘org-superstar-headline-bullets-list’ (dependent on
the headline’s level), display a bullet according to
‘org-superstar-todo-bullet-alist’ (dependent on the TODO
keyword).

If set to the symbol ‘hide’, hide the leading bullet entirely
instead."
  :group 'org-superstar
  :type '(choice
          (const :tag "Enable special TODO item bullets" t)
          (const :tag "Disable special TODO item bullets" nil)
          (const :tag "Hide TODO item bullets altogether" hide)))

(defvar-local org-superstar-lightweight-lists nil
  "Non-nil means circumvent expensive calls to ‘org-superstar-plain-list-p’.

There is usually no need to use this variable directly; instead,
use the command ‘org-superstar-toggle-lightweight-lists’.")

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

(defface org-superstar-ordered-item
  '((default . (:inherit default)))
  "Face used to display ordered list item bullets."
  :group 'org-superstar)

(defface org-superstar-first
  '((default . (:inherit org-warning)))
  "Face used to display the first bullet of an inline task.
This face is only used when ‘org-inlinetask-show-first-star’ is
non-nil."
  :group 'org-superstar)


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


;;; Hooks

(defvar org-superstar-prettify-headline-hook nil
  "Hook run when Org Superstar prettifies a headline.

The hook functions can access the match data, with the following groups
defined for access with functions like ‘match-beginning’ and
‘match-end’:

0: The headline, defined as all asterisks from beginning of line,
   followed by a (mandatory) space character, inclusive.
1: The trailing asterisk (headline star/bullet).
2: The second last asterisk.  By default only used for inline tasks.
3: All asterisks except for the trailing asterisk (leading stars).
4: The first asterisk.  By default only used for inline tasks.

Groups 2 and 4 are contained in 3.  In the special case of level 2
headings (2 asterisks), the second asterisk is part of group 4, taking
precedence.")

(defvar org-superstar-prettify-inlinetask-hook nil
  "Hook run when Org Superstar prettifies an inline task.

The hook functions have access to the same match data as those in
‘org-superstar-prettify-headline-hook', which see.")


;;; Predicates

(defun org-superstar-plain-list-p ()
  "Return non-nil if the current match is a proper plain list.

This function may be expensive for files with very large plain
lists; consider using ‘org-superstar-toggle-lightweight-lists’ in
such cases to avoid slowdown."
  (or org-superstar-lightweight-lists
      (save-match-data
        (org-list-in-valid-context-p))))

(defun org-superstar-headline-or-inlinetask-p ()
  "Return t if the current match is a proper headline or inlinetask."
  (save-match-data
    (and (org-at-heading-p) t)))

(defun org-superstar-headline-p ()
  "Return t if the current match is a proper headline."
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


;;; Public Accessor Functions

(defun org-superstar-heading-level ()
  "Return the heading level N of the currently matched headline.

It is computed from the match data, which is expected to
encompass the headline (N asterisks) and a single whitespace."
  (- (match-end 0) (match-beginning 0) 1))

(defun org-superstar-hbullet (&optional level)
  "Return the desired headline bullet replacement for LEVEL N.

This function either returns a character, a string, or nil.  A character
is meant to be a drop-in replacement for the default asterisk, a string
is meant to be processed by ‘compose-region’, and nil means to hide the
bullet from view entirely.

If LEVEL nil, it is computed from the match data, which is expected to
encompass the headline (N asterisks) and a single whitespace.

If the headline is also a TODO item, you can override the usually
displayed bullet depending on the TODO keyword by setting
‘org-superstar-special-todo-items’ to t and adding relevant TODO
keyword entries to ‘org-superstar-todo-bullet-alist’.

This function takes user set variables such as
‘org-superstar-cycle-headline-bullets’ into account.  For more
information on how to customize headline bullets, see
‘org-superstar-headline-bullets-list’."
  ;; string-to-char no longer makes sense here.
  ;; If you want to support strings properly, return the string.
  ;; However, allowing for fallback means the list may contain
  ;; strings, chars or conses.  The cons must be resolved.
  ;; Hence, a new funtion is needed to keep the complexity to a minimum.
  (let* ((level (or level (org-superstar-heading-level)))
         (ncycle org-superstar-cycle-headline-bullets)
         (n (if org-odd-levels-only (/ (1- level) 2) (1- level)))
         (todo-bullet (when org-superstar-special-todo-items
                        (org-superstar--todo-bullet))))
    (cond (todo-bullet
           (unless (eq todo-bullet 'hide)
             todo-bullet))
          ((and (integerp ncycle) (> ncycle 0))
           (org-superstar--nth-headline-bullet (% n ncycle)))
          ((and (integerp ncycle) (< ncycle 0))
           ;; Remember, ncycle is negative.
           (let* ((loop-start (+ (org-superstar--hbullets-length) ncycle))
                  (k (- n loop-start)))
             (org-superstar--nth-headline-bullet
              (if (< n loop-start)
                  n
                (+ loop-start (% k (- ncycle)))))))
          (ncycle
           (org-superstar--nth-headline-bullet
            (% n (org-superstar--hbullets-length))))
          (t
           (org-superstar--nth-headline-bullet
            (min n (1- (org-superstar--hbullets-length))))))))


(defun org-superstar-lbullet ()
  "Return the correct leading bullet for the current display.

See ‘org-superstar-leading-bullet’ and ‘org-superstar-leading-fallback’."
  (if (org-superstar-graphic-p)
      org-superstar-leading-bullet
    org-superstar-leading-fallback))

(defun org-superstar-fbullet ()
  "Return the correct first inline task star for the current display.

See ‘org-superstar-first-inlinetask-bullet’ and
‘org-superstar-first-inlinetask-fallback’."
  (if (org-superstar-graphic-p)
      org-superstar-first-inlinetask-bullet
    org-superstar-first-inlinetask-fallback))


;;; Private Accessor Functions

(defun org-superstar--get-todo (pom)
  "Return the TODO keyword at point or marker POM.
If no TODO property is found, return nil."
  (save-match-data
    (let ((todo-property
           (cdar (org-entry-properties pom "TODO"))))
      (when (stringp todo-property)
        todo-property))))

(defun org-superstar--todo-assoc (todo-kw)
  "Obtain alist entry for the string keyword TODO-KW.

If TODO-KW has no explicit entry in the alist
‘org-superstar-todo-bullet-alist’, but there is an entry for the
symbol ‘default’, return it instead.  Otherwise, return nil."
  (or
   (assoc todo-kw
          org-superstar-todo-bullet-alist
          ;; I would use assoc-string, but then I'd have to deal with
          ;; what to do should the user create a TODO keyword
          ;; "default" for some forsaken reason.
          (lambda (x y) (and (stringp x)
                             (string= x y))))
   (assq 'default
         org-superstar-todo-bullet-alist)))

(defun org-superstar--todo-bullet ()
  "Return the desired TODO item bullet, if defined.

If no entry can be found in ‘org-superstar-todo-bullet-alist’ for
the current keyword, return nil.

If ‘org-superstar-special-todo-items’ is set to the symbol
‘hide’, return that instead."
  (let* ((todo-kw
          (org-superstar--get-todo (match-beginning 0)))
         (todo-bullet
          (cdr (org-superstar--todo-assoc todo-kw))))
    (cond
     ((not todo-kw)
      nil)
     ((eq org-superstar-special-todo-items 'hide)
      'hide)
     ((characterp todo-bullet)
      todo-bullet)
     ((listp todo-bullet)
      (when-let ((todo-fallback (cadr todo-bullet))
                 (todo-bullet (car todo-bullet)))
        (if (org-superstar-graphic-p)
            todo-bullet
          todo-fallback))))))

(defun org-superstar--hbullets-length ()
  "Return the length of ‘org-superstar-headline-bullets-list’."
  (length org-superstar-headline-bullets-list))

(define-obsolete-function-alias
  'org-superstar--hbullet
  'org-superstar-hbullet "1.7.0")

(defun org-superstar--nth-headline-bullet (n)
  "Return the Nth specified headline bullet or its corresponding fallback.
N counts from zero.  Headline bullets are specified in
‘org-superstar-headline-bullets-list’."
  (let ((bullet-entry
         (elt org-superstar-headline-bullets-list n)))
    (cond
     ((characterp bullet-entry)
      bullet-entry)
     ;; Strip bullets provided as strings down to their first char.
     ;; The main reason hbullets can be defined using strings is
     ;; because org-bullets did it.
     ((stringp bullet-entry)
      (string-to-char bullet-entry))
     ;; If the element is a cons, assume the user knows what they are
     ;; doing.
     ((org-superstar-graphic-p)
      (elt bullet-entry 0))
     (t
      (elt bullet-entry 1)))))

(defun org-superstar--ibullet (bullet-string)
  "Return BULLET-STRINGs desired UTF-8 replacement.

Each of the three regular plain list bullets +, - and * will be
replaced by their corresponding entry in ‘org-superstar-item-bullet-alist’."
  (if-let ((new-bullet (cdr (assq (string-to-char bullet-string)
                                  org-superstar-item-bullet-alist))))
      (string new-bullet)
    bullet-string))

(define-obsolete-function-alias 'org-superstar--lbullet
  'org-superstar-lbullet "1.7.0")

(define-obsolete-function-alias 'org-superstar--fbullet
  'org-superstar-fbullet "1.7.0")

(define-obsolete-function-alias 'org-superstar--heading-level
  'org-superstar-heading-level "1.7.0")


;;; Fontification

(defun org-superstar--prettify-ibullets ()
  "Prettify plain list bullets.

This function uses ‘org-superstar-plain-list-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-plain-list-p)
    (let ((current-bullet (match-string 1)))
      (put-text-property (match-beginning 1)
                         (match-end 1)
                         'display
                         (org-superstar--ibullet current-bullet))
    'org-superstar-item)))

(defun org-superstar--prettify-obullets ()
  "Prettify ordered list bullets.

This function uses ‘org-superstar-plain-list-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-plain-list-p)
    'org-superstar-ordered-item))

(defun org-superstar--prettify-main-hbullet ()
  "Prettify the trailing star in a headline.

This function uses ‘org-superstar-headline-or-inlinetask-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-headline-or-inlinetask-p)
    (let ((bullet (org-superstar-hbullet)))
      (if bullet
          (compose-region (match-beginning 1) (match-end 1)
                          bullet)
        (org-superstar--make-invisible 1)))
    'org-superstar-header-bullet))

(defun org-superstar--prettify-other-hbullet ()
  "Prettify the second last star in a headline.
This is only done if the particular title’s level is part of an
inline task, see ‘org-inlinetask-min-level’.

This function uses ‘org-superstar-inlinetask-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-inlinetask-p)
    (let ((bullet (org-superstar-hbullet)))
      (if bullet
          (compose-region (match-beginning 2) (match-end 2)
                          bullet)
        (org-superstar--make-invisible 2)))
    '(org-superstar-header-bullet org-inlinetask)))

(defun org-superstar--prettify-leading-hbullets ()
  "Prettify the leading bullets of a header line.
Unless ‘org-hide-leading-stars’ is non-nil, each leading star is
rendered as ‘org-superstar-leading-bullet’ and inherits face
properties from ‘org-superstar-leading’.

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
                        (org-superstar-lbullet)))
      'org-superstar-leading)))

(defun org-superstar--prettify-first-bullet ()
  "Prettify the first bullet of an inline task.
If ‘org-inlinetask-show-first-star’ is non-nil, the first star of
an inlinetask is rendered as ‘org-superstar-first-inlinetask-bullet’
and inherits face properties from ‘org-superstar-first’.

If viewed from a terminal, ‘org-superstar-first-inlinetask-fallback’
is used instead of the regular bullet to avoid errors.

This function uses ‘org-superstar-inlinetask-p’ to avoid
prettifying bullets in (for example) source blocks."
  (when (org-superstar-inlinetask-p)
    (let ((star-beg (match-beginning 4)))
      (compose-region star-beg (1+ star-beg)
                      (org-superstar-fbullet))
      'org-superstar-first)))

(defun org-superstar--run-heading-hooks ()
  "Run hooks for user-defined modifications to headlines and inline tasks.

This function will only execute the appropriate hooks for the type of
heading at point."
  (cond
   ((org-superstar-headline-p)
    (run-hooks 'org-superstar-prettify-headline-hook))
   ((org-superstar-inlinetask-p)
    (run-hooks 'org-superstar-prettify-inlinetask-hook)))
  nil)

(defun org-superstar--prettify-indent ()
  "Set up ‘org-indent-inlinetask-first-star’ buffer-locally.
Restart Org Indent Mode to enforce the change to take effect, if
enabled.  This way, ‘org-indent-mode’ uses the correct bullet
instead of a star.  If Org Indent is not loaded, this function
does nothing.

See also ‘org-superstar-first-inlinetask-bullet’."
  (when (featurep 'org-indent)
    (let ((bullet-components (org-superstar-fbullet))
          (bullet "*"))
      (cond
       ((characterp bullet-components)
        (setq bullet (string bullet-components)))
       ;; bullet-components must be a string => compsoe!
       (t
        (setq bullet
              (compose-string bullet nil nil
                              bullet-components))))
      (setq-local org-indent-inlinetask-first-star
                  (org-add-props bullet '(face org-superstar-first))))
    (when (and org-indent-mode
               (featurep 'org-inlinetask))
      (org-indent-mode 0)
      (org-indent-mode 1))))

(defun org-superstar--unprettify-indent ()
  "Revert ‘org-indent-inlinetask-first-star’ to default value.
If Org Indent Mode is enabled, also restart it if necessary."
  (when (featurep 'org-indent)
    (kill-local-variable 'org-indent-inlinetask-first-star)
    (when (and org-indent-mode
               (featurep 'org-inlinetask))
      (org-indent-mode 0)
      (org-indent-mode 1))))

(defun org-superstar--make-invisible (subexp)
  "Make part of the text matched by the last search invisible.
SUBEXP, a number, specifies which parenthesized expression in the
last regexp.  If there is no SUBEXPth pair, do nothing."
  (when-let ((start (match-beginning subexp))
             (end (match-end subexp)))
    (put-text-property
     start end 'invisible 'org-superstar-hide)))

(defun org-superstar--invisibility-off ()
  "Disable invisibility caused by Org Superstar."
  (remove-from-invisibility-spec '(org-superstar-hide)))

(defun org-superstar--invisibility-on ()
  "Enable invisibility caused by Org Superstar."
  (add-to-invisibility-spec '(org-superstar-hide)))

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
                 (1 (org-superstar--prettify-ibullets)))
                ("^[ \t]*\\(?1:[[:digit:]]+[.)]\\) "
                 (1 (org-superstar--prettify-obullets)))))
          ,@(when (and org-superstar-prettify-item-bullets
                       org-list-allow-alphabetical)
              '(("^[ \t]*\\(?1:[a-zA-Z][.)]\\) "
                 (1 (org-superstar--prettify-obullets)))))
          ,@(unless (eq org-superstar-prettify-item-bullets 'only)
              `(("^\\(?3:\\(?4:\\*?\\)\\**?\\(?2:\\*?\\)\\)\\(?1:\\*\\) "
                 (1 (org-superstar--prettify-main-hbullet) prepend)
                 ,@(unless (or org-hide-leading-stars
                               org-superstar-remove-leading-stars)
                     '((3 (org-superstar--prettify-leading-hbullets)
                          t)))
                 ,@(when org-superstar-remove-leading-stars
                     '((3 (org-superstar--make-invisible 3))))
                 ,@(when (featurep 'org-inlinetask)
                     '((2 (org-superstar--prettify-other-hbullet)
                          t)))
                 ,@(when (and (featurep 'org-inlinetask)
                              org-inlinetask-show-first-star
                              (not org-superstar-remove-leading-stars)
                              (not (and (featurep 'org-indent) org-indent-mode)))
                     '((4 (org-superstar--prettify-first-bullet)
                          prepend)))
                 (0 (org-superstar--run-heading-hooks))))))))

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
  :lighter nil
  :keymap nil
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
    (org-superstar--invisibility-on)
    (org-superstar--prettify-indent)
    (when org-superstar-remove-leading-stars
      ;; perhaps no longer necessary
      (setq-local global-disable-point-adjustment t))
    (add-hook 'pre-command-hook #'org-superstar--invisibility-off nil t)
    (add-hook 'post-command-hook #'org-superstar--invisibility-on nil t))
   ;; Clean up and exit.
   (t
    (org-superstar--invisibility-off)
    (font-lock-remove-keywords nil org-superstar--font-lock-keywords)
    (setq org-superstar--font-lock-keywords
          (default-value 'org-superstar--font-lock-keywords))
    (org-superstar--unprettify-hbullets)
    (org-superstar--fontify-buffer)
    (org-superstar--unprettify-indent)
    (remove-hook 'pre-command-hook #'org-superstar--invisibility-off t)
    (remove-hook 'post-command-hook #'org-superstar--invisibility-on t)
    nil)))

(defun org-superstar-restart ()
  "Re-enable Org Superstar mode, if the mode is enabled."
  (interactive)
  (when org-superstar-mode
    (org-superstar-mode 0)
    (org-superstar-mode 1)))


(provide 'org-superstar)
;;; org-superstar.el ends here
