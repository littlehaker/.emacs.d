;;; smartparens.el --- Autoinsert pairs of defined brackets and wrap regions

;; Copyright (C) 2012 Matus Goljer

;; Author: Matus Goljer <matus.goljer@gmail.com>
;; Maintainer: Matus Goljer <matus.goljer@gmail.com>
;; Created: 17 Nov 2012
;; Keywords: abbrev convenience editing
;; URL: https://github.com/Fuco1/smartparens

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See github readme at https://github.com/Fuco1/smartparens

;;; Code:

(require 'dash)
(require 'thingatpt)
(eval-when-compile (require 'cl)
                   (defvar cua--region-keymap))
(declare-function cua-replace-region "cua-base")
(declare-function cua--pre-command-handler "cua-base")
(declare-function delete-selection-pre-hook "delsel")

(defun sp-cheat-sheet (&optional arg)
  "Generate a cheat sheet of all the smartparens interactive functions.

Without a prefix argument, print only the short documentation and examples.

With non-nil prefix argument, show the full documentation for each function.

You can follow the links to the function or variable help page.
To get back to the full list, use \\[help-go-back].

You can use `beginning-of-defun' and `end-of-defun' to jump to
the previous/next entry.

Examples are fontified using the `font-lock-string-face' for
better orientation."
  (interactive "P")
  (setq arg (not arg))
  (let ((do-not-display '(
                          smartparens-mode
                          smartparens-global-mode
                          turn-on-smartparens-mode
                          turn-off-smartparens-mode
                          sp--cua-replace-region
                          sp-wrap-cancel
                          sp-remove-active-pair-overlay
                          sp--self-insert-command
                          sp-wrap-tag-beginning
                          sp-wrap-tag-end
                          sp-wrap-tag-done
                          sp-splice-sexp-killing-around ;; is aliased to `sp-raise-sexp'
                          show-smartparens-mode
                          show-smartparens-global-mode
                          turn-on-show-smartparens-mode
                          turn-off-show-smartparens-mode
                          ))
        (do-not-display-with-arg '(
                                   sp-use-paredit-bindings
                                   sp-use-smartparens-bindings
                                   ))
        (commands (loop for i in (cdr (assoc-string (locate-library "smartparens") load-history))
                        if (and (consp i) (eq (car i) 'defun) (commandp (cdr i)))
                        collect (cdr i))))
    (with-current-buffer (get-buffer-create "*Smartparens cheat sheet*")
      (let ((standard-output (current-buffer))
            (help-xref-following t))
        (toggle-read-only -1)
        (erase-buffer)
        (help-mode)
        (smartparens-mode 1)
        (help-setup-xref (list #'sp-cheat-sheet)
                         (called-interactively-p 'interactive))
        (toggle-read-only -1)
        (--each (--remove (or (memq it do-not-display)
                              (and arg (memq it do-not-display-with-arg)))
                          commands)
          (unless (equal (symbol-name it) "advice-compilation")
            (let ((start (point)) kill-from)
              (insert (propertize (symbol-name it) 'face 'font-lock-function-name-face))
              (insert " is ")
              (describe-function-1 it)
              (save-excursion
                (when arg
                  (goto-char start)
                  (forward-paragraph 1)
                  (forward-line 1)
                  (if (looking-at "^It is bound")
                      (forward-paragraph 2)
                    (forward-paragraph 1))
                  (setq kill-from (point))
                  (when (re-search-forward "^Examples:" nil t)
                    (delete-region kill-from
                                   (save-excursion
                                     (forward-line 1)
                                     (point))))))
              (insert (propertize (concat
                                   "\n\n"
                                   (make-string 72 ?―)
                                   "\n\n") 'face 'font-lock-function-name-face)))))
        (goto-char (point-min))
        (while (re-search-forward "\\(->\\|​\\)" nil t)
          (let ((thing (bounds-of-thing-at-point 'line)))
            (put-text-property (car thing) (cdr thing) 'face 'font-lock-string-face)))
        (goto-char (point-min))
        (while (re-search-forward "|" nil t)
          (put-text-property (1- (point)) (point) 'face 'font-lock-warning-face))
        (goto-char (point-min))
        (while (re-search-forward "^It is bound to \\(.*?\\)\\." nil t)
          (put-text-property (match-beginning 1) (match-end 1) 'face 'font-lock-keyword-face))
        (goto-char (point-min))
        (while (re-search-forward ";;.*?$" nil t)
          (put-text-property (match-beginning 0) (match-end 0) 'face 'font-lock-comment-face))
        (help-make-xrefs)
        (goto-char (point-min))))
    (pop-to-buffer "*Smartparens cheat sheet*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

;;;###autoload
(defvar sp-keymap (make-sparse-keymap)
  "Keymap used for `smartparens-mode'.")
(defvaralias 'smartparens-mode-map 'sp-keymap)

(defvar sp-paredit-bindings '(
                              ("C-M-f" . sp-forward-sexp) ;; navigation
                              ("C-M-b" . sp-backward-sexp)
                              ("C-M-u" . sp-backward-up-sexp)
                              ("C-M-d" . sp-down-sexp)
                              ("C-M-p" . sp-backward-down-sexp)
                              ("C-M-n" . sp-up-sexp)
                              ("M-s" . sp-splice-sexp) ;; depth-changing commands
                              ("M-<up>" . sp-splice-sexp-killing-backward)
                              ("M-<down>" . sp-splice-sexp-killing-forward)
                              ("M-r" . sp-splice-sexp-killing-around)
                              ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
                              ("C-<right>" . sp-forward-slurp-sexp)
                              ("C-}" . sp-forward-barf-sexp)
                              ("C-<left>" . sp-forward-barf-sexp)
                              ("C-(" . sp-backward-slurp-sexp)
                              ("C-M-<left>" . sp-backward-slurp-sexp)
                              ("C-{" . sp-backward-barf-sexp)
                              ("C-M-<right>" . sp-backward-barf-sexp)
                              ("M-S" . sp-split-sexp) ;; misc
                              )
  "Alist containing the default paredit bindings to corresponding
smartparens functions.")

;;;###autoload
(defun sp-use-paredit-bindings ()
  "Initiate `sp-keymap' with paredit-compatible bindings for
corresponding functions provided by smartparens.  See variable
`sp-paredit-bindings'."
  (interactive)
  (--each sp-paredit-bindings
    (define-key sp-keymap (read-kbd-macro (car it)) (cdr it))))

(defvar sp-smartparens-bindings '(
                                  ("C-M-f" . sp-forward-sexp)
                                  ("C-M-b" . sp-backward-sexp)
                                  ("C-M-d" . sp-down-sexp)
                                  ("C-M-a" . sp-backward-down-sexp)
                                  ("C-S-d" . sp-beginning-of-sexp)
                                  ("C-S-a" . sp-end-of-sexp)
                                  ("C-M-e" . sp-up-sexp)
                                  ("C-M-u" . sp-backward-up-sexp)
                                  ("C-M-n" . sp-next-sexp)
                                  ("C-M-p" . sp-previous-sexp)
                                  ("C-M-k" . sp-kill-sexp)
                                  ("C-M-w" . sp-copy-sexp)
                                  ("M-<delete>" . sp-unwrap-sexp)
                                  ("M-<backspace>" . sp-backward-unwrap-sexp)
                                  ("C-<right>" . sp-forward-slurp-sexp)
                                  ("C-<left>" . sp-forward-barf-sexp)
                                  ("C-M-<left>" . sp-backward-slurp-sexp)
                                  ("C-M-<right>" . sp-backward-barf-sexp)
                                  ("M-D" . sp-splice-sexp)
                                  ("C-M-<delete>" . sp-splice-sexp-killing-forward)
                                  ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
                                  ("C-S-<backspace>" . sp-splice-sexp-killing-around)
                                  ("C-]" . sp-select-next-thing-exchange)
                                  ("C-M-]" . sp-select-next-thing)
                                  ("M-F" . sp-forward-symbol)
                                  ("M-B" . sp-backward-symbol)
                                  )
  "Alist containing the default smartparens bindings.")

;;;###autoload
(defun sp-use-smartparens-bindings ()
  "Initiate `sp-keymap' with smartparens bindings for navigation functions.
See variable `sp-smartparens-bindings'."
  (interactive)
  (--each sp-smartparens-bindings
    (define-key sp-keymap (read-kbd-macro (car it)) (cdr it))))

(defun sp--set-base-key-bindings (&optional symbol value)
  "Set up the default keymap based on `sp-base-key-bindings'.

This function is also used as a setter for this customize value."
  (when symbol (set-default symbol value))
  (cond
   ((eq sp-base-key-bindings 'sp)
    (sp-use-smartparens-bindings))
   ((eq sp-base-key-bindings 'paredit)
    (sp-use-paredit-bindings))))

(defun sp--update-override-key-bindings (&optional symbol value)
  "Override the key bindings with values from `sp-override-key-bindings'.

This function is also used as a setter for this customize value."
  (when symbol (set-default symbol value))
  ;; this also needs to reload the base set, if any is present.
  (sp--set-base-key-bindings)
  (--each sp-override-key-bindings
    (define-key sp-keymap (read-kbd-macro (car it)) (cdr it))))

(defcustom sp-base-key-bindings nil
  "A default set of key bindings for commands provided by smartparens.

Paredit binding adds the paredit bindings to the corresponding
smartparens commands. It does not add bindings to any other
commands, or commands that do not have a paredit counterpart.

Smartparens binding adds bindings to most common smartparens
commands.  These are somewhat inspired by paredit, but in many
cases differ.

Note that neither \"paredit\" nor \"smartparens\" bindings add a
binding for all the provided commands."
  :type '(radio
          (const :tag "Don't use any default set of bindings" nil)
          (const :tag "Use smartparens set of bindings" sp)
          (const :tag "Use paredit set of bindings" paredit))
  :set 'sp--set-base-key-bindings
  :group 'smartparens)

(defcustom sp-override-key-bindings nil
  "An alist of bindings and commands that should override the base key set.

If you wish to override a binding from the base set, set the
value for the binding to the `kbd' recognizable string constant
and command to the command symbol you wish to bind there.

If you wish to disable a binding from the base set, set the value
for the command to nil.

Examples:
 (\"C-M-f\" . sp-forward-sexp)
 (\"C-<right>\" . nil)

See `sp-base-key-bindings'."
  :type '(alist
          :key-type string
          :value-type symbol)
  :set 'sp--update-override-key-bindings
  :group 'smartparens)

(defvar sp-escape-char nil
  "Character used to escape quotes inside strings.")
(make-variable-buffer-local 'sp-escape-char)

(defvar sp-pair-list nil
  "List of pairs for autoinsertion or wrapping.

Maximum length of opening or closing pair is
`sp-max-pair-length-c' characters.")
(make-variable-buffer-local 'sp-pair-list)

(defvar sp-local-pairs nil
  "List of pair definitions used for current buffer.")
(make-variable-buffer-local 'sp-local-pairs)

(defvar sp-last-operation nil
  "Symbol holding the last successful operation.")
(make-variable-buffer-local 'sp-last-operation)

(defvar sp-previous-point -1
  "Location of point before last command.

This is only updated when some pair-overlay is active.  Do not
rely on the value of this variable anywhere else!")
(make-variable-buffer-local 'sp-previous-point)

(defvar sp-wrap-point nil
  "Save the value of point before attemt to wrap a region.

Used for restoring the original state if the wrapping is
cancelled.")
(make-variable-buffer-local 'sp-wrap-point)

(defvar sp-wrap-mark nil
  "Save the value of mark before attemt to wrap a region.

Used for restoring the original state if the wrapping is
cancelled.")
(make-variable-buffer-local 'sp-wrap-mark)

(defvar sp-last-inserted-characters ""
  "Characters typed during the wrapping selection.

If wrapping is cancelled, these characters are re-inserted to the
location of point before the wrapping.")
(make-variable-buffer-local 'sp-last-inserted-characters)

(defvar sp-last-wrapped-region nil
  "Information about the last wrapped region.
The format is the same as returned by `sp-get-sexp'.")
(make-variable-buffer-local 'sp-last-wrapped-region)

(defvar sp-point-inside-string nil
  "Non-nil if point is inside a string.

Used to remember the state from before `self-insert-command' is
run.")

(defconst sp-max-pair-length-c 10
  "Maximum length of an opening or closing delimiter.

Only the pairs defined by `sp-pair' are considered.  Tag pairs
can be of any length.")

(defvar sp-pairs '((t
                    .
                    ((:open "\\\\(" :close "\\\\)" :actions (insert wrap))
                     (:open "\\{"   :close "\\}"   :actions (insert wrap))
                     (:open "\\("   :close "\\)"   :actions (insert wrap))
                     (:open "\\\""  :close "\\\""  :actions (insert wrap))
                     (:open "/*"    :close "*/"    :actions (insert wrap))
                     (:open "\""    :close "\""    :actions (insert wrap))
                     (:open "'"     :close "'"     :actions (insert wrap))
                     (:open "("     :close ")"     :actions (insert wrap))
                     (:open "["     :close "]"     :actions (insert wrap))
                     (:open "{"     :close "}"     :actions (insert wrap))
                     (:open "`"     :close "`"     :actions (insert wrap)))))
  "List of pair definitions.

Maximum length of opening or closing pair is
`sp-max-pair-length-c' characters.")

(defvar sp-tags nil
  "List of tag definitions.  See `sp-local-tag' for more information.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customize & Mode definitions

(defgroup smartparens ()
  "Smartparens minor mode."
  :group 'editor)

;;;###autoload
(define-minor-mode smartparens-mode
  "Toggle smartparens mode."
  :init-value nil
  :lighter " SP"
  :group 'smartparens
  :keymap sp-keymap
  (if smartparens-mode
      (progn
        ;; setup local pair replacements
        (sp--update-local-pairs)
        ;; set the escape char
        (dotimes (char 256)
          (unless sp-escape-char
            (if (= ?\\ (char-syntax char))
                (setq sp-escape-char (string char)))))
        (when (sp--delete-selection-p)
          (sp--init-delete-selection-mode-emulation))
        (run-hooks 'smartparens-enabled-hook))
    (run-hooks 'smartparens-disabled-hook)))

(defvar sp-trigger-keys nil
  "List of trigger keys.")

(defun sp--update-trigger-keys (&optional remove)
  "Update the trigger keys in `sp-keymap'.

Trigger key is any character present in any pair's opening or
closing delimiter.  Each trigger key must map to
`sp--self-insert-command'.

The optional argument REMOVE is a string of trigger keys to
remove.  If non-nil, remove the trigger keys defined by this
string.  After the removal, all the pairs are re-checked."
  (when remove
    (--each (split-string remove "" t)
      (define-key sp-keymap it nil)))

  (setq sp-trigger-keys nil)
  (dolist (mode-pairs sp-pairs)
    (dolist (pair (cdr mode-pairs))
      (let ((open (plist-get pair :open))
            (close (plist-get pair :close)))
        (when open
          (setq sp-trigger-keys (append (split-string open "" t) sp-trigger-keys))
          (--each (split-string open "" t)
            (define-key sp-keymap it 'sp--self-insert-command)))
        (when close
          (setq sp-trigger-keys (append (split-string close "" t) sp-trigger-keys))
          (--each (split-string close "" t)
            (define-key sp-keymap it 'sp--self-insert-command))))))

  (dolist (mode-tags sp-tags)
    (dolist (tag (cdr mode-tags))
      (let ((trig (plist-get tag :trigger)))
        (setq sp-trigger-keys (append (split-string trig "" t) sp-trigger-keys))
        (--each (split-string trig "" t)
          (define-key sp-keymap it 'sp--self-insert-command)))))

  (setq sp-trigger-keys (-distinct sp-trigger-keys)))

(defun sp--keybinding-fallback (&optional key-sequence)
  "Return the fall-back command as if `smartparens-mode' were disabled."
  (let ((smartparens-mode nil)
        (keys (or key-sequence (car sp-recent-keys))))
    (key-binding keys t)))

(defun sp--update-local-pairs ()
  "Update local pairs after removal or at mode initialization."
  (setq sp-local-pairs (sp--merge-with-local major-mode))
  (setq sp-local-pairs (--filter (plist-get it :actions) sp-local-pairs))
  ;; update the `sp-pair-list'.  This is a list only containing
  ;; (open.close) cons pairs for easier querying.  We also must order
  ;; it by length of opening delimiter in descending order (first
  ;; value is the longest)
  (let ((l))
    (--each sp-local-pairs
      (!cons (cons (plist-get it :open) (plist-get it :close)) l))
    (setq l (sort l #'(lambda (x y) (> (length (car x)) (length (car y))))))
    (setq sp-pair-list l)))

(defun sp--update-local-pairs-everywhere (&rest modes)
  "Run `sp--update-local-pairs' in all buffers.

This is necessary to update all the buffer-local definitions.  If
MODES is non-nil, only update buffers with `major-mode' equal to
MODES."
  (setq modes (-flatten modes))
  (--each (buffer-list)
    (with-current-buffer it
      (when (and smartparens-mode
                 (or (not modes)
                     (memq major-mode modes)))
        (sp--update-local-pairs)))))

(defvar smartparens-enabled-hook nil
  "Called after `smartparens-mode' is turned on.")

(defvar smartparens-disabled-hook nil
  "Called after `smartparens-mode' is turned off.")

;; global custom
(defcustom sp-ignore-modes-list '(
                                  minibuffer-inactive-mode
                                  )
  "Modes where smartparens mode is inactive if allowed globally."
  :type '(repeat symbol)
  :group 'smartparens)

;;;###autoload
(define-globalized-minor-mode smartparens-global-mode
  smartparens-mode
  turn-on-smartparens-mode)

;;;###autoload
(defun turn-on-smartparens-mode ()
  "Turn on `smartparens-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (eq (get major-mode 'mode-class) 'special))
    (smartparens-mode t)))

;;;###autoload
(defun turn-off-smartparens-mode ()
  "Turn off `smartparens-mode'."
  (interactive)
  (smartparens-mode -1))

;; insert custom
(defcustom sp-autoinsert-pair t
  "If non-nil, autoinsert pairs.  See `sp-insert-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoinsert-if-followed-by-same 2
  "Customizes behaviour of pair insertion if the point is followed by
the same opening pair as currently inserted pair.

The first option does not change the insertion behaviour and pairs are
inserted normally.  For example |() followed by ( would produce (|)().

The second option inserts the pair only if the opening pair
following point is not the same as currently inserted pair.  For
example |() followed by ( would produce (|().  If next character
isn't part of any pair, insert normally.

The third option behaves as second, but if the opening and closing
pairs are the same, and we are looking at the closing pair, insert the
whole pair.  For example \"|\" followed by \" produce \"\"|\"\".  This
is useful in modes where pairs of same characters have special
meaning, such as `markdown-mode' and * for italics and ** for bold.

The forth option is a combination of first and third.  The pairs
where opening and closing pair are different are always inserted
normally.  The pairs with same opening and closing delimiter are
only inserted if the enclosing expression is empty (for nested
quotations etc.), otherwise the closing delimiter is skipped
instead."
  :type '(radio
          (const :tag "Insert the pair normally" 0)
          (const :tag "Insert the pair only if not followed by same" 1)
          (const :tag "Insert the pair only if not followed by same, but if the closing pair is the same as opening, insert new pair (useful for nested quote insertion)" 2)
          (const :tag "Insert the pair if opening and closing pair is the same and the containing expression is empty and always insert other pairs normally." 3))
  :group 'smartparens)

(defcustom sp-autoinsert-if-followed-by-word nil
  "If non-nil, autoinsert the whole pair even if point is followed by word.

For example |word followed by ( would produce (|)word.  If nil,
it would produce (|word."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoinsert-quote-if-followed-by-closing-pair nil
  "If non-nil, autoinsert string quote pair even if the point is followed by closing pair.

This option only changes behaviour of the insertion process if
point is inside a string.  In other words, if string is not
closed and next character is a closing pair.

For example, in a situation like this:

  [\"some text|]

after pressing \", one would probably want to insert the closing
quote, not a nested pair (\\\"\\\"), to close the string literal
in the array.  To enable such behaviour, set this variable to
nil.

Note: the values of this varible seem to be backward, i.e. it is
\"enabled\" when the value is nil.  This was an unfortunate
choice of wording.  It is kept this way to preserve backward
compatibility.  The intended meaning is \"insert the pair if
followed by closing pair?\", t = yes."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoinsert-inhibit-functions nil
  "List of functions to call before autoinserting a pair.

If any of these return t, the pair is not inserted.  The
functions take two arguments: current opening pair and a boolean
value indicating if the point is inside string or comment.

This option is deprecated.  You should instead use the :when
and :unless properties of `sp-pair'."
  :type 'hook
  :group 'smartparens)

(defcustom sp-autoskip-closing-pair t
  "If non-nil, skip the following closing pair.

See `sp-skip-closing-pair' for more info."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-cancel-autoskip-on-backward-movement t
  "If non-nil, autoskip of closing pair is cancelled not only
when point is moved outside of the pair, but also if the point
moved backwards.  See `sp-skip-closing-pair' for more info."
  :type 'boolean
  :group 'smartparens)

;; delete custom
(defcustom sp-autodelete-pair t
  "If non-nil, auto delete pairs.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-closing-pair t
  "If non-nil, auto delete the whole closing-pair.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-opening-pair t
  "If non-nil, auto delete the whole opening-pair.  See `sp-delete-pair'."
  :type 'boolean
  :group 'smartparens)

;; wrap custom
(defcustom sp-autowrap-region t
  "If non-nil, wrap the active region with pair.

See `sp-wrap-region' and `sp-wrap-region-init'."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autodelete-wrap t
  "If non-nil, auto delete both opening and closing pair of most recent wrapping.

Deletion command must be the very first command after the
insertion, otherwise normal behaviour is applied."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-wrap-repeat-last 1
  "Context in which smartparens repeats the last wrap.

If the last operation was a wrap and we insert another pair at
the beginning or end of the last wrapped region, repeat the
wrap on this region with current pair."
  :type '(radio
          (const :tag "Do not repeat wrapping" 0)
          (const :tag "Only repeat if current tag is the same as the last one" 1)
          (const :tag "Always repeat if the point is after the opening/closing delimiter of last wrapped region" 2))
  :group 'smartparens)

;; escaping custom
(defcustom sp-autoescape-string-quote t
  "If non-nil, autoescape string quotes if typed inside string."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-autoescape-string-quote-if-empty '(
                                                 python-mode
                                                 )
  "List of modes where the string quotes aren't escaped if the string is empty.

You can list modes where multiple quote characters are used for
multi-line strings, such as `python-mode' to make the insertion
less annoying (that is, three times pressing \" would insert
\"\"\"|\"\"\" instead of \"\\\"\\\"|\\\"\\\"\")."
  :type '(repeat symbol)
  :group 'smartparens)

(defcustom sp-navigate-consider-sgml-tags '(
                                            html-mode
                                            )
  "List of modes where sgml tags are considered to be sexps."
  :type '(repeat symbol)
  :group 'smartparens)

;; navigation & manip custom
(defcustom sp-navigate-consider-symbols t
  "If non-nil, consider symbols outside balanced expressions as such.

Symbols are recognized by function `sp-forward-symbol'.  This
setting affect all the navigation and manipulation functions
where it make sense.

Also, special handling of strings is enabled, where the whole
string delimited with \"\" is considered as one token."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-navigate-reindent-after-up 'interactive
  "If non-nil, reindent sexp after jumping out of it using `sp-up-sexp'.

The whitespace between the closing delimiter and last \"thing\"
inside the expression is removed.  It works analogically for the
`sp-backward-up-sexp'."
  :type '(radio
          (const :tag "Always reindent" always)
          (const :tag "Reindent only if called interactively" interactive)
          (const :tag "Never reindent" nil))
  :group 'smartparens)

(defcustom sp-navigate-close-if-unbalanced nil
  "If non-nil, insert the closing pair of the un-matched pair on `sp-up-sexp'.

The closing delimiter is inserted after the symbol at
point (using `sp-previous-sexp')."
  :type 'boolean
  :group 'smartparens)

;; ui custom
(defcustom sp-highlight-pair-overlay t
  "If non-nil, autoinserted pairs are highlighted until point is inside the pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-highlight-wrap-overlay t
  "If non-nil, wrap overlays are highlighted during editing of the wrapping pair."
  :type 'boolean
  :group 'smartparens)

(defcustom sp-highlight-wrap-tag-overlay t
  "If non-nil, wrap tag overlays are highlighted during editing of the wrapping tag pair."
  :type 'boolean
  :group 'smartparens)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection mode emulation

(defun sp--delete-selection-p ()
  "Return t if `delete-selection-mode' or `cua-delete-selection' is enabled."
  (or (and (boundp 'delete-selection-mode) delete-selection-mode)
      (and (boundp 'cua-delete-selection) cua-delete-selection cua-mode)))

(defun sp--cua-replace-region (&optional arg)
  "If `smartparens-mode' is on, emulate `self-insert-command',
else call `cua-replace-region'"
  (interactive "p")
  (setq this-original-command 'self-insert-command)
  (if smartparens-mode
      (self-insert-command (or arg 1))
    (cua-replace-region)))

(defun sp--init-delete-selection-mode-emulation ()
  "Initialize smartparens delete selection emulation.  The
original hooks are removed and handled by sp's pre-command
handler."
  ;; make sure the `delete-selection-pre-hook' is not active and that
  ;; delsel is actually loaded.  We need the delete-selection-pre-hook
  ;; command!
  (when delete-selection-mode
    (remove-hook 'pre-command-hook 'delete-selection-pre-hook))
  ;; if cua-mode is active, replace the `self-insert-command' binding
  ;; and the cua--pre-command-handler hook.
  (when cua-mode
    (define-key cua--region-keymap [remap self-insert-command] 'sp--cua-replace-region)
    (remove-hook 'pre-command-hook 'cua--pre-command-handler)))

(defadvice cua-mode (after cua-mode-fix-selection activate)
  (when (and cua-mode)
    (define-key cua--region-keymap [remap self-insert-command] 'sp--cua-replace-region)
    (remove-hook 'pre-command-hook 'cua--pre-command-handler)))

(defadvice delete-selection-mode (after delete-selection-mode-fix-selection activate)
  (when (and delete-selection-mode)
    (remove-hook 'pre-command-hook 'delete-selection-pre-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc functions

(defmacro !delete (elm list)
  "Destructive: Set LIST to (delete ELM LIST)."
  `(setq ,list (delete ,elm ,list)))

(defmacro !cddr (list)
  "Destructive: Set LIST to the cdr of LIST."
  `(setq ,list (cddr ,list)))

(defmacro sp-with-modes (arg &rest forms)
  "Add ARG as first argument to each form in FORMS.

This can be used with `sp-local-pair' calls to automatically
insert the modes."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (form) (append (list (car form) arg) (cdr form))) forms)))

(font-lock-add-keywords 'emacs-lisp-mode '(("\\<sp-with-modes\\>" . font-lock-keyword-face)) 'append)

(defmacro --last (form list)
  "Anaphoric form of `-last'."
  (let ((n (make-symbol "needle")))
    `(let (,n)
       (--each ,list
         (when ,form (setq ,n it)))
       ,n)))

(defun -last (pred list)
  "Return the last x in LIST where (PRED x) is non-nil, else nil."
  (--last (funcall pred it) list))

(defun sp--reverse-string (str)
  "Reverse the string STR."
  (concat (reverse (append str nil))))

(defun sp-point-in-string (&optional p)
  "Return t if point is inside string or documentation string.

If optional argument P is present test this instead of point."
  (ignore-errors
    (save-excursion
      (nth 3 (syntax-ppss p)))))

(defun sp-point-in-comment (&optional p)
  "Return t if point is inside comment.

If optional argument P is present test this instead off point."
  (setq p (or p (point)))
  (ignore-errors
    (save-excursion
      (or (nth 4 (syntax-ppss p))
          ;; this also test opening and closing comment delimiters... we
          ;; need to chack that it is not newline, which is in "comment
          ;; ender" class in elisp-mode, but we just want it to be
          ;; treated as whitespace
          (and (< p (point-max))
               (memq (char-syntax (char-after p)) '(?< ?>))
               (not (eq (char-after p) ?\n)))))))

(defun sp-point-in-string-or-comment (&optional p)
  "Return t if point is inside string, documentation string or a comment.

If optional argument P is present, test this instead of point."
  (or (sp-point-in-string p)
      (sp-point-in-comment p)))

(defun sp--single-key-description (event)
  "Return a description of the last event.  Replace all the function
key symbols with garbage character (ň).

TODO: fix this!"
  (let ((original (single-key-description event)))
    (cond
     ((string-match-p "<.*?>" original) "ň")
     ((string-match-p "SPC" original) " ")
     (t original))))

(defun sp--split-string (string by)
  "Split STRING on BY.  This simply calls `split-string' and if it
returns a list of length one, empty string is inserted to the
beginning."
  (let ((sp (split-string string by)))
    (if (not (cdr sp)) (cons "" sp) sp)))

(defun sp--this-command-self-insert-p ()
  "Return t if `this-command' is some sort of `self-insert-command'."
  (memq this-original-command '(self-insert-command
                                org-self-insert-command
                                sp--self-insert-command)))

(defun sp--signum (x)
  "Return 1 if X is positive, -1 if negative, 0 if zero."
  (cond ((> x 0) 1) ((< x 0) -1) (t 0)))

(eval-when (compile eval load)
  (defun sp--get-substitute (keyword-list struct list)
    "Only ever call this from sp-get!  This function do the
replacement of all the keywords with actual calls to sp-get."
    (if (listp list)
        (mapcar (lambda (x) (sp--get-substitute keyword-list struct x)) list)
      (if (memq list keyword-list)
          `(sp-get ,struct ,list)
        list))))

;; The structure returned by sp-get-sexp is a plist with following properties:
;;
;; :beg    - point in the buffer before the opening delimiter (ignoring prefix)
;; :end    - point in the buffer after the closing delimiter
;; :op     - opening delimiter
;; :cl     - closing delimiter
;; :prefix - expression prefix
;;
;; This structure should never be accessed directly and should only be
;; exposed by the sp-get macro.  This way, we can later change the
;; internal representation without much trouble.

(defmacro sp-get (struct attr)
  "Get a property from a structure.

STRUCT is a plist with the format as returned by `sp-get-sexp'.
Which means this macro also works with `sp-get-symbol',
`sp-get-string' and `sp-get-thing'.

ATTR is an attribute we want to query.  Currently supported
attributes are:

:beg       - point in buffer before the opening delimiter
:end       - point in the buffer after the closing delimiter
:beg-in    - point in buffer after the opening delimiter
:end-in    - point in buffer before the closing delimiter
:beg-prf   - point in buffer before the prefix of this expression
:op        - opening delimiter
:cl        - closing delimiter
:op-l      - length of the opening pair
:cl-l      - length of the closing pair
:len       - length of the entire expression, including enclosing
delimiters and the prefix
:len-out   - length of the the pair ignoring the prefix, including
delimiters
:len-in    - length of the pair inside the delimiters
:prefix    - expression prefix
:prefix-l  - expression prefix length

In addition to simple queries, this macro understands arbitrary
forms where any of the aforementioned attributes are used.
Therefore, you can for example query for
\"(+ :op-l :cl-l)\".  This query would return the sum of lengths
of opening and closing delimiter.  A query
\"(concat :prefix :op)\" would return the string containing
expression prefix and the opening delimiter.

This replacement is considered any time when the ATTR argument is
a list and not a single keyword."
  (let ((keyword-list '(:beg :end :beg-in :end-in :beg-prf
                             :op :cl :op-l :cl-l :len :len-out :len-in
                             :prefix :prefix-l)))
    (cond
     ;; if the attr is a list, we replace all the tags with appropriate
     ;; calls to sp-get. Example: (sp-get ok (- :end :beg))
     ((listp attr)
      (sp--get-substitute keyword-list struct attr))
     (t
      (case attr
        ;; point in buffer before the opening delimiter
        (:beg         `(plist-get ,struct :beg))
        ;; point in the buffer after the closing delimiter
        (:end         `(plist-get ,struct :end))
        ;; point in buffer after the opening delimiter
        (:beg-in      `(+ (plist-get ,struct :beg) (length (plist-get ,struct :op))))
        ;; point in buffer before the closing delimiter
        (:end-in      `(- (plist-get ,struct :end) (length (plist-get ,struct :cl))))
        ;; point in buffer before the prefix of this expression
        (:beg-prf     `(- (plist-get ,struct :beg) (length (plist-get, struct :prefix))))
        ;; opening delimiter
        (:op          `(plist-get ,struct :op))
        ;; closing delimiter
        (:cl          `(plist-get ,struct :cl))
        ;; length of the opening pair
        (:op-l        `(length (plist-get ,struct :op)))
        ;; length of the closing pair
        (:cl-l        `(length (plist-get ,struct :cl)))
        ;; length of the entire expression, including enclosing delimiters and the prefix
        (:len         `(- (plist-get ,struct :end)
                          (plist-get ,struct :beg)
                          (- (length (plist-get ,struct :prefix)))))
        ;; length of the the pair ignoring the prefix, including delimiters
        (:len-out     `(- (plist-get ,struct :end) (plist-get ,struct :beg)))
        ;; length of the pair inside the delimiters
        (:len-in      `(- (plist-get ,struct :end)
                          (plist-get ,struct :beg)
                          (length (plist-get ,struct :op))
                          (length (plist-get ,struct :cl))))
        ;; expression prefix
        (:prefix      `(plist-get ,struct :prefix))
        ;; expression prefix length
        (:prefix-l    `(length (plist-get ,struct :prefix))))))))

(defmacro sp--compare-sexps (a b &optional what)
  "Return non-nil if the expressions A and B are equal.

Two expressions are equal if their :beg property is the same.

If optional argument WHAT is non-nil, use it as a keyword on
which to do the comparsion."
  (setq what (or what :beg))
  `(equal (sp-get ,a ,what) (sp-get ,b ,what)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding/removing of pairs/bans/allows etc.

(defun sp--merge-prop (old-pair new-pair prop)
  "Merge a property PROP from NEW-PAIR into OLD-PAIR.
The list OLD-PAIR must not be nil."
  (let ((new-val (plist-get new-pair prop)))
    (case prop
      (:close (plist-put old-pair :close new-val))
      (:prefix (plist-put old-pair :prefix new-val))
      ((:actions :when :unless :pre-handlers :post-handlers)
       (case (car new-val)
         (:add (plist-put old-pair prop (-union (plist-get old-pair prop) (cdr new-val))))
         (:rem (plist-put old-pair prop (-difference (plist-get old-pair prop) (cdr new-val))))
         (t
          (cond
           ;; this means we have ((:add ...) (:rem ...)) argument
           ((and new-val
                 (listp (car new-val)))
            (let ((a (assq :add new-val))
                  (r (assq :rem new-val)))
              (plist-put old-pair prop (-union (plist-get old-pair prop) (cdr a)))
              (plist-put old-pair prop (-difference (plist-get old-pair prop) (cdr r)))))
           (t
            (plist-put old-pair prop (plist-get new-pair prop))))))))))

(defun sp--merge-pairs (old-pair new-pair)
  "Merge OLD-PAIR and NEW-PAIR.
This modifies the OLD-PAIR by side effect."
  (let ((ind 0))
    (--each new-pair
      (when (= 0 (% ind 2))
        (sp--merge-prop old-pair new-pair it))
      (setq ind (1+ ind))))
  old-pair)

(defun sp--update-pair (old-pair new-pair)
  "Copy properties from NEW-PAIR to OLD-PAIR.
The list OLD-PAIR must not be nil."
  (let ((ind 0))
    (--each new-pair
      (when (= 0 (% ind 2))
        (plist-put old-pair it (plist-get new-pair it)))
      (setq ind (1+ ind))))
  old-pair)

(defun sp--update-pair-list (pair mode)
  "Update the PAIR for major mode MODE.  If this pair is not
defined yet for this major mode, add it.  If this pair is already
defined, replace all the properties in the old definition with
values from PAIR."
  ;; get the structure relevant to mode.  t means global setting
  (let ((struct (--first (eq mode (car it)) sp-pairs)))
    (if (not struct)
        (!cons (cons mode (list pair)) sp-pairs)
      ;; this does NOT merge changes, only replace the values at
      ;; properties.  Open delimiter works as ID as usual.
      (let ((old-pair (--first (equal (plist-get pair :open)
                                      (plist-get it :open))
                               (cdr struct))))
        (if (not old-pair)
            (setcdr struct (cons pair (cdr struct)))
          (sp--update-pair old-pair pair)))))
  sp-pairs)

(defun sp--get-pair (open list)
  "Get the pair with id OPEN from list LIST."
  (--first (equal open (plist-get it :open)) list))

(defun sp--get-pair-definition (open list &optional prop)
  "Get the definition of a pair identified by OPEN from list LIST.

If PROP is non-nil, return the value of that property instead."
  (let ((pair (sp--get-pair open list)))
    (if prop (plist-get pair prop) pair)))

(defun sp-get-pair-definition (open mode &optional prop)
  "Get the definition of pair identified by OPEN (opening
delimiter) for major mode MODE (or global definition if MODE is
t).

If PROP is non-nil, return the value of that property instead."
  (sp--get-pair-definition open (cdr (assq mode sp-pairs)) prop))

(defun sp-get-pair (open &optional prop)
  "Return the current value of pair defined by OPEN in the
current buffer, querying the variable `sp-local-pairs'.

If PROP is non-nil, return the value of that property instead."
  (sp--get-pair-definition open sp-local-pairs prop))

(defun sp--merge-with-local (mode)
  "Merge the global pairs definitions with definitions for major mode MODE."
  (let* ((global (cdr (assq t sp-pairs)))
         (local (cdr (assq mode sp-pairs)))
         (result nil))
    ;; copy the pairs on global list first.  This creates new plists
    ;; so we can modify them without changing the global "template"
    ;; values.
    (dolist (old-pair global)
      (!cons (list :open (plist-get old-pair :open)) result))

    ;; merge the global list with result.  This basically "deep copy"
    ;; global list.  We use `sp--merge-pairs' because it also clones
    ;; the list properties (actions, filters etc.)
    (dolist (new-pair global)
      (let ((old-pair (sp--get-pair (plist-get new-pair :open) result)))
        (sp--merge-pairs old-pair new-pair)))

    ;; for each local pair, merge it into the global definition
    (dolist (new-pair local)
      (let ((old-pair (sp--get-pair (plist-get new-pair :open) result)))
        (if old-pair
            (sp--merge-pairs old-pair new-pair)
          ;; pair does not have global definition, simply copy it over
          (!cons
           ;; this "deep copy" the new-pair
           (sp--merge-pairs (list :open (plist-get new-pair :open)) new-pair)
           ;; TODO: remove the nil lists from the definitions
           result))))
    result))

(defun sp--generate-wrapping-function (pair binding keymap)
  (let ((fun-name (intern (concat "sp---wrap-with-" (mapconcat 'int-to-string (string-to-list pair) "-")))))
    (fset fun-name `(lambda (&optional arg)
                      ,(concat "Wrap the following expression with pair \"\\=" pair "\".

If ARG is positive N, wrap N following expressions.  If ARG is
negative -N, wrap N preceeding expressions.")
                      (interactive "p")
                      (sp-select-next-thing-exchange arg)
                      (execute-kbd-macro (kbd ,pair))))
    (define-key keymap (read-kbd-macro binding) fun-name)))

(defun* sp-pair (open
                 close
                 &key
                 (actions '(wrap insert))
                 when
                 unless
                 pre-handlers
                 post-handlers
                 bind)
  "Add a pair definition.

OPEN is the opening delimiter.  Every pair is uniquely determined
by this string.

CLOSE is the closing delimiter.  You can use nil for this
argument if you are updating an existing definition.  In this
case, the old value is retained.

ACTIONS is a list of actions that smartparens will perform with
this pair.  Possible values are:

- insert  - autoinsert the closing pair when opening pair is
  typed.
- wrap    - wrap an active region with the pair defined by opening
  delimiter if this is typed while region is active.

If the ACTIONS argument has value :rem, the pair is removed.
This can be used to remove default pairs you don't want to use.
For example: (sp-pair \"[\" nil :actions :rem)

WHEN is a list of predicates that test whether the action
should be performed in current context.  The values in the list
should be names of the predicates (that is symbols, not
lambdas!).  They should accept three arguments: opening
delimiter (which uniquely determines the pair), action and
context.  The context argument can have values:

- string  - if point is inside string or comment.
- code    - if point is inside code.  This context is only
  recognized in programming modes that define string semantics.

If *any* filter returns t, the action WILL be performed.

UNLESS is a list of predicates.  The conventions are the same as
for the WHEN list.  If *any* filter on this list returns t, the
action WILL NOT be performed.  The predicates in the WHEN list
are checked first, and if any of them succeeds, the UNLESS list
is not checked.

Note: the functions on the WHEN/UNLESS lists are also called
\"filters\" in the documentation.

All the filters are run *after* the trigger character is
inserted.

PRE-HANDLERS is a list of functions that are called before there
has been some action caused by this pair.  The arguments are the
same as for filters.  Context is relative to the point *before*
the last inserted character.  Because of the nature of the
wrapping operation, this hook is not called if the action is
wrapping.

POST-HANDLERS is a list of functions that are called after there
has been some action caused by this pair.  The arguments are the
same as for filters.  Context is relative to current position of
point *after* the closing pair was inserted.

After a wrapping action, the point might end on either side of
the wrapped region, depending on the original direction.  You can
use the variable `sp-last-wrapped-region' to retrieve information
about the wrapped region and position the point to suit your
needs.

BIND is a key binding to which a \"wrapping\" action will be
bound.  This function will be generated on the fly by
smartparens, using name
\"sp---wrap-with-<ASCII-OF-CHAR1>-<ASCII-OF-CHAR2>-...\".  The
binding will be added to global keymap.  When executed, it will
wrap ARG (default 1) expressions with this pair (like
`paredit-wrap-round' and friends)."
  (if (eq actions :rem)
      (let ((remove (concat
                     (sp-get-pair-definition open t :open)
                     (sp-get-pair-definition open t :close)))
            (global-list (assq t sp-pairs)))
        (setcdr global-list (--remove (equal (plist-get it :open) open) (cdr global-list)))
        (sp--update-trigger-keys remove))
    (let ((pair nil))
      (setq pair (plist-put pair :open open))
      (when close (plist-put pair :close close))
      (dolist (arg '((:actions . actions)
                     (:when . when)
                     (:unless . unless)
                     (:pre-handlers . pre-handlers)
                     (:post-handlers . post-handlers)))
        ;; We only consider "nil" as a proper value if the property
        ;; already exists in the pair.  In that case, we will set it to
        ;; nil.  This allows for removing properties in global
        ;; definitions.
        (when (or (eval (cdr arg))
                  (sp-get-pair-definition open t (car arg)))
          (plist-put pair (car arg) (eval (cdr arg)))))
      (sp--update-pair-list pair t))
    (sp--update-trigger-keys)
    (when bind (sp--generate-wrapping-function open bind global-map)))
  (sp--update-local-pairs-everywhere)
  sp-pairs)

(defun* sp-local-pair (modes
                       open
                       close
                       &key
                       (actions '(:add))
                       (when '(:add))
                       (unless '(:add))
                       (pre-handlers '(:add))
                       (post-handlers '(:add))
                       bind
                       prefix)
  "Add a local pair definition or override a global definition.

MODES can be a single mode or a list of modes where these settings
should be applied.

PREFIX is a regular expression matching an optional prefix for
this pair in the specified major modes.  If not specified, the
characters of expression prefix syntax class are automatically
considered instead.  This can be used to attach custom prefixes
to pairs, such as prefix \"\\function\" in \\function{arg} in
`LaTeX-mode'.

The rest of the arguments have same semantics as in `sp-pair'.

If the pair is not defined globally, ACTIONS defaults to (wrap
insert) instead of (:add) (which inherits global settings)

The pairs are uniquely identified by the opening delimiter.  If you
replace the closing one with a different string in the local
definition, this will override the global closing delimiter.

The list arguments can optionally be of form starting with
\":add\" or \":rem\" when these mean \"add to the global list\"
and \"remove from the global list\" respectivelly.  Otherwise,
the global list is replaced.  If you wish to both add and remove
things with single call, use \"((:add ...) (:rem ...))\" as an
argument.  Therefore,

  :when '(:add my-test)

would mean \"use the global settings for this pair, but also this
additional test\". If no value is provided for list arguments,
they default to \"(:add)\" which means they inherit the list from
the global definition.

To disable a pair in a major mode, simply set its actions set to
nil. This will ensure the pair is not even loaded when the mode is
activated.

If BIND is non-nil, the bindings are added into major mode keymap
called \"foo-mode-map\".  If the mode does not follow this
convention, you will need to bind the function manually.  The
bindings are not added into `smartparens-mode-map' to prevent
clashes between different modes."
  (if (eq actions :rem)
      (let ((remove ""))
        (dolist (m (-flatten (list modes)))
          (setq remove (concat remove
                               (sp-get-pair-definition open m :open)
                               (sp-get-pair-definition open m :close)))
          (let ((mode-pairs (assq m sp-pairs)))
            (setcdr mode-pairs
                    (--remove (equal (plist-get it :open) open)
                              (cdr mode-pairs)))))
        (sp--update-trigger-keys remove))
    (dolist (m (-flatten (list modes)))
      (let* ((pair nil))
        (setq pair (plist-put pair :open open))
        (when close (plist-put pair :close close))
        (when prefix (plist-put pair :prefix prefix))
        (when (and (not (sp-get-pair-definition open t))
                   (equal actions '(:add)))
          (setq actions '(wrap insert)))
        (plist-put pair :actions actions)
        (plist-put pair :when when)
        (plist-put pair :unless unless)
        (plist-put pair :pre-handlers pre-handlers)
        (plist-put pair :post-handlers post-handlers)
        (sp--update-pair-list pair m)
        (ignore-errors
          (when bind (sp--generate-wrapping-function
                      open bind
                      (symbol-value (intern (concat (symbol-name m) "-map"))))))))
    (sp--update-trigger-keys))
  (sp--update-local-pairs-everywhere (-flatten (list modes)))
  sp-pairs)

(defun* sp-local-tag (modes trig open close &key
                            (transform 'identity)
                            (actions '(wrap insert))
                            post-handlers)
  "Add a tag definition.

MODES is a mode or a list of modes where this tag should
activate.  It is impossible to define global tags.

TRIG is the trigger sequence.  It can be a string of any length.
If more triggers share a common prefix, the shortest trigger is
executed.

OPEN is the format of the opening tag.  This is inserted before
the active region.

CLOSE is the format of the closing tag.  This is inserted after
the active region.

Opening and closing tags can optionally contain the _ character.

If the opening tag contains the _ character, after you type the
trigger, the region is wrapped with \"skeleton\" tags and a
special tag editing mode is entered.  The text you now type is
substituted for the _ character in the opening tag.

If the closing tag contains the _ character, the text from the
opening pair is mirrored to the closing pair and substituted for
the _ character.

TRANSFORM is a function name (symbol) that is called to perform a
transformation of the opening tag text before this is inserted to
the closing tag.  For example, in html tag it might simply select
the name of the tag and cut off the tag attributes (like
class/style etc.).  Defaults to identity.

ACTIONS is a list of actions this tag should support. Currently,
only \"wrap\" action is supported.  Usually, you don't need to
specify this argument.

POST-HANDLERS is a list of functions that are called after the
tag is inserted.  If the tag does contain the _ character, these
functions are called after the tag editing mode is exited.  Each
function on this list should accept two arguments: the trigger
string and the action."
  (dolist (mode (-flatten (list modes)))
    (let* ((tag-list (assq mode sp-tags))
           (tag (--first (equal trig (plist-get it :trigger)) (cdr tag-list)))
           (new-tag nil))
      (setq new-tag (plist-put new-tag :trigger trig))
      (plist-put new-tag :open open)
      (plist-put new-tag :close close)
      (when transform (plist-put new-tag :transform transform))
      (when actions (plist-put new-tag :actions actions))
      (when post-handlers (plist-put new-tag :post-handlers post-handlers))
      (if tag-list
          (if (not actions)
              (setcdr tag-list (--remove (equal trig (plist-get it :trigger)) (cdr tag-list)))
            (if (not tag)
                (setcdr tag-list (cons new-tag (cdr tag-list)))
              (sp--update-pair tag new-tag)))
        ;; mode doesn't exist
        (when actions
          (!cons (cons mode (list new-tag)) sp-tags)))))
  (sp--update-trigger-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overlay management

;; burlywood4
(defface sp-pair-overlay-face
  '((t (:inherit highlight)))
  "The face used to highlight pair overlays."
  :group 'smartparens)

(defface sp-wrap-overlay-face
  '((t (:inherit sp-pair-overlay-face)))
  "The face used to highlight wrap overlays."
  :group 'smartparens)

(defface sp-wrap-tag-overlay-face
  '((t (:inherit sp-pair-overlay-face)))
  "The face used to highlight wrap tag overlays."
  :group 'smartparens)

(defvar sp-pair-overlay-list '()
  "List of overlays used for tracking inserted pairs.

When a pair is inserted, an overlay is created over it.  When the
user starts typing the closing pair we will not insert it again.
If user leaves the overlay, it is canceled and the insertion
works again as usual.")
(make-variable-buffer-local 'sp-pair-overlay-list)

(defvar sp-wrap-overlays nil
  "Cons pair of wrap overlays.")
(make-variable-buffer-local 'sp-wrap-overlays)

(defvar sp-wrap-tag-overlays nil
  "Cons pair of tag wrap overlays.")
(make-variable-buffer-local 'sp-wrap-tag-overlays)

(defvar sp-pair-overlay-keymap (make-sparse-keymap)
  "Keymap for the pair overlays.")
(define-key sp-pair-overlay-keymap (kbd "C-g") 'sp-remove-active-pair-overlay)

(defvar sp-wrap-overlay-keymap (make-sparse-keymap)
  "Keymap for the wrap overlays.")
(define-key sp-wrap-overlay-keymap (kbd "C-g") 'sp-wrap-cancel)

(defvar sp-wrap-tag-overlay-keymap (make-sparse-keymap)
  "Keymap for the wrap tag overlays.")
(define-key sp-wrap-tag-overlay-keymap (kbd "C-g") 'sp-wrap-tag-done)
(define-key sp-wrap-tag-overlay-keymap (kbd "C-a") 'sp-wrap-tag-beginning)
(define-key sp-wrap-tag-overlay-keymap (kbd "C-e") 'sp-wrap-tag-end)

(defun sp--overlays-at (&optional pos)
  "Simple wrapper of `overlays-at' to get only overlays from
smartparens.  Smartparens functions must use this function
instead of `overlays-at' directly."
  (--filter (overlay-get it 'type) (overlays-at (or pos (point)))))

(defun sp--point-in-overlay-p (overlay)
  "Return t if point is in OVERLAY."
  (and (< (point) (overlay-end overlay))
       (> (point) (overlay-start overlay))))

(defun sp--get-overlay-length (overlay)
  "Compute the length of OVERLAY."
  (- (overlay-end overlay) (overlay-start overlay)))

(defun sp--get-active-overlay (&optional type)
  "Get active overlay.  Active overlay is the shortest overlay at
point.  Optional argument TYPE restrict overlays to only those
with given type."
  (let ((overlays (sp--overlays-at)))
    (when type
      (setq overlays (--filter (eq (overlay-get it 'type) type) overlays)))
    (cond
     ((not overlays) nil)
     ((not (cdr overlays)) (car overlays))
     (t
      (--reduce (if (< (sp--get-overlay-length it) (sp--get-overlay-length acc)) it acc) overlays)))))

(defun sp--pair-overlay-create (start end id)
  "Create an overlay over the currently inserted pair for
tracking the position of the point.  START and END are the
boundaries of the overlay, ID is the id of the pair."
  (let ((overlay (make-overlay start end nil nil t)))
    (overlay-put overlay 'priority 100)
    (overlay-put overlay 'keymap sp-pair-overlay-keymap)
    (overlay-put overlay 'pair-id id)
    (overlay-put overlay 'type 'pair)
    (!cons overlay sp-pair-overlay-list)
    (sp--pair-overlay-fix-highlight)
    (add-hook 'post-command-hook 'sp--pair-overlay-post-command-handler nil t)))

(defun sp-wrap-cancel (&optional can-delete)
  "Cancel the active wrapping.

If optional argument CAN-DELETE is non-nil the
`sp-last-inserted-characters' string is inserted at `sp-wrap-point'."
  (interactive)
  (let ((oleft (car sp-wrap-overlays))
        (oright (cdr sp-wrap-overlays)))
    ;; kill the insides of the "pair" if `delete-selection-mode'
    ;; emulation is enabled
    (when (and (sp--delete-selection-p) can-delete)
      (kill-region (overlay-end oleft) (overlay-start oright))
      (setq sp-wrap-point (overlay-start oleft)))
    (delete-region (overlay-start oleft) (overlay-end oleft))
    (delete-region (overlay-start oright) (overlay-end oright))
    (delete-overlay oleft)
    (delete-overlay oright)
    (setq sp-wrap-overlays nil)
    (setq sp-previous-point -1)

    (goto-char sp-wrap-point)
    (when can-delete
      (insert sp-last-inserted-characters))))

(defun sp--pair-overlay-fix-highlight ()
  "Fix highlighting of the pair overlays.  Only the active overlay
should be highlighted."
  (--each (sp--overlays-at) (overlay-put it 'face nil))
  (let* ((active (sp--get-active-overlay))
         (type (and active (overlay-get active 'type))))
    (if active
        (cond
         ((eq 'wrap-tag type)
          (when sp-highlight-wrap-tag-overlay
            (overlay-put active 'face 'sp-wrap-tag-overlay-face)))
         ((eq 'pair type)
          (when sp-highlight-pair-overlay
            (overlay-put active 'face 'sp-pair-overlay-face))))
      ;; edge case where we're at the end of active overlay.  If
      ;; there is a wrap-tag overlay, restore it's face
      (when sp-wrap-tag-overlays
        (overlay-put (car sp-wrap-tag-overlays) 'face 'sp-wrap-tag-overlay-face)))))

(defun sp--pair-overlay-post-command-handler ()
  "Remove all pair overlays that doesn't have point inside them,
are of zero length, or if point moved backwards."
  ;; if the point moved backwards, remove all overlays
  (if (and sp-cancel-autoskip-on-backward-movement
           (< (point) sp-previous-point))
      (dolist (o sp-pair-overlay-list) (sp--remove-overlay o))
    ;; else only remove the overlays where point is outside them or
    ;; their length is zero
    (dolist (o (--remove (and (sp--point-in-overlay-p it)
                              (> (sp--get-overlay-length it) 0))
                         sp-pair-overlay-list))
      (sp--remove-overlay o)))
  (when sp-pair-overlay-list
    (setq sp-previous-point (point))))

(defun sp-remove-active-pair-overlay ()
  "Deactivate the active overlay.  See `sp--get-active-overlay'."
  (interactive)
  (let ((active-overlay (sp--get-active-overlay 'pair)))
    (when active-overlay
      (sp--remove-overlay active-overlay))))

(defun sp--remove-overlay (overlay)
  "Remove OVERLAY."
  ;; if it's not a pair overlay, nothing happens here anyway
  (!delete overlay sp-pair-overlay-list)
  ;; if we have zero pair overlays, remove the post-command hook
  (when (not sp-pair-overlay-list)
    (remove-hook 'post-command-hook 'sp--pair-overlay-post-command-handler t)
    ;; this is only updated when sp--pair-overlay-post-command-handler
    ;; is active.  Therefore, we need to reset this to 1.  If not, newly
    ;; created overlay could be removed right after creation - if
    ;; sp-previous-point was greater than actual point
    (setq sp-previous-point -1))
  (delete-overlay overlay)
  (sp--pair-overlay-fix-highlight))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair insertion/deletion/skipping

(defun sp-in-string-p (id action context)
  "Return t if point is inside string or comment, nil otherwise."
  (eq context 'string))

(defun sp-in-code-p (id action context)
  "Return t if point is inside code, nil otherwise."
  (eq context 'code))

(defun sp-in-math-p (id action context)
  "Return t if point is inside code, nil otherwise."
  (when (functionp 'texmathp)
    (texmathp)))

(defun sp-point-after-word-p (id action context)
  "Return t if point is after a word, nil otherwise.

This predicate is only tested on \"insert\" action."
  (when (eq action 'insert)
    (save-excursion
      (backward-char 1)
      (looking-back "\\sw\\|\\s_"))))

(defun sp--do-action-p (id action &optional use-inside-string)
  "Return t if action ACTION can be performed with pair ID.

If ACTION is a list, return t if at least one action from the
list can be performed.

If USE-INSIDE-STRING is non-nil, use value of
`sp-point-inside-string' instead of testing with
`sp-point-in-string-or-comment'."
  (setq action (-flatten (list action)))
  (let* ((pair (sp-get-pair id))
         (actions (plist-get pair :actions))
         (when-l (plist-get pair :when))
         (unless-l (plist-get pair :unless))
         (in-string (if use-inside-string
                        ;; if we're not inside a string, we can still
                        ;; be inside a comment!
                        (or sp-point-inside-string (sp-point-in-comment))
                      (sp-point-in-string-or-comment)))
         (context (cond
                   (in-string 'string)
                   (t 'code)))
         a r)
    (while (and action (not r))
      (setq a (car action))
      (setq r (when (memq a actions)
                ;;(and (when-clause) (not (unless-clause)))
                (and (or (not when-l)
                         (ignore-errors (run-hook-with-args-until-success 'when-l id a context)))
                     (or (not unless-l)
                         (not (ignore-errors (run-hook-with-args-until-success 'unless-l id a context)))))))
      (!cdr action))
    r))

(defun sp--get-context (type)
  "Return the context constant.  TYPE is type of the handler."
  (let ((in-string (case type
                     (:pre-handlers
                      (save-excursion
                        (backward-char 1)
                        (sp-point-in-string-or-comment)))
                     (:post-handlers
                      (sp-point-in-string-or-comment)))))
    (if in-string 'string 'code)))

(defun sp--run-hook-with-args (id type action)
  "Run all the hooks for pair ID of type TYPE on action ACTION."
  (ignore-errors
    (let ((hook (sp-get-pair id type))
          (context (sp--get-context type)))
      (run-hook-with-args 'hook id action context))))

;; TODO: add a test for a symbol property that would tell this handler
;; not to re=set `sp-last-operation'. Useful for example in "macro
;; funcions" like `my-wrap-with-paren'.
(defun sp--post-command-hook-handler ()
  "Handle the situation after some command has executed."
  (when smartparens-mode
    ;; handle the wrap overlays
    (when sp-wrap-overlays
      (let* ((overlay (car sp-wrap-overlays))
             (start (overlay-start overlay))
             (end (overlay-end overlay))
             (p (point)))
        (when (or (< p sp-previous-point)
                  (> p end)
                  (< p start))
          (sp-wrap-cancel))))
    (when sp-wrap-overlays
      (setq sp-previous-point (point)))

    (unless (sp--this-command-self-insert-p)
      (setq sp-last-operation nil))

    ;; unless the last command was a self-insert, remove the
    ;; information about the last wrapped region.  It is only used
    ;; for: 1. deleting the wrapping immediately after the wrap,
    ;; 2. re-wrapping region immediatelly after a sucessful wrap.
    ;; Therefore,t he deletion should have no ill-effect.  If the
    ;; necessity will arise, we can add a different flag.
    (unless (sp--this-command-self-insert-p)
      (setq sp-last-wrapped-region nil))))

(defmacro sp--setaction (action &rest forms)
  `(if (not action)
       (setq action (progn ,@forms))
     (progn ,@forms)))

(defun sp--self-insert-command (arg)
  "This command is a wrapper around `self-insert-command'.

If the just-typed key is a possible trigger for any pair,
`self-insert-command' is called and the special behaviours are
handled in its advice provided by `smartparens-mode'.  If the
just-typed key is not a trigger, fall back to the commant that
would execute if smartparens-mode were disabled."
  (interactive "p")
  (if (and (member (sp--single-key-description last-command-event) sp-trigger-keys)
           (not buffer-read-only))
      (progn
        (setq this-command 'self-insert-command)
        (self-insert-command arg))
    (sp--call-fallback-command)))

(defun sp--call-fallback-command ()
  "Call the command bound to last key sequence as if SP were disabled."
  (let ((com (sp--keybinding-fallback
              (when buffer-read-only
                (single-key-description last-command-event))))
        (smartparens-mode nil))
    (when (and com (commandp com))
      (setq this-original-command com)
      (call-interactively com))))

(defadvice self-insert-command (around self-insert-command-adviced activate)
  (setq sp-point-inside-string (sp-point-in-string))

  ad-do-it

  (when smartparens-mode
    (setq sp-recent-keys (cons
                          (sp--single-key-description last-command-event)
                          (-take 19 sp-recent-keys)))
    (let (op action)
      (if (= 1 (ad-get-arg 0))
          (progn
            (setq op sp-last-operation)
            (cond
             ((region-active-p)
              (sp-wrap-region-init))
             (sp-wrap-overlays
              (sp-wrap-region))
             (t
              (sp--setaction action (sp-insert-pair))
              (sp--setaction action (sp-skip-closing-pair))
              ;; try to call the fallback function bound to this key.
              ;; That is a function that would normally run if SP was
              ;; inactive. TODO: should this be customizable?
              (when (not action)
                (let ((fb-fun (sp--keybinding-fallback)))
                  (when (and (not (eq fb-fun 'self-insert-command))
                             (lookup-key sp-keymap (vector last-command-event)))
                    (delete-char -1)
                    (sp--call-fallback-command)
                    (setq action t))))
              ;; if nothing happened, we just inserted a character, so
              ;; set the apropriate operation.  We also need to check
              ;; for `sp--self-insert-no-escape' not to overwrite
              ;; it.  See `sp-autoinsert-quote-if-followed-by-closing-pair'.
              (when (and (not action)
                         (not (eq sp-last-operation 'sp-self-insert-no-escape)))
                (setq sp-last-operation 'sp-self-insert))
              ;; if it was a quote, escape it
              (when (and (eq sp-last-operation 'sp-self-insert)
                         sp-point-inside-string
                         sp-autoescape-string-quote
                         (eq (preceding-char) ?\"))
                (save-excursion
                  (backward-char 1)
                  (insert sp-escape-char))))))
        (setq sp-last-operation 'sp-self-insert)))))

(defun sp--delete-selection-mode-handle (&optional from-wrap)
  "Call the original `delete-selection-pre-hook'."
  (if smartparens-mode
      (cond
       ;; try the cua-mode emulation with `cua-delete-selection'
       ((and (boundp 'cua-mode) cua-mode
             (or (not (sp--this-command-self-insert-p))
                 (not sp-autowrap-region)))
        ;; if sp-autowrap-region is disabled, we need to translate
        ;; `sp--cua-replace-region' back to `self-insert-command'
        ;; because this is *pre* command hook
        ;; TODO: why do we need sp-cua-replace-region?
        (when (and (not sp-autowrap-region)
                   (eq this-command 'sp--cua-replace-region))
          (setq this-command 'self-insert-command))
        (cua--pre-command-handler))
       ;; this handles the special case after `self-insert-command' if
       ;; `sp-autowrap-region' is t.
       ((and (boundp 'cua-mode) cua-mode from-wrap)
        (setq this-command this-original-command)
        (cua-replace-region))
       ;; if not self-insert, just run the hook from
       ;; `delete-selection-mode'
       ((and (boundp 'delete-selection-mode) delete-selection-mode
             (or from-wrap
                 (not sp-autowrap-region)
                 (not (sp--this-command-self-insert-p))))
        (delete-selection-pre-hook)))
    ;; this handles the callbacks properly if the smartparens mode is
    ;; disabled.  Smartparens-mode adds advices on cua-mode and
    ;; delete-selection-mode that automatically remove the callbacks
    (cond
     ((and (boundp 'cua-mode) cua-mode
           (not (member 'pre-command-hook 'cua--pre-command-handler)))
      (cua--pre-command-handler))
     ((and (boundp 'delete-selection-mode) delete-selection-mode
           (not (member 'pre-command-hook 'delete-selection-pre-hook)))
      (delete-selection-pre-hook)))))

(defun sp--pre-command-hook-handler ()
  "Main handler of pre-command-hook.

Handle the `delete-selection-mode' or `cua-delete-selection'
stuff here."
  (sp--delete-selection-mode-handle))

(defvar sp-recent-keys nil
  "Last 20 typed keys, registered via `self-insert-command'.")

(defun sp--get-recent-keys ()
  "Return 10 recent keys in reverse order (most recent first) as a string."
  (apply #'concat sp-recent-keys))

(defun sp--get-pair-list ()
  "Return all pairs that are recognized in this
`major-mode' and do not have same opening and closing delimiter.
This is used for navigation functions."
  (--filter (not (string= (car it) (cdr it))) sp-pair-list))

(defun sp--get-allowed-pair-list ()
  "Return all pairs that are recognized in this
`major-mode', do not have same opening and closing delimiter and
are allowed in the current context.  See also
`sp--get-pair-list'."
  (--filter (and (sp--do-action-p (car it) 'insert)
                 (not (equal (car it) (cdr it)))) sp-pair-list))

(defun sp--get-pair-list-wrap ()
  "Return the list of all pairs that can be used for wrapping."
  (--filter (sp--do-action-p (car it) 'wrap) sp-pair-list))

(defun* sp--get-opening-regexp (&optional (pair-list (sp--get-pair-list)))
  "Return regexp matching any opening pair."
  (regexp-opt (--map (car it) pair-list)))

(defun* sp--get-closing-regexp (&optional (pair-list (sp--get-pair-list)))
  "Return regexp matching any closing pair."
  (regexp-opt (--map (cdr it) pair-list)))

(defun* sp--get-allowed-regexp (&optional (pair-list (sp--get-allowed-pair-list)))
  "Return regexp matching any opening or closing
delimiter for any pair allowed in current context."
  (regexp-opt (--mapcat (list (car it) (cdr it)) pair-list)))

(defun sp--get-last-wraped-region (beg end open close)
  "Return `sp-get-sexp' style plist about the last wrapped region.

Note: this function does not retrieve the actual value of
`sp-last-wrapped-region', it merely construct the plist from the
provided values."
  (let ((b (make-marker))
        (e (make-marker)))
    (set-marker b beg)
    (set-marker e end)
    (set-marker-insertion-type e t)
    `(:beg ,b :end ,e :op ,open :cl ,close :prefix "")))

(defun sp-wrap-region-init ()
  "Initialize the region wrapping."
  (when sp-autowrap-region
    ;; if we can't possibly form a wrap, just insert the char and do
    ;; nothing.  If `sp--delete-selection-p' is true, run
    ;; `sp--delete-selection-mode-handle' with t that means it was
    ;; called from withing wrapping procedure
    (if (--none? (string-prefix-p (sp--single-key-description last-command-event) (car it)) (sp--get-pair-list-wrap))
        (let ((p (1- (point)))
              (m (mark)))
          ;; test if we can at least start a tag wrapping.  If not,
          ;; delete the region if apropriate
          (unless (sp-wrap-tag-region-init)
            (sp--delete-selection-mode-handle t)
            (when (and (sp--delete-selection-p)
                       (< m p)
                       (= (length (sp--single-key-description last-command-event)) 1))
              (insert (sp--single-key-description last-command-event)))))
      (let* ((p (1- (point))) ;; we want the point *before* the
             ;; insertion of the character
             (m (mark))
             (ostart (if (> p m) m p))
             (oend (if (> p m) p m))
             (last-keys (sp--get-recent-keys))
             ;;(last-keys "\"\"\"\"\"\"\"\"")
             (active-pair (--first (string-prefix-p (sp--reverse-string (car it)) last-keys) (sp--get-pair-list-wrap))))

        (deactivate-mark)
        ;; if we can wrap right away, do it without creating overlays,
        ;; we can save ourselves a lot of needless trouble :)
        (if active-pair
            (unless (sp-wrap-tag-region-init)
              (let* ((len (+ (length (car active-pair)) (length (cdr active-pair))))
                     (strbound))
                (if (< p m)
                    (progn
                      ;; we delete the opening here to determine the
                      ;; string bounds... not pretty, but there's
                      ;; probably no better solution.
                      (delete-char -1)
                      (setq strbound (sp-get-quoted-string-bounds))
                      ;; and insert it right back
                      (insert (car active-pair))
                      (save-excursion
                        (goto-char m)
                        (insert (cdr active-pair))))
                  (delete-char (- 1))
                  (setq strbound (sp-get-quoted-string-bounds))
                  (insert (cdr active-pair))
                  (goto-char m)
                  (insert (car active-pair))
                  (goto-char (+ len p)))
                (setq sp-last-operation 'sp-wrap-region)
                (setq sp-last-wrapped-region
                      (if (< p m)
                          (sp--get-last-wraped-region
                           p (+ len m -1)
                           (car active-pair) (cdr active-pair))
                        (sp--get-last-wraped-region
                         m (+ len p)
                         (car active-pair) (cdr active-pair))))
                ;; only autoescape "" pair, so it has to be one-char
                ;; length, therefore we can handle it here
                (when (and (equal (car active-pair) "\"")
                           (equal (cdr active-pair) "\""))
                  (sp--wrap-region-autoescape strbound))
                sp-last-wrapped-region)

              (sp--run-hook-with-args (car active-pair) :post-handlers 'wrap))

          ;; save the position and point so we can restore it on cancel.
          (setq sp-wrap-point p)
          (setq sp-wrap-mark m)

          ;; We need to remember what was removed in case wrap is
          ;; cancelled.  Then these characters are re-inserted.
          (setq sp-last-inserted-characters (sp--single-key-description last-command-event))

          ;; if point > mark, we need to remove the character at the end and
          ;; insert it to the front.
          (when (> p m)
            (delete-char (- 1))
            (goto-char ostart)
            (insert sp-last-inserted-characters)
            (setq oend (1+ oend)))

          (let* ((oleft (make-overlay ostart (1+ ostart) nil nil t))
                 (oright (make-overlay oend oend nil nil t)))

            ;; insert the possible pair into end overlay
            (let ((close-pair (cdr (--last (string-prefix-p
                                            sp-last-inserted-characters
                                            (car it))
                                           (sp--get-pair-list-wrap)))))
              (when close-pair
                (save-excursion
                  (goto-char oend)
                  (insert close-pair))))

            (setq sp-wrap-overlays (cons oleft oright))
            (when sp-highlight-wrap-overlay
              (overlay-put oleft 'face 'sp-wrap-overlay-face)
              (overlay-put oright 'face 'sp-wrap-overlay-face))
            (overlay-put oleft 'priority 100)
            (overlay-put oright 'priority 100)
            (overlay-put oleft 'keymap sp-wrap-overlay-keymap)
            (overlay-put oleft 'type 'wrap)

            (goto-char (1+ ostart))))))))

(defun sp-wrap-region ()
  "Wrap region."
  ;; this method is only called if there's an active region.  It should
  ;; never be called manually!
  (when sp-autowrap-region
    (let* ((oleft (car sp-wrap-overlays))
           (oright (cdr sp-wrap-overlays)))
      (setq sp-last-inserted-characters
            (concat sp-last-inserted-characters
                    (sp--single-key-description last-command-event)))
      (let* ((active-pair (--last (string-prefix-p
                                   sp-last-inserted-characters
                                   (car it))
                                  (sp--get-pair-list-wrap)))
             (open-pair (car active-pair))
             (close-pair (cdr active-pair)))

        ;; call sp-wrap-tag-region-init here.  See if we can extend the
        ;; current wrap-beginning into a tag
        (unless (sp-wrap-tag-region-init)
          ;; update the close pair
          (if close-pair
              (save-excursion
                (delete-region (overlay-start oright) (overlay-end oright))
                (goto-char (overlay-start oright))
                (insert close-pair))
            ;; if we don't have any, it means there is no way to
            ;; complete the pair...  abort
            (sp-wrap-cancel t))

          ;; we've completed a pairing!
          (when (equal sp-last-inserted-characters open-pair)
            (let ((s (overlay-start oleft))
                  (e (overlay-end oright))
                  (oplen (length open-pair))
                  (cplen (length close-pair)))
              (delete-overlay oleft)
              (delete-overlay oright)
              (setq sp-wrap-overlays nil)
              (setq sp-previous-point -1)
              (if (< sp-wrap-point sp-wrap-mark)
                  (goto-char (+ sp-wrap-point oplen))
                (goto-char (+ sp-wrap-point oplen cplen)))
              ;; update info for possible following delete
              (setq sp-last-operation 'sp-wrap-region)
              (setq sp-last-wrapped-region
                    (sp--get-last-wraped-region s e open-pair close-pair))

              (sp--run-hook-with-args open-pair :post-handlers 'wrap))))))))

(defun sp--get-active-tag (recent)
  "Return the first tag that matches its trigger to
the prefix of RECENT and is allowed in current mode.  Such a tag
should be unique."
  ;; extract all the triggers that are prefix of the "recent"
  ;; vector, then sort them by length and return the shortest one.
  (let* ((tag-list (assq major-mode sp-tags))
         (triggers-list (--map (plist-get it :trigger) (cdr tag-list)))
         (triggers (--filter (string-prefix-p recent it) triggers-list)))
    (setq triggers (sort triggers (lambda (x y) (< (length x) (length y)))))
    (when (car triggers)
      (--first (equal (car triggers) (plist-get it :trigger)) (cdr tag-list)))))

(defun sp-wrap-tag-region-init ()
  "Init a region wrapping with a tag pair.
This is called from `sp-wrap-region-init' or
`sp-wrap-region' (usually on failure) to see if the currently
entered \"wrap\" can be extended as a tag.  The tag always gets
priority from the regular wrap."
  (when sp-autowrap-region
    ;; we can either enter tagwrapping from already present wrap or
    ;; from nothing (if the wrap-init failed to find any usable wrap)
    ;; or at failure (the entered wrap doesn't match any pair)
    (if sp-wrap-overlays ;; called from within the wrap-mode
        (let* ((oleft (car sp-wrap-overlays))
               (oright (cdr sp-wrap-overlays))
               (active-tag (sp--get-active-tag sp-last-inserted-characters)))
          (when active-tag
            ;; if we've found a tag trigger, enter the tag editing mode
            (if (eq (length sp-last-inserted-characters) (length (plist-get active-tag :trigger)))
                (progn
                  (delete-region (overlay-start oright) (overlay-end oright))
                  (sp--wrap-tag-create-overlays active-tag
                                                (overlay-start oleft)
                                                (-
                                                 (overlay-start oright)
                                                 (sp--get-overlay-length oleft)))
                  (delete-overlay oleft)
                  (delete-overlay oright)
                  (setq sp-wrap-overlays nil)
                  (setq sp-previous-point -1)) ;; do we need this?
              t) ;; return t as it is possible to extend current wrap
            ;; into a tag insertion mode
            ))
      ;; here we need to look at the last inserted character
      (let* ((p (1- (point)))
             (m (mark))
             (ostart (if (> p m) m p))
             (oend (if (> p m) p m))
             (active-tag (sp--get-active-tag
                          (sp--single-key-description last-command-event))))
        (when active-tag
          (setq sp-last-inserted-characters (sp--single-key-description last-command-event))
          (setq sp-wrap-point p)
          (setq sp-wrap-mark m)
          (when (> p m)
            (delete-char (- 1))
            (goto-char ostart)
            (insert sp-last-inserted-characters)
            (setq oend (1+ oend)))
          (if (= 1 (length (plist-get active-tag :trigger)))
              ;; the tag is only 1 character long, we can enter
              ;; insertion mode right away
              ;; I don't know why it needs 1- here, but it does :D
              (sp--wrap-tag-create-overlays active-tag ostart (1- oend))
            ;; we don't have a wrap, but we can maybe start a tag
            ;; wrap.  So just init the wrapping overlays as usual, and
            ;; let `sp-wrap-region' handle it
            (let* ((oleft (make-overlay ostart (1+ ostart) nil nil t))
                   (oright (make-overlay oend oend nil nil t)))
              (setq sp-wrap-overlays (cons oleft oright))
              (when sp-highlight-wrap-overlay
                (overlay-put oleft 'face 'sp-wrap-overlay-face)
                (overlay-put oright 'face 'sp-wrap-overlay-face))
              (overlay-put oleft 'priority 100)
              (overlay-put oright 'priority 100)
              (overlay-put oleft 'keymap sp-wrap-overlay-keymap)
              (overlay-put oleft 'type 'wrap)
              (goto-char (1+ ostart)))))))))

(defun sp--wrap-tag-create-overlays (tag ostart oend)
  "Create the wrap tag overlays.

TAG is the tag definition from `sp-tags'.

OSTART is the start of the modified area, including the pair
trigger string.

OEND is the end of the modified area, that is the end of the
wrapped region, exluding any existing possible wrap."
  (let* ((tag-open (sp--split-string (plist-get tag :open) "_"))
         (tag-close (sp--split-string (plist-get tag :close) "_"))
         (o (apply #'+ (mapcar #'length tag-open)))
         (c (apply #'+ (mapcar #'length tag-close))))
    ;; setup the wrap pairs
    ;; opening one
    (goto-char ostart)
    (delete-char (length (plist-get tag :trigger)))
    (insert (apply #'concat tag-open))
    (backward-char (length (cadr tag-open)))

    ;; closing one
    (save-excursion
      (goto-char (+ oend o))
      (insert (apply #'concat tag-close)))

    (if (cdr (split-string (plist-get tag :open) "_"))
        (let ((oleft (make-overlay
                      (+ ostart (length (car tag-open)))
                      (+ ostart (length (car tag-open)))
                      nil nil t))
              (oright (make-overlay
                       (+ oend o (length (car tag-close)))
                       (+ oend o (length (car tag-close)))
                       nil nil t)))
          (setq sp-wrap-tag-overlays (cons oleft oright))
          (when sp-highlight-wrap-tag-overlay
            (overlay-put oleft 'face 'sp-wrap-tag-overlay-face)
            (overlay-put oright 'face 'sp-wrap-tag-overlay-face))
          (overlay-put oleft 'priority 100)
          (overlay-put oright 'priority 100)
          (overlay-put oleft 'keymap sp-wrap-tag-overlay-keymap)
          (overlay-put oleft 'type 'wrap-tag)
          (overlay-put oleft 'active-tag tag)
          (overlay-put oleft 'modification-hooks '(sp--wrap-tag-update))
          (overlay-put oleft 'insert-in-front-hooks '(sp--wrap-tag-update))
          (overlay-put oleft 'insert-behind-hooks '(sp--wrap-tag-update))
          (add-hook 'post-command-hook 'sp--wrap-tag-post-command-handler))
      ;; if the tag didn't have any substitution, that means we only
      ;; insert the "brackets" and not enter the tag-insertion mode.
      ;; Therefore we move the point to the original position, so it
      ;; behaves just like normal wrap
      (if (> sp-wrap-mark sp-wrap-point)
          (goto-char (+ sp-wrap-point o))
        (goto-char (+ sp-wrap-point o c)))
      (let ((post-handlers (plist-get tag :post-handlers)))
        (run-hook-with-args 'post-handlers (plist-get tag :trigger) 'wrap)))
    (setq sp-last-operation 'sp-wrap-tag)))

(defun sp--wrap-tag-update (overlay after? beg end &optional length)
  "Called after any modification inside wrap tag overlay."
  (let* ((oleft (car sp-wrap-tag-overlays))
         (oright (cdr sp-wrap-tag-overlays))
         (active-tag (overlay-get oleft 'active-tag))
         (transform (plist-get active-tag :transform))
         (open (buffer-substring (overlay-start oleft) (overlay-end oleft))))
    (when (string-match-p "_" (plist-get active-tag :close))
      (save-excursion
        (delete-region (overlay-start oright) (overlay-end oright))
        (goto-char (overlay-start oright))
        (insert (funcall transform open))))))

(defun sp--wrap-tag-post-command-handler ()
  "Terminate the tag insertion mode if the point jumps out of the tag overlay."
  (if (or (not sp-wrap-tag-overlays)
          (< (point) (overlay-start (car sp-wrap-tag-overlays)))
          (> (point) (overlay-end (car sp-wrap-tag-overlays))))
      (sp-wrap-tag-done)))

(defun sp-match-sgml-tags (tag)
  "Split the html tag TAG at the first space and return its name."
  (let* ((split (split-string tag " "))
         (close (car split)))
    close))

(defun sp-wrap-tag-beginning ()
  "Move point to the beginning of the wrap tag editation area."
  (interactive)
  (goto-char (overlay-start (car sp-wrap-tag-overlays))))

(defun sp-wrap-tag-end ()
  "Move point to the end of the wrap tag editation area."
  (interactive)
  (goto-char (overlay-end (car sp-wrap-tag-overlays))))

(defun sp-wrap-tag-done ()
  "Finish editing of tag."
  (interactive)
  (let* ((oleft (car sp-wrap-tag-overlays))
         (oright (cdr sp-wrap-tag-overlays))
         (active-tag (overlay-get oleft 'active-tag))
         (post-handlers (plist-get active-tag :post-handlers)))
    (delete-overlay oleft)
    (delete-overlay oright)
    (setq sp-wrap-tag-overlays nil)
    (remove-hook 'post-command-hook 'sp--wrap-tag-post-command-handler)
    (run-hook-with-args 'post-handlers (plist-get active-tag :trigger) 'wrap)))

(defun sp--wrap-region-autoescape (strbound)
  "If we wrap a region with \"\" quotes, and the whole region was
inside a string, automatically escape the enclosing quotes.  If
we wrap a region that wasn't a string, automatically quote any
string quotes inside it.

This is internal function and should be only called after a
wrapping."
  (when sp-autoescape-string-quote
    (let ((b (sp-get sp-last-wrapped-region :beg))
          (e (sp-get sp-last-wrapped-region :end))
          was-beg)
      (cond
       ((and strbound
             (> b (car strbound))
             (< e (cdr strbound)))
        ;; the wrapped region is inside a string, escape the enclosing
        ;; quotes
        (save-excursion
          (goto-char b)
          (insert sp-escape-char)
          (goto-char (1- e))
          (insert sp-escape-char))
        ;; update the sp-last-wrapped-region info to \" pair
        (setq sp-last-wrapped-region
              (sp--get-last-wraped-region b e "\\\"" "\\\"")))
       (t
        (setq was-beg (< (point) e))
        (goto-char b)
        (while (search-forward-regexp "\\([^\\]\\)\"" (1- e) t)
          (replace-match "\\1\\\\\"" t))
        (setq sp-last-wrapped-region
              (sp--get-last-wraped-region b e "\"" "\""))
        (if was-beg (goto-char (1+ b)) (goto-char e)))))))

(defun sp-insert-pair ()
  "Automatically insert the closing pair if it is allowed in current context.

You can disable this feature completely for all modes and all pairs by
setting `sp-autoinsert-pair' to nil.

You can globally disable insertion of closing pair if point is
followed by the matching opening pair.  It is disabled by
default.  See `sp-autoinsert-if-followed-by-same' for more info.

You can globally disable insertion of closing pair if point is
followed by word.  It is disabled by default.  See
`sp-autoinsert-if-followed-by-word' for more info."
  (let* ((last-keys (sp--get-recent-keys))
         ;; (last-keys "\"\"\"\"\"\"\"\"\"\"\"\"")
         ;; we go through all the opening pairs and compare them to
         ;; last-keys.  If the opair is a prefix of last-keys, insert
         ;; the closing pair
         (active-pair (--first (string-prefix-p (sp--reverse-string (car it)) last-keys) sp-pair-list))
         (open-pair (car active-pair))
         (close-pair (cdr active-pair)))
    ;; Test "repeat last wrap" here.  If we wrap a region and then
    ;; type in a pair, wrap again around the last active region.  This
    ;; should probably be tested in the `self-insert-command'
    ;; advice... but we're lazy :D
    (if (and sp-autowrap-region
             active-pair
             (sp--wrap-repeat-last active-pair))
        sp-last-operation
      (when (and sp-autoinsert-pair
                 active-pair
                 (not (eq sp-last-operation 'sp-skip-closing-pair))
                 (sp--do-action-p open-pair 'insert t)
                 (if sp-autoinsert-if-followed-by-word t
                   (or (= (point) (point-max))
                       (not (and (eq (char-syntax (following-char)) ?w)
                                 (not (eq (following-char) ?\'))))))
                 (if sp-autoinsert-quote-if-followed-by-closing-pair t
                   (if (and (eq (char-syntax (preceding-char)) ?\")
                            ;; this is called *after* the character is
                            ;; inserted.  Therefore, if we are not in string, it
                            ;; must have been closed just now
                            (not (sp-point-in-string)))
                       (let ((pattern (sp--get-closing-regexp)))
                         ;; If we simply insert closing ", we also
                         ;; don't want to escape it.  Therefore, we
                         ;; need to set `sp-last-operation'
                         ;; accordingly to be checked in
                         ;; `self-insert-command' advice.
                         (if (looking-at pattern)
                             (progn (setq sp-last-operation 'sp-self-insert-no-escape) nil)
                           t))
                     t))
                 (cond
                  ((eq sp-autoinsert-if-followed-by-same 0) t)
                  ((eq sp-autoinsert-if-followed-by-same 1)
                   (not (looking-at (regexp-quote open-pair))))
                  ((eq sp-autoinsert-if-followed-by-same 2)
                   (or (not (looking-at (regexp-quote open-pair)))
                       (and (equal open-pair close-pair)
                            (eq sp-last-operation 'sp-insert-pair)
                            (save-excursion
                              (backward-char 1)
                              (sp--looking-back (regexp-quote open-pair))))))
                  ((eq sp-autoinsert-if-followed-by-same 3)
                   (or (or (not (looking-at (regexp-quote open-pair)))
                           (and (equal open-pair close-pair)
                                (eq sp-last-operation 'sp-insert-pair)
                                (save-excursion
                                  (backward-char 1)
                                  (sp--looking-back (regexp-quote open-pair)))))
                       (not (equal open-pair close-pair)))))
                 (not (run-hook-with-args-until-success
                       'sp-autoinsert-inhibit-functions
                       open-pair
                       (or sp-point-inside-string (sp-point-in-comment)))))

        (sp--run-hook-with-args open-pair :pre-handlers 'insert)

        (insert close-pair)
        (backward-char (length close-pair))
        (sp--pair-overlay-create (- (point) (length open-pair))

                                 (+ (point) (length close-pair))
                                 open-pair)

        ;; we only autoescape if the pair is a single character string
        ;; delimiter.  More elaborate pairs are probably already
        ;; escaped.  We leave the responsibility to the user, since
        ;; it's not that common and the usecases might vary -> there's
        ;; no good "default" case.
        (when (and sp-autoescape-string-quote
                   sp-point-inside-string
                   (equal open-pair "\"")
                   (equal close-pair "\"")
                   (or (not (memq major-mode sp-autoescape-string-quote-if-empty))
                       ;; test if the string is empty here
                       (not (and (equal (char-after (1+ (point))) ?\")
                                 (equal (char-after (- (point) 2)) ?\")))))
          (save-excursion
            (backward-char 1)
            (insert sp-escape-char)
            (forward-char 1)
            (insert sp-escape-char))
          (overlay-put (sp--get-active-overlay 'pair) 'pair-id "\\\""))

        (sp--run-hook-with-args open-pair :post-handlers 'insert)
        (setq sp-recent-keys nil)
        (setq sp-last-operation 'sp-insert-pair)))))

(defun sp--wrap-repeat-last (active-pair)
  "If the last operation was a wrap and `sp-wrap-repeat-last' is
non-nil, repeat the wrapping with this pair around the last
active region."
  (unless (= 0 sp-wrap-repeat-last)
    (when sp-last-wrapped-region
      (let* ((b (sp-get sp-last-wrapped-region :beg))
             (e (sp-get sp-last-wrapped-region :end))
             (op (sp-get sp-last-wrapped-region :op))
             (oplen (length op))
             (cllen (sp-get sp-last-wrapped-region :cl-l))
             (acolen (length (car active-pair))))
        (when (and
               (cond
                ((= 1 sp-wrap-repeat-last)
                 (equal (car active-pair) op))
                ((= 2 sp-wrap-repeat-last) t))
               (memq sp-last-operation '(sp-self-insert sp-wrap-region))
               (or (= (point) (+ b oplen acolen))
                   (= (point) e)))
          ;; TODO: this does not handle escaping of "double quote", that
          ;; is if we repeat quote wrap after quote wrap.  I think it is
          ;; reasonable to assume this will never happen, or very very
          ;; rarely. (same goes for option 2)
          (delete-char (- acolen))
          (if (< (point) e)
              (progn (goto-char (+ b oplen))
                     (insert (car active-pair))
                     (goto-char (- e cllen))
                     (insert (cdr active-pair))
                     (setq sp-last-wrapped-region
                           (sp--get-last-wraped-region
                            (+ b oplen) (point)
                            (car active-pair) (cdr active-pair)))
                     (goto-char (+ b oplen acolen)))
            (goto-char b)
            (insert (car active-pair))
            (goto-char e)
            (insert (cdr active-pair))
            (setq sp-last-wrapped-region
                  (sp--get-last-wraped-region
                   b e (car active-pair) (cdr active-pair))))
          (setq sp-last-operation 'sp-wrap-region)
          (sp--run-hook-with-args (car active-pair) :post-handlers 'wrap)
          sp-last-operation)))))

(defun sp-skip-closing-pair ()
  "If point is inside an inserted pair, and the user only moved forward
with point (that is, only inserted text), if the closing pair is
typed, we shouldn't insert it again but skip forward.

For example, pressing ( is followed by inserting the pair (|).  If
we then type 'word' and follow by ), the result should be (word)|
instead of (word)|).

If the user moved backwards or outside the
pair, this behaviour is cancelled.  This behaviour can be globally
disabled by setting `sp-cancel-autoskip-on-backward-movement' to
nil.

This behaviour can be globally disabled by setting
`sp-autoskip-closing-pair' to nil."
  (when (and sp-autoskip-closing-pair
             sp-pair-overlay-list
             (sp--get-active-overlay 'pair))
    (let* ((overlay (sp--get-active-overlay 'pair))
           (open-pair (overlay-get overlay 'pair-id))
           (close-pair (cdr (assoc open-pair sp-pair-list)))
           ;; how many chars have we already typed
           (already-skipped (- (length close-pair) (- (overlay-end overlay) (point)))))
      ;; only if we're at the closing pair or inside it
      (when (>= already-skipped 0)
        ;; rest of yet-untyped close-pair
        (let ((close-pair-rest (substring close-pair already-skipped))
              (last last-command-event))
          (when (and (looking-at (regexp-quote close-pair-rest))
                     ;; start deletion only if point is not right
                     ;; after the opening pair *after* the potential
                     ;; closing character was inserted (if opening
                     ;; pair and closing pair are the same, it would
                     ;; delete it right after the insertion otherwise)
                     (> (- (point) (overlay-start overlay)) (length open-pair)))
            (if (equal (sp--single-key-description last) (substring close-pair-rest 0 1))
                (progn
                  (forward-char 1)
                  (delete-char (- 1))
                  (setq sp-last-operation 'sp-skip-closing-pair))
              ;; Charactar that is not part of the closing pair was
              ;; typed.  Only remove overlays if we're inside the
              ;; closing pair.  If we are at the beginning, we are
              ;; allowed to type other characters
              (when (> already-skipped 0)
                (dolist (o sp-pair-overlay-list) (sp--remove-overlay o))))))))))

(defun sp-delete-pair (&optional arg)
  "Automatically delete opening or closing pair, or both, depending on
position of point.

If the point is inside an empty pair, automatically delete both.  That
is, [(|) turns to [|, [\{|\} turns to [|.  Can be disabled by setting
`sp-autodelete-pair' to nil.

If the point is behind a closing pair or behind an opening pair delete
it as a whole.  That is, \{\}| turns to \{|, \{| turns to |.  Can be
disabled by setting `sp-autodelete-closing-pair' and
`sp-autodelete-opening-pair' to nil.

If the last operation was a wrap and `sp-autodelete-wrap' is
enabled, invoking this function will unwrap the expression, that
is remove the just added wrapping."
  ;; NOTE: Only use delete-char inside this function, so we
  ;; don't activate the advice recursively!

  ;; only activate if argument is 1 (this is 0-th argument of the
  ;; delete-backward-char), otherwise the user wants to delete
  ;; multiple character, so let him do that
  (when (and (= arg 1)
             smartparens-mode)
    (if (and sp-autodelete-wrap
             (eq sp-last-operation 'sp-wrap-region))
        (let ((p (point))
              (b (sp-get sp-last-wrapped-region :beg))
              (e (sp-get sp-last-wrapped-region :end))
              (o (sp-get sp-last-wrapped-region :op-l))
              (c (sp-get sp-last-wrapped-region :cl-l)))
          ;; if the last operation was `sp-wrap-region', and we are at
          ;; the position of either opening or closing pair, delete the
          ;; just-inserted pair
          (when (or (= p (+ b o))
                    (= p e))
            (insert "x") ;dummy char to account for the regularly deleted one
            (save-excursion
              (goto-char e)
              (delete-char (- c))
              (goto-char b)
              (delete-char o))
            (setq sp-last-operation 'sp-delete-pair-wrap)))
      (let ((p (point))
            (inside-pair (--first (and (sp--looking-back (regexp-quote (car it)))
                                       (looking-at (regexp-quote (cdr it))))
                                  sp-pair-list))
            (behind-pair (--first (sp--looking-back (regexp-quote (cdr it))) sp-pair-list))
            (opening-pair (--first (sp--looking-back (regexp-quote (car it))) sp-pair-list)))
        (cond
         ;; we're just before the closing quote of a string.  If there
         ;; is an opening or closing pair behind the point, remove
         ;; it.  This is only really relevant if the pair ends in the
         ;; same character as string quote.  We almost never want to
         ;; delete it as an autopair (it would "open up the string").
         ;; So, word\"|" and <backspace> should produce word\|" or
         ;; word|" (if \" is autopair) instead of word\|.
         ((and (sp-point-in-string)
               (not (sp-point-in-string (1+ p)))
               (sp-point-in-string (1- p))) ;; the string isn't empty
          (cond ;; oh, you ugly duplication :/
           ((and behind-pair sp-autodelete-closing-pair)
            (delete-char (- (1- (length (car behind-pair)))))
            (setq sp-last-operation 'sp-delete-pair-closing))
           ((and opening-pair sp-autodelete-opening-pair)
            (delete-char (- (1- (length (car opening-pair)))))
            (setq sp-last-operation 'sp-delete-pair-opening))))
         ;; we're inside a pair
         ((and inside-pair sp-autodelete-pair)
          (delete-char (length (cdr inside-pair)))
          (delete-char (- (1- (length (car inside-pair)))))
          (setq sp-last-operation 'sp-delete-pair))
         ;; we're behind a closing pair
         ((and behind-pair sp-autodelete-closing-pair)
          (delete-char (- (1- (length (cdr behind-pair)))))
          (setq sp-last-operation 'sp-delete-pair-closing))
         ;; we're behind an opening pair and there's no closing pair
         ((and opening-pair sp-autodelete-opening-pair)
          (delete-char (- (1- (length (car opening-pair)))))
          (setq sp-last-operation 'sp-delete-pair-opening)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation

(defun sp--looking-back (regexp &optional limit not-greedy)
  "Return non-nil if text before point matches regular expression REGEXP.

With optional argument LIMIT search only that many characters
backward.  If LIMIT is nil, default to `sp-max-pair-length-c'.

If optional argument NON-GREEDY is t search for any matching
sequence, not necessarily the longest possible."
  (setq limit (or limit sp-max-pair-length-c))
  (let ((from (max 1 (- (point) limit)))
        (to (point))
        (greedy (not not-greedy))
        has-match)
    (set-match-data '(0 0))
    (if greedy
        (save-excursion
          (goto-char from)
          (while (and (not has-match) (< (point) to))
            (looking-at regexp)
            (if (= (match-end 0) to)
                (setq has-match t)
              (forward-char 1)))
          has-match)
      (save-excursion
        (not (null (search-backward-regexp (concat "\\(?:" regexp "\\)\\=") from t)))))))

(defun sp--search-backward-regexp (regexp &optional bound noerror)
  "Works just like `search-backward-regexp', but returns the
longest possible match.  That means that searching for
\"defun|fun\" backwards would return \"defun\" instead of
\"fun\", which would be matched first.

This is an internal function.  Only use this for searching for
pairs!"
  (when (search-backward-regexp regexp bound noerror)
    (goto-char (match-end 0))
    (sp--looking-back regexp)
    (goto-char (match-beginning 0))))

(defmacro sp--get-bounds (name docstring test)
  "Generate a function called NAME that return the bounds of
object bounded by TEST."
  (declare (indent 1))
  `(defun ,name ()
     ,docstring
     (when ,test
       (let ((open (save-excursion
                     (while ,test
                       (forward-char -1))
                     (1+ (point))))
             (close (save-excursion
                      (while ,test
                        (forward-char 1))
                      (1- (point)))))
         (cons open close)))))

(sp--get-bounds sp-get-quoted-string-bounds
  "If the point is inside a quoted string, return its bounds."
  (nth 3 (syntax-ppss)))

(sp--get-bounds sp-get-comment-bounds
  "If the point is inside a comment, return its bounds."
  (or (sp-point-in-comment)
      (looking-at "[[:space:]]+;;")))

(defun sp--get-string-or-comment-bounds ()
  "Get the bounds of string or comment the point is in."
  (or (sp-get-quoted-string-bounds)
      (sp-get-comment-bounds)))

(defmacro sp--search-and-save-match (search-fn pattern bound res beg end str)
  "Save the last match info."
  `(progn
     (setq ,res (funcall ,search-fn ,pattern ,bound t))
     (when ,res
       (setq ,beg (match-beginning 0))
       (setq ,end (match-end 0))
       (setq ,str (match-string 0)))))

(defvar sp--lisp-modes '(emacs-lisp-mode inferior-emacs-lisp-mode lisp-interaction-mode scheme-mode lisp-mode eshell-mode slime-repl-mode clojure-mode common-lisp-mode)
  "List of Lisp modes.")

;; TODO: since this function is used for all the navigation, we should
;; optimaze it a lot! Get some elisp profiler! Also, we should split
;; this into smaller functions (esp. the "first expression search"
;; business)
(defun sp-get-paired-expression (&optional back)
  "Find the nearest balanced pair expression after point.

The expressions considered are those delimited by pairs on
`sp-pair-list'."
  (let* ((search-fn (if (not back) 'search-forward-regexp 'sp--search-backward-regexp))
         (pair-list (sp--get-pair-list))
         (in-string-or-comment (sp-point-in-string-or-comment))
         (string-bounds (and in-string-or-comment (sp--get-string-or-comment-bounds)))
         (fw-bound (if in-string-or-comment (cdr string-bounds) (point-max)))
         (bw-bound (if in-string-or-comment (car string-bounds) (point-min)))
         (s nil) (e nil) (active-pair nil) (forward nil) (failure nil)
         (mb nil) (me nil) (ms nil) (r nil) (done nil))
    (save-excursion
      (while (not done)
        ;; search for the first opening pair.  Here, only consider tags
        ;; that are allowed in the current context.
        (sp--search-and-save-match search-fn
                                   (sp--get-allowed-regexp)
                                   (if back bw-bound fw-bound)
                                   r mb me ms)
        (when (not (sp-point-in-string-or-comment))
          (setq in-string-or-comment nil))
        ;; if the point originally wasn't inside of a string or comment
        ;; but now is, jump out of the string/comment and only search
        ;; the code.  This ensures that the comments and strings are
        ;; skipped if we search inside code.
        (if (and (not in-string-or-comment)
                 (sp-point-in-string-or-comment))
            (let* ((bounds (sp--get-string-or-comment-bounds))
                   (jump-to (if back (1- (car bounds)) (1+ (cdr bounds)))))
              (goto-char jump-to))
          (setq done t)))
      (when r
        (setq active-pair (--first (equal ms (car it)) pair-list))
        (if active-pair
            (progn
              (setq forward t)
              (setq s mb)
              (when back
                (forward-char (length (car active-pair)))))
          (setq active-pair (--first (equal ms (cdr it)) pair-list))
          (setq e me)
          (when (not back)
            (backward-char (length (cdr active-pair)))))
        (let* ((open (if forward (car active-pair) (cdr active-pair)))
               (close (if forward (cdr active-pair) (car active-pair)))
               (needle (regexp-opt (list (car active-pair) (cdr active-pair))))
               (search-fn (if forward 'search-forward-regexp 'search-backward-regexp))
               (depth 1)
               (eof (if forward 'eobp 'bobp))
               (b (if forward fw-bound bw-bound)))
          (while (and (> depth 0) (not (funcall eof)))
            (sp--search-and-save-match search-fn needle b r mb me ms)
            (if r
                (unless (or (and (not in-string-or-comment)
                                 (sp-point-in-string-or-comment))
                            ;; we need to check if the match isn't
                            ;; preceded by escape sequence.  This is a
                            ;; bit tricky to do right, so for now we
                            ;; just handle emacs-lisp \ or ? escape
                            ;; prefix
                            (and (> mb 1)
                                 (member major-mode sp--lisp-modes)
                                 (member (buffer-substring (1- mb) mb) '("\\" "?"))))
                  (if (equal ms open)
                      (setq depth (1+ depth))
                    (setq depth (1- depth))))
              (unless (minibufferp)
                (message "Search failed.  This means there is unmatched expression somewhere or we are at the beginning/end of file."))
              (setq depth -1)
              (setq failure t)))
          (if forward
              (setq e me)
            (setq s mb))
          (unless failure
            (cond
             ((or (and (sp-point-in-string-or-comment s) (not (sp-point-in-string-or-comment e)))
                  (and (not (sp-point-in-string-or-comment s)) (sp-point-in-string-or-comment e)))
              (unless (minibufferp)
                (message "Opening or closing pair is inside a string or comment and matching pair is outside (or vice versa).  Ignored."))
              nil)
             (t
              (let* ((op (if forward open close))
                     (pref (sp-get-pair op :prefix)))
                (list :beg s
                      :end e
                      :op op
                      :cl (if forward close open)
                      :prefix (sp--get-prefix s pref)))))))))))

(defun sp-get-sexp (&optional back)
  "Find the nearest balanced expression that is after (before) point.

Search backward if BACK is non-nil.  This also means, if the
point is inside an expression, this expression is returned.

For the moment, this function (ignores) pairs where the opening and
closing pair is the same, as it is impossible to correctly
determine the opening/closing relation without keeping track of
the content of the entire buffer.

If the search starts outside a comment, all subsequent comments
are skipped.

If the search starts inside a string or comment, it tries to find
the first balanced expression that is completely contained inside
the string or comment.  If no such expression exist, a warning is
raised (for example, when you comment out imbalanced expression).
However, if you start a search from within a string and the next
complete sexp lies completely outside, this is returned.  Note
that this only works in modes where strings and comments are
properly defined via the syntax tables.

The return value is a plist with following keys:

  :beg    - point in the buffer before the opening
  delimiter (ignoring prefix)
  :end    - point in the buffer after the closing delimiter
  :op     - opening delimiter
  :cl     - closing delimiter
  :prefix - expression prefix

However, you should never access this structure directly as it is
subject to change.  Instead, use the macro `sp-get' which also
provide shortcuts for many commonly used queries (such as length
of opening/closing delimiter or prefix)."
  (cond
   (sp-prefix-tag-object
    (sp-get-sgml-tag back))
   (sp-prefix-pair-object
    (sp-get-paired-expression back))
   ((memq major-mode sp-navigate-consider-sgml-tags)
    (let ((paired (sp-get-paired-expression back)))
      (if (and paired
               (equal "<" (sp-get paired :op)))
          ;; if the point is inside the tag delimiter, return the pair.
          (if (sp-get paired (and (<= :beg-in (point)) (>= :end-in (point))))
              paired
            (sp-get-sgml-tag back))
        paired)))
   (t (sp-get-paired-expression back))))

(defun sp-get-enclosing-sexp (&optional arg)
  "Return the balanced expression that wraps point at the same level.

With ARG, ascend that many times.  This funciton expect positive
argument."
  (setq arg (or arg 1))
  (save-excursion
    (let ((n arg)
          (ok t)
          (okr))
      (while (and (> n 0) ok)
        (setq ok t)
        (setq okr nil)
        ;; if we are inside string, get the string bounds and "string
        ;; expression"
        (when (sp-point-in-string)
          (setq okr (sp-get-string)))
        ;; get the "normal" expression defined by pairs
        (let ((p (point)))
          (setq ok (sp-get-sexp))
          (cond
           ((and ok (= (sp-get ok :beg) p))
            (goto-char (sp-get ok :end))
            (setq n (1+ n)))
           ((and ok (< (sp-get ok :beg) p))
            (goto-char (sp-get ok :end)))
           (t
            (while (and ok (>= (sp-get ok :beg) p))
              (setq ok (sp-get-sexp))
              (when ok (goto-char (sp-get ok :end)))))))
        ;; if the pair expression is completely enclosed inside a
        ;; string, return the pair expression, otherwise return the
        ;; string expression
        (when okr
          (unless (and ok
                       (> (sp-get ok :beg) (sp-get okr :beg))
                       (< (sp-get ok :end) (sp-get okr :end)))
            (setq ok okr)
            (goto-char (sp-get ok :end))))
        (setq n (1- n)))
      ok)))

(defun sp-get-list-items (&optional lst)
  "Return the information about expressions inside LST.

LST should be a data structure in format as returned by
`sp-get-sexp'.

The return value is a list of such structures in order as they
occur inside LST describing each expression, with LST itself
prepended to the front.

If LST is nil, the list at point is used (that is the list
following point after `sp-backward-up-sexp' is called)."
  (let ((r nil))
    (save-excursion
      (unless lst
        (setq lst (sp-backward-up-sexp)))
      (when lst
        (goto-char (sp-get lst :beg-in))
        (while (< (point) (sp-get lst :end))
          (!cons (sp-forward-sexp) r))
        (cons lst (nreverse (cdr r)))))))

(defun* sp--get-prefix (&optional (p (point)) use-mode-regexp)
  "Get the prefix of EXPR. Prefix is any continuous sequence of
characters in \"expression prefix\" syntax class."
  (save-excursion
    (goto-char p)
    (if use-mode-regexp
        (when (looking-back use-mode-regexp)
          (substring-no-properties (match-string 0)))
      (skip-syntax-backward "'")
      (buffer-substring-no-properties (point) p))))

(defun sp-get-symbol (&optional back)
  "Find the nearest symbol that is after point, or before point if BACK is non-nil.

This also means, if the point is inside a symbol, this symbol is
returned.  Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'.

The return value is a plist with the same format as the value
returned by `sp-get-sexp'."
  (let (b e prefix)
    (save-excursion
      (if back
          (progn
            (sp-skip-backward-to-symbol)
            (sp-forward-symbol -1)
            (setq b (point))
            (sp-forward-symbol 1)
            (setq e (point)))
        (sp-skip-forward-to-symbol)
        (sp-forward-symbol 1)
        (setq e (point))
        (sp-forward-symbol -1)
        (setq b (point))))
    (list :beg b :end e :op "" :cl "" :prefix (sp--get-prefix b))))

(defun sp--get-string (bounds)
  "Return the `sp-get-sexp' format info about the string.

This function simply transforms BOUNDS, which is a cons (BEG
. END) into format compatible with `sp-get-sexp'."
  (list :beg (1- (car bounds))
        :end (1+ (cdr bounds))
        :op (char-to-string (char-after (cdr bounds)))
        :cl (char-to-string (char-after (cdr bounds)))
        :prefix ""))

(defun sp-get-string (&optional back)
  "Find the nearest string after point, or before if BACK is non-nil.

This also means if the point is inside a string, this string is
returned.  If there are another symbols between point and the
string, nil is returned.  That means that this funciton only
return non-nil if the string is the very next meaningful
expression.

The return value is a plist with the same format as the value
returned by `sp-get-sexp'."
  (let (b e)
    (if (sp-point-in-string)
        (let ((r (sp-get-quoted-string-bounds)))
          (sp--get-string r))
      (save-excursion
        (if back
            (sp-skip-backward-to-symbol)
          (sp-skip-forward-to-symbol))
        (let ((r (sp-get-quoted-string-bounds)))
          (when r (sp--get-string r)))))))

(defun sp-get-whitespace ()
  "Get the whitespace around point.

Whitespace here is defined as any of the characters: space, tab
and newline."
  (list :beg (save-excursion (skip-chars-backward " \t\n") (point))
        :end (save-excursion (skip-chars-forward " \t\n") (point))
        :op ""
        :cl ""
        :prefix ""))

(defun sp--sgml-get-tag-name (match)
  (let ((sub (if (equal "/" (substring match 1 2))
                 (substring match 2)
               (substring match 1))))
    (car (split-string sub "\\( \\|>\\)"))))

(defun sp--sgml-opening-p (tag)
  (not (equal "/" (substring tag 1 2))))

(defun sp-get-sgml-tag (&optional back)
  (save-excursion
    (let ((search-fn (if (not back) 'search-forward-regexp 'search-backward-regexp))
          tag tag-name needle
          open-start open-end
          close-start close-end)
      (when (funcall search-fn "</?.*?\\s-?.*?>" nil t)
        (setq tag (substring-no-properties (match-string 0)))
        (setq tag-name (sp--sgml-get-tag-name tag))
        (setq needle (concat "</?" tag-name))
        (let* ((forward (sp--sgml-opening-p tag))
               (search-fn (if forward 'search-forward-regexp 'search-backward-regexp))
               (depth 1))
          (save-excursion
            (if (not back)
                (progn
                  (setq open-end (point))
                  (search-backward-regexp "<" nil t)
                  (setq open-start (point)))
              (setq open-start (point))
              (search-forward-regexp ">" nil t)
              (setq open-end (point))))
          (cond
           ((and (not back) (not forward))
            (goto-char (match-beginning 0)))
           ((and back forward)
            (goto-char (match-end 0))))
          (while (> depth 0)
            (if (funcall search-fn needle nil t)
                (if (sp--sgml-opening-p (match-string 0))
                    (if forward (setq depth (1+ depth)) (setq depth (1- depth)))
                  (if forward (setq depth (1- depth)) (setq depth (1+ depth))))
              (setq depth -1)))
          (if (eq depth -1)
              (progn (message "Search failed. No matching tag found.") nil)
            (save-excursion
              (if forward
                  (progn
                    (setq close-start (match-beginning 0))
                    (search-forward-regexp ">" nil t)
                    (setq close-end (point)))
                (setq close-start (point))
                (search-forward-regexp ">" nil t)
                (setq close-end (point))))
            (let ((op (buffer-substring-no-properties open-start open-end))
                  (cl (buffer-substring-no-properties close-start close-end)))
              (list :beg (if forward open-start close-start)
                    :end (if forward close-end open-end)
                    :op (if forward op cl)
                    :cl (if forward cl op)
                    :prefix ""))))))))

(defvar sp-prefix-tag-object nil
  "If non-nil, only consider tags while searching for next thing.")

(defvar sp-prefix-pair-object nil
  "If non-nil, only consider pairs while searching for next thing.

Pairs are defined as expressions delimited by pairs from
`sp-pair-list'.")

(defvar sp-prefix-symbol-object nil
  "If non-nil, only consider symbols while searching for next thing.

Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'.")

(defun sp-prefix-tag-object (&optional arg)
  "Read the command and invoke it on the next tag object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-tag-object] \\[sp-forward-sexp]\" will move two tag
expressions forward, ignoring possible symbols or paired
expressions inbetween.

Tag object is anything delimited by sgml tag."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-tag-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-prefix-pair-object (&optional arg)
  "Read the command and invoke it on the next pair object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-pair-object] \\[sp-forward-sexp]\" will move two paired
expressions forward, ignoring possible symbols inbetween.

Pair object is anything delimited by pairs from `sp-pair-list'."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-pair-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-prefix-symbol-object (&optional arg)
  "Read the command and invoke it on the next pair object.

If you specify a regular emacs prefix argument this is passed to
the executed command.  Therefore, executing
\"\\[universal-argument] 2 \\[sp-prefix-symbol-object] \\[sp-forward-sexp]\" will move two symbols
forward, ignoring any structure.

Symbol is defined as a chunk of text recognized by
`sp-forward-symbol'."
  (interactive "P")
  (let* ((cmd (read-key-sequence "" t))
         (com (key-binding cmd))
         (sp-prefix-symbol-object t))
    (if (commandp com)
        (call-interactively com)
      (execute-kbd-macro cmd))))

(defun sp-get-thing (&optional back)
  "Find next thing after point, or before if BACK is non-nil.

Thing is either symbol (`sp-get-symbol'),
string (`sp-get-string') or balanced expression recognized by
`sp-get-sexp'.

If `sp-navigate-consider-symbols' is nil, only balanced
expressions are considered."
  (cond
   (sp-prefix-tag-object (sp-get-sgml-tag back))
   (sp-prefix-pair-object (sp-get-paired-expression back))
   (sp-prefix-symbol-object (sp-get-symbol back))
   (t
    (if back
        (if (not sp-navigate-consider-symbols)
            (sp-get-sexp t)
          (save-excursion
            (sp-skip-backward-to-symbol t)
            (cond
             ((when (looking-back ">") (sp-get-sgml-tag t)))
             ((sp--looking-back (sp--get-closing-regexp) nil t)
              (sp-get-sexp t))
             ((sp--looking-back (sp--get-opening-regexp) nil t)
              (sp-get-sexp t))
             ((eq (char-syntax (preceding-char)) 34)
              (sp-get-string t))
             (t (sp-get-symbol t)))))
      (if (not sp-navigate-consider-symbols)
          (sp-get-sexp nil)
        (save-excursion
          (sp-skip-forward-to-symbol t)
          (cond
           ((when (looking-at "<") (sp-get-sgml-tag nil)))
           ((looking-at (sp--get-opening-regexp))
            (sp-get-sexp nil))
           ((looking-at (sp--get-closing-regexp))
            (sp-get-sexp nil))
           ((eq (char-syntax (following-char)) 34)
            (sp-get-string nil))
           (t (sp-get-symbol nil)))))))))

(defun sp-forward-sexp (&optional arg)
  "Move forward across one balanced expression.

With ARG, do it that many times.  Negative arg -N means move
backward across N balanced expressions.  If there is no forward
expression, jump out of the current one (effectively doing
`sp-up-sexp').

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples: (prefix arg in comment)

  |(foo bar baz)   -> (foo bar baz)|

  (|foo bar baz)   -> (foo| bar baz)

  (|foo bar baz)   -> (foo bar| baz) ;; 2

  (foo (bar baz|)) -> (foo (bar baz)|)"
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (sp-backward-sexp (- arg))
    (let* ((n arg)
           (ok t))
      (while (and ok (> n 0))
        (setq ok (sp-get-thing))
        (setq n (1- n))
        (when ok (goto-char (sp-get ok :end))))
      ok)))

(defun sp-backward-sexp (&optional arg)
  "Move backward across one balanced expression (sexp).

With ARG, do it that many times.  Negative arg -N means move
forward across N balanced expressions.  If there is no previous
expression, jump out of the current one (effectively doing
`sp-backward-up-sexp').

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples: (prefix arg in comment)

  (foo bar baz)|   -> |(foo bar baz)

  (foo| bar baz)   -> (|foo bar baz)

  (foo bar| baz)   -> (|foo bar baz) ;; 2

  (|(foo bar) baz) -> ((|foo bar) baz)"
  (interactive "p")
  (setq arg (or arg 1))
  (if (< arg 0)
      (sp-forward-sexp (- arg))
    (let* ((n arg)
           (ok t))
      (while (and ok (> n 0))
        (setq ok (sp-get-thing t))
        (setq n (1- n))
        (when ok (goto-char (sp-get ok :beg))))
      ok)))

(defun sp-next-sexp (&optional arg)
  "Move forward to the beginning of next balanced expression.

With ARG, do it that many times.  If there is no next expression
at current level, jump one level up (effectively doing
`sp-backward-up-sexp').  Negative arg -N means move to the
beginning of N-th previous balanced expression.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples:

  ((foo) |bar (baz quux)) -> ((foo) bar |(baz quux))

  ((foo) bar |(baz quux)) -> |((foo) bar (baz quux))"
  (interactive "p")
  (setq arg (or arg 1))
  (if (> arg 0)
      (if (= arg 1)
          (let ((ok (sp-get-thing)))
            (when ok
              (if (= (point) (sp-get ok :beg))
                  (progn (sp-forward-sexp 2)
                         (sp-backward-sexp))
                (goto-char (sp-get ok :beg))
                ok)))
        (sp-forward-sexp arg)
        (sp-backward-sexp))
    (sp-backward-sexp (- arg))))

(defun sp-previous-sexp (&optional arg)
  "Move backward to the end of previous balanced expression.

With ARG, do it that many times.  If there is no next
expression at current level, jump one level up (effectively
doing `sp-up-sexp').  Negative arg -N means move to the end of
N-th following balanced expression.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions.

Examples:

  ((foo) bar| (baz quux)) -> ((foo)| bar (baz quux))

  ((foo)| bar (baz quux)) -> ((foo) bar (baz quux))|"
  (interactive "p")
  (setq arg (or arg 1))
  (if (> arg 0)
      (if (= arg 1)
          (let ((ok (sp-get-thing t)))
            (when ok
              (if (= (point) (sp-get ok :end))
                  (progn (sp-backward-sexp 2)
                         (sp-forward-sexp))
                (goto-char (sp-get ok :end))
                ok)))
        (sp-backward-sexp arg)
        (sp-forward-sexp))
    (sp-forward-sexp (- arg))))

(defun sp--raw-argument-p (arg)
  "Return t if ARG represents raw argument, that is a non-empty list."
  (and (listp arg) (car arg)))

(defun sp--negate-argument (arg)
  "Return the argument ARG but negated.

If the argument is a raw prefix argument (cons num nil) return a
list with its car negated.  If the argument is just the - symbol,
return 1.  If the argument is nil, return -1.  Otherwise negate
the input number."
  (cond
   ((sp--raw-argument-p arg) (list (- (car arg))))
   ((eq arg '-) 1)
   ((not arg) -1)
   (t (- arg))))

(defun sp-down-sexp (&optional arg)
  "Move forward down one level of sexp.

With ARG, do this that many times.  A negative argument -N means
move backward but still go down a level.

If ARG is raw prefix argument \\[universal-argument], descend forward as much as
possible.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument], jump to the beginning of
current list.

If the point is inside sexp and there is no down expression to
descend to, jump to the beginning of current one.  If moving
backwards, jump to end of current one.

Examples:

  |foo (bar (baz quux)) -> foo (|bar (baz quux))

  |foo (bar (baz quux)) -> foo (bar (|baz quux)) ;; 2

  |foo (bar (baz (quux) blab)) -> foo (bar (baz (|quux) blab)) ;; \\[universal-argument]

  (foo (bar baz) |quux) -> (|foo (bar baz) quux)

  (blab foo |(bar baz) quux) -> (|blab foo (bar baz) quux) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (last-point -1))
    (if (and raw (= (abs arg) 16))
        ;; jump to the beginning/end of current list
        (let ((enc (sp-get-enclosing-sexp)))
          (when enc
            (if (> arg 0)
                (goto-char (sp-get enc :beg-in))
              (goto-char (sp-get enc :end-in)))
            (setq ok enc)))
      ;; otherwise descend normally
      (while (and ok (> n 0))
        (setq ok (sp-get-sexp (< arg 0)))
        ;; if the prefix was C-u, we do not decrease n and instead set
        ;; it to -1 when (point) == "last ok"
        (if raw
            (when (= (point) last-point)
              (setq n -1))
          (setq n (1- n)))
        (when ok
          (setq last-point (point))
          (if (< arg 0)
              (goto-char (sp-get ok :end-in))
            (goto-char (sp-get ok :beg-in))))))
    ok))

(defun sp-backward-down-sexp (&optional arg)
  "Move backward down one level of sexp.

With ARG, do this that many times.  A negative argument -N means
move forward but still go down a level.

If ARG is raw prefix argument \\[universal-argument], descend backward as much as
possible.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument], jump to the end of current
list.

If the point is inside sexp and there is no down expression to
descend to, jump to the end of current one.  If moving forward,
jump to beginning of current one.

Examples:

  foo (bar (baz quux))| -> foo (bar (baz quux)|)

  (bar (baz quux)) foo| -> (bar (baz quux|)) foo ;; 2

  foo (bar (baz (quux) blab))| -> foo (bar (baz (quux|) blab)) ;; \\[universal-argument]

  (foo| (bar baz) quux) -> (foo (bar baz) quux|)

  (foo (bar baz) |quux blab) -> (foo (bar baz) quux blab|) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (sp-down-sexp (sp--negate-argument arg)))

(defun sp-beginning-of-sexp (&optional arg)
  "Jump to beginning of the sexp the point is in.

The beginning is the point after the opening delimiter.

With no argument, this is the same as calling
\\[universal-argument] \\[universal-argument] `sp-down-sexp'

With ARG positive N > 1, move forward out of the current
expression, move N-2 expressions forward and move down one level
into next expression.

With ARG negative -N < 1, move backward out of the current
expression, move N-1 expressions backward and move down one level
into next expression.

With ARG raw prefix argument \\[universal-argument] move out of the current expressions
and then to the beginning of enclosing expression.

Examples:

  (foo (bar baz) quux| (blab glob)) -> (|foo (bar baz) quux (blab glob))

  (foo (bar baz|) quux (blab glob)) -> (foo (|bar baz) quux (blab glob))

  (|foo) (bar) (baz quux) -> (foo) (bar) (|baz quux) ;; 3

  (foo bar) (baz) (quux|) -> (|foo bar) (baz) (quux) ;; -3

  ((foo bar) (baz |quux) blab) -> (|(foo bar) (baz quux) blab) ;; \\[universal-argument]"
  (interactive "P")
  (let ((raw (sp--raw-argument-p arg))
        (arg (prefix-numeric-value arg)))
    (cond
     ((and raw (= arg 4))
      (sp-up-sexp)
      (sp-beginning-of-sexp))
     ((= arg 1)
      (sp-down-sexp '(16)))
     ((< arg 0)
      (sp-backward-up-sexp)
      (sp-forward-sexp (1+ arg))
      (sp-down-sexp))
     ((> arg 0)
      (sp-up-sexp)
      (sp-forward-sexp (- arg 2))
      (sp-down-sexp)))))

(defun sp-end-of-sexp (&optional arg)
  "Jump to end of the sexp the point is in.

The end is the point before the closing delimiter.

With no argument, this is the same as calling
\\[universal-argument] \\[universal-argument] `sp-backward-down-sexp'.

With ARG positive N > 1, move forward out of the current
expression, move N-1 expressions forward and move down backward
one level into previous expression.

With ARG negative -N < 1, move backward out of the current
expression, move N-2 expressions backward and move down backward
one level into previous expression.

With ARG raw prefix argument \\[universal-argument] move out of the current expressions
and then to the end of enclosing expression.

Examples:

  (foo |(bar baz) quux (blab glob)) -> (foo (bar baz) quux (blab glob)|)

  (foo (|bar baz) quux (blab glob)) -> (foo (bar baz|) quux (blab glob))

  (|foo) (bar) (baz quux) -> (foo) (bar) (baz quux|) ;; 3

  (foo bar) (baz) (quux|) -> (foo bar|) (baz) (quux) ;; -3

  ((foo |bar) (baz quux) blab) -> ((foo bar) (baz quux) blab|) ;; \\[universal-argument]"
  (interactive "P")
  (let ((raw (sp--raw-argument-p arg))
        (arg (prefix-numeric-value arg)))
    (cond
     ((and raw (= arg 4))
      (sp-up-sexp)
      (sp-end-of-sexp))
     ((= arg 1)
      (sp-down-sexp '(-16)))
     ((< arg 0)
      (sp-backward-up-sexp)
      (sp-forward-sexp (+ 2 arg))
      (sp-backward-down-sexp))
     ((> arg 0)
      (sp-up-sexp)
      (sp-forward-sexp (1- arg))
      (sp-backward-down-sexp)))))

(defun sp-up-sexp (&optional arg interactive)
  "Move forward out of one level of parentheses.

With ARG, do this that many times.  A negative argument means
move backward but still to a less deep spot.

The argument INTERACTIVE is for internal use only.

If called interactively and `sp-navigate-reindent-after-up' is
non-nil, remove the whitespace between end of the expression and
the last \"thing\" inside the expression.

Examples:

  (foo |(bar baz) quux blab) -> (foo (bar baz) quux blab)|

  (foo (bar |baz) quux blab) -> (foo (bar baz) quux blab)| ;; 2

  (foo bar |baz              -> (foo bar baz)| ;; re-indent the expression
​   )

  (foo  |(bar baz)           -> (foo)| (bar baz) ;; close unbalanced expr."
  (interactive "p\np")
  (setq arg (or arg 1))
  (setq interactive (if (memq major-mode sp-navigate-consider-sgml-tags) nil interactive))
  (let ((ok (sp-get-enclosing-sexp (abs arg))))
    (if ok
        (progn
          (if (> arg 0)
              (goto-char (sp-get ok :end))
            (goto-char (sp-get ok :beg)))
          (when (and (= (abs arg) 1)
                     (or (eq sp-navigate-reindent-after-up 'always)
                         (and (eq sp-navigate-reindent-after-up 'interactive)
                              interactive)))
            ;; TODO: this needs different indent rules for different
            ;; modes.  Should we concern with such things?  Lisp rules are
            ;; funny in HTML... :/ For now, disable this in html-mode by
            ;; setting interactive to nil.
            (save-excursion
              (if (> arg 0)
                  (progn
                    (goto-char (sp-get ok :end-in))
                    (let ((prev (sp-get-thing t)))
                      ;; if the expression is empty remove everything inside
                      (if (sp--compare-sexps ok prev)
                          (delete-region (sp-get ok :beg-in) (sp-get ok :end-in))
                        (delete-region (sp-get prev :end) (point)))))
                (goto-char (sp-get ok :beg-in))
                (let ((next (sp-get-thing)))
                  (if (sp--compare-sexps ok next)
                      (delete-region (sp-get ok :beg-in) (sp-get ok :end-in))
                    (delete-region (point) (sp-get next :beg))))))))
      ;; on forward up, we can detect that the pair was not closed.
      ;; Therefore, jump sexps backwards until we hit the error, then
      ;; extract the opening pair and insert it at point.  Only works
      ;; for pairs defined in `sp-pair-list'.
      (when (and (> arg 0)
                 sp-navigate-close-if-unbalanced)
        (let (active-pair)
          (save-excursion
            (while (sp-backward-sexp))
            (sp-skip-backward-to-symbol t)
            (when (sp--looking-back (sp--get-opening-regexp))
              (let* ((op (match-string 0)))
                (setq active-pair (assoc op sp-pair-list)))))
          (when active-pair
            (sp-previous-sexp)
            (insert (cdr active-pair))))))
    ok))

(defun sp-backward-up-sexp (&optional arg interactive)
  "Move backward out of one level of parentheses.

With ARG, do this that many times.  A negative argument means
move forward but still to a less deep spot.

The argument INTERACTIVE is for internal use only.

If called interactively and `sp-navigate-reindent-after-up' is
non-nil, remove the whitespace between beginning of the
expression and the first \"thing\" inside the expression.

Examples:

  (foo (bar baz) quux| blab) -> |(foo (bar baz) quux blab)

  (foo (bar |baz) quux blab) -> |(foo (bar baz) quux blab) ;; 2

  (                  -> |(foo bar baz)
​    foo |bar baz)"
  (interactive "p\np")
  (setq arg (or arg 1))
  (sp-up-sexp (- arg) interactive))

(defun sp-kill-sexp (&optional arg dont-kill)
  "Kill the balanced expression following point.

If point is inside an expression and there is no following
expression, kill the topmost enclosing expression.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

With ARG being raw prefix \\[universal-argument], kill all the expressions from
point up until the end of current list.  With raw prefix \\[negative-argument] \\[universal-argument],
kill all the expressions from beginning of current list up until
point.  If point is inside a symbol, this is also killed.  If
there is no expression after/before the point, just delete the
whitespace up until the closing/opening delimiter.

With ARG being raw prefix \\[universal-argument] \\[universal-argument], kill current list (the list
point is inside).

If ARG is nil, default to 1 (kill single expression forward)

If second optional argument DONT-KILL is non-nil, save the to be
killed region in the kill ring, but do not kill the region from
buffer.

With `sp-navigate-consider-symbols', symbols and strings are also
considered balanced expressions.

Examples:

 (foo |(abc) bar)  -> (foo | bar) ;; nil, defaults to 1

 (foo (bar) | baz) -> |           ;; 2

 (foo |(bar) baz)  -> |           ;; \\[universal-argument] \\[universal-argument]

 (1 |2 3 4 5 6)    -> (1|)        ;; \\[universal-argument]

 (1 |2 3 4 5 6)    -> (1 | 5 6)   ;; 3

 (1 2 3 4 5| 6)    -> (1 2 3 | 6) ;; -2

 (1 2 3 4| 5 6)    -> (|5 6)      ;; - \\[universal-argument]

 (1 2 |   )        -> (1 2|)      ;; \\[universal-argument], kill useless whitespace

Note: prefix argument is shown after the example in
\"comment\". Assumes `sp-navigate-consider-symbols' equal to t."
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (ok t)
         (b (point-max))
         (e (point-min))
         (kill-fn (if dont-kill 'copy-region-as-kill 'kill-region)))
    (cond
     ;; kill to the end or beginning of list
     ((and raw
           (= n 4))
      (let ((next (sp-get-thing (< arg 0)))
            (enc (sp-get-enclosing-sexp)))
        (if (sp--compare-sexps next enc)
            (let ((del (sp-get-whitespace)))
              (sp-get del (delete-region :beg :end)))
          (if (> arg 0)
              (funcall kill-fn (sp-get next :beg-prf) (sp-get enc :end-in))
            (funcall kill-fn (sp-get next :end) (sp-get enc :beg-in)))
          (let ((del (sp-get-whitespace)))
            (sp-get del (delete-region :beg :end))))))
     ;; kill the enclosing list
     ((and raw
           (= n 16))
      (let ((lst (sp-backward-up-sexp)))
        (funcall kill-fn (sp-get lst :beg-prf) (sp-get lst :end))))
     ;; regular kill
     (t
      (save-excursion
        (while (and (> n 0) ok)
          (setq ok (sp-forward-sexp (sp--signum arg)))
          (when (< (sp-get ok :beg-prf) b) (setq b (sp-get ok :beg-prf)))
          (when (> (sp-get ok :end) e) (setq e (sp-get ok :end)))
          (setq n (1- n))))
      (when ok
        (let ((bm (set-marker (make-marker) b)))
          (funcall kill-fn b e)
          ;; kill useless junk whitespace, but only if we're actually
          ;; killing the region
          (when (not dont-kill)
            (let ((bdel (save-excursion
                          (when (looking-back " ")
                            (skip-chars-backward " \t")
                            (when (not (looking-back (sp--get-opening-regexp)))
                              (forward-char)))
                          (point)))
                  (edel (save-excursion
                          (when (looking-at " ")
                            (skip-chars-forward " \t")
                            (when (not (looking-at (sp--get-closing-regexp)))
                              (backward-char)))
                          (point))))
              (delete-region bdel edel))
            (indent-according-to-mode)
            ;; kill useless newlines
            (when (string-match-p "\n" (buffer-substring-no-properties bm (point)))
              (delete-region bm (point))))))))))

(defun sp-backward-kill-sexp (&optional arg dont-kill)
  "Kill the balanced expression preceding point.

This is exactly like calling `sp-kill-sexp' with minus ARG.
In other words, the direction of all commands is reversed.  For
more information, see the documentation of `sp-kill-sexp'.

Examples:

  (foo (abc)| bar)           -> (foo | bar)

  blab (foo (bar baz) quux)| -> blab |

  (1 2 3 |4 5 6)             -> (|4 5 6) ;; \\[universal-argument]"
  (interactive "P")
  (sp-kill-sexp (sp--negate-argument arg) dont-kill))

(defun sp-copy-sexp (&optional arg)
  "Copy the following ARG expressions to the kill-ring.

This is exactly like calling `sp-kill-sexp' with second argument
t.  All the special prefix arguments work the same way."
  (interactive "P")
  (save-excursion
    (sp-kill-sexp arg t)))

(defun sp-backward-copy-sexp (&optional arg)
  "Copy the previous ARG expressions to the kill-ring.

This is exactly like calling `sp-backward-kill-sexp' with second argument
t.  All the special prefix arguments work the same way."
  (interactive "P")
  (save-excursion
    (sp-kill-sexp (sp--negate-argument arg) t)))

(defun sp-transpose-sexp (&optional arg)
  "Transpose the expressions around point.

The operation will move the point after the transposed block, so
the next transpose will \"drag\" it forward.

With arg positive N, apply that many times, dragging the
expression forward.

With arg negative -N, apply N times backward, pushing the word
before cursor backward.  This will therefore not transpose the
expressions before and after point, but push the expression
before point over the one before it.

Examples:

  foo |bar baz     -> bar foo| baz

  foo |bar baz     -> bar baz foo| ;; 2

  (foo) |(bar baz) -> (bar baz) (foo)|

  (foo bar)        ->    (baz quux)   ;; keeps the formatting
​    |(baz quux)            |(foo bar)

  foo bar baz|     -> foo baz| bar ;; -1"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg)))
    ;; if we're inside a symbol, we need to move out of it first
    (when (> arg 0)
      (when (and (memq (char-syntax (following-char)) '(?w ?_))
                 (memq (char-syntax (preceding-char)) '(?w ?_)))
        (sp-forward-sexp)))
    (while (> n 0)
      (when (< arg 0) (sp-backward-sexp))
      (let ((next (save-excursion (sp-forward-sexp)))
            (prev (save-excursion (sp-backward-sexp)))
            ins between)
        (save-excursion
          (goto-char (sp-get next :beg-prf))
          (setq ins (sp-get next (delete-and-extract-region :beg-prf :end)))
          (setq between (delete-and-extract-region (sp-get prev :end) (point)))
          (goto-char (sp-get prev :beg-prf))
          (insert ins between))
        (when (< arg 0)
          (goto-char (+ (sp-get prev :beg-prf) (sp-get next :len))))
        (setq n (1- n))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "paredit" operations

(defun sp-forward-slurp-sexp (&optional arg)
  "Add sexp following the current list in it by moving the closing delimiter.

If the current list is the last in a parent list, extend that
list (and possibly apply recursively until we can extend a list
or end of file).

If ARG is N, apply this function that many times.

If ARG is negative -N, extend the opening pair instead (that is,
backward).

If ARG is raw prefix \\[universal-argument], extend all the way to the end of the parent list.

If both the current expression and the expression to be slurped
are strings, they are joined together.

Examples:

  (foo |bar) baz        -> (foo |bar baz)

  [(foo |bar)] baz      -> [(foo |bar) baz]

  [(foo |bar) baz]      -> [(foo |bar baz)]

  ((|foo) bar baz quux) -> ((|foo bar baz quux)) ;; with \\[universal-argument]

  \"foo| bar\" \"baz quux\" -> \"foo| bar baz quux\""
  (interactive "P")
  (if (> (prefix-numeric-value arg) 0)
      (let ((n (abs (prefix-numeric-value arg)))
            (enc (sp-get-enclosing-sexp))
            (ins-space 0)
            next-thing ok)
        (when enc
          (save-excursion
            (if (sp--raw-argument-p arg)
                (progn
                  (goto-char (sp-get enc :end))
                  (setq next-thing (sp-get-enclosing-sexp))
                  (when next-thing
                    (goto-char (sp-get next-thing :end-in))
                    (insert (sp-get enc :cl))
                    (goto-char (sp-get enc :end))
                    (delete-char (sp-get enc (- :cl-l)))
                    (indent-region (sp-get enc :beg-prf) (sp-get next-thing :end))))
              (while (> n 0)
                (goto-char (sp-get enc :end))
                (setq ok enc)
                (setq next-thing (sp-get-thing nil))
                (while (< (sp-get next-thing :beg) (sp-get ok :beg))
                  (goto-char (sp-get next-thing :end))
                  (setq ok next-thing)
                  (setq next-thing (sp-get-thing nil)))
                (if ok
                    (progn
                      (if (and (equal (sp-get next-thing :cl) "\"")
                               (equal (sp-get ok :cl) "\""))
                          (progn
                            (sp--join-sexp ok next-thing)
                            (goto-char (- (sp-get next-thing :end) 2))
                            (plist-put enc :end (- (sp-get next-thing :end) 2)))
                        (delete-char (sp-get ok (- :cl-l)))
                        (when (= (sp-get ok :end) (sp-get next-thing :beg-prf))
                          (insert " ")
                          (setq ins-space -1))
                        (goto-char (- (sp-get next-thing :end) (sp-get ok :cl-l) ins-space))
                        (insert (sp-get ok :cl))
                        (indent-region (sp-get ok :beg-prf) (point))
                        ;; HACK: update the "enc" data structure if ok==enc
                        (when (= (sp-get enc :beg) (sp-get ok :beg)) (plist-put enc :end (point))))
                      (setq n (1- n)))
                  (message "We can't slurp without breaking strictly balanced expression. Ignored.")
                  (setq n -1)))))))
    (sp-backward-slurp-sexp (sp--negate-argument arg))))

(defun sp-backward-slurp-sexp (&optional arg)
  "Add the sexp preceding the current list in it by moving the opening delimiter.

If the current list is the first in a parent list, extend that
list (and possibly apply recursively until we can extend a list
or beginning of file).

If arg is N, apply this function that many times.

If arg is negative -N, extend the closing pair instead (that is,
forward).

If ARG is raw prefix \\[universal-argument], extend all the way to the beginning of the parent list.

If both the current expression and the expression to be slurped
are strings, they are joined together.

Examples:

  foo (bar| baz)        -> (foo bar| baz)

  foo [(bar| baz)]      -> [foo (bar| baz)]

  [foo (bar| baz)]      -> [(foo bar| baz)]

  (foo bar baz (|quux)) -> ((foo bar baz |quux)) ;; with \\[universal-argument]

  \"foo bar\" \"baz |quux\" -> \"foo bar baz |quux\""
  (interactive "P")
  (if (> (prefix-numeric-value arg) 0)
      (let ((n (abs (prefix-numeric-value arg)))
            (enc (sp-get-enclosing-sexp))
            next-thing ok)
        (when enc
          (save-excursion
            (if (sp--raw-argument-p arg)
                (progn
                  (goto-char (sp-get enc :beg-prf))
                  (setq next-thing (sp-get-enclosing-sexp))
                  (when next-thing
                    (delete-char (sp-get enc (+ :op-l :prefix-l)))
                    (goto-char (sp-get next-thing :beg-in))
                    (insert (sp-get enc :prefix) (sp-get enc :op))
                    (indent-region (sp-get next-thing :beg-in) (sp-get enc :end))))
              (while (> n 0)
                (goto-char (sp-get enc :beg-prf))
                (setq ok enc)
                (setq next-thing (sp-get-thing t))
                (while (> (sp-get next-thing :end) (sp-get ok :end))
                  (goto-char (sp-get next-thing :beg-prf))
                  (setq ok next-thing)
                  (setq next-thing (sp-get-thing t)))
                (if ok
                    (progn
                      (if (and (equal (sp-get next-thing :cl) "\"")
                               (equal (sp-get ok :cl) "\""))
                          (progn
                            (sp--join-sexp next-thing ok)
                            (goto-char (sp-get next-thing :beg-prf))
                            (plist-put enc :beg (sp-get next-thing :beg)))
                        (delete-char (sp-get ok (+ :op-l :prefix-l)))
                        (when (= (sp-get ok :beg-prf) (sp-get next-thing :end))
                          (insert " "))
                        (goto-char (sp-get next-thing :beg-prf))
                        (insert (sp-get ok :prefix) (sp-get ok :op))
                        (indent-region (point) (sp-get ok :end))
                        ;; HACK: update the "enc" data structure if ok==enc
                        (when (sp--compare-sexps enc ok) (plist-put enc :beg (- (point) (sp-get ok :op-l)))))
                      (setq n (1- n)))
                  (message "We can't slurp without breaking strictly balanced expression. Ignored.")
                  (setq n -1)))))))
    (sp-forward-slurp-sexp (sp--negate-argument arg))))

(defun sp-add-to-previous-sexp (&optional arg)
  "Add the expression following point to the list preceding point.

With ARG positive N add that many expressions after point to the
preceding list.

If ARG is raw prefix argument \\[universal-argument] add all expressions until
the end of enclosing list to the previous list.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] add the current
list into the previous list.

Examples:

  (foo bar) |baz quux        -> (foo bar |baz) quux

  (foo bar) |baz quux        -> (foo bar |baz quux) ;; 2

  (blab (foo bar) |baz quux) -> (blab (foo bar |baz quux)) ;; \\[universal-argument]

  (foo bar) (baz |quux)      -> (foo bar (baz |quux)) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (save-excursion
    (cond
     ((equal arg '(16))
      (sp-backward-up-sexp)
      (sp-backward-down-sexp)
      (sp-forward-slurp-sexp))
     (t
      (sp-backward-down-sexp)
      (sp-forward-slurp-sexp arg)))))

(defun sp-add-to-next-sexp (&optional arg)
  "Add the expressions preceding point to the list following point.

With ARG positive N add that many expressions before point to the
following list.

If ARG is raw prefix argument \\[universal-argument] add all expressions until
the beginning of enclosing list to the following list.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] add the current
list into the following list.

Examples:

  foo bar| (baz quux)        -> foo (bar| baz quux)

  foo bar| (baz quux)        -> (foo bar| baz quux) ;; 2

  (foo bar |(bar quux) blab) -> ((foo bar |bar quux) blab) ;; \\[universal-argument]

  (foo |bar) (baz quux)      -> ((foo |bar) baz quux) ;; \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (save-excursion
    (cond
     ((equal arg '(16))
      (sp-up-sexp)
      (sp-down-sexp)
      (sp-backward-slurp-sexp))
     (t
      (sp-down-sexp)
      (sp-backward-slurp-sexp arg)))))

(defmacro sp--barf-sexp (fw-1)
  "Generate forward/backward barf functions."
  `(let ((n (abs arg)))
     (while (> n 0)
       (let* ((ok (sp-get-enclosing-sexp))
              ,@(when (not fw-1) '((prefix nil) (prefix-len 0)))
              next-thing)
         (if ok
             (save-excursion
               (goto-char ,(if fw-1 '(sp-get ok :end-in)
                             '(sp-get ok :beg-in)))
               ;; NOTE: sp-get-thing is "reversed", if we barf forward
               ;; we search from end of list backward for the thing to
               ;; barf.
               (setq next-thing (sp-get-thing ,fw-1))
               (if (and next-thing
                        (/= (sp-get next-thing :beg) (sp-get ok :beg))) ;; ok == next-thing
                   (progn
                     (delete-char ,(if fw-1 '(sp-get ok :cl-l)
                                     '(sp-get ok (- (+ :op-l :prefix-l)))))
                     (goto-char ,(if fw-1 '(sp-get next-thing :beg)
                                   '(- (sp-get next-thing :end)
                                       (sp-get ok (+ :op-l :prefix-l)))))
                     ,(if fw-1 '(sp-skip-backward-to-symbol t)
                        ;; skip the prefix backward.  We don't have
                        ;; info about this prefix, since it is the
                        ;; "following-sexp" to the one being jumped
                        ;; over -- next-thing
                        '(progn
                           (sp-skip-forward-to-symbol t)
                           (skip-syntax-backward "'")))
                     (insert ,(if fw-1 '(sp-get ok :cl)
                                '(sp-get ok (concat :prefix :op))))
                     ;; reindent the "barfed region"
                     (indent-region (sp-get ok :beg-prf) (sp-get ok :end))
                     (setq n (1- n)))
                 (message "The expression is empty.")
                 (setq n -1)))
           (message "We can't barf without breaking strictly balanced expression. Ignored.")
           (setq n -1))))))

(defun sp-forward-barf-sexp (&optional arg)
  "Remove the last sexp in the current list by moving the closing delimiter.

If ARG is positive number N, barf that many expressions.

If ARG is negative number -N, contract the opening pair instead.

If ARG is raw prefix \\[universal-argument], barf all expressions from the one after
point to the end of current list and place the point before the
closing delimiter of the list.

If the current list is empty, do nothing.

Examples: (prefix arg in comment)

  (foo bar| baz)   -> (foo bar|) baz   ;; nil (defaults to 1)

  (foo| [bar baz]) -> (foo|) [bar baz] ;; 1

  (1 2 3| 4 5 6)   -> (1 2 3|) 4 5 6   ;; \\[universal-argument] (or numeric prefix 3)

  (foo bar| baz)   -> foo (bar| baz)   ;; -1"
  (interactive "P")
  (let ((raw (sp--raw-argument-p arg))
        (old-arg arg)
        (arg (prefix-numeric-value arg)))
    (if (> arg 0)
        (if raw
            (let* ((lst (sp-get-list-items))
                   (last nil))
              (!cdr lst)
              (while (and lst (>= (point) (sp-get (car lst) :beg))) (setq last (car lst)) (!cdr lst))
              (setq arg (length lst))
              (when (/= arg 0)
                (sp--barf-sexp t)
                (when (> (point) (sp-get last :end)) (goto-char (sp-get last :end)))))
          (sp--barf-sexp t))
      (sp-backward-barf-sexp (sp--negate-argument old-arg)))))

(defun sp-backward-barf-sexp (&optional arg)
  "This is exactly like calling `sp-forward-barf-sexp' with minus ARG.
In other words, instead of contracting the closing pair, the
opening pair is contracted.  For more information, see the
documentation of `sp-forward-barf-sexp'.

Examples:

  (foo bar| baz) -> foo (bar| baz)

  ([foo bar] |baz) -> [foo bar] (|baz)

  (1 2 3 |4 5 6) -> 1 2 3 (|4 5 6) ;; \\[universal-argument] (or 3)"
  (interactive "P")
  (let ((raw (sp--raw-argument-p arg))
        (old-arg arg)
        (arg (prefix-numeric-value arg)))
    (if (> arg 0)
        (if raw
            (let* ((lst (sp-get-list-items))
                   (n 0))
              (!cdr lst)
              (while (and lst (> (point) (sp-get (car lst) :end))) (!cdr lst) (setq n (1+ n)))
              (setq arg n)
              (when (/= arg 0)
                (sp--barf-sexp nil)
                (when (< (point) (sp-get (car lst) :beg-prf)) (goto-char (sp-get (car lst) :beg-prf)))))
          (sp--barf-sexp nil))
      (sp-forward-barf-sexp (sp--negate-argument old-arg)))))

;; TODO: get rid of the macro anyway, it's stupid!
(defmacro sp--skip-to-symbol-1 (forward)
  "Generate `sp-skip-forward-to-symbol' or `sp-skip-backward-to-symbol'."
  (let ((inc (if forward '1+ '1-))
        (dec (if forward '1- '1+))
        (forward-fn (if forward 'forward-char 'backward-char))
        (next-char-fn (if forward 'following-char 'preceding-char))
        (looking (if forward 'looking-at 'sp--looking-back)))
    `(let ((in-comment (sp-point-in-comment))
           ;; HACK: if we run out of current context this might skip a
           ;; pair that was not allowed before.  However, such a call is
           ;; never made in SP, so it's OK for now
           (allowed-pairs (sp--get-allowed-regexp)))
       (while (and (not (or (eobp)
                            (and stop-after-string
                                 (not (sp-point-in-string))
                                 (sp-point-in-string (,dec (point))))
                            (and stop-at-string
                                 (not (sp-point-in-string))
                                 (sp-point-in-string (,inc (point))))
                            (,looking allowed-pairs)))
                   (or (member (char-syntax (,next-char-fn)) '(?< ?> ?! ?| ?\ ?\" ?' ?.))
                       (unless in-comment (sp-point-in-comment))))
         (,forward-fn 1)))))

(defun sp-skip-forward-to-symbol (&optional stop-at-string stop-after-string)
  "Skip whitespace and comments moving forward.
If STOP-AT-STRING is non-nil, stop before entering a string (if
not already in a string).  If STOP-AFTER-STRING is non-nil, stop
after exiting a string.

Examples:

  foo|   bar -> foo   |bar

  foo|   [bar baz] -> foo   |[bar baz]"
  (interactive)
  (sp--skip-to-symbol-1 t))

(defun sp-skip-backward-to-symbol (&optional stop-at-string stop-after-string)
  "Skip whitespace and comments moving backward.
If STOP-AT-STRING is non-nil, stop before entering a string (if
not already in a string).  If STOP-AFTER-STRING is non-nil, stop
after exiting a string.

Examples:

  foo   |bar -> foo|   bar

  [bar baz]   |foo -> [bar baz]|   foo"
  (interactive)
  (sp--skip-to-symbol-1 nil))

(defmacro sp--forward-symbol-1 (fw-1)
  "Generate forward/backward symbol functions."
  (let ((goto-where (if fw-1 '(match-end 0) '(match-beginning 0)))
        (look-at-open (if fw-1 '(looking-at open) '(sp--looking-back open)))
        (look-at-close (if fw-1 '(looking-at close) '(sp--looking-back close))))
    `(let ((n (abs arg))
           (fw (> arg 0))
           (open (sp--get-opening-regexp))
           (close (sp--get-closing-regexp)))
       (if fw
           (while (> n 0)
             ;; First we need to get to the beginning of a symbol.  This means
             ;; skipping all whitespace and pair delimiters until we hit
             ;; something in \sw or \s_
             (while (cond ((not (memq
                                 (char-syntax (,(if fw-1 'following-char 'preceding-char)))
                                 '(?w ?_)))
                           (,(if fw-1 'forward-char 'backward-char)) t)
                          (,look-at-open
                           (goto-char ,goto-where))
                          (,look-at-close
                           (goto-char ,goto-where))))
             (while (and ,(if fw-1 '(not (eobp)) '(not (bobp)))
                         (not (or ,look-at-open
                                  ,look-at-close))
                         (memq (char-syntax
                                (,(if fw-1 'following-char 'preceding-char)))
                               '(?w ?_)))
               (,(if fw-1 'forward-char 'backward-char)))
             (setq n (1- n)))
         (,(if fw-1 'sp-backward-symbol 'sp-forward-symbol) n)))))

(defun sp-forward-symbol (&optional arg)
  "Move point to the next position that is the end of a symbol.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
backward direction.

A symbol is any sequence of characters that are in either the
word constituent or symbol constituent syntax class.  Current
symbol only extend to the possible opening or closing delimiter
as defined by `sp-add-pair' even if part of this delimiter
would match \"symbol\" syntax classes.

Examples:

  |foo bar baz          -> foo| bar baz

  |foo (bar (baz))      -> foo (bar| (baz)) ;; 2

  |foo (bar (baz) quux) -> foo (bar (baz) quux|) ;; 4"
  (interactive "p")
  (setq arg (or arg 1))
  (sp--forward-symbol-1 t))

(defun sp-backward-symbol (&optional arg)
  "Move point to the next position that is the beginning of a symbol.

With ARG being positive number N, repeat that many times.

With ARG being Negative number -N, repeat that many times in
forward direction.

A symbol is any sequence of characters that are in either the word
constituent or symbol constituent syntax class.  Current symbol only
extend to the possible opening or closing delimiter as defined by
`sp-add-pair' even if part of this delimiter would match \"symbol\"
syntax classes.

Examples:

  foo bar| baz            -> foo |bar baz

  ((foo bar) baz)|        -> ((foo |bar) baz) ;; 2

  (quux ((foo) bar) baz)| -> (|quux ((foo) bar) baz) ;; 4"
  (interactive "p")
  (setq arg (or arg 1))
  (sp--forward-symbol-1 nil))

(defun sp--unwrap-sexp (sexp)
  "Unwrap expression defined by SEXP.

Warning: this function remove possible empty lines and reindents
the unwrapped sexp, so the SEXP structure will no longer
represent a valid object in a buffer!"
  (delete-region
   (sp-get sexp :end-in)
   (sp-get sexp :end))
  (delete-region
   (sp-get sexp :beg-prf)
   (sp-get sexp :beg-in))
  ;; if the delimiters were the only thing on the line, we should also
  ;; get rid of the (possible) empty line that will be the result of
  ;; their removal.  This is especially nice in HTML mode or
  ;; long-running tags like \[\] in latex.
  (let ((new-start (sp-get sexp :beg-prf))
        (new-end (sp-get sexp (- :end-in :op-l :prefix-l)))
        indent-from indent-to)
    (save-excursion
      (goto-char new-end)
      (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
        (let ((b (bounds-of-thing-at-point 'line)))
          (delete-region (car b) (cdr b))))
      (setq indent-to (point))
      (goto-char new-start)
      (when (string-match-p "^[\n\t ]+\\'" (thing-at-point 'line))
        (let ((b (bounds-of-thing-at-point 'line)))
          (delete-region (car b) (cdr b))))
      (setq indent-from (point)))
    (indent-region indent-from indent-to)))

(defun sp-unwrap-sexp (&optional arg)
  "Unwrap the following expression.

With ARG N, unwrap Nth expression as returned by
`sp-forward-sexp'.  If ARG is negative -N, unwrap Nth expression
backwards as returned by `sp-backward-sexp'.

Return the information about the just unwrapped expression.  Note
that this structure does not represent a valid expression in the
buffer.

Examples:

  |(foo bar baz)     -> |foo bar baz

  (foo bar| baz)     -> foo bar| baz

  |(foo) (bar) (baz) -> |(foo) bar (baz) ;; 2"
  (interactive "p")
  (setq arg (or arg 1))
  (let ((sp-navigate-consider-symbols nil))
    (let ((ok (save-excursion (sp-forward-sexp arg))))
      (when ok (sp--unwrap-sexp ok))
      ok)))

(defun sp-backward-unwrap-sexp (&optional arg)
  "Unwrap the previous expression.

With ARG N, unwrap Nth expression as returned by
`sp-backward-sexp'.  If ARG is negative -N, unwrap Nth expression
forward as returned by `sp-forward-sexp'.

Examples:

  (foo bar baz)|     -> foo bar baz|

  (foo bar)| (baz)   -> foo bar| (baz)

  (foo) (bar) (baz)| -> foo (bar) (baz) ;; 3"
  (interactive "p")
  (sp-unwrap-sexp (- (or arg 1))))

(defun sp-splice-sexp (&optional arg)
  "Unwrap the current list.

With ARG N, unwrap Nth list as returned by applying `sp-up-sexp'
N times.  This function expect positive arg.

Examples:

  (foo (bar| baz) quux) -> (foo bar| baz quux)

  (foo |(bar baz) quux) -> foo |(bar baz) quux

  (foo (bar| baz) quux) -> foo (bar| baz) quux ;; 2"
  (interactive "p")
  (setq arg (or arg 1))
  (let ((ok (sp-get-enclosing-sexp arg)))
    (when ok (sp--unwrap-sexp ok))))

(defun sp--splice-sexp-do-killing (beg end expr &optional jump-end)
  "Save the text in the region between BEG and END inside EXPR,
then delete EXPR and insert the saved text.

If optional argument JUPM-END is equal to the symbol 'end move
the point after the re-inserted text."
  (let (str p)
    (setq str (buffer-substring-no-properties beg end))
    (delete-region (sp-get expr :beg-prf) (sp-get expr :end))
    (save-excursion
      (insert str)
      (indent-region (sp-get expr :beg-prf) (point))
      (setq p (point)))
    (when (eq jump-end 'end) (goto-char p))))

;; The following two functions could be very simply implemented using
;; `sp-splice-sexp-killing-around' but these are more efficient
;; implementations.  With sufficiently big lists the difference is
;; noticable.
(defun sp-splice-sexp-killing-backward (&optional arg)
  "Unwrap the current list and kill all the expressions
between start of this list and the point.

With the optional argument ARG, repeat that many times.  This
argument should be positive number.

Examples:

  (foo (let ((x 5)) |(sqrt n)) bar)  -> (foo |(sqrt n) bar)

​  (when ok|                             |(perform-operation-1)
​    (perform-operation-1)            ->  (perform-operation-2)
​    (perform-operation-2))

​  (save-excursion                    -> |(awesome-stuff-happens) ;; 2
​    (unless (test)
​      |(awesome-stuff-happens)))

Note that to kill only the content and not the enclosing
delimiters you can use \\[universal-argument] \\[sp-backward-kill-sexp].
See `sp-kill-sexp' for more information."
  (interactive "p")
  (while (> arg 0)
    (let ((ok (sp-get-enclosing-sexp 1)))
      (if ok
          (sp--splice-sexp-do-killing
           (sp-get (sp-get-thing) :beg-prf)
           (sp-get ok :end-in)
           ok)
        (setq arg -1)))
    (setq arg (1- arg))))

(defun sp-splice-sexp-killing-forward (&optional arg)
  "Unwrap the current list and kill all the expressions between
the point and the end of this list.

With the optional argument ARG, repeat that many times.  This
argument should be positive number.

Examples:

  (a (b c| d e) f) -> (a b c| f)

  (+ (x |y z) w)   -> (+ x| w)

Note that to kill only the content and not the enclosing
delimiters you can use \\[universal-argument] \\[sp-kill-sexp].
See `sp-kill-sexp' for more information."
  (interactive "p")
  (while (> arg 0)
    (let ((ok (sp-get-enclosing-sexp 1)))
      (if ok
          (sp--splice-sexp-do-killing
           (sp-get (sp-get-thing t) :end) ;search backward
           (sp-get ok :beg-in)
           ok 'end)
        (setq arg -1)))
    (setq arg (1- arg))))

(defun sp-splice-sexp-killing-around (&optional arg)
  "Unwrap the current list and kill everything inside except next expression.

With ARG save that many next expressions.  With ARG negative -N,
save that many expressions backward.

If ARG is raw prefix argument \\[universal-argument] this function behaves exactly
the same as `sp-splice-sexp-killing-backward'.

If ARG is negative raw prefix argument \\[negative-argument] \\[universal-argument] this function
behaves exactly the same as `sp-splice-sexp-killing-forward'.

Note that the behaviour with the prefix argument seems to be
reversed.  This is because the backward variant is much more
common and hence deserve shorter binding.

If ARG is raw prefix argument \\[universal-argument] \\[universal-argument] raise the expression the point
is inside of.  This is the same as `sp-backward-up-sexp' followed by
`sp-splice-sexp-killing-around'.

Examples:

  (a b |(c d) e f)      -> |(c d)     ;; with arg = 1

  (a b |c d e f)        -> |c d       ;; with arg = 2

  (- (car x) |a 3)      -> (car x)|   ;; with arg = -1

  (foo (bar |baz) quux) -> |(bar baz) ;; with arg = \\[universal-argument] \\[universal-argument]"
  (interactive "P")
  (cond
   ((equal arg '(4))
    (sp-splice-sexp-killing-backward 1))
   ((equal arg '(-4))
    (sp-splice-sexp-killing-forward 1))
   (t
    (if (equal arg '(16))
        (progn
          (sp-backward-up-sexp)
          (setq arg 1))
      (setq arg (prefix-numeric-value arg)))
    (let ((ok (sp-get-enclosing-sexp)) str)
      (when ok
        (sp-select-next-thing-exchange arg)
        (sp--splice-sexp-do-killing
         (region-beginning)
         (region-end)
         ok (if (> arg 0) nil 'end)))))))

(defalias 'sp-raise-sexp 'sp-splice-sexp-killing-around)

(defun sp-convolute-sexp (&optional arg)
  "Convolute balanced expressions.

Save the expressions preceding point and delete them.  Then
splice the resulting expression.  Wrap the current enclosing list
with the delimiters of the spliced list and insert the saved
expressions.

With ARG positive N, move up N lists before wrapping.

Examples:

We want to move the `while' before the `let'.

​  (let ((stuff 1)            |(while (we-are-good)
​        (other 2))              (let ((stuff 1)
​    (while (we-are-good)  ->          (other 2))
​     |(do-thing 1)                (do-thing 1)
​      (do-thing 2)                (do-thing 2)
​      (do-thing 3)))              (do-thing 3)))

  (forward-char (sp-get env |:op-l)) -> |(sp-get env (forward-char :op-l))"
  (interactive "p")
  (sp-forward-whitespace)
  (let* ((old (point))
         (bot (sp-beginning-of-sexp))
         raise cl-own-line)
    ;; we need to check if the :cl was on its own line
    (save-excursion
      (goto-char (sp-get bot :end))
      (when (string-match-p (concat "^[\n\t ]*"
                                    (regexp-quote (sp-get bot (concat :cl)))
                                    "[\n\t ]*\\'")
                            (thing-at-point 'line))
        (setq cl-own-line t)))
    (setq raise (buffer-substring (point) old))
    (delete-region (point) old)
    (sp-unwrap-sexp -1)
    (let ((enc (sp-backward-up-sexp arg)))
      (goto-char (sp-get enc :end))
      (insert (if cl-own-line "\n" "") (sp-get bot :cl))
      (goto-char (sp-get enc :beg-prf))
      (save-excursion
        (sp-get bot (insert :prefix :op))
        (insert raise))
      (indent-region
       (sp-get enc :beg-prf)
       (+ (sp-get enc :end) (length raise) (sp-get bot (+ :op-l :cl-l :prefix-l)))))))

(defun sp-absorb-sexp (&optional arg)
  "Absorb previous expression.

Save the expressions preceding point and delete them.  Then slurp
an expression backward and insert the saved expressions.

With ARG positive N, absorb that many expressions.

Examples:

​  (do-stuff 1)         (save-excursion
​  (save-excursion  ->   |(do-stuff 1)
​   |(do-stuff 2))        (do-stuff 2))

  foo bar (concat |baz quux) -> (concat |foo bar baz quux) ;; 2"
  (interactive "p")
  (sp-forward-whitespace)
  (let* ((old (point))
         (raise (progn
                  (sp-beginning-of-sexp)
                  (buffer-substring (point) old))))
    (delete-region (point) old)
    (sp-backward-slurp-sexp arg)
    (sp-forward-whitespace)
    (sp-beginning-of-sexp)
    (insert raise)
    (save-excursion
      (sp-backward-up-sexp)
      (indent-sexp)))
  (sp-forward-whitespace))

(defun sp-emit-sexp (&optional arg)
  "Move all expression preceding point except the first one out of the current list.

With ARG positive N, keep that many expressions from the start of
the current list.

This is similar as `sp-backward-barf-sexp' but it also drags the
first N expressions with the delimiter.

Examples:

​  (save-excursion     ​(do-stuff 1)
​    (do-stuff 1)      (do-stuff 2)
​    (do-stuff 2)  ->  (save-excursion
​   |(do-stuff 3))      |(do-stuff 3))

​  (while not-done-yet       (execute-only-once)
​    (execute-only-once) ->  (while not-done-yet    ;; arg = 2
​   |(execute-in-loop))       |(execute-in-loop))"
  (interactive "p")
  (let (save-text)
    (save-excursion
      (sp-beginning-of-sexp)
      (let* ((start (point)))
        (sp-forward-sexp arg)
        (sp-skip-forward-to-symbol t)
        (setq save-text (buffer-substring start (point)))
        (delete-region start (point))))
    (save-excursion (sp-backward-barf-sexp '(4)))
    (sp-down-sexp)
    (insert save-text)
    (save-excursion
      (sp-backward-up-sexp)
      (indent-sexp))))

(defun sp-forward-whitespace ()
  "Skip forward past the whitespace characters."
  (interactive)
  (skip-chars-forward " \t\n"))

(defun sp-backward-whitespace ()
  "Skip backward past the whitespace characters."
  (interactive)
  (skip-chars-backward " \t\n"))

(defun sp-split-sexp ()
  "Split the list or string the point is on into two.

Examples:

  (foo bar |baz quux)   -> (foo bar) |(baz quux)

  \"foo bar |baz quux\"   -> \"foo bar\" |\"baz quux\"

  ([foo |bar baz] quux) -> ([foo] |[bar baz] quux)"
  (interactive)
  (let ((ok (sp-get-enclosing-sexp 1)))
    (when ok
      (forward-char (- (prog1 (sp-backward-whitespace) (insert (sp-get ok :cl)))))
      (save-excursion (sp-forward-whitespace) (insert (sp-get ok :op))))))

(defun sp--join-sexp (prev next)
  "Join the expressions PREV and NEXT if they are of the same type.

The expression with smaller :beg is considered the previous one,
so the input order does not actually matter.

Return the information about resulting expression."
  (if (and (equal (sp-get prev :op) (sp-get next :op))
           (equal (sp-get prev :cl) (sp-get next :cl)))
      ;; if there's some prefix on the second expression, remove it.
      ;; We do not move it to the first expression, it is assumed
      ;; there's one already
      (progn
        (if (> (sp-get prev :beg) (sp-get next :beg))
            (let ((tmp prev))
              (setq prev next)
              (setq next tmp)))
        (delete-region (sp-get next :beg-prf) (sp-get next :beg-in))
        (delete-region (sp-get prev :end-in) (sp-get prev :end))
        (list :beg (sp-get prev :beg)
              :end (- (sp-get next (- :end :op-l :prefix-l)) (sp-get prev :cl-l))
              :op (sp-get prev :op)
              :cl (sp-get prev :cl)
              :prefix (sp-get prev :prefix)))
    (message "The expressions to be joined are of different type.")))

(defun sp-join-sexp (&optional arg)
  "Join the sexp before and after point if they are of the same type.

If ARG is positive N, join N expressions after the point with the
one before the point.

If ARG is negative -N, join N expressions before the point with
the one after the point.

If ARG is a raw prefix \\[universal-argument] join all the things up until the end
of current expression.

The joining stops at the first expression of different type.

Examples:

  (foo bar) |(baz)                    -> (foo bar |baz)

  (foo) |(bar) (baz)                  -> (foo |bar baz) ;; 2

  [foo] [bar] |[baz]                  -> [foo bar |baz] ;; -2

  (foo bar (baz)| (quux) (blob bluq)) -> (foo bar (baz| quux blob bluq)) ;; \\[universal-argument]"
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (n (abs arg))
         (prev (save-excursion (sp-backward-sexp (sp--signum arg))))
         next)
    (save-excursion
      (cond
       ((and raw (= n 4))
        (setq next (sp-forward-sexp (sp--signum arg)))
        (while (cond
                ((> arg 0)
                 (> (sp-get next :beg) (sp-get prev :end)))
                ((< arg 0)
                 (< (sp-get next :end) (sp-get prev :beg))))
          (setq prev (sp--join-sexp prev next))
          (setq next (sp-forward-sexp (sp--signum arg)))))
       (t (while (> n 0)
            (setq next (sp-forward-sexp (sp--signum arg)))
            (setq prev (sp--join-sexp prev next))
            (setq n (1- n)))))
      prev)))

;; TODO: make the begin/end calculation more sane :P. This is turning
;; into a bowl of spaghetti
(defun sp-select-next-thing (&optional arg)
  "Set active region over ARG next things as recognized by `sp-get-thing'.

If ARG is negative -N, select that many expressions backward.

If ARG is a raw prefix \\[universal-argument] select all the things up until the
end of current expression.

If ARG is a raw prefix \\[universal-argument] \\[universal-argument] select the current expression (as
if doing `sp-backward-up-sexp' followed by
`sp-select-next-thing').

If the currently active region contains a balanced expression,
following invocation of `sp-select-next-thing' will select the
inside of this expression (from :beg-in to :end-in, see
`sp-get').  Therefore calling this function twice with no active
region will select the inside of the next expression.

If the point is right in front of the expression any potential
prefix is ignored.  For example, '|(foo) would only select (foo)
and not include ' in the selection.  If you wish to also select '
you have to move the point backwards.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
  (interactive "P")
  (let* ((raw (sp--raw-argument-p arg))
         (arg (prefix-numeric-value arg))
         (old-point (point))
         (rb (region-beginning))
         (re (region-end))
         (first (sp-forward-sexp (sp--signum arg)))
         (last first)
         (b (if first
                (if (> arg 0) (sp-get first :beg-prf) (sp-get first :end))
              (error "No following expressions")))
         (e (cond
             ;; select things up until the end of current list,
             ;; ignoring whitespace and possible comments inside
             ((and raw
                   (= (abs arg) 4))
              (let ((enc (sp-get-enclosing-sexp)))
                (if enc
                    (save-excursion
                      (if (> arg 0)
                          (progn
                            (goto-char (sp-get enc :end-in))
                            (sp-skip-backward-to-symbol t)
                            (point))
                        (goto-char (sp-get enc :beg-in))
                        (sp-skip-forward-to-symbol t)
                        (point)))
                  (error "No enclosing expression"))))
             ;; select current list
             ((and raw
                   (= (abs arg) 16))
              (let ((enc (sp-get-enclosing-sexp)))
                ;; UGLY! We override b here. (can we even do that
                ;; safely in elisp?)
                (if (not enc)
                    (error "No enclosing expression")
                  (setq b (sp-get enc :beg-prf))
                  (sp-get enc :end))))
             ;; regular select, just select ARG things
             (t
              (when (> (abs arg) 1)
                (setq last (sp-forward-sexp (* (sp--signum arg) (1- (abs arg))))))
              (cond
               ;; if a region is already active and the argument is 1,
               ;; try to select the inside of the pair. If inside is
               ;; selected, select the whole pair again
               ((and (= arg 1)
                     (region-active-p)
                     (eq rb (sp-get last :beg-prf))
                     (eq re (sp-get last :end)))
                (setq b (sp-get last :beg-in))
                (sp-get last :end-in))
               (t
                (if (> arg 0) (sp-get last :end) (sp-get last :beg-prf))))))))
    (push-mark nil t)
    ;; if we moved forward check if the old-point was in front of an
    ;; expression and after a prefix. If so, remove the prefix from
    ;; the selection
    (when (and (> arg 0)
               (= (sp-get first :beg) old-point)
               (= (sp-get first :beg-prf) b))
      (setq b (sp-get first :beg)))
    (set-mark b)
    (goto-char e)))

(defun sp-select-previous-thing (&optional arg)
  "Set active region over ARG previous things as recognized by `sp-get-thing'.

If ARG is negative -N, select that many expressions forward.

With `sp-navigate-consider-symbols' symbols and strings are also
considered balanced expressions."
  (interactive "P")
  (sp-select-next-thing (sp--negate-argument arg)))

(defun sp-select-next-thing-exchange (&optional arg)
  "Just like `sp-select-next-thing' but run `exchange-point-and-mark' afterwards."
  (interactive "P")
  (sp-select-next-thing arg)
  (exchange-point-and-mark))

(defun sp-select-previous-thing-exchange (&optional arg)
  "Just like `sp-select-previous-thing' but run `exchange-point-and-mark' afterwards."
  (interactive "P")
  (sp-select-previous-thing arg)
  (exchange-point-and-mark))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show-smartparens-mode

(defgroup show-smartparens nil
  "Show smartparens minor mode."
  :group 'smartparens)

(defcustom sp-show-pair-delay 0.125
  "Time in seconds to delay before showing a matching pair."
  :type '(number :tag "seconds")
  :group 'show-smartparens)

(defface sp-show-pair-match-face
  '((((class color) (background light))
     :background "turquoise")       ; looks OK on tty (becomes cyan)
    (((class color) (background dark))
     :background "steelblue3")      ; looks OK on tty (becomes blue)
    (((background dark))
     :background "grey50")
    (t
     :background "gray"))
  "`show-smartparens-mode' face used for a matching pair."
  :group 'show-smartparens)

(defface sp-show-pair-mismatch-face
  '((((class color)) (:foreground "white" :background "purple"))
    (t (:inverse-video t)))
  "`show-smartparens-mode' face used for a mismatching pair."
  :group 'show-smartparens)

(defvar sp-show-pair-idle-timer nil)

(defvar sp-show-pair-overlays nil)

;;;###autoload
(define-minor-mode show-smartparens-mode
  "Toggle visualization of matching pairs.  When enabled, any
matching pair is highlighted after `sp-show-pair-delay' seconds
of Emacs idle time if the point is immediately in front or after
a pair.  This mode works similarly to `show-paren-mode', but
support custom pairs."
  :init-value nil
  :group 'show-smartparens
  (if show-smartparens-mode
      (unless sp-show-pair-idle-timer
        (setq sp-show-pair-idle-timer
              (run-with-idle-timer sp-show-pair-delay t
                                   'sp-show--pair-function)))
    (when sp-show-pair-overlays
      (sp-show--pair-delete-overlays))))

;;;###autoload
(define-globalized-minor-mode show-smartparens-global-mode
  show-smartparens-mode
  turn-on-show-smartparens-mode)

;;;###autoload
(defun turn-on-show-smartparens-mode ()
  "Turn on `show-smartparens-mode'."
  (interactive)
  (unless (or (member major-mode sp-ignore-modes-list)
              (eq (get major-mode 'mode-class) 'special))
    (show-smartparens-mode t)))

;;;###autoload
(defun turn-off-show-smartparens-mode ()
  "Turn off `show-smartparens-mode'."
  (interactive)
  (show-smartparens-mode -1))

(defun sp-show--pair-function ()
  "Display the show pair overlays."
  (when show-smartparens-mode
    (let* ((pair-list (sp--get-allowed-pair-list))
           (opening (sp--get-opening-regexp pair-list))
           (closing (sp--get-closing-regexp pair-list))
           ok match)
      (cond
       ((looking-at opening)
        (setq match (match-string 0))
        ;; we can use `sp-get-thing' here because we *are* at some
        ;; pair opening, and so only the tag or the sexp can trigger.
        (setq ok (sp-get-thing))
        (if ok
            (sp-get ok (sp-show--pair-create-overlays :beg :end :op-l :cl-l))
          (sp-show--pair-create-mismatch-overlay (point) (length match))))
       ((sp--looking-back closing)
        (setq match (match-string 0))
        (setq ok (sp-get-thing t))
        (if ok
            (sp-get ok (sp-show--pair-create-overlays :beg :end :op-l :cl-l))
          (sp-show--pair-create-mismatch-overlay (- (point) (length match))
                                                 (length match))))
       (sp-show-pair-overlays
        (sp-show--pair-delete-overlays))))))

(defun sp-show--pair-create-overlays (start end olen clen)
  "Create the show pair overlays."
  (when sp-show-pair-overlays
    (sp-show--pair-delete-overlays))
  (let* ((oleft (make-overlay start (+ start olen) nil t nil))
         (oright (make-overlay (- end clen) end nil t nil)))
    (setq sp-show-pair-overlays (cons oleft oright))
    (overlay-put oleft 'face 'sp-show-pair-match-face)
    (overlay-put oright 'face 'sp-show-pair-match-face)
    (overlay-put oleft 'priority 1000)
    (overlay-put oright 'priority 1000)
    (overlay-put oleft 'type 'show-pair)))

(defun sp-show--pair-create-mismatch-overlay (start len)
  "Create the mismatch pair overlay."
  (when sp-show-pair-overlays
    (sp-show--pair-delete-overlays))
  (let ((o (make-overlay start (+ start len) nil t nil)))
    (setq sp-show-pair-overlays (cons o nil))
    (overlay-put o 'face 'sp-show-pair-mismatch-face)
    (overlay-put o 'priority 1000)
    (overlay-put o 'type 'show-pair)))

(defun sp-show--pair-delete-overlays ()
  "Remove both show pair overlays."
  (when sp-show-pair-overlays
    (when (car sp-show-pair-overlays)
      (delete-overlay (car sp-show-pair-overlays)))
    (when (cdr sp-show-pair-overlays)
      (delete-overlay (cdr sp-show-pair-overlays)))
    (setq sp-show-pair-overlays nil)))

;; global initialization
(sp--update-trigger-keys)
(defadvice delete-backward-char (before sp-delete-pair-advice activate)
  (save-match-data
    (sp-delete-pair (ad-get-arg 0))))
(add-hook 'post-command-hook 'sp--post-command-hook-handler)
(add-hook 'pre-command-hook 'sp--pre-command-hook-handler)
(sp--set-base-key-bindings)
(sp--update-override-key-bindings)

(defvar sp--mc/cursor-specific-vars
  '(
    sp-wrap-point
    sp-wrap-mark
    sp-last-wrapped-region
    sp-pair-overlay-list
    sp-wrap-overlays
    sp-wrap-tag-overlays
    sp-last-operation
    sp-previous-point
    )
  "A list of vars that need to be tracked on a per-cursor basis.")

(eval-after-load 'multiple-cursors
  '(dolist (it sp--mc/cursor-specific-vars)
     (add-to-list 'mc/cursor-specific-vars it)))

(provide 'smartparens)

;; Local Variables:
;; coding: utf-8
;; End:

;;; smartparens.el ends here
