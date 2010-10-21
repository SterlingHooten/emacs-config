;;; ibuffer.el --- operate on buffers like dired

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Author: Colin Walters <walters@cis.ohio-state.edu>
;; Created: 8 Sep 2000
;; Version: 1.9
;; X-RCS: $Id: ibuffer.el,v 1.115 2001/04/18 04:56:23 walters Exp $
;; URL: http://www.cis.ohio-state.edu/~walters
;; Keywords: buffer, convenience

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; ibuffer.el is an advanced replacement for the `buffer-menu' which
;; is normally distributed with Emacs.  Its interface is intended to
;; be analogous to that of Dired.

;; Place this file somewhere in your `load-path', and add:

;; (require 'ibuffer)

;; to your ~/.emacs file.  After that, you should be able to type 'M-x
;; ibuffer' to get started.  'h' will give a brief usage description.

;; Note that if you want to get the best performance out of ibuffer
;; (especially if you usually have more than 50 buffers open), you
;; should byte-compile it.  Type 'M-x emacs-lisp-byte-compile' when
;; you have this file open, and a compiled version will be written in
;; the same directory.

;; A few tips on usage:

;; * If you use global-font-lock-mode, and want colorization in
;;   ibuffer, be sure to say

;; (global-font-lock-mode 1)

;;  before you do (require 'ibuffer) in your ~/.emacs file.  For more
;;  precise control over highlighting, customize the variable
;;  `ibuffer-fontification-level'.

;; Just for fun, here is the author's favorite `ibuffer-formats'
;; configuration:

;; (setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
;;                         (size 6 -1 :right) " " (mode 16 16 :center)
;;                          " " (process 8 -1) " " filename)
;;                     	   (mark " " (name 16 -1) " " filename))
;;       ibuffer-elide-long-columns t)

;; Remember, you can switch between formats using
;; `ibuffer-switch-format', bound to ` by default. 

;;; Change Log:

;; Changes from 1.8 to 1.9

;;  * Use :value for `const' customization types.  Patch from Alastair
;;    Burt <burt@dfki.de>.
;;  * `ibuffer-do-occur' has been improved substantially.  It now
;;    takes an optional argument NLINES, which specifies the number of
;;    lines of context to give, just like `occur'.  Also like `occur',
;;    `ibuffer-do-occur' can "revert" the buffer (bound to "g" by
;;    default).
;;  * The keybindings "+" and "-" have been changed to only
;;    temporarily hide or show buffers.  The variables
;;    `ibuffer-always-show-regexps' and `ibuffer-never-show-regexps'
;;    are still available, though, for more permanent customization.
;;  * Don't move point when calling `ibuffer-bury-buffer' (thanks
;;    Stefan Reich�r <reichoer@riic.at>)
;;  * Use `facep' to test for face names, not `boundp'.
;;  * New variable `ibuffer-always-show-last-buffer'.
;;  * `ibuffer-redisplay' doesn't reorder or refilter the buffers now.
;;    This makes it noticeably faster.
;;  * `ibuffer' now takes two more optional arguments (QUALIFERS and
;;    NAME), which allows you to create specialized Ibuffer buffers
;;    programmatically.  Suggestion from Mario Lang
;;    <mlang@home.delysid.org>.
;;  * The macro `ibuffer-define-limiter' has been improved, so now you
;;    can put all the information needed to define your own limitation
;;    in one place, instead of frobbing `ibuffer-limiting-alist'.  The
;;    predefined limitations are defined using
;;    `ibuffer-define-limiter'.
;;  * New macro `ibuffer-define-sorter'.

;; Changes from 1.7 to 1.8

;;  * New macro `ibuffer-define-column', used internally to define the
;;    built-in columns.  You can use it to define your own columns, as
;;    well.
;;  * Mouse stuff should now work with XEmacs.
;;  * The `ibuffer-formats' variable now allows a :right, :center, or
;;    :left keyword to specify the alignment of a column.
;;  * New variable `ibuffer-elide-long-columns'.
;;  * Fontification in `ibuffer-do-occur' has been improved.  Now
;;    highlights each match in the search (if
;;    `ibuffer-fontification-level' is non-nil).  Suggestion from
;;    jari.aalto@poboxes.com (Jari Aalto+mail.emacs).
;;  * Display the names of buffers being operated on for dangerous
;;    operations, as Dired does.  Suggestion from Roland Winkler
;;    <Roland.Winkler@physik.uni-erlangen.de>.
;;  * New variables `ibuffer-never-show-regexps' and
;;    `ibuffer-maybe-show-regexps'.  Suggestion from Christoph Conrad
;;    <christoph.conrad@gmx.de>.
;;  * New function `ibuffer-bury-buffer', from Christoph Conrad.
;;  * Use diff mode for *Ibuffer-diff* buffers iff it is loaded.
;;  * If `ibuffer-shrink-to-minimum-size', then shrink after buffer
;;    contents are inserted.  Patch from Juanma Barranquero
;;    <lektu@uol.com.br>.
;;  * Test for `font-lock-mode' in XEmacs.  Patch from Nevin Kapur
;;    <nevin@jhu.edu>.
;;  * Several miscellaneous patches from Juanma Barranquero.

;; Changes from 1.6c to 1.7

;;  * INCOMPATIBLE CHANGE: Now uses the variable `ibuffer-formats',
;;    which should hopefully be simpler to configure.  The title is
;;    now automatically generated.
;;  * Use `yes-or-no-p' for dangerous operations, instead of
;;    `y-or-n-p'.  Yes, this means you have to type "yes" now for
;;    confirmation instead of just "y"; the change was made for
;;    constistency with Dired.
;;  * New function `ibuffer-mouse-limit-by-mode'.  This allows you to
;;    click on a major mode in the buffer and it will toggle limiting
;;    to that major mode.  This function is bound to mouse-2 by
;;    default.
;;  * Display the directory path in the "Filename" column for Dired
;;    buffers. (To disable this feature, set `ibuffer-dired-filenames'
;;    to nil).
;;  * Thanks to Juanma Barranquero <lektu@uol.com.br> for several
;;    patches.
;;  * Attempt to improve documentation (using checkdoc.el)
;;  * New internal macro `ibuffer-define-limiter'.
;;  * New variable `ibuffer-fontification-alist' which describes how
;;    to fontify buffers.
;;  * Rename variable `ibuffer-colorize-level' to
;;    `ibuffer-fontification-level'.
;;  * New variable `ibuffer-title-face'.
;;  * New variables `ibuffer-use-other-window' and
;;    `ibuffer-shrink-to-minimum-size'.
;;  * Ibuffer now tries to be smarter about setting its modification
;;    flag, like Dired does.  Idea from Roland Winkler.
;;  * Define `ibuffer-do-save' with `ibuffer-define-op'.
;;  * Fix minor bug in `ibuffer-do-save' definition where it wouldn't
;;    switch back to the Ibuffer buffer.
;;  * Make Ibuffer-Occur buffers read-only.

;; Changes from 1.6b to 1.6c

;;  * Add missing `interactive' spec for `ibuffer-do-query-replace'.
;;  * Use &rest to read `query-replace' args.
;;  * Redisplay after an operation.

;; Changes from 1.6a to 1.6b

;;  * Try to use faces depending on whether or not they are defined.

;; Changes from 1.6 to 1.6a

;;  * Change `font-lock-doc-face' to `font-lock-constant-face',
;;    because Emacs 20 doesn't define the former.

;; Changes from 1.5a to 1.6

;;  * Implement multiple formats, and the ability to switch between
;;    them.  This means the variable `ibuffer-line-format' is now
;;    `ibuffer-line-formats', and the variable `ibuffer-title-string'
;;    is now `ibuffer-title-strings'.
;;  * Add colorization for some types of buffers.
;;  * Clean up internals a bit.

;; Changes from 1.5 to 1.5a

;;  * Fix bug in definition of `ibuffer-define-op'.

;; Changes from 1.4 to 1.5

;;  * New variable `ibuffer-directory-abbrev-alist'.
;;  * Include Dired directories in `ibuffer-mark-by-file-name-regexp'.
;;  * New variable `ibuffer-case-fold-search'. (thanks Roland Winkler)
;;  * Don't throw an error upon deletion of the last buffer in the
;;    list.  (thanks Sriram Karra)
;;  * Try not to execute ibuffer operations if we aren't in an ibuffer
;;    buffer. (thanks Sriram Karra)

;; Changes from 1.3a to 1.4

;;  * New function `ibuffer-do-replace-regexp'.
;;  * Check for dead buffers when initializing the prompt of
;;    `ibuffer-mark-by-mode'. (thanks Sriram Karra)
;;  * Don't mark wildcard dired buffers as dissociated. (thanks Roland
;;    Winkler)
;;  * Remove redundant (function ...) declarations.
;;  * Documentation updates.
;;  * Add a `save-excursion' form around operations defined with
;;    ibuffer-define-simple-op.
;;  * New variable `ibuffer-title-string'.
;;  * Remove useless variable `ibuffer-special-modeline'.
;;  * New functions `ibuffer-forward-line', and
;;    `ibuffer-backward-line', used to implement circular traversal.
;;  * `ibuffer-limit-by-mode' now defaults to the major mode of the
;;    buffer at point.

;; Changes from 1.3 to 1.3a

;;  * Name the occur buffer "*Ibuffer-occur*" instead of
;;    " *Ibuffer-occur", so it's easier to switch back to.

;; Changes from 1.2 to 1.3:

;;  * Implementation of multi-buffer `occur'.  This includes a
;;    function `ibuffer-do-occur', a new derived mode,
;;    `ibuffer-occur-mode', and associated helper functions:
;;    `ibuffer-occur-mouse-display-occurence',
;;    `ibuffer-occur-goto-occurence',
;;    `ibuffer-occur-display-occurence'.
;;  * New function `ibuffer-mark-unsaved-buffers'.  This marks buffers
;;     which have an associated file, but are not saved.
;;  * New function `ibuffer-mark-dissociated-buffers'.  This marks
;;    buffers which have an associated file, but the associated file
;;    does not exist.  (suggested by Roland Winkler)
;;  * New function `ibuffer-do-rename-uniquely'.
;;  * New macro `ibuffer-define-simple-op'.
;;  * Define some simple operations using above macro.
;;  * Don't change the current line during `ibuffer-map-lines'.
;;  * Actually define `ibuffer-do-toggle-read-only'  (thanks Roland
;;    Winkler).
;;  * Clean up messages to user.

;; Changes from 1.1 to 1.2:

;;  * Add ability to limit by content (not as slow as you might
;;    think).
;;  * New operations: 'A' to view all of the marked buffers in the
;;    selected frame, and 'H' to view them each in a separate frame.
;;  * Many compatibility keybindings with `buffer-menu' added.
;;  * Add %w format specifier to display narrowing state.
;;  * Add %p format specifier to display buffer process status
;;  * Add %{ %} format modifiers for making text bold
;;  * Add buffer process status to the default display
;;  * New function: ibuffer-diff-with-file, bound to = by default
;;  * Allow using 'd' to mark buffers explicitly for deletion, and
;;    'x' to delete them.  This is orthogonal to the regular marks.
;;  * More documentation.
;;  * Default limiting keybinding changed from # to / (thanks Kai
;;    Gro�johann)
;;  * Allow using 0-9 for digit prefix arguments
;;  * New function `ibuffer-other-window', to begin viewing an ibuffer
;;    in another window.
;;  * Move point to the beginning of the buffer when updating (thanks
;;    Kai Gro�johann)
;;  * Bury the ibuffer when switching to another buffer (thanks Kai
;;    Gro�johann)
;;  * Byte compile the format specifier when entering ibuffer-mode.

;; Changes from 1.0 to 1.1:

;;  * Addition of of "limiting".  You can view only buffers that match
;;    a certain predicate.
;;  * mouse-1 now toggles the highlighting state of a buffer.
;;  * mouse-3 now pops up a menu of operations
;;  * Switch to the buffer being saved if we're prompted for a
;;    filename.
;;  * Do an update every time after 'M-x ibuffer'.
;;  * Keep mark information even when doing an update (g)
;;  * Allow customization of status characters
;;  * Remove killed buffers during mapping (prevents weird errors)
;;  * Start from the beginning of the buffer during a query-replace
;;  * Downcase major mode name during major-mode sorting to prevent
;;    e.g. Man-mode from being first.
;;  * Hide the 'Edit' menu.
;;  * Fix highlighting bug when name is at the beginning
;;  * Don't change point while doing a query replace.

;;; Bugs:

;;  * When point is near the end of the ibuffer buffer, when we're
;;    using `ibuffer-use-other-window', and an operation is performed,
;;    the window is recentered farther down, which is not what we
;;    want.
;;  * Under XEmacs, when `ibuffer-use-other-window' is t, window
;;    ordering is messed up for `ibuffer-confirm-operation-on'.
;;  * When `ibuffer-use-other-window' is t, ibuffer calls
;;    `pop-to-buffer' to display the ibuffer. If the current frame has
;;    just two side-by-side windows, that has the unfortunate effect
;;    of chosing one of them to display the ibuffer, which often it is
;;    too narrow.

;;; Code:

(require 'easymenu)
(require 'derived)
(require 'font-lock)

(eval-when-compile
  ;; From Emacs 21
  (defvar show-trailing-whitespace)
  (require 'cl))

;; This is kind of a hack, but we don't want to force loading of
;; Dired.
(unless (featurep 'dired)
  (defvar dired-directory nil))

(defun ibuffer-depropertize-string (str &optional nocopy)
  "Return a copy of STR with text properties removed.
If optional argument NOCOPY is non-nil, actually modify the string directly."
  (let ((str (if nocopy
		 str
	       (copy-sequence str))))
    (set-text-properties 0 (length str) nil str)
    str))

;; Emacs 20 compatibility
(if (fboundp 'propertize)
    (defalias 'ibuffer-propertize 'propertize)
  (defun ibuffer-propertize (string &rest properties)
    "Return a copy of STRING with text properties added.

 [Note: this docstring has been copied from the Emacs 21 version]

First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result."
    (let ((str (copy-sequence string)))
      (add-text-properties 0 (length str)
                           properties
                           str)
      str)))

;; Emacs 20/XEmacs compatibility
(cond ((fboundp 'make-temp-file)
       (defalias 'ibuffer-make-temp-file 'make-temp-file))
      ((and (boundp 'temporary-file-directory)
	    (stringp temporary-file-directory))
       (defun ibuffer-make-temp-file (prefix)
	 "Create a temporary file.  DO NOT USE THIS FUNCTION.
This function does not create files atomically, and is thus insecure."
	 (let ((name (concat temporary-file-directory (make-temp-name prefix))))
	   (while (file-exists-p name)
	     (setq name (concat temporary-file-directory (make-temp-name prefix))))
	   (append-to-file (point-min) (point-min) name)
	   name)))
      ((or (featurep 'xemacs)
	   (string-match "XEmacs\\|Lucid" (emacs-version)))
       (defun ibuffer-make-temp-file (prefix)
	 "Create a temporary file.  DO NOT USE THIS FUNCTION.
This function does not create files atomically, and is thus insecure."
	 (let ((name (expand-file-name (make-temp-name prefix) (temp-directory))))
	   (while (file-exists-p name)
	     (setq name (expand-file-name (make-temp-name prefix) (temp-directory))))
	   (append-to-file (point-min) (point-min) name)
	   name)))
      (t
       (error "Couldn't create a suitable definition of `make-temp-file'.")))

(if (fboundp 'window-list)
    (defun ibuffer-window-list ()
      (window-list nil 'nomini))
  (defun ibuffer-window-list ()
    (let ((ibuffer-window-list-result nil))
      (walk-windows #'(lambda (win) (push win ibuffer-window-list-result)) 'nomini)
      (nreverse ibuffer-window-list-result))))

(if (fboundp 'assq-delete-all)
    (defalias 'ibuffer-delete-alist 'assq-delete-all)
  ;; shamelessly stolen from gnus-util.el
  (defun ibuffer-delete-alist (key alist)
    "Delete all entries in ALIST that have a key eq to KEY."
    (let (entry)
      (while (setq entry (assq key alist))
        (setq alist (delq entry alist)))
      alist)))

;; From Paul Graham's "ANSI Common Lisp", adapted for Emacs Lisp here.
(defmacro ibuffer-aif (test true-body &rest false-body)
  "Evaluate TRUE-BODY or FALSE-BODY depending on value of TEST.
If TEST returns non-nil, bind `it' to the value, and evaluate
TRUE-BODY.  Otherwise, evaluate forms in FALSE-BODY as if in `progn'.
Compare with `if'."
  (let ((sym (gensym "--ibuffer-aif-")))
    `(let ((,sym ,test))
       (if ,sym
	   (let ((it ,sym))
	     ,true-body)
	 (progn
	   ,@false-body)))))
;; (put 'ibuffer-aif 'lisp-indent-function 2)

(defmacro ibuffer-awhen (test &rest body)
  "Evaluate BODY if TEST returns non-nil.
During evaluation of body, bind `it' to the value returned by TEST."
  `(ibuffer-aif ,test
       (progn ,@body)
     nil))
;; (put 'ibuffer-awhen 'lisp-indent-function 1)

;; XEmacs compatibility stuff
(if (fboundp 'line-beginning-position)
    (defalias 'ibuffer-line-beginning-position 'line-beginning-position)
  (defun ibuffer-line-beginning-position ()
    (save-excursion
      (beginning-of-line)
      (point))))

(if (fboundp 'line-end-position)
    (defalias 'ibuffer-line-end-position 'line-end-position)
  (defun ibuffer-line-end-position ()
    (save-excursion
      (end-of-line)
      (point))))

(if (fboundp 'window-buffer-height)
    (defalias 'ibuffer-window-buffer-height 'window-buffer-height)
  (defalias 'ibuffer-window-buffer-height 'window-displayed-height))

(defvar ibuffer-use-keymap-for-local-map
  (or (featurep 'xemacs)
      (string-match "XEmacs\\|Lucid" (emacs-version)))
  "Non-nil if we should use the `keymap' property instead of `local-map'.")

;; Deal with different interfaces to mouse events. Sigh.
(cond ((fboundp 'posn-point)
       ;; Emacs
       (defun ibuffer-event-position (event)
	 (posn-point (event-start event))))
      ((or (featurep 'xemacs)
	   (string-match "XEmacs\\|Lucid" (emacs-version)))
       (defun ibuffer-event-position (event)
	 (event-point event)))
      (t
       (error "Couldn't make a suitable definition of `ibuffer-event-position'")))

(cond ((fboundp 'posn-window)
       ;; Emacs
       (defun ibuffer-event-window (event)
	 (posn-window (event-start event))))
      ((or (featurep 'xemacs)
	   (string-match "XEmacs\\|Lucid" (emacs-version)))
       (defun ibuffer-event-window (event)
	 (event-window event)))
      (t
       (error "Couldn't make a suitable definition of `ibuffer-event-window'")))

(defgroup ibuffer nil
  "An advanced replacement for `buffer-menu'.

Ibuffer allows you to operate on buffers in a manner much like Dired.
Operations include sorting, marking by regular expression, and
selectable views."
  :link '(url-link "http://www.cis.ohio-state.edu/~walters")
  :group 'convenience)

(defcustom ibuffer-use-other-window nil
  "If non-nil, display the Ibuffer in another window by default."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-shrink-to-minimum-size nil
  "If non-nil, minimize the size of the Ibuffer window when applicable."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-case-fold-search case-fold-search
  "If non-nil, ignore case when searching."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-default-sorting-mode 'recency
  "The criteria by which to sort the buffers.

Note that this variable is local to each ibuffer buffer.  Thus, you
can have multiple ibuffer buffers open, each with a different sorted
view of the buffers."
  :type '(choice (const :tag "Last view time" :value recency)
		 (const :tag "Lexicographic" :value file-name)
		 (const :tag "Buffer size" :value size)
		 (const :tag "Major mode" :value major-mode))
  :group 'ibuffer)
(defvar ibuffer-sorting-mode nil)

(defcustom ibuffer-default-sorting-reversep nil
  "If non-nil, reverse the default sorting order."
  :type 'boolean
  :group 'ibuffer)
(defvar ibuffer-sorting-reversep nil)

(defcustom ibuffer-formats '((mark modified read-only " " (name 16 -1)
				   " " (size 6 -1 :right)
				   " " (mode 16 16 :right) " " filename)
			     (mark " " (name 16 -1) " " filename))
  "A list of ways to display buffer lines.
Each element is a list containing forms like:
 TYPE - A symbol naming the column, or a function of two arguments,
which should return a string.  The first argument is the buffer
object, and the second is the mark on that buffer.
or
 \"STRING\" - A literal string to display.
or
 (TYPE MIN-SIZE MAX-SIZE &optional ALIGN) - TYPE is a symbol naming
the column, and MIN-SIZE and MAX-SIZE are integers (or functions of no
arguments returning an integer) which constrict the size of a column.
If MAX-SIZE is -1, there is no upper bound.  The default values are 0
and -1, respectively.  There is an optional element ALIGN which
describes the alignment of the column; it can be :left , :center or
:right.

Valid TYPEs are:
 mark modified read-only name size mode process filename

By typing \\[ibuffer-switch-format], you can rotate the display
between the specified formats."
  :type '(repeat sexp)
  :group 'ibuffer)

(defcustom ibuffer-elide-long-columns t
  "If non-nil, then elide column entries which exceed their max length."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-never-show-regexps nil
  "A list of regexps matching buffer names not to display."
  :type '(repeat regexp)
  :group 'ibuffer)

(defcustom ibuffer-always-show-regexps nil
  "A list of regexps matching buffer names to always display.
Note that buffers matching one of these regexps will be shown
regardless of limiting."
  :type '(repeat regexp)
  :group 'ibuffer)

(defcustom ibuffer-maybe-show-regexps '("^ ")
  "A list of regexps maching buffer names to display conditionally.
Viewing of these buffers is enabled by giving a non-nil prefix
argument to `ibuffer-update'.

Note that this filtering occurs before limiting."
  :type '(repeat regexp)
  :group 'ibuffer)

(defvar ibuffer-current-format nil)

(defcustom ibuffer-modified-char ?*
  "The character to display for modified buffers."
  :type 'character
  :group 'ibuffer)

(defcustom ibuffer-read-only-char ?%
  "The character to display for read-only buffers."
  :type 'character
  :group 'ibuffer)

(defcustom ibuffer-marked-char ?>
  "The character to display for marked buffers."
  :type 'character
  :group 'ibuffer)

(defcustom ibuffer-deletion-char ?D
  "The character to display for buffers marked for deletion."
  :type 'character
  :group 'ibuffer)

(defcustom ibuffer-expert nil
  "If non-nil, don't ask for confirmation of \"dangerous\" operations."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-old-time 3
  "The number of days before a buffer is considered \"old\"."
  :type 'integer
  :group 'ibuffer)

(defcustom ibuffer-view-ibuffer nil
  "If non-nil, display the current Ibuffer buffer itself.
Note that this has a drawback - the data about the current Ibuffer
buffer will most likely be inaccurate.  This includes modification
state, size, etc."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-always-show-last-buffer nil
  "If non-nil, always display the previous buffer.  This variable
takes precedence over limiting, and even
`ibuffer-never-show-regexps'."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-dired-filenames t
  "If non-nil, use the `dired-directory' variable as a filename."
  :type 'boolean
  :group 'ibuffer)

(defcustom ibuffer-hooks nil
  "Hooks run when `ibuffer' is called."
  :type 'hook
  :group 'ibuffer)

(defcustom ibuffer-mode-hooks nil
  "Hooks run upon entry into `ibuffer-mode'."
  :type 'hook
  :group 'ibuffer)

(defcustom ibuffer-fontification-level (not
					(not (cond ((boundp 'global-font-lock-mode)
						    global-font-lock-mode)
						   ((boundp 'font-lock-mode)
						    font-lock-mode)
						   (t
						    nil))))
					      
  "The level of fontification used.

If t, then fontify all buffer names according to
`ibuffer-colorize-alist', then fontify marked buffers.  Also fontify
the title string according to `ibuffer-title-face'.

If `:medium', then only fontify the title string and marked buffers.

If `:mark-only', then just fontify marked buffers.

If nil, perform no fontification."
  :type '(choice (const :tag "Full" :value t)
		 (const :tag "Title and Marked" :value :medium)
		 (const :tag "Marked only" :value :mark-only)
		 (const :tag "None" :value nil))
  :group 'ibuffer)

;; Simply using a macrolet doesn't work here because Custom quotes the
;; initial argument, and then `eval's it later.
(defmacro ibuffer-one-of-faces (&rest faces)
  (setq faces (append faces '(default-face default)))
  (append
   '(cond)
   (mapcar #'(lambda (x)
	       `((facep ',x)
		 ,x)) faces)
   `((t 'default))))

(defface ibuffer-marked-face '((t (:foreground "green")))
  "Face used for displaying marked buffers."
  :group 'ibuffer)

(defface ibuffer-deletion-face '((t (:foreground "red")))
  "Face used for displaying buffers marked for deletion."
  :group 'ibuffer)

(defcustom ibuffer-title-face (ibuffer-one-of-faces
			       font-lock-type-face)
  "Face used for the title string."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-special-buffer-face (ibuffer-one-of-faces
					font-lock-keyword-face
					font-lock-variable-name-face
					font-lock-reference-face)
  "Face used for displaying \"special\" buffers."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-help-buffer-face (ibuffer-one-of-faces
				     font-lock-comment-face
				     font-lock-string-face
				     font-lock-constant-face)
  "Face used for displaying help buffers (info, apropos, help)."
  :type 'face
  :group 'ibuffer)
  
(defcustom ibuffer-dired-buffer-face (ibuffer-one-of-faces
				      font-lock-function-name-face
				      font-lock-type-face)
  "Face used for displaying dired buffers."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-read-only-buffer-face (ibuffer-one-of-faces
					  font-lock-reference-face
					  font-lock-type-face
					  font-lock-comment-face)
  "Face used for displaying read-only buffers."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-hidden-buffer-face (ibuffer-one-of-faces
				       font-lock-warning-face
				       font-lock-comment-face
				       font-lock-constant-face)
  "Face used for displaying normally hidden buffers."
  :type 'face
  :group 'ibuffer)

(defcustom ibuffer-occur-match-face (ibuffer-one-of-faces
				     font-lock-warning-face
				     underline)
  "Face used for displaying matched strings for `ibuffer-do-occur'."
  :type 'face
  :group 'ibuffer)

;; Now we don't need it anymore.
(unintern 'ibuffer-one-of-faces)

(defcustom ibuffer-fontification-alist
  `((10 buffer-read-only ,ibuffer-read-only-buffer-face)
    (15 (string-match "^*" (buffer-name)) ,ibuffer-special-buffer-face)
    (20 (string-match "^ " (buffer-name)) ,ibuffer-hidden-buffer-face)
    (25 (memq major-mode '(help-mode apropos-mode info-mode)) ,ibuffer-help-buffer-face)
    (30 (eq major-mode 'dired-mode) ,ibuffer-dired-buffer-face))
  "An alist describing how to fontify buffers.
Each element should be of the form (PRIORITY FORM FACE), where
PRIORITY is an integer, FORM is an arbitrary form to evaluate in the
buffer, and FACE is the face to use for fontification.  If the FORM
evaluates to non-nil, then FACE will be put on the buffer name.  The
element with the highest PRIORITY takes precedence.

Note that this fontification only takes place if
`ibuffer-fontification-level' is `t'."
  :type '(repeat
	  (list (integer sexp face)))
  :group 'ibuffer)

(defcustom ibuffer-directory-abbrev-alist nil
  "An alist of file name abbreviations like `directory-abbrev-alist'."
  :type '(repeat (cons :format "%v"
		       :value ("" . "")
		       (regexp :tag "From")
		       (regexp :tag "To")))
  :group 'ibuffer)

(defvar ibuffer-mode-map nil)
(unless ibuffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "0") 'digit-argument)
    (define-key map (kbd "1") 'digit-argument)
    (define-key map (kbd "2") 'digit-argument)
    (define-key map (kbd "3") 'digit-argument)
    (define-key map (kbd "4") 'digit-argument)
    (define-key map (kbd "5") 'digit-argument)
    (define-key map (kbd "6") 'digit-argument)
    (define-key map (kbd "7") 'digit-argument)
    (define-key map (kbd "8") 'digit-argument)
    (define-key map (kbd "9") 'digit-argument)

    (define-key map (kbd "m") 'ibuffer-mark-forward)
    (define-key map (kbd "t") 'ibuffer-toggle-marks)
    (define-key map (kbd "u") 'ibuffer-unmark-forward)
    (define-key map (kbd "=") 'ibuffer-diff-with-file)
    (define-key map (kbd "DEL") 'ibuffer-unmark-backward)
    (define-key map (kbd "M-DEL") 'ibuffer-unmark-all)
    (define-key map (kbd "* *") 'ibuffer-unmark-all)
    (define-key map (kbd "* M") 'ibuffer-mark-by-mode)
    (define-key map (kbd "* m") 'ibuffer-mark-modified-buffers)
    (define-key map (kbd "* u") 'ibuffer-mark-unsaved-buffers)
    (define-key map (kbd "* s") 'ibuffer-mark-special-buffers)
    (define-key map (kbd "* r") 'ibuffer-mark-read-only-buffers)
    (define-key map (kbd "* /") 'ibuffer-mark-dired-buffers)
    (define-key map (kbd "* e") 'ibuffer-mark-dissociated-buffers)
    (define-key map (kbd "* h") 'ibuffer-mark-help-buffers)
    (define-key map (kbd ".") 'ibuffer-mark-old-buffers)
    
    (define-key map (kbd "d") 'ibuffer-mark-for-delete)
    (define-key map (kbd "k") 'ibuffer-mark-for-delete)
    (define-key map (kbd "x") 'ibuffer-do-kill-on-deletion-marks)
  
    ;; immediate operations
    (define-key map (kbd "n") 'ibuffer-forward-line)
    (define-key map (kbd "SPC") 'forward-line)
    (define-key map (kbd "p") 'ibuffer-backward-line)
    (define-key map (kbd "l") 'ibuffer-redisplay)
    (define-key map (kbd "g") 'ibuffer-update)
    (define-key map "`" 'ibuffer-switch-format)
    (define-key map "-" 'ibuffer-add-to-tmp-hide)
    (define-key map "+" 'ibuffer-add-to-tmp-show)
    (define-key map "b" 'ibuffer-bury-buffer)
    (define-key map (kbd ",") 'ibuffer-toggle-sorting-mode)
    (define-key map (kbd "s i") 'ibuffer-invert-sorting)
    (define-key map (kbd "s a") 'ibuffer-do-sort-by-buffer-name)
    (define-key map (kbd "s v") 'ibuffer-do-sort-by-recency)
    (define-key map (kbd "s s") 'ibuffer-do-sort-by-size)
    (define-key map (kbd "s m") 'ibuffer-do-sort-by-major-mode)

    (define-key map (kbd "/ m") 'ibuffer-limit-by-mode)
    (define-key map (kbd "/ n") 'ibuffer-limit-by-name)
    (define-key map (kbd "/ c") 'ibuffer-limit-by-content)
    (define-key map (kbd "/ f") 'ibuffer-limit-by-filename)
    (define-key map (kbd "/ >") 'ibuffer-limit-by-size-gt)
    (define-key map (kbd "/ <") 'ibuffer-limit-by-size-lt)
    (define-key map (kbd "/ /") 'ibuffer-limit-disable)
  
    (define-key map (kbd "q") 'ibuffer-quit)
    (define-key map (kbd "h") 'describe-mode)
    (define-key map (kbd "?") 'describe-mode)

    (define-key map (kbd "% n") 'ibuffer-mark-by-name-regexp)
    (define-key map (kbd "% m") 'ibuffer-mark-by-mode-regexp)
    (define-key map (kbd "% f") 'ibuffer-mark-by-file-name-regexp)
  
    ;; marked operations
    (define-key map (kbd "A") 'ibuffer-do-view)
    (define-key map (kbd "D") 'ibuffer-do-delete)
    (define-key map (kbd "E") 'ibuffer-do-eval)
    (define-key map (kbd "I") 'ibuffer-do-query-replace-regexp)
    (define-key map (kbd "H") 'ibuffer-do-view-other-frame)
    (define-key map (kbd "N") 'ibuffer-do-shell-command-replace)
    (define-key map (kbd "O") 'ibuffer-do-occur)
    (define-key map (kbd "P") 'ibuffer-do-print)
    (define-key map (kbd "Q") 'ibuffer-do-query-replace)
    (define-key map (kbd "R") 'ibuffer-do-rename-uniquely)
    (define-key map (kbd "S") 'ibuffer-do-save)
    (define-key map (kbd "T") 'ibuffer-do-toggle-read-only)
    (define-key map (kbd "U") 'ibuffer-do-replace-regexp)
    (define-key map (kbd "V") 'ibuffer-do-revert)
    (define-key map (kbd "B") 'ibuffer-do-bury)
    (define-key map (kbd "W") 'ibuffer-do-view-and-eval)
    (define-key map (kbd "X") 'ibuffer-do-shell-command)
  
    (define-key map (kbd "k") 'ibuffer-do-kill-lines)

    (define-key map (kbd "RET") 'ibuffer-visit-buffer)
    (define-key map (kbd "e") 'ibuffer-visit-buffer)
    (define-key map (kbd "f") 'ibuffer-visit-buffer)
    (define-key map (kbd "o") 'ibuffer-visit-buffer-other-window)
    (define-key map (kbd "C-o") 'ibuffer-visit-buffer-other-window-noselect)
    (define-key map (kbd "v") 'ibuffer-do-view)
    (define-key map (kbd "C-x 4 RET") 'ibuffer-visit-buffer-other-window)
    (define-key map (kbd "C-x 5 RET") 'ibuffer-visit-buffer-other-frame)

    (setq ibuffer-mode-map map))
  )
 
(defvar ibuffer-name-map nil)
(unless ibuffer-name-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ibuffer-mode-map)
    (define-key map [(mouse-1)] 'ibuffer-mouse-toggle-mark)
    (define-key map [(mouse-2)] 'ibuffer-mouse-visit-buffer)
    (define-key map [down-mouse-3] 'ibuffer-mouse-popup-menu)
    
    (setq ibuffer-name-map map)))

(defvar ibuffer-mode-name-map nil)
(unless ibuffer-mode-name-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ibuffer-mode-map)
    (define-key map [(mouse-2)] 'ibuffer-mouse-limit-by-mode)
    (define-key map (kbd "RET") 'ibuffer-interactive-limit-by-mode)
    (setq ibuffer-mode-name-map map)))

;; quiet the byte-compiler
(defvar ibuffer-mode-operate-menu nil)
(defvar ibuffer-mode-mark-menu nil)
(defvar ibuffer-mode-regexp-menu nil)
(defvar ibuffer-mode-sort-menu nil)
(defvar ibuffer-mode-limit-menu nil)
(defvar ibuffer-mode-immediate-menu nil)

(defvar ibuffer-mode-hooks nil)

(defvar ibuffer-delete-window-on-quit nil
  "Whether or not to delete the window upon exiting `ibuffer'.")

(defvar ibuffer-did-modification nil)

(defvar ibuffer-sorting-functions-alist nil
  "An alist of functions which describe how to sort buffers.

Note: You most likely do not want to modify this variable directly;
use `ibuffer-define-sorter' instead.

The alist elements are constructed like (NAME DESCRIPTION FUNCTION)
Where NAME is a symbol describing the sorting method, DESCRIPTION is a
short string which will be displayed in the minibuffer and menu, and
FUNCTION is a function of two arguments, which will be the buffers to
compare.")

(defvar ibuffer-limiting-qualifiers nil
  "A list like (SYMBOL . QUALIFIER) which filters the current buffer list.
See also `ibuffer-limiting-alist'.")

;; This is now frobbed by `ibuffer-define-limiter'.
(defvar ibuffer-limiting-alist nil
  "An alist of (SYMBOL DESCRIPTION FUNCTION) which describes a filter.

You most likely do not want to modify this variable directly; see
`ibuffer-define-limiter'.

SYMBOL is the symbolic name of the filter.  DESCRIPTION is used when
displaying information to the user.  FUNCTION is given a buffer and
the value of the qualifier, and returns non-nil if and only if the
buffer should be displayed.")

(defvar ibuffer-tmp-hide-regexps nil
  "A list of regexps which should match buffer names to not show.")
(make-variable-buffer-local 'ibuffer-tmp-hide-regexps)
  
(defvar ibuffer-tmp-show-regexps nil
  "A list of regexps which should match buffer names to always show.")
(make-variable-buffer-local 'ibuffer-tmp-show-regexps)
  
;;; Menu definitions
(unless ibuffer-mode-limit-menu
  (easy-menu-define
    ibuffer-mode-limit-menu ibuffer-mode-map ""
    '("Limit"
      ["Disable all limiting" ibuffer-limit-disable t]
      ["Limit by major mode..." ibuffer-limit-by-mode t]
      ["Limit by buffer name..." ibuffer-limit-by-name t]
      ["Limit by filename..." ibuffer-limit-by-filename t]
      ["Limit by size less than..." ibuffer-limit-by-size-lt t]
      ["Limit by size greater than..." ibuffer-limit-by-size-gt t]
      ["Limit by content..." ibuffer-limit-by-content t])))

(unless ibuffer-mode-sort-menu
  (easy-menu-define
    ibuffer-mode-sort-menu ibuffer-mode-map ""
    '("Sort"
      ["Toggle sorting mode" ibuffer-toggle-sorting-mode t]
      ["Reverse sorting order" ibuffer-invert-sorting t]
      ["Sort by view time" ibuffer-do-sort-by-recency t]
      ["Sort lexicographically" ibuffer-do-sort-by-buffer-name t]
      ["Sort by buffer size" ibuffer-do-sort-by-size t]
      ["Sort by major mode" ibuffer-do-sort-by-major-mode t])))

(unless ibuffer-mode-immediate-menu
  (easy-menu-define
    ibuffer-mode-immediate-menu ibuffer-mode-map ""
    '("Immediate"
      ["View this buffer" ibuffer-visit-buffer t]
      ["View (other window)" ibuffer-visit-buffer-other-window t]
      ["View (other frame)" ibuffer-visit-buffer-other-frame t]
      ["Diff with file" ibuffer-diff-with-file t]
      ["Redisplay" ibuffer-redisplay t]
      ["Update" ibuffer-update t]
      ["Switch display format" ibuffer-switch-format t]
      ["Customize Ibuffer" ibuffer-customize t])))

(unless ibuffer-mode-regexp-menu
  (easy-menu-define
    ibuffer-mode-regexp-menu ibuffer-mode-map ""
    '("Regexp"
      ["Mark by buffer name..." ibuffer-mark-by-name-regexp t]
      ["Mark by major mode..." ibuffer-mark-by-mode-regexp t]
      ["Mark by file name..." ibuffer-mark-by-file-name-regexp t])))

(unless ibuffer-mode-mark-menu
  (easy-menu-define
    ibuffer-mode-mark-menu ibuffer-mode-map ""
    '("Mark"
      ["Toggle marks" ibuffer-toggle-marks t]
      ["Mark" ibuffer-mark-forward t]
      ["Unmark" ibuffer-unmark-forward t]
      ["Mark by mode..." ibuffer-mark-by-mode t]
      ["Mark modified buffers" ibuffer-mark-modified-buffers t]
      ["Mark unsaved buffers" ibuffer-mark-unsaved-buffers t]
      ["Mark read-only buffers" ibuffer-mark-read-only-buffers t]
      ["Mark special buffers" ibuffer-mark-special-buffers t]
      ["Mark dired buffers" ibuffer-mark-dired-buffers t]
      ["Mark dissociated buffers" ibuffer-mark-dissociated-buffers t]
      ["Mark help buffers" ibuffer-mark-help-buffers t]
      ["Mark old buffers" ibuffer-mark-old-buffers t]
      ["Unmark All" ibuffer-unmark-all t])))

(unless ibuffer-mode-operate-menu
  (easy-menu-define
    ibuffer-mode-operate-menu ibuffer-mode-map ""
    '("Operate"
      ["View" ibuffer-do-view t]
      ["View (separate frame)" ibuffer-do-view-other-frame t]
      ["Save" ibuffer-do-save t]
      ["Replace (regexp)..." ibuffer-do-replace-regexp t]
      ["Query Replace..." ibuffer-do-query-replace t]
      ["Query Replace (regexp)..." ibuffer-do-query-replace-regexp t]
      ["Print" ibuffer-do-print t]
      ["Revert" ibuffer-do-revert t]
      ["Bury" ibuffer-do-bury t]
      ["Rename Uniquely" ibuffer-do-rename-uniquely t]
      ["Kill" ibuffer-do-delete t]
      ["List lines matching..." ibuffer-do-occur t]
      ["Shell Command..." ibuffer-do-shell-command t]
      ["Shell Command (replace)..." ibuffer-do-shell-command-replace t]
      ["Eval..." ibuffer-do-eval t]
      ["Eval (viewing buffer)..." ibuffer-do-view-and-eval t])))

(defvar ibuffer-popup-menu
  '("Ibuffer"
    ("Operate"
     ("Save" . ibuffer-do-save)
     ("Replace (regexp)..." . ibuffer-do-replace-regexp)
     ("Query Replace..." . ibuffer-do-query-replace)
     ("Query Replace Regexp..." . ibuffer-do-query-replace-regexp)
     ("Print" . ibuffer-do-print)
     ("Revert" . ibuffer-do-revert)
     ("Rename Uniquely" . ibuffer-do-rename-uniquely)
     ("Kill" . ibuffer-do-delete)
     ("List lines matching..." . ibuffer-do-occur)
     ("Shell Command..." . ibuffer-do-shell-command)
     ("Shell Command (replace)..." . ibuffer-do-shell-command-replace)
     ("Eval..." . ibuffer-do-eval)
     ("Eval (viewing buffer)..." . ibuffer-do-view-and-eval))))

;;; Utility functions
(defun ibuffer-columnize-and-insert-list (list &optional pad-width)
  "Insert LIST into the current buffer in as many columns as possible.
The maximum number of columns is determined by the current window
width and the longest string in LIST."
  (unless pad-width
    (setq pad-width 3))
  (let ((width (window-width))
	(max (+ (apply #'max (mapcar #'length list))
		pad-width)))
    (let ((columns (/ width max)))
      (when (zerop columns)
	(setq columns 1))
      (while list
	(dotimes (i (1- columns))
	  (insert (concat (car list) (make-string (- max (length (car list)))
						  ? )))
	  (setq list (cdr list)))
	(when (not (null list))
	  (insert (pop list)))
	(insert "\n")))))

(defun ibuffer-accumulate-lines (count)
  (save-excursion
    (let ((forwardp (> count 0))
	  (result nil))
      (while (not (or (zerop count)
		      (if forwardp
			  (eobp)
			(bobp))))
	(if forwardp
	    (decf count)
	  (incf count))
	(push
	 (buffer-substring
	  (ibuffer-line-beginning-position)
	  (ibuffer-line-end-position))
	 result)
	(forward-line (if forwardp 1 -1)))
      (nreverse result))))

;;; Miscellaneous functions
(defun ibuffer-customize ()
  "Begin customizing Ibuffer interactively."
  (interactive)
  (customize-group 'ibuffer))

(defun ibuffer-add-to-tmp-hide (regexp)
  "Add REGEXP to `ibuffer-tmp-hide-regexps'.
This means that buffers whose name matches REGEXP will not be shown
for this ibuffer session."
  (interactive
   (list
    (read-from-minibuffer "Never show buffers matching: "
			  (regexp-quote (buffer-name (ibuffer-current-buffer t))))))
  (push regexp ibuffer-tmp-hide-regexps))

(defun ibuffer-add-to-tmp-show (regexp)
  "Add REGEXP to `ibuffer-tmp-show-regexps'.
This means that buffers whose name matches REGEXP will always be shown
for this ibuffer session."
  (interactive
   (list
    (read-from-minibuffer "Always show buffers matching: "
			  (regexp-quote (buffer-name (ibuffer-current-buffer t))))))
  (push regexp ibuffer-tmp-show-regexps))

(defun ibuffer-mouse-toggle-mark (event)
  "Toggle the marked status of the buffer chosen with the mouse."
  (interactive "e")
  (unwind-protect
      (save-excursion
	(goto-char (ibuffer-event-position event))
	(let ((mark (ibuffer-current-mark)))
	  (setq buffer-read-only nil)
	  (if (eq mark ibuffer-marked-char)
	      (ibuffer-set-mark ? )
	    (ibuffer-set-mark ibuffer-marked-char))))
    (setq buffer-read-only t)))

(defun ibuffer-mouse-visit-buffer (event)
  "Visit the buffer chosen with the mouse."
  (interactive "e")
  (switch-to-buffer
   (save-excursion
     (goto-char (ibuffer-event-position event))
     (ibuffer-current-buffer))))

(defun ibuffer-mouse-popup-menu (event)
  "Display a menu of operations."
  (interactive "e")
  (let ((pt (ibuffer-event-position event)))
    (unwind-protect
	(save-excursion
	  (setq buffer-read-only nil)
	  (let ((choice (x-popup-menu t ibuffer-popup-menu)))
	    (message "choice: %s" choice)
	    (when choice
	      (ibuffer-save-marks
		;; hm.  we could probably do this in a better fashion
		(ibuffer-unmark-all)
		(setq buffer-read-only nil)
		(goto-char pt)
		(ibuffer-set-mark ibuffer-marked-char)
		(setq buffer-read-only nil)
		(call-interactively choice)
		(setq buffer-read-only nil)))))
      (setq buffer-read-only t))))

(defun ibuffer-mouse-limit-by-mode (event)
  "Enable or disable limiting by the major mode chosen via mouse."
  (interactive "e")
  (ibuffer-interactive-limit-by-mode (ibuffer-event-position event)))

(defun ibuffer-interactive-limit-by-mode (point)
  (interactive "d")
  (goto-char point)
  (let ((buf (ibuffer-current-buffer)))
    (ibuffer-limit-by-mode (if (ibuffer-get-qualifier 'mode)
			       nil
			     (with-current-buffer buf
			       major-mode)))))
(defun ibuffer-goto-beg ()
  (goto-char (point-min))
  (while (and (get-text-property (point) 'ibuffer-title)
	      (not (eobp)))
    (forward-line 1)))

(defun ibuffer-backward-line (&optional arg)
  "Move backwards ARG lines, wrapping around the list if necessary."
  (interactive "P")
  (unless arg
    (setq arg 1))
  (while (> arg 0)
    (forward-line -1)
    (when (get-text-property (point) 'ibuffer-title)
      (goto-char (point-max))
      (forward-line -1)
      (setq arg 0))
    (setq arg (1- arg))))

(defun ibuffer-forward-line (&optional arg)
  "Move forward ARG lines, wrapping around the list if necessary."
  (interactive "P")
  (unless arg
    (setq arg 1))
  (while (> arg 0)
    (forward-line 1)
    (when (eobp)
      (ibuffer-goto-beg))
    (setq arg (1- arg))))

(defmacro ibuffer-save-marks (&rest body)
  "Save the marked status of the buffers and execute BODY; restore marks."
  `(let ((ibuffer-save-marks-tmp-mark-list (ibuffer-current-state-list)))
     (unwind-protect
	 (progn
	   ,@body)
       (ibuffer-insert-buffers-and-marks ibuffer-save-marks-tmp-mark-list))
     (ibuffer-redisplay t)))
;; (put 'ibuffer-save-marks 'lisp-indent-function 0)

(defun ibuffer-visit-buffer ()
  "Visit the buffer on this line."
  (interactive)
  (let ((buf (ibuffer-current-buffer)))
    (unless (buffer-live-p buf)
      (error "Buffer %s has been killed!" buf))
    (bury-buffer (current-buffer))
    (switch-to-buffer buf)))

(defun ibuffer-visit-buffer-other-window (&optional noselect)
  "Visit the buffer on this line in another window."
  (interactive)
  (let ((buf (ibuffer-current-buffer)))
    (unless (buffer-live-p buf)
      (error "Buffer %s has been killed!" buf))
    (bury-buffer (current-buffer))
    (if noselect
	(let ((curwin (selected-window)))
	  (pop-to-buffer buf)
	  (select-window curwin))
      (switch-to-buffer-other-window buf))))

(defun ibuffer-visit-buffer-other-window-noselect ()
  "Visit the buffer on this line in another window, but don't select it."
  (interactive)
  (ibuffer-visit-buffer-other-window t))

(defun ibuffer-visit-buffer-other-frame ()
  "Visit the buffer on this line in another frame."
  (interactive)
  (let ((buf (ibuffer-current-buffer)))
    (unless (buffer-live-p buf)
      (error "Buffer %s has been killed!" buf))
    (bury-buffer (current-buffer))
    (switch-to-buffer-other-frame buf)))

;; From Christoph Conrad <christoph.conrad@gmx.de> and
;; Stefan Reich�r <reichoer@riic.at>
(defun ibuffer-bury-buffer ()
  "Bury the buffer on this line."
  (interactive)
  (let ((buf (ibuffer-current-buffer))
	(line (+ 1 (count-lines 1 (point)))))
    (unless (buffer-live-p buf)
      (error "Buffer %s has been killed!" buf))
    (bury-buffer buf)
    (ibuffer-update nil t)
    (goto-line line)))
  
(defun ibuffer-diff-with-file ()
  "View the differences between this buffer and its associated file.
This requires the external program \"diff\" to be in your `exec-path'."
  (interactive)
  (let* ((buf (ibuffer-current-buffer))
	 (buf-filename (with-current-buffer buf
			 buffer-file-name)))
    (unless (buffer-live-p buf)
      (error "Buffer %s has been killed" buf))
    (unless buf-filename
      (error "Buffer %s has no associated file" buf))
    (let ((diff-buf (get-buffer-create "*Ibuffer-diff*")))
      (with-current-buffer diff-buf
	(setq buffer-read-only nil)
	(erase-buffer))
      (let ((tempfile (ibuffer-make-temp-file "ibuffer-diff-")))
	(unwind-protect
	    (progn
	      (with-current-buffer buf
		(write-region (point-min) (point-max) tempfile nil 'nomessage))
	      (if (zerop
		   (apply #'call-process "diff" nil diff-buf nil
			  (append
			   (when (and (boundp 'ediff-custom-diff-options)
				      (stringp ediff-custom-diff-options))
			     (list ediff-custom-diff-options))
			   (list buf-filename tempfile))))
		  (message "No differences found")
		(progn
		  (with-current-buffer diff-buf
		    (goto-char (point-min))
		    (if (fboundp 'diff-mode)
			(diff-mode)
		      (fundamental-mode)))
		  (display-buffer diff-buf))))
	  (when (file-exists-p tempfile)
	    (delete-file tempfile)))))
      nil))

(defun ibuffer-mark-on-buffer (func)
  (let ((count
	 (ibuffer-map-lines
	  #'(lambda (buf mark beg end)
	      (when (funcall func buf)
		(ibuffer-set-mark ibuffer-marked-char)
		t)))))
    (ibuffer-redisplay t)
    (message "Marked %s buffers" count)))

(defun ibuffer-mark-by-name-regexp (regexp)
  "Mark all buffers whose name matches REGEXP."
  (interactive "sMark by name (regexp): ")
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (string-match regexp (buffer-name buf)))))

(defun ibuffer-mark-by-mode-regexp (regexp)
  "Mark all buffers whose major mode matches REGEXP."
  (interactive "sMark by major mode (regexp): ")
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (with-current-buffer buf
	 (string-match regexp mode-name)))))

(defun ibuffer-mark-by-file-name-regexp (regexp)
  "Mark all buffers whose file name matches REGEXP."
  (interactive "sMark by file name (regexp): ")
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (let ((name (or (buffer-file-name buf)
		       (with-current-buffer buf
			 (and ibuffer-dired-filenames
			      (stringp dired-directory)
			      dired-directory)))))
	 (when name
	   (string-match regexp name))))))

(defun ibuffer-mark-by-mode (mode)
  "Mark all buffers whose major mode equals MODE."
  (interactive
   (list (intern (completing-read "Mark by major mode: " obarray
				  #'(lambda (e)
				      ;; kind of a hack...
                                      (and (fboundp e)
                                           (string-match "-mode$"
                                                         (symbol-name e))))
				  t
				  (let ((buf (ibuffer-current-buffer)))
				    (if (and buf (buffer-live-p buf))
					(with-current-buffer buf
					  (symbol-name major-mode))
				      ""))))))
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (with-current-buffer buf
	 (eq major-mode mode)))))
  

(defun ibuffer-mark-modified-buffers ()
  "Mark all modified buffers."
  (interactive)
  (ibuffer-mark-on-buffer
   #'(lambda (buf) (buffer-modified-p buf))))

(defun ibuffer-mark-unsaved-buffers ()
  "Mark all modified buffers that have an associated file."
  (interactive)
  (ibuffer-mark-on-buffer
   #'(lambda (buf) (and (with-current-buffer buf buffer-file-name)
			(buffer-modified-p buf)))))

(defun ibuffer-mark-special-buffers ()
  "Mark all buffers whose name begins and ends with '*'."
  (interactive)
  (ibuffer-mark-on-buffer
   #'(lambda (buf) (string-match "^\\*.+\\*$"
				 (buffer-name buf)))))

(defun ibuffer-mark-read-only-buffers ()
  "Mark all read-only buffers."
  (interactive)
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (with-current-buffer buf
	 buffer-read-only))))

(defun ibuffer-mark-dired-buffers ()
  "Mark all `dired' buffers."
  (interactive)
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (with-current-buffer buf
	 (eq major-mode 'dired-mode)))))

(defun ibuffer-mark-dissociated-buffers ()
  "Mark all buffers whose associated file does not exist."
  (interactive)
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (with-current-buffer buf
	 (or
	  (and buffer-file-name
	       (not (file-exists-p buffer-file-name)))
	  (and (eq major-mode 'dired-mode)
	       ibuffer-dired-filenames
	       (stringp dired-directory)
	       (not (file-exists-p (file-name-directory dired-directory)))))))))

(defun ibuffer-mark-help-buffers ()
  "Mark buffers like *Help*, *Apropos*, *Info*."
  (interactive)
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (with-current-buffer buf
	 (or
	  (eq major-mode 'apropos-mode)
	  (eq major-mode 'help-mode)
	  (eq major-mode 'info-mode))))))

(defun ibuffer-mark-old-buffers ()
  "Mark buffers which have not been viewed in `ibuffer-old-time' days."
  (interactive)
  (ibuffer-mark-on-buffer
   #'(lambda (buf)
       (with-current-buffer buf
	 ;; hacked from midnight.el
	 (when buffer-display-time
	   (let* ((tm (current-time))
		  (now (+ (* (float (ash 1 16)) (car tm))
			  (float (cadr tm)) (* 0.0000001 (caddr tm))))
		  (then (+ (* (float (ash 1 16))
			      (car buffer-display-time))
			   (float (cadr buffer-display-time))
			   (* 0.0000001 (caddr buffer-display-time)))))
	     (> (- now then) (* 24 60 60 ibuffer-old-time))))))))

(defun ibuffer-do-view (&optional other-frame)
  "View marked buffers.
If optional argument OTHER-FRAME is non-nil, then display each
marked buffer in a new frame.  Otherwise, display each buffer as
a new window in the current frame."
  (interactive)
  (let ((marked-bufs (ibuffer-get-marked-buffers)))
    (if (null marked-bufs)
	(message "No buffers marked; use 'm' to mark a buffer")
      (unless (and other-frame
		   (not ibuffer-expert)
		   (> (length marked-bufs) 3)
		   (not (yes-or-no-p (format "Really create a new frame for %s buffers? "
					  (length marked-bufs)))))
	(set-buffer-modified-p nil)
	(delete-other-windows)
	(switch-to-buffer (pop marked-bufs))
	(let ((height (/ (1- (frame-height)) (1+ (length marked-bufs)))))
	  (mapcar (if other-frame
		      #'(lambda (buf)
			  (let ((curframe (selected-frame)))
			    (select-frame (new-frame))
			    (switch-to-buffer buf)
			    (select-frame curframe)))
		    #'(lambda (buf)
			(split-window nil height)
			(other-window 1)
			(switch-to-buffer buf)))
		  marked-bufs))))))

(defun ibuffer-do-view-other-frame ()
  "View each of the marked buffers in a separate frame."
  (interactive)
  (ibuffer-do-view t))

;;; An implementation of multi-buffer `occur'.

(define-derived-mode ibuffer-occur-mode occur-mode "Ibuffer-Occur"
  "A special form of Occur mode for multiple buffers.
Note this major mode is not meant for interactive use!
See also `occur-mode'."
  (define-key ibuffer-occur-mode-map (kbd "n") 'forward-line)
  (define-key ibuffer-occur-mode-map (kbd "q") 'bury-buffer)
  (define-key ibuffer-occur-mode-map (kbd "p") 'previous-line)
  (define-key ibuffer-occur-mode-map (kbd "RET") 'ibuffer-occur-display-occurence)
  (define-key ibuffer-occur-mode-map (kbd "f") 'ibuffer-occur-goto-occurence)
  (define-key ibuffer-occur-mode-map [(mouse-2)] 'ibuffer-occur-mouse-display-occurence)
  (setq revert-buffer-function #'ibuffer-occur-revert-buffer-function)
  (setq buffer-read-only nil)
  (erase-buffer)
  (setq buffer-read-only t)
  (message (concat
	    "Use RET "
	    (if (or (and (< 21 emacs-major-version)
			 window-system)
		    (featurep 'mouse))
	       "or mouse-2 ")
	    "to display an occurence.")))

(defun ibuffer-occur-mouse-display-occurence (e)
  "Display occurence on this line in another window."
  (interactive "e")
  (let* ((occurbuf (window-buffer (ibuffer-event-window e)))
	 (target (with-current-buffer occurbuf
		   (get-text-property (ibuffer-event-position e)
				      'ibuffer-occur-target))))
    (unless target
      (error "No occurence on this line"))
    (let ((buf (car target))
	  (line (cdr target)))
      (switch-to-buffer occurbuf)
      (delete-other-windows)
      (pop-to-buffer buf)
      (goto-line line))))

(defun ibuffer-occur-goto-occurence ()
  "Switch to the buffer which has the occurence on this line."
  (interactive)
  (ibuffer-occur-display-occurence t))

(defun ibuffer-occur-display-occurence (&optional goto)
  "Display occurence on this line in another window."
  (interactive "P")
  (let ((target (get-text-property (point) 'ibuffer-occur-target)))
    (unless target
      (error "No occurence on this line"))
    (let ((buf (car target))
	  (line (cdr target)))
      (delete-other-windows)
      (if goto
	  (switch-to-buffer buf)
	(pop-to-buffer buf))
      (goto-line line))))

(defun ibuffer-do-occur (regexp &optional nlines)
  "View lines which match REGEXP in all marked buffers.
Optional argument NLINES says how many lines of context to display: it
defaults to one."
  (interactive
   (list (let* ((default (car regexp-history))
		(input
		 (read-from-minibuffer
		  (if default
		      (format "List lines matching regexp (default `%s'): "
			      default)
		    "List lines matching regexp: ")
		  nil
		  nil
		  nil
		  'regexp-history)))
	   (if (equal input "")
	       default
	     input))
	 current-prefix-arg))
  (if (or (not (integerp nlines))
	  (< nlines 0))
      (setq nlines 1))
  (if (= (ibuffer-count-marked-lines) 0)
      (message "No buffers marked; use 'm' to mark a buffer")
    (let ((ibuffer-do-occur-bufs nil))
      ;; Accumulate a list of marked buffers
      (ibuffer-map-marked-lines
       #'(lambda (buf mark beg end)
	   (push buf ibuffer-do-occur-bufs)))
      (ibuffer-do-occur-1 regexp ibuffer-do-occur-bufs
			  (get-buffer-create "*Ibuffer-occur*")
			  nlines))))

(defun ibuffer-do-occur-1 (regexp buffers out-buf nlines)
  (let ((count (ibuffer-occur-engine regexp buffers out-buf nlines)))
    (if (> count 0)
	(progn
	  (switch-to-buffer out-buf)
	  (setq buffer-read-only t)
	  (delete-other-windows)
	  (goto-char (point-min))
	  (message "Found %s matches in %s buffers" count (length buffers)))
      (message "No matches found"))))

(defvar ibuffer-occur-props nil)
(make-variable-buffer-local 'ibuffer-occur-props)

(defun ibuffer-occur-revert-buffer-function (ignore-auto noconfirm)
  "Update the *Ibuffer occur* buffer."
  (assert (eq major-mode 'ibuffer-occur-mode))
  (ibuffer-do-occur-1 (car ibuffer-occur-props)
		      (cadr ibuffer-occur-props)
		      (current-buffer)
		      (caddr ibuffer-occur-props)))

(defun ibuffer-occur-engine (regexp buffers out-buf nlines)
  (macrolet ((insert-get-point
	      (&rest args)
	      `(progn
		 (insert ,@args)
		 (point)))
	     (maybe-put-text-property
	      (beg end &rest args)
	      `(when ibuffer-fontification-level
		 (put-text-property ,beg ,end ,@args)))
	     (maybe-ibuffer-propertize
	      (obj &rest args)
	      (let ((objsym (gensym "--maybe-ibuffer-propertize-")))
		`(let ((,objsym ,obj))
		   (if ibuffer-fontification-level
		       (ibuffer-propertize ,objsym ,@args)
		     ,objsym)))))
    (with-current-buffer out-buf
      (ibuffer-occur-mode)
      (setq buffer-read-only nil)
      (let ((globalcount 0))
	;; Map over all the buffers
	(dolist (buf buffers)
	  (when (buffer-live-p buf)
	    (let ((c 0)	;; count of matched lines
		  (l 1)	;; line count
		  (headerpt (with-current-buffer out-buf (point))))
	      (save-excursion
		(set-buffer buf)
		(save-excursion
		  (goto-char (point-min)) ;; begin searching in the buffer
		  (while (not (eobp))
		    ;; The line we're matching against
		    (let ((curline (buffer-substring
				    (ibuffer-line-beginning-position)
				    (ibuffer-line-end-position))))
		      (when (string-match regexp curline)
			(incf c) ;; increment match count
			(incf globalcount)
			;; Depropertize the string, and maybe highlight the matches
			(setq curline
			      (progn
				(ibuffer-depropertize-string curline t)
				(when ibuffer-fontification-level
				  (let ((len (length curline))
					(start 0))
				    (while (and (< start len)
						(string-match regexp curline start))
				      (put-text-property (match-beginning 0)
							 (match-end 0)
							 'face ibuffer-occur-match-face
							 curline)
				      (setq start (match-end 0)))))
				curline))
			;; Generate the string to insert for this match
			(let ((data
			       (if (= nlines 1)
				   ;; The simple display style
				   (concat (maybe-ibuffer-propertize
					    (format "%-6d:" l)
					    'face 'bold)
					   curline
					   "\n")
				 ;; The complex multi-line display style
				 (let ((prevlines (nreverse
						   (ibuffer-accumulate-lines (- nlines))))
				       (nextlines (ibuffer-accumulate-lines nlines))
				       ;; The lack of `flet' seriously sucks.
				       (fun #'(lambda (lines)
						(mapcar
						 #'(lambda (line)
						     (concat "      :" line "\n"))
						 lines))))
				   (setq prevlines (funcall fun prevlines))
				   (setq nextlines (funcall fun nextlines))
				   ;; Yes, I am trying to win the award for the
				   ;; most consing.
				   (apply #'concat
					  (nconc
					   prevlines
					   (list
					    (concat
					     (maybe-ibuffer-propertize
					      (format "%-6d" l)
					      'face 'bold)
					     ":"
					     curline
					     "\n"))
					   nextlines))))))
			  ;; Actually insert the match display data
			  (with-current-buffer out-buf
			    (let ((beg (point))
				  (end (insert-get-point
					data)))
			      (unless (= nlines 1)
				(insert "-------\n"))
			      (put-text-property
			       beg (1- end) 'ibuffer-occur-target (cons buf l))
			      (put-text-property
			       beg (1- end) 'mouse-face 'highlight))))))
		    ;; On to the next line...
		    (incf l)
		    (forward-line 1))))
	      (when (not (zerop c)) ;; is the count zero?
		(with-current-buffer out-buf
		  (goto-char headerpt)
		  (let ((beg (point))
			(end (insert-get-point
			      (format "%d lines matching \"%s\" in buffer %s\n"
				      c regexp (buffer-name buf)))))
		    (maybe-put-text-property beg (1- end) 'face 'underline))
		  (goto-char (point-max)))))))
	(setq ibuffer-occur-props (list regexp buffers nlines))
	;; Return the number of matches
	globalcount))))

(defun ibuffer-confirm-operation-on (operation names)
  "Display a buffer asking whether to perform OPERATION on NAMES."
  (unless ibuffer-expert
    (if (= (length names) 1)
	(yes-or-no-p (format "Really %s buffer %s? " operation (car names)))
      (let ((buf (get-buffer-create "*Ibuffer confirmation*")))
	(with-current-buffer buf
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (ibuffer-columnize-and-insert-list names)
	  (goto-char (point-min))
	  (setq buffer-read-only t))
	(let ((lastwin (car (last (ibuffer-window-list)))))
	  ;; Now attempt to display the buffer...
	  (save-window-excursion
	    (select-window lastwin)
	    ;; The window might be too small to split; in that case,
	    ;; try a few times to increase its size before giving up.
	    (let ((attempts 0)
		  (trying t))
	      (while trying
		(condition-case err
		    (progn
		      (split-window)
		      (setq trying nil))
		  (error
		   ;; Handle a failure
		   (if (or (> (incf attempts) 4)
			   (and (stringp (cadr err))
				;; This definitely falls in the ghetto hack category...
				(not (string-match "too small" (cadr err)))))
		       (apply #'signal err)
		     (enlarge-window 3))))))
	    ;; This part doesn't work correctly sometimes under XEmacs.
	    (select-window (next-window))
	    (switch-to-buffer buf)
	    (if (> (- (ibuffer-window-buffer-height (selected-window)) 2)
		   (window-height))
		(unwind-protect
		    (progn
		      (delete-other-windows)
		      (yes-or-no-p (format "Really %s %d buffers? "
					   operation (length names))))
		  (kill-buffer buf))
	      (unwind-protect
		  (progn
		    (shrink-window-if-larger-than-buffer)
		    (yes-or-no-p (format "Really %s %d buffers? "
					 operation (length names))))
		(delete-window)
		(kill-buffer buf)))))))))

(defmacro* ibuffer-define-op (op args
				 (&key documentation
				       interactive
				       mark
				       modifier-p
				       dangerous
				       (opstring "operated on")
				       (active-opstring "Operate on")
				       complex)
				 &rest body)
  `(defun ,(intern (concat "ibuffer-do-" (symbol-name op))) ,args
     ,(if (stringp documentation)
	  documentation
	(format "%s marked buffers." active-opstring))
     ,(if (not (null interactive))
	  `(interactive ,interactive)
	'(interactive))
     (assert (eq major-mode 'ibuffer-mode))
     (setq ibuffer-did-modification nil)
     (let ((marked-names  (,(case mark
			      (:deletion
			       'ibuffer-deletion-marked-buffer-names)
			      (t
			       'ibuffer-marked-buffer-names)))))
       (if (= (length marked-names) 0)
	   (message "No buffers marked; use 'm' to mark a buffer")
	 ,(let* ((finish (append
			  '(progn)
			  (if (eq modifier-p t)
			      '((setq ibuffer-did-modification t))
			      ())
			    `((ibuffer-redisplay t)
			      (message ,(concat (capitalize opstring)
						" %s buffers") count))))
		   (inner-body (if complex
				   `(progn ,@body)
				 `(progn
				    (with-current-buffer buf
				      (save-excursion
					,@body))
				    t)))
		   (body `(let ((count
				 (,(case mark
				     (:deletion
				      'ibuffer-map-deletion-lines)
				     (t
				      'ibuffer-map-marked-lines))
				  #'(lambda (buf mark beg end)
				      ,(if (eq modifier-p :maybe)
					   `(let ((ibuffer-tmp-previous-buffer-modification
						   (buffer-modified-p buf)))
					      (prog1 ,inner-body
						(when (not (eq ibuffer-tmp-previous-buffer-modification
							       (buffer-modified-p buf)))
						  (setq ibuffer-did-modification t))))
					 inner-body)))))
			    ,finish)))
	      (if dangerous
		  `(when (ibuffer-confirm-operation-on ,active-opstring marked-names)
		     ,body)
		body))))))

;; (put 'ibuffer-define-op 'lisp-indent-function 2)

(ibuffer-define-op save ()
  (:documentation
   "Save marked buffers as with `save-buffer'."
   :complex t
   :opstring "saved"
   :modifier-p :maybe)
  (when (buffer-modified-p buf)
    (if (not (with-current-buffer buf
	       buffer-file-name))
	;; handle the case where we're prompted
	;; for a file name
	(save-window-excursion
	  (switch-to-buffer buf)
	  (save-buffer))
      (with-current-buffer buf
	(save-buffer))))
  t)

(ibuffer-define-op print ()
  (:documentation
   "Print marked buffers as with `print-buffer'."
   :opstring "printed"
   :modifier-p nil)
  (print-buffer))

(ibuffer-define-op rename-uniquely ()
  (:documentation
   "Rename marked buffers as with `rename-uniquely'."
   :opstring "renamed"
   :modifier-p t)
  (rename-uniquely))

(ibuffer-define-op toggle-read-only ()
  (:documentation
   "Toggle read only status in marked buffers."
   :opstring "toggled read only status in"
   :modifier-p t)
  (toggle-read-only))

(ibuffer-define-op replace-regexp (from-str to-str)
  (:documentation
   "Perform a `replace-regexp' in marked buffers."
   :interactive
   (let* ((from-str (read-from-minibuffer "Replace regexp: "))
	  (to-str (read-from-minibuffer (concat "Replace " from-str
						" with: "))))
     (list from-str to-str))
   :opstring "replaced in"
   :complex t
   :modifier-p :maybe)
  (save-window-excursion
    (switch-to-buffer buf)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search ibuffer-case-fold-search))
	(while (re-search-forward from-str nil t)
	  (replace-match to-str))))
    t))

(ibuffer-define-op query-replace (&rest args)
  (:documentation
   "Perform a `query-replace' in marked buffers."
   :interactive
   (query-replace-read-args "Query replace" t)
   :opstring "replaced in"
   :complex t
   :modifier-p :maybe)
  (save-window-excursion
    (switch-to-buffer buf)
    (save-excursion
      (let ((case-fold-search ibuffer-case-fold-search))
	(goto-char (point-min))
	(apply #'query-replace args)))
    t))

(ibuffer-define-op query-replace-regexp (&rest args)
  (:documentation
   "Perform a `query-replace-regexp' in marked buffers."
   :interactive
   (query-replace-read-args "Query replace regexp" t)
   :opstring "replaced in"
   :complex t
   :modifier-p :maybe)
  (save-window-excursion
    (switch-to-buffer buf)
    (save-excursion
      (let ((case-fold-search ibuffer-case-fold-search))
	(goto-char (point-min))
	(apply #'query-replace-regexp args)))
    t))

(ibuffer-define-op delete ()
  (:documentation
   "Kill marked buffers as with `kill-this-buffer'."
   :opstring "killed"
   :active-opstring "kill"
   :dangerous t
   :complex t
   :modifier-p t)
  (if (kill-buffer buf)
      'kill
    nil))

(ibuffer-define-op kill-on-deletion-marks ()
  (:documentation
   "Kill buffers marked for deletion as with `kill-this-buffer'."
   :opstring "killed"
   :active-opstring "kill"
   :dangerous t
   :complex t
   :mark :deletion
   :modifier-p t)
  (if (kill-buffer buf)
      'kill
    nil))

(ibuffer-define-op revert ()
  (:documentation
   "Revert marked buffers as with `revert-buffer'."
   :dangerous t
   :opstring "reverted"
   :active-opstring "revert"
   :modifier-p :maybe)
  (revert-buffer t t))

(ibuffer-define-op shell-command (command)
  (:documentation
   "Execute shell command COMMAND on the contents of each marked buffer."
   :interactive "sShell command: "
   :opstring "Shell command executed on"
   :modifier-p nil)
  (shell-command-on-region
   (point-min) (point-max) command
   (get-buffer-create "* ibuffer-shell-output*")))

(ibuffer-define-op shell-command-replace (command)
  (:documentation
   "Replace the contents of each marked buffer with the output of COMMAND."
   :interactive "sShell command (replace): "
   :opstring "Buffer contents replaced in"
   :active-opstring "replace buffer contents in"
   :dangerous t
   :modifier-p t)
  (with-current-buffer buf
    (shell-command-on-region (point-min) (point-max)
			     command nil t)))

(ibuffer-define-op eval (form)
  (:documentation
   "Evaluate FORM in each of the buffers.
Does not display the buffer during evaluation. See
`ibuffer-do-view-and-eval' for that."
   :interactive "xEval in buffers (form): "
   :opstring "evaluated in"
   :modifier-p :maybe)
  (eval form))

(ibuffer-define-op view-and-eval (form)
  (:documentation
   "Evaluate FORM while displaying each of the marked buffers.
To evaluate a form without viewing the buffer, see `ibuffer-do-eval'."
   :interactive "xEval viewing buffers (form): "
   :opstring "evaluated in"
   :complex t
   :modifier-p :maybe)
  (let ((ibuffer-buf (current-buffer)))
    (unwind-protect
	(progn
	  (switch-to-buffer buf)
	  (eval form))
      (switch-to-buffer ibuffer-buf))))

(defun ibuffer-do-kill-lines ()
  "Hide all of the currently marked lines."
  (interactive)
  (if (= (ibuffer-count-marked-lines) 0)
      (message "No buffers marked; use 'm' to mark a buffer")
    (let ((count
	   (ibuffer-map-marked-lines
	    #'(lambda (buf mark beg end)
		'kill))))
      (message "Killed %s lines" count))))

(defun ibuffer-unmark-all ()
  "Unmark all currently marked buffers."
  (interactive)
  (if (= (ibuffer-count-marked-lines) 0)
      (message "No buffers marked; use 'm' to mark a buffer")
    (ibuffer-map-marked-lines
     #'(lambda (buf mark beg end)
	 (ibuffer-set-mark ? )
	 t))))

(defun ibuffer-toggle-marks ()
  "Toggle which buffers are marked.
In other words, unmarked buffers become marked, and marked buffers
become unmarked."
  (interactive)
  (let ((count
	 (ibuffer-map-lines
	  #'(lambda (buf mark beg end)
	      (cond ((eq mark ibuffer-marked-char)
		     (ibuffer-set-mark ? )
		     nil)
		    ((eq mark ? )
		     (ibuffer-set-mark ibuffer-marked-char)
		     t)
		    (t
		     nil))))))
    (message "%s buffers marked" count))
  (ibuffer-redisplay t))

(defun ibuffer-mark-forward (arg)
  "Mark the buffer on this line, and move forward ARG lines."
  (interactive "P")
  (ibuffer-mark-interactive arg ibuffer-marked-char 1))

(defun ibuffer-unmark-forward (arg)
  "Unmark the buffer on this line, and move forward ARG lines."
  (interactive "P")
  (ibuffer-mark-interactive arg ?  1))

(defun ibuffer-unmark-backward (arg)
  "Unmark the buffer on this line, and move backward ARG lines."
  (interactive "P")
  (ibuffer-mark-interactive arg ?  -1))

(defun ibuffer-mark-interactive (arg mark movement)
  (assert (eq major-mode 'ibuffer-mode))
  (unless arg
    (setq arg 1))
  (while (and (get-text-property (ibuffer-line-beginning-position)
				 'ibuffer-title)
	      (not (eobp)))
    (forward-line 1))
  (while (> arg 0)
    (unwind-protect
	(progn
	  (setq buffer-read-only nil)
	  (ibuffer-set-mark mark))
      (setq buffer-read-only t))
    (forward-line movement)
    (when (or (get-text-property (ibuffer-line-beginning-position)
				 'ibuffer-title)
	      (eobp))
      (forward-line (- movement))
      (setq arg 0))
    (setq arg (1- arg))))

(defun ibuffer-set-mark (mark)
  (assert (eq major-mode 'ibuffer-mode))
  (let ((beg (ibuffer-line-beginning-position))
	(end (ibuffer-line-end-position)))
    (setq ibuffer-did-modification t)
    (put-text-property beg end
		       'ibuffer-properties
		       (list (ibuffer-current-buffer)
			     mark))
    (ibuffer-redisplay-current)))

(defun ibuffer-mark-for-delete (arg)
  "Mark the buffers on ARG lines forward for deletion."
  (interactive "P")
  (ibuffer-mark-interactive arg ibuffer-deletion-char 1))

(defun ibuffer-propertize-field (field properties)
  "Search for text property FIELD, and place PROPERTIES on matching text."
  (let ((beg (ibuffer-line-beginning-position))
	(end (ibuffer-line-end-position)))
    (let ((fieldbeg (text-property-any beg end 'ibuffer-field field)))
      (when fieldbeg
	(save-excursion
	  (goto-char fieldbeg)
	  (let ((fieldend (next-single-property-change fieldbeg
						       'ibuffer-field
						       nil
						       end)))
	    (while properties
	      (put-text-property fieldbeg fieldend
				 (car properties) (cadr properties))
	      (setq properties (cddr properties)))))))))
  
(defun ibuffer-current-buffer (&optional must-be-live)
  (let ((buf (car (get-text-property (ibuffer-line-beginning-position)
				     'ibuffer-properties))))
    (when (and must-be-live
	       (not (buffer-live-p buf)))
      (error "Buffer %s has been killed!" buf))
    buf))
      
(defun ibuffer-current-mark ()
  (let* ((props (get-text-property (ibuffer-line-beginning-position)
				   'ibuffer-properties))
	 (buf (car props))
	 (mark (cadr props)))
    (unless mark
      (error "Ibuffer error: buffer %s: invalid mark %s" buf mark))
    mark))

(defun ibuffer-current-format ()
  (or ibuffer-current-format
      (setq ibuffer-current-format
	    (car ibuffer-formats))
      (error "No format!")))
  
(defun ibuffer-princ-to-string (obj)
  (with-output-to-string
    (princ obj)))

(defun ibuffer-expand-format (format)
  (mapcar #'(lambda (form)
	      (if (stringp form)
		  form
		(let ((sym (intern
			    (concat "ibuffer-make-column-"
				     (symbol-name
				      (if (consp form)
					  (car form)
					form)))))
		      (min (if (consp form)
			       (if (symbolp (cadr form))
				   (funcall (cadr form))
				 (cadr form))
			     0))
		      (max (if (consp form)
			       (if (symbolp (caddr form))
				   (funcall (caddr form))
				 (caddr form))
			     -1))
		      (align (ibuffer-aif (and (consp form)
					       (cadddr form))
				 it
			       :left)))
		  (if (fboundp sym)
		      (list sym min max align)
		    (error "Unknown column %s in ibuffer-formats" sym)))))
	  format))

;; (defun ibuffer-compile-formats ()
;;   (setq ibuffer-compiled-formats
;; 	(mapcar
;; 	 #'(lambda (format)
;; 	     (byte-compile
;; 	      `(lambda (buffer mark)
;; 		 ,@(mapcar #'ibuffer-compile-formatter format))

(defmacro* ibuffer-define-column (symbol (&key name extra-props) &rest body)
  "Define a column SYMBOL for use with `ibuffer-formats'.

BODY will be called with `buffer' bound to the buffer object, and
`mark' bound to the current mark on the buffer.  The current buffer
will be `buffer'.

If NAME is given, it will be used as a title for the column.
Otherwise, the title will default to a capitalized version of the
SYMBOL's name.
EXTRA-PROPS is a plist of additional properties to add to the text,
such as `mouse-face'.

Note that this macro expands into a `defun' for a function named
ibuffer-make-column-NAME."
  (let ((sym (intern (concat "ibuffer-make-column-"
			     (symbol-name symbol)))))
    `(progn
       (defun ,sym
	 (buffer mark)
	 ,(append `(ibuffer-propertize
		    (with-current-buffer buffer
		      ,@body))
		  `('ibuffer-field ',symbol)
		  extra-props))
       (put (quote ,sym) 'ibuffer-column-name
		 (if (stringp ,name)
		     ,name
		   (capitalize (symbol-name (quote ,symbol))))))))
;; (put 'ibuffer-define-column 'lisp-indent-function 'defun)

(ibuffer-define-column mark (:name " ")
  (string mark))

(ibuffer-define-column read-only (:name "R")
  (if buffer-read-only
      "%"
    " "))

(ibuffer-define-column modified (:name "M")
  (if (buffer-modified-p)
      (string ibuffer-modified-char)
    " "))

;; Having to conditionalize this sucks.
(if ibuffer-use-keymap-for-local-map
    (ibuffer-define-column name (:extra-props
				 ('mouse-face 'highlight 'keymap ibuffer-name-map))
			   (buffer-name))
  (ibuffer-define-column name (:extra-props
			       ('mouse-face 'highlight 'local-map ibuffer-name-map))
			 (buffer-name)))
  
(ibuffer-define-column size ()
  (format "%s" (buffer-size)))

(if ibuffer-use-keymap-for-local-map
    (ibuffer-define-column mode (:extra-props
				 ('mouse-face 'highlight
					      'keymap ibuffer-mode-name-map))
			   (format "%s" mode-name))
  (ibuffer-define-column mode (:extra-props
			       ('mouse-face 'highlight
					    'local-map
					    ibuffer-mode-name-map))
			 (format "%s" mode-name)))

(ibuffer-define-column process ()
  (let ((proc (get-buffer-process buffer)))
    (format "%s" (if proc
		     (list proc (process-status proc))
		   "none"))))

(ibuffer-define-column filename ()
  (let ((directory-abbrev-alist ibuffer-directory-abbrev-alist))
    (abbreviate-file-name
     (or buffer-file-name
	 (and ibuffer-dired-filenames
	      dired-directory)
	 ""))))

;; Reduce string consing.
(defconst ibuffer-space-strings [""
				 " "
				 "  "
				 "   "
				 "    "
				 "     "])
(defconst ibuffer-max-space-string-len 5)

(defun ibuffer-make-space-string (width)
  (if (> width ibuffer-max-space-string-len)
      (make-string width ? )
    (aref ibuffer-space-strings width)))

(defun ibuffer-format-column (str width alignment)
  (let* ((left (ibuffer-make-space-string (/ width 2)))
         (right (ibuffer-make-space-string
		 (- width (length left)))))
    (case alignment
      (:right (concat left right str))
      (:center (concat left str right))
      (t (concat str left right)))))

;; This is a pretty bad hack.
(defvar ibuffer-column-sizes nil
  "A vector of the maximum length of a line in each column.
For internal ibuffer use; do not modify.")

(defun ibuffer-insert-buffer-line (buffer mark format)
  "Insert a line describing BUFFER and MARK using FORMAT."
  (assert (eq major-mode 'ibuffer-mode))
  (let* ((beg (point))
	 (i 0))
    ;; Actually evaluate the format.
    (dolist (form format)
      (insert
       (if (stringp form)
	   form
	 (let ((sym (car form))
	       (min (cadr form))
	       (max (caddr form))
	       (align (cadddr form)))
	   (let* ((str (funcall sym buffer mark))
		  (strlen (length str))
		  (val
		   (cond ((< strlen min)
                          (ibuffer-format-column str (- min strlen) align))
			 ((and (> max 0)
			       (> strlen max))
			  (let* ((substr (substring str 0 max))
				 (substrlen (length substr)))
			    (if (and ibuffer-elide-long-columns
				     (> substrlen 6))
				(concat (substring substr 0 (- substrlen 3))
					(if (not (null ibuffer-fontification-level))
					    (ibuffer-propertize "..." 'face 'bold)
					  "..."))
			      substr)))
			 (t
			  str)))
		  (len (length val))
		  (max (aref ibuffer-column-sizes i)))
	     (when (> len max)
	       (aset ibuffer-column-sizes i len))
	     (setq i (1+ i))
	     val)))))
    (put-text-property beg (point)
		       'ibuffer-properties (list buffer mark))
    ;; Replace with marking face, if applicable.
    (when (not (null ibuffer-fontification-level))
      (cond ((eq mark ibuffer-marked-char)
	     (ibuffer-propertize-field 'name
				       '(face ibuffer-marked-face)))
	    ((eq mark ibuffer-deletion-char)
	     (ibuffer-propertize-field 'name
				       '(face ibuffer-deletion-face)))
	    ;; If it's not marked, fontify according to `ibuffer-fontification-alist'.
	    ((eq ibuffer-fontification-level t)
	     (let ((level -1))
	       (dolist (e ibuffer-fontification-alist)
		 (when (and (> (car e) level)
			    (with-current-buffer buffer
			      (eval (cadr e))))
		   (setq level (car e))
		   (ibuffer-propertize-field 'name
					     (list 'face (caddr e)))))))))
    (insert "\n")
    (goto-char beg)))

(defun ibuffer-redisplay-current ()
  (assert (eq major-mode 'ibuffer-mode))
  (when (eobp)
    (forward-line -1))
  (beginning-of-line)
  (let ((buf (ibuffer-current-buffer)))
    (when buf
      (let ((mark (ibuffer-current-mark)))
	(delete-region (point) (1+ (ibuffer-line-end-position)))
	(ibuffer-insert-buffer-line
	 buf mark
	 (ibuffer-expand-format (ibuffer-current-format)))
	(ibuffer-maybe-shrink-to-fit)))))

(defun ibuffer-marked-buffer-names ()
  (ibuffer-buffer-names-with-mark ibuffer-marked-char))

(defun ibuffer-deletion-marked-buffer-names ()
  (ibuffer-buffer-names-with-mark ibuffer-deletion-char))

(defun ibuffer-buffer-names-with-mark (mark)
  (let ((ibuffer-buffer-names-with-mark-result nil))
    (ibuffer-map-lines-nomodify
     #'(lambda (buf mk beg end)
	 (when (eq mark mk)
	   (push (buffer-name buf)
		 ibuffer-buffer-names-with-mark-result))))
    ibuffer-buffer-names-with-mark-result))

(defun ibuffer-count-marked-lines ()
  (ibuffer-map-lines-nomodify
   #'(lambda (buf mark beg end)
       (eq mark ibuffer-marked-char))))

(defun ibuffer-count-deletion-lines ()
  (ibuffer-map-lines-nomodify
   #'(lambda (buf mark beg end)
       (eq mark ibuffer-deletion-char))))

(defun ibuffer-map-deletion-lines (func)
  (ibuffer-map-on-mark ibuffer-deletion-char func))

(defun ibuffer-map-marked-lines (func)
  (prog1 (ibuffer-map-on-mark ibuffer-marked-char func)
    (ibuffer-redisplay t)))
   
(defun ibuffer-map-on-mark (mark func)
  (ibuffer-map-lines
   #'(lambda (buf mk beg end)
       (if (eq mark mk)
	   (funcall func buf mark beg end)
	 nil))))

(defun ibuffer-map-lines (function &optional nomodify)
  "Call FUNCTION for each buffer in an ibuffer.
Don't set the ibuffer modification flag iff NOMODIFY is non-nil.

 FUNCTION is called with four arguments: the buffer object itself, the
current mark symbol, and the beginning and ending line positions."
  (assert (eq major-mode 'ibuffer-mode))
  (let ((curline (count-lines (point-min)
			      (ibuffer-line-beginning-position)))
	(ibuffer-map-lines-total 0)
        (ibuffer-map-lines-count 0))
    (unwind-protect
         (progn
           (setq buffer-read-only nil)
           (goto-char (point-min))
           (while (and (get-text-property (point) 'ibuffer-title)
                       (not (eobp)))
             (forward-line 1))
           (while (not (eobp))
             (let ((result
                    (if (buffer-live-p (ibuffer-current-buffer))
                        (save-excursion
                          (funcall function
                                   (ibuffer-current-buffer)
                                   (ibuffer-current-mark)
                                   (ibuffer-line-beginning-position)
                                   (1+ (ibuffer-line-end-position))))
                      ;; Kill the line if the buffer is dead
                      'kill)))
               ;; A given mapping function should return:
               ;; `nil' if it chose not to affect the buffer
               ;; `kill' means the remove line from the buffer list
               ;; `t' otherwise
	       (incf ibuffer-map-lines-total)
               (cond ((null result)
                      (forward-line 1))
                     ((eq result 'kill)
                      (delete-region (ibuffer-line-beginning-position)
                                     (1+ (ibuffer-line-end-position)))
                      (incf ibuffer-map-lines-count))
                     (t
                      (incf ibuffer-map-lines-count)
                      (forward-line 1)))))
           ibuffer-map-lines-count)
      (progn (setq buffer-read-only t)
             (unless (zerop ibuffer-map-lines-total)
               (ibuffer-maybe-shrink-to-fit))
	     (unless nomodify
	       (set-buffer-modified-p nil))
	     (goto-line (1+ curline))))))

(defun ibuffer-map-lines-nomodify (function)
  "As `ibuffer-map-lines', but don't set the modification flag."
  (ibuffer-map-lines function t))

(defun ibuffer-get-marked-buffers ()
  "Return a list of buffer objects currently marked."
  (delq nil
	(mapcar #'(lambda (e)
		    (when (eq (cdr e) ibuffer-marked-char)
		      (car e)))
		(ibuffer-current-state-list))))

(defun ibuffer-current-state-list ()
  "Return a list like (BUF MARK) of all buffers in an ibuffer."
  (let ((ibuffer-tmp-result-tmp '()))
    ;; ah, if only we had closures.  I bet this will mysteriously
    ;; break later.  Don't blame me.
    (ibuffer-map-lines-nomodify
     #'(lambda (buf mark beg end)
	 (when (buffer-live-p buf)
	   (push (cons buf mark) ibuffer-tmp-result-tmp))))
    (nreverse ibuffer-tmp-result-tmp)))

(defun ibuffer-canonicalize-state-list (bmarklist)
  "Order BMARKLIST in the same way as the current buffer list."
  (delq nil
	(mapcar #'(lambda (buf) (assq buf bmarklist)) (buffer-list))))

(defun ibuffer-current-buffers-with-marks ()
  "Return a list like (BUF MARK) of all open buffers."
  (let ((bufs (ibuffer-current-state-list)))
    (mapcar #'(lambda (buf) (let ((e (assq buf bufs)))
			      (if e
				  e
				(cons buf ? ))))
	    (buffer-list))))

(defun ibuffer-name-matches-regexps (name regexps)
  (let ((hit nil))
    (dolist (regexp regexps)
      (when (string-match regexp name)
	(setq hit t)))
    hit))
  
(defun ibuffer-filter-buffers (ibuffer-buf last bmarklist all)
  (delq nil
	(mapcar
	 ;; element should be like (BUFFER . MARK)
	 #'(lambda (e)
	     (let* ((buf (car e))
		    (name (buffer-name buf)))
	       (when
		   (or
		    ;; This takes precedence over anything else
		    (and ibuffer-always-show-last-buffer
			 (eq last buf))
		    (ibuffer-name-matches-regexps
		     name
		     ibuffer-tmp-show-regexps)
		    (and (not
			  (or
			   (ibuffer-name-matches-regexps
			    name
			    ibuffer-tmp-hide-regexps)
			   (ibuffer-name-matches-regexps
			    name
			    ibuffer-never-show-regexps)))
			 (or all
			     (not
			      (ibuffer-name-matches-regexps
			       name
			       ibuffer-maybe-show-regexps)))
			 (or ibuffer-view-ibuffer
			     (not (eq ibuffer-buf buf)))
			 (or
			  (ibuffer-included-in-limits-p buf)
			  (ibuffer-name-matches-regexps
			   name
			   ibuffer-always-show-regexps))))
		 e)))
	 bmarklist)))

(defun ibuffer-included-in-limits-p (buf)
  (not
   (memq nil ;; a filter will return nil if it failed
	 (mapcar
	  ;; filter should be like (TYPE . QUALIFIER)
	  #'(lambda (filter)
	      ;; filterdat should be like (TYPE DESCRIPTION FUNC)
	      (let ((filterdat (assq (car filter)
				     ibuffer-limiting-alist)))
		;; just a sanity check
		(unless filterdat
		  (ibuffer-limit-disable)
		  (error "Undefined filter %s" (car filter)))
		(funcall (caddr filterdat)
			 buf
			 (cdr filter))))
	  ibuffer-limiting-qualifiers))))

(defun ibuffer-limit-disable ()
  "Disable all limits currently in effect."
  (interactive)
  (setq ibuffer-limiting-qualifiers nil)
  (ibuffer-update-mode-name)
  (ibuffer-update nil t))

(defun ibuffer-update-mode-name ()
  (setq mode-name "Ibuffer")
  (dolist (qual ibuffer-limiting-qualifiers)
    (let ((type (assq (car qual) ibuffer-limiting-alist)))
      (unless qual
	(error "Ibuffer: bad qualifier %s" qual))
      (setq mode-name (concat mode-name " ["
			      (cadr type) ": " (format "%s]" (cdr qual)))))))

(defun ibuffer-add-qualifier (type qual)
  "Add a qualifier to the local variable `ibuffer-limiting-qualifiers'."
  (setq ibuffer-limiting-qualifiers
	(cons (cons type qual)
	      ibuffer-limiting-qualifiers)))

(defun ibuffer-remove-qualifier (type)
  "Remove a qualifier from the local variable `ibuffer-limiting-qualifiers'."
  (setq ibuffer-limiting-qualifiers
	(ibuffer-delete-alist type
			      ibuffer-limiting-qualifiers)))

(defun ibuffer-get-qualifier (type)
  "Retrieve the data associated with qualifier TYPE.
See also `ibuffer-limiting-qualifiers'."
  (cdr (assq type ibuffer-limiting-qualifiers)))

(defmacro* ibuffer-define-limiter (name param (&key documentation
						    reader
						    description)
					&rest body)
  "Define a limitation named NAME, with parameter PARAM.
DOCUMENTATION is the documentation of the function.
READER is a form which should read a qualifier from the user.
DESCRIPTION is a short string describing the limitation.

BODY should contain forms which will be evaluated to test whether or
not a particular buffer should be displayed or not.  The forms in BODY
will be evaluated with BUF bound to the buffer object, and QUALIFIER
bound to the current value of the limitation."
  `(progn
     (defun ,(intern (concat "ibuffer-limit-by-" (symbol-name name))) (,param)
       ,(concat (or documentation "")
	      "\nTo disable the limit, call this function again.")
       (interactive
	(list
	 (if (ibuffer-get-qualifier ',name)
	     nil
	   (progn
	     ,reader))))
       (cond (,param
	      (ibuffer-add-qualifier ',name ,param)
	      (message
	       (format ,(concat (format "View limited by %s:"
					description)
				" %s")
			       ,param)))
	     (t
	      (ibuffer-remove-qualifier ',name)
	      (message ,(format "Limiting by %s disabled"
				description))))
       (ibuffer-update nil t))
     (push (list ',name ,description
		 #'(lambda (buf qualifier)
		     ,@body))
	   ibuffer-limiting-alist)))

;; (put 'ibuffer-define-limiter 'lisp-indent-function 2)

(ibuffer-define-limiter mode mode
  (:documentation
   "Toggle current view to buffers with major mode MODE."
   :description "major mode"
   :reader
   (intern
    (completing-read "Limit by major mode: " obarray
		     #'(lambda (e)
			 (string-match "-mode$"
				       (symbol-name e)))
		     t
		     (let ((buf (ibuffer-current-buffer)))
		       (if (and buf (buffer-live-p buf))
			   (with-current-buffer buf
			     (symbol-name major-mode))
			 "")))))
  (eq qualifier (with-current-buffer buf major-mode)))

(ibuffer-define-limiter name regexp
  (:documentation
   "Toggle current view to buffers with name matching REGEXP."
   :description "buffer name"
   :reader
   (read-from-minibuffer "Limit by name (regexp): "))
  (string-match qualifier (buffer-name buf)))

;; (ibuffer-define-limiter filename regexp
;;   (:documentation
;;    "Toggle current view to buffers with filename matching REGEXP."
;;    :description "filename"
;;    :reader
;;    (read-from-minibuffer "Limit by filename (regexp): "))
;;   (ibuffer-awhen (buffer-file-name buf)
;;     (string-match qualifier it)))

;; The dired bits are missing in the version above
(ibuffer-define-limiter filename regexp
  (:documentation
   "Toggle current view to buffers with filename matching REGEXP."
   :description "filename"
   :reader
   (read-from-minibuffer "Limit by filename (regexp): "))
  (ibuffer-awhen (or (buffer-file-name buf) 
                     (with-current-buffer buf
                       (and ibuffer-dired-filenames
                            (stringp dired-directory)
                            dired-directory)))
    (string-match qualifier it)))

(ibuffer-define-limiter size-gt size
  (:documentation
   "Toggle current view to buffers with size greater than SIZE."
   :description "size greater than"
   :reader
   (string-to-number (read-from-minibuffer "Limit by size greater than: ")))
  (> (with-current-buffer buf (buffer-size))
     qualifier))

(ibuffer-define-limiter size-lt size
  (:documentation
   "Toggle current view to buffers with size less than SIZE."
   :description "size less than"
   :reader
   (string-to-number (read-from-minibuffer "Limit by size less than: ")))
  (< (with-current-buffer buf (buffer-size))
     qualifier))
  
(ibuffer-define-limiter content regexp
  (:documentation
   "Toggle current view to buffers whose contents match REGEXP."
   :description "content"
   :reader
   (read-from-minibuffer "Limit by content (regexp): "))
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (re-search-forward qualifier nil t))))

(defmacro* ibuffer-define-sorter (name (&key documentation
					     description)
				       &rest body)
  "Define a method of sorting named NAME.
DOCUMENTATION is the documentation of the function, which will be called
`ibuffer-do-sort-by-NAME'.
DESCRIPTION is a short string describing the sorting method.

For sorting, the forms in BODY will be evaluated with `a' bound to one
buffer object, and `b' bound to another.  BODY should return a non-nil
value if and only if `a' is \"less than\" `b'."
  `(progn
     (defun ,(intern (concat "ibuffer-do-sort-by-" (symbol-name name))) ()
       ,(or documentation "No :documentation specified for this sorting method.")
       (interactive)
       (setq ibuffer-sorting-mode ',name)
       (ibuffer-redisplay t))
     (push (list ',name ,description
		 #'(lambda (a b)
		     ,@body))
	   ibuffer-sorting-functions-alist)))
;; (put 'ibuffer-define-sorter 'lisp-indent-function 1)

(ibuffer-define-sorter major-mode
  (:documentation
  "Sort the buffers by major modes.
Ordering is lexicographic."
  :description "major mode")
  (string-lessp (downcase
		 (symbol-name (with-current-buffer
				  (car a)
				major-mode)))
		(downcase
		 (symbol-name (with-current-buffer
				  (car b)
				major-mode)))))

(ibuffer-define-sorter buffer-name
  (:documentation
  "Sort the buffers by their names.
Ordering is lexicographic."
  :description "buffer name")
  (string-lessp
   (buffer-name (car a))
   (buffer-name (car b))))

(ibuffer-define-sorter size
  (:documentation
   "Sort the buffers by their size."
   :description "size")
  (< (with-current-buffer (car a)
       (buffer-size))
     (with-current-buffer (car b)
       (buffer-size))))

;; This is a special case.  Oh well, there goes the whole design :)
(defun ibuffer-do-sort-by-recency ()
  "Sort the buffers by last view time."
  (interactive)
  (setq ibuffer-sorting-mode 'recency)
  (ibuffer-redisplay t))

(defun ibuffer-toggle-sorting-mode ()
  "Toggle the current sorting mode.
Possible sorting modes are:
 Recency - the last time the buffer was viewed
 Name - the name of the buffer
 Major Mode - the name of the major mode of the buffer
 Size - the size of the buffer"
  (interactive)
  (let* ((keys (mapcar #'car ibuffer-sorting-functions-alist))
	 (entry (memq ibuffer-sorting-mode keys))
	 (next (or (cadr entry) (car keys)))
	 (nextentry (assq next ibuffer-sorting-functions-alist)))
    (if (and entry nextentry)
	(progn
	  (setq ibuffer-sorting-mode next)
	  (message "Sorting by %s" (cadr nextentry)))
      (progn
	(setq ibuffer-sorting-mode 'recency)
	(message "Sorting by last view time"))))
  (ibuffer-redisplay t))

(defun ibuffer-invert-sorting ()
  "Toggle whether or not sorting is in reverse order."
  (interactive)
  (setq ibuffer-sorting-reversep (not ibuffer-sorting-reversep))
  (message "Sorting order %s"
	   (if ibuffer-sorting-reversep
	       "reversed"
	     "normal"))
  (ibuffer-redisplay t))

(defun ibuffer-update-format ()
  (when (null ibuffer-current-format)
    (setq ibuffer-current-format (car ibuffer-formats)))
  (when (null ibuffer-formats)
    (error "Ibuffer error: no formats!"))
  ;; Find all the named columns, and create a vector where we will
  ;; store the maximum length of a string in that column.
  (setq ibuffer-column-sizes
	(make-vector (let ((count 0))
		       (dolist (e ibuffer-current-format count)
			 (when (or (symbolp e)
				   (consp e))
			   (setq count (1+ count)))))
		     -1)))

(defun ibuffer-switch-format ()
  "Switch the current display format."
  (interactive)
  (assert (eq major-mode 'ibuffer-mode))
  (unless (consp ibuffer-formats)
    (error "Ibuffer error: No formats!"))
  (setq ibuffer-current-format
	(or (cadr (member ibuffer-current-format ibuffer-formats))
	    (car ibuffer-formats)))
  (ibuffer-update-format)
  (ibuffer-redisplay t))

(defun ibuffer-maybe-shrink-to-fit ()
  "If `ibuffer-shrink-to-minimum-size' is non-nil, reduce the window size."
  (assert (eq major-mode 'ibuffer-mode))
  (when ibuffer-shrink-to-minimum-size
    ;; `fit-window-to-buffer', new in Emacs 21, is much better.
    (if (and (fboundp 'fit-window-to-buffer)
	     ;; If the user chose to use another window, force that
	     ;; behavior.
	     (not ibuffer-delete-window-on-quit))
	(fit-window-to-buffer)
      (shrink-window-if-larger-than-buffer))))

(defun ibuffer-update-title (format)
  (assert (eq major-mode 'ibuffer-mode))
  (if (get-text-property (point-min) 'ibuffer-title)
      (delete-region (point-min)
		     (next-single-property-change
		      (point-min) 'ibuffer-title)))
  (goto-char (point-min))
  (put-text-property
   (point)
   (progn
     (let ((opos (point))
	   (i 0))
       ;; Insert the title names.
       (dolist (element format)
	 (insert
	  (if (stringp element)
	      element
	    (let ((sym (car element))
		  (min (cadr element))
		  (max (caddr element))
		  (align (cadddr element)))
	      (let* ((name (or (get sym 'ibuffer-column-name)
                               (error "Unknown column %s in ibuffer-formats" sym)))
		     (len (length name)))
		(prog1
		    (if (< len min)
			(ibuffer-format-column name
					       (- min len)
					       align)
		      name)
		  (setq i (1+ i))))))))
       ;; Maybe highlight the titles
       (when (or (eq ibuffer-fontification-level :medium)
		 (eq ibuffer-fontification-level t))
	 (put-text-property opos (point) 'face ibuffer-title-face))
       (insert "\n")
       ;; Add the underlines
       (dotimes (i (- (point) opos 1))
	 (insert
	  (let ((prevchar (save-excursion
			       (forward-line -1)
			       (beginning-of-line)
			       (forward-char i)
			       (or (char-after (point))
				   ?  ))))
	    (if (or (char-equal prevchar ? )
		    (char-equal prevchar ?\n))
		?  ?-))))
       (insert "\n"))
     (point))
   'ibuffer-title t))

(defun ibuffer-redisplay (&optional silent)
  "Redisplay the current list of buffers.

If SILENT is non-`nil', do not generate progress messages."
  (interactive)
  (unless silent
    (message "Redisplaying current buffer list..."))
  (let ((blist (ibuffer-current-state-list)))
    (when (null blist)
      (if ibuffer-limiting-qualifiers
	  (message "No buffers! (note: limiting in effect)")
	(error "No buffers!")))
    (ibuffer-insert-buffers-and-marks blist t)
    (ibuffer-update-mode-name)
    (unless silent
      (message "Redisplaying current buffer list...done"))))
  
(defun ibuffer-update (arg &optional silent)
  "Regenerate the list of all buffers.
Display buffers whose name matches one of `ibuffer-maybe-show-regexps'
iff arg ARG is non-nil.
Do not display messages if SILENT is non-nil."
  (interactive "P")
  (let ((blist (ibuffer-filter-buffers (current-buffer)
				       (cadr (buffer-list))
				       (ibuffer-current-buffers-with-marks)
				       arg)))
    (when (null blist)
      (if ibuffer-limiting-qualifiers
	  (message "No buffers! (note: limiting in effect)")
	(error "No buffers!")))
    (unless silent
      (message "Updating buffer list..."))
    (ibuffer-insert-buffers-and-marks blist
				      arg)
    (ibuffer-update-mode-name)
    (unless silent
      (message "Updating buffer list...done")))
  (ibuffer-goto-beg))

(defun ibuffer-insert-buffers-and-marks (bmarklist &optional all)
  (assert (eq major-mode 'ibuffer-mode))
  (let ((--ibuffer-insert-buffers-and-marks-format
	 (ibuffer-expand-format (ibuffer-current-format)))
	(orig (count-lines (point-min) (point))))
    (unwind-protect
	(progn
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (ibuffer-update-format)
	  (let ((entries
		 (let* ((sortdat (assq ibuffer-sorting-mode
				       ibuffer-sorting-functions-alist))
			(func (caddr sortdat)))
		   (let ((result
			  ;; actually sort the buffers
			  (if (and sortdat func)
			      (sort bmarklist func)
			    bmarklist)))
		     ;; perhaps reverse the sorted buffer list
		     (if ibuffer-sorting-reversep
			 result
		       (nreverse result))))))
	    (dolist (entry entries)
	      (ibuffer-insert-buffer-line (car entry)
					  (cdr entry)
					  --ibuffer-insert-buffers-and-marks-format)))
	  (ibuffer-update-title --ibuffer-insert-buffers-and-marks-format))
      (setq buffer-read-only t)
      (set-buffer-modified-p ibuffer-did-modification)
      (setq ibuffer-did-modification nil)
      (ibuffer-maybe-shrink-to-fit)
      (goto-line (1+ orig)))))

(defun ibuffer-quit ()
  "Quit this `ibuffer' session.
Delete the current window iff `ibuffer-delete-window-on-quit' is non-nil."
  (interactive)
  (if ibuffer-delete-window-on-quit
      (progn
	(bury-buffer)
	(unless (= (count-windows) 1)
	  (delete-window)))
    (bury-buffer)))

;;;###autoload
(defun ibuffer-other-window ()
  "Begin using `ibuffer' to edit a list of buffers in another window.
Type 'h' after entering ibuffer for more information."
  (interactive)
  (ibuffer t))

;;;###autoload
(defun ibuffer (&optional other-window-p name qualifiers)
  "Begin using `ibuffer' to edit a list of buffers.
Type 'h' after entering ibuffer for more information.

Optional argument OTHER-WINDOW-P says to use another window.
Optional argument NAME specifies the name of the buffer; it defaults
to \"*Ibuffer*\".
Optional argument QUALIFIERS is an initial set of limiting qualifiers
to use; see `ibuffer-limiting-qualifiers'."
  (interactive "P")
  (when ibuffer-use-other-window
    (setq other-window-p (not other-window-p)))
  (let ((buf (get-buffer-create (or name "*Ibuffer*"))))
    (if other-window-p
	(pop-to-buffer buf)
      (switch-to-buffer buf)))
  (unless (eq major-mode 'ibuffer-mode)
    (ibuffer-mode))
  (setq ibuffer-delete-window-on-quit other-window-p)
  (when qualifiers
    (setq ibuffer-limiting-qualifiers qualifiers))
  (ibuffer-update nil)
  (unwind-protect
      (progn
	(setq buffer-read-only nil)
	(run-hooks 'ibuffer-hooks))
    (setq buffer-read-only t))
  (unless ibuffer-expert
    (message "Commands: m, u, t, RET, g, k, S, D, Q; q to quit; h for help")))

(defun ibuffer-mode ()
  "A major mode for viewing a list of buffers.
In ibuffer, you can conveniently perform many operations on the
currently open buffers, in addition to limiting your view to a
particular subset of them, and sorting by various criteria.

Operations on marked buffers:

  '\\[ibuffer-do-save]' - Save the marked buffers
  '\\[ibuffer-do-view]' - View the marked buffers in this frame.
  '\\[ibuffer-do-view-other-frame]' - View the marked buffers in another frame.
  '\\[ibuffer-do-revert]' - Revert the marked buffers.
  '\\[ibuffer-do-toggle-read-only]' - Toggle read-only state of marked buffers.
  '\\[ibuffer-do-delete]' - Kill the marked buffers.
  '\\[ibuffer-do-replace-regexp]' - Replace by regexp in each of the marked
          buffers.
  '\\[ibuffer-do-query-replace]' - Query replace in each of the marked buffers.
  '\\[ibuffer-do-query-replace-regexp]' - As above, with a regular expression.
  '\\[ibuffer-do-print]' - Print the marked buffers.
  '\\[ibuffer-do-occur]' - List lines in all marked buffers which match
          a given regexp (like the function `occur').
  '\\[ibuffer-do-shell-command]' - Run a shell command on the contents of
          the marked buffers.
  '\\[ibuffer-do-shell-command-replace]' - Replace the contents of the marked
          buffers with the output of a shell command.
  '\\[ibuffer-do-eval]' - Evaluate a form in each of the marked buffers.  This
          is a very flexible command.  For example, if you want to make all
          of the marked buffers read only, try using (toggle-read-only 1) as
          the input form.
  '\\[ibuffer-do-view-and-eval]' - As above, but view each buffer while the form
          is evaluated.
  '\\[ibuffer-do-kill-lines]' - Remove the marked lines from the *Ibuffer* buffer,
          but don't kill the associated buffer.
  '\\[ibuffer-do-kill-on-deletion-marks]' - Kill all buffers marked for deletion.

Marking commands:

  '\\[ibuffer-mark-forward]' - Mark the buffer at point.
  '\\[ibuffer-toggle-marks]' - Unmark all currently marked buffers, and mark
          all unmarked buffers.
  '\\[ibuffer-unmark-forward]' - Unmark the buffer at point.
  '\\[ibuffer-unmark-backward]' - Unmark the buffer at point, and move to the
          previous line.
  '\\[ibuffer-unmark-all]' - Unmark all marked buffers.
  '\\[ibuffer-mark-by-mode]' - Mark buffers by major mode.
  '\\[ibuffer-mark-unsaved-buffers]' - Mark all \"unsaved\" buffers.
          This means that the buffer is modified, and has an associated file.
  '\\[ibuffer-mark-modified-buffers]' - Mark all modified buffers,
          regardless of whether or not they have an associated file.
  '\\[ibuffer-mark-special-buffers]' - Mark all buffers whose name begins and
          ends with '*'.
  '\\[ibuffer-mark-dissociated-buffers]' - Mark all buffers which have
          an associated file, but that file doesn't currently exist.
  '\\[ibuffer-mark-read-only-buffers]' - Mark all read-only buffers.
  '\\[ibuffer-mark-dired-buffers]' - Mark buffers in `dired' mode.
  '\\[ibuffer-mark-help-buffers]' - Mark buffers in `help-mode', `apropos-mode', etc.
  '\\[ibuffer-mark-old-buffers]' - Mark buffers older than `ibuffer-old-time'.
  '\\[ibuffer-mark-for-delete]' - Mark the buffer at point for deletion.
  '\\[ibuffer-mark-by-name-regexp]' - Mark buffers by their name, using a regexp.
  '\\[ibuffer-mark-by-mode-regexp]' - Mark buffers by their major mode, using a regexp.
  '\\[ibuffer-mark-by-file-name-regexp]' - Mark buffers by their filename, using a regexp.

Limiting commands:

  '\\[ibuffer-limit-by-mode]' - Limit the view by major mode.
  '\\[ibuffer-limit-by-name]' - Limit the view by buffer name.
  '\\[ibuffer-limit-by-content]' - Limit the view by buffer content.
  '\\[ibuffer-limit-by-filename]' - Limit the view by filename.
  '\\[ibuffer-limit-by-size-gt]' - Limit the view by buffer size.
  '\\[ibuffer-limit-by-size-lt]' - Limit the view by buffer size.
  '\\[ibuffer-limit-disable]' - Remove all limiting currently in effect.
    
Sorting commands:

  '\\[ibuffer-toggle-sorting-mode]' - Rotate between the various sorting modes.
  '\\[ibuffer-invert-sorting]' - Reverse the current sorting order.
  '\\[ibuffer-do-sort-by-buffer-name]' - Sort the buffers lexicographically.
  '\\[ibuffer-do-sort-by-recency]' - Sort the buffers by last viewing time.
  '\\[ibuffer-do-sort-by-size]' - Sort the buffers by size.
  '\\[ibuffer-do-sort-by-major-mode]' - Sort the buffers by major mode.

Other commands:

  '\\[ibuffer-switch-format] - Change the current display format.
  '\\[forward-line]' - Move point to the next line.
  '\\[previous-line]' - Move point to the previous line.
  '\\[ibuffer-redisplay]' - Redisplay the current buffer list.
  '\\[ibuffer-update]' - As above, but add new buffers to the list.
  '\\[ibuffer-quit]' - Bury the Ibuffer buffer.
  '\\[describe-mode]' - This help.
  '\\[ibuffer-diff-with-file]' - View the differences between this buffer
          and its associated file.
  '\\[ibuffer-visit-buffer]' - View the buffer on this line.
  '\\[ibuffer-visit-buffer-other-window]' - As above, but in another window.
  '\\[ibuffer-visit-buffer-other-window-noselect]' - As both above, but don't select
          the new window.
  '\\[ibuffer-bury-buffer]' - Bury (not kill!) the buffer on this line.

Information on Limiting:

 You can limit your ibuffer view to a selection of the buffers, via
different critera.  For example, suppose you are working on an Emacs
Lisp project.  You can create an Ibuffer buffer which is limited to
just `emacs-lisp' modes via '\\[ibuffer-limit-by-mode]
emacs-lisp-mode RET'.

To undo a limit, type the limit keybinding again.  For instance, you
can undo the limit in the example above by typing '\\[ibuffer-limit-by-mode]' again.

You can even combine limits: Suppose you only want to see buffers in
`emacs-lisp' mode, whose names begin with \"gnus\".  You can
accomplish this via: '\\[ibuffer-limit-by-mode] emacs-lisp-mode RET
                      \\[ibuffer-limit-by-name] ^gnus RET'.

If you wish to disable all limiting currently in effect, use
  '\\[ibuffer-limit-disable]'."
  (kill-all-local-variables)
  (use-local-map ibuffer-mode-map)
  (setq major-mode 'ibuffer-mode)
  (setq mode-name "Ibuffer")
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq truncate-lines t)
  ;; This makes things less ugly for Emacs 21 users with a non-nil
  ;; `show-trailing-whitespace'.
  (setq show-trailing-whitespace nil)
  (set (make-local-variable 'ibuffer-sorting-mode)
       ibuffer-default-sorting-mode)
  (set (make-local-variable 'ibuffer-sorting-reversep)
       ibuffer-default-sorting-reversep)
  (set (make-local-variable 'ibuffer-limiting-qualifiers) nil)
  (set (make-local-variable 'ibuffer-current-format) nil)
  (set (make-local-variable 'ibuffer-column-sizes) nil)
  (set (make-local-variable 'ibuffer-did-modifiction) nil)
  (set (make-local-variable 'ibuffer-delete-window-on-quit) nil)
  (easy-menu-add ibuffer-mode-operate-menu)
  (easy-menu-add ibuffer-mode-mark-menu)
  (easy-menu-add ibuffer-mode-regexp-menu)
  (easy-menu-add ibuffer-mode-immediate-menu)
  (easy-menu-add ibuffer-mode-sort-menu)
  (easy-menu-add ibuffer-mode-limit-menu)
  (define-key ibuffer-mode-map [menu-bar edit] 'undefined)
  (ibuffer-update-format)
  (run-hooks 'ibuffer-mode-hooks)
  ;; called after mode hooks to allow the user to add limits
  (ibuffer-update-mode-name))

(provide 'ibuffer)

;; Local Variables:
;; coding: iso-8859-1
;; End:

;;; ibuffer.el ends here
