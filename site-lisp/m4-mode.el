;;; m4-mode.el --- m4 code editing commands for Emacs

;; Copyright (C) 1996-1997, 2001-2015 Free Software Foundation, Inc.

;; Author: Andrew Csillag <drew_csillag@geocities.com>
;; Maintainer: Andrew Csillag <drew_csillag@geocities.com>
;; Keywords: languages, faces

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A smart editing mode for m4 macro definitions.  It seems to have most of the
;; syntax right (sexp motion commands work, but function motion commands don't).
;; It also sets the font-lock syntax stuff for colorization

;; To Do's:

;; * want to make m4-m4-(buffer|region) look sorta like M-x compile look&feel ?
;; * sexp motion commands don't seem to work right

;;; Thanks:
;;;         to Akim Demaille and Terry Jones for the bug reports
;;;         to Simon Marshall for the regexp tip
;;;         to Martin Buchholz for some general fixes

;;; Code:

(defgroup m4 nil
  "m4 code editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "m4-"
  :group 'languages)

(defcustom m4-program "m4"
  "File name of the m4 executable.
If m4 is not in your PATH, set this to an absolute file name."
  :version "24.4"
  :type 'file
  :group 'm4)

;;options to m4
(defcustom m4-program-options nil
  "Options to pass to `m4-program'."
  :type '(repeat string)
  :group 'm4)

;;to use --prefix-builtins, you can use
;;(defconst m4-program-options '("-P"))
;;or
;;(defconst m4-program-options '("--prefix-builtins"))

(defvar m4-builtins
  '(
    ;; dnl is not here, dnl prefers comment font-lock
    "builtin" "changecom" "changequote" "debugfile"
    "debugmode" "decr" "define" "defn" "divert"
    "divnum" "dumpdef" "errprint" "esyscmd" "eval"
    "format" "ifdef" "ifelse" "include" "incr" "index"
    "indir" "len" "m4exit" "m4wrap" "maketemp" "mkstemp"
    "patsubst" "popdef" "pushdef" "regexp" "shift"
    "sinclude" "substr" "syscmd" "sysval" "traceoff"
    "traceon" "translit" "undefine" "undivert" "paste" "spaste"
    )
  "M4 unprefixed builtin list.")

(defvar m4-font-lock-keywords
  `(
    ;; comments
    ("\\_<\\(_*m4_\\)?dnl\\_>.*$"
     . font-lock-comment-face)
    ("[^\\$]\\(#.*\\)$"
     . (1 font-lock-comment-face))

    ;; special builtins
    (,(regexp-opt
       '("__file__" "m4__file__"
         "__line__" "m4__line__"
         "__program__" "m4__program__"
         "__unix__" "__gnu__" "__os2__" "__windows__") 'symbols)
     . font-lock-preprocessor-face)

    ;; builtins
    (,(regexp-opt m4-builtins 'symbols)
     . font-lock-function-name-face)

    ;; `_*m4_' prefixed macros
    ("\\_<_*m4_[_a-zA-Z0-9]+\\_>"
     . font-lock-function-name-face)
    
    ;; argument variables
    ("\\(\\$[*#@]\\)"
     . (1 font-lock-variable-name-face t)) 
    ("\\(\\$[0-9]+\\)"
     . (1 font-lock-variable-name-face t)) 

    )
  "Default font-lock-keywords for `m4 mode'.")

(defcustom m4-mode-hook nil
  "Hook called by `m4-mode'."
  :type 'hook
  :group 'm4)

;;this may still need some work
(defvar m4-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?` "('" table)
    (modify-syntax-entry ?' ")`" table)
    (modify-syntax-entry ?# "<\n" table)
    (modify-syntax-entry ?\n ">#" table)
    (modify-syntax-entry ?{  "_" table)
    (modify-syntax-entry ?}  "_" table)
    ;; FIXME: This symbol syntax for underscore looks OK on its own, but it's
    ;; odd that it should have the same syntax as { and } are these really
    ;; valid in m4 symbols?
    (modify-syntax-entry ?_  "_" table)
    ;; FIXME: These three chars with word syntax look wrong.
    (modify-syntax-entry ?*  "w" table)
    (modify-syntax-entry ?\"  "w" table)
    (modify-syntax-entry ?\"  "w" table)
    table)
  "Syntax table used while in `m4-mode'.")

(defvar m4-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'm4-m4-buffer)
    (define-key map "\C-c\C-r" 'm4-m4-region)
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map [menu-bar m4-mode] (cons "M4" menu-map))
    (define-key menu-map [m4c]
      '(menu-item "Comment Region" comment-region
		  :help "Comment Region"))
    (define-key menu-map [m4b]
      '(menu-item "M4 Buffer" m4-m4-buffer
		  :help "Send contents of the current buffer to m4"))
    (define-key menu-map [m4r]
      '(menu-item "M4 Region" m4-m4-region
		  :help "Send contents of the current region to m4"))
    map))

(defun m4-m4-buffer ()
  "Send contents of the current buffer to m4."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   (mapconcat 'identity (cons m4-program m4-program-options) "\s")
   "*m4-output*" nil)
  (switch-to-buffer-other-window "*m4-output*"))

(defun m4-m4-region ()
  "Send contents of the current region to m4."
  (interactive)
  (shell-command-on-region
   (point) (mark)
   (mapconcat 'identity (cons m4-program m4-program-options) "\s")
   "*m4-output*" nil)
  (switch-to-buffer-other-window "*m4-output*"))

(defun m4-current-defun-name ()
  "Return the name of the M4 function at point, or nil."
  (save-excursion
    (if (re-search-backward
	 "^\\(\\(m4_\\)?define\\|A._DEFUN\\|\\(m4_\\)?defun\\)([[{<`]?\\([A-Za-z0-9_]+\\)" nil t)
	(match-string-no-properties 3))))

;;;###autoload
(define-derived-mode m4-mode prog-mode "m4"
  "A major mode to edit m4 macro files."
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local parse-sexp-ignore-comments t)
  (setq-local add-log-current-defun-function #'m4-current-defun-name)
  (setq font-lock-defaults '(m4-font-lock-keywords nil)))

(provide 'm4-mode)

;;; m4-mode.el ends here
