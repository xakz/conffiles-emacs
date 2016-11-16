;;; init.el --- Emacs init script.
;;
;;
;;
;;
;;; Commentary:
;;
;;
;;; Code:

;; Garbage collector settings (in testing, not sure that reducing gc frequency
;; is good on the long run)
(setq gc-cons-threshold (* 32 1024 1024))
(setq gc-cons-percentage 0.5)

;; package.el settings
(require 'package)
(package-initialize) ;; packages magic (setup load-paths and autoloads)
;; Set elpa repositories
(setq package-archives '(
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")
			 ;("melpa-stable" . "http://stable.melpa.org/packages/")
			 ;("marmalade" . "http://marmalade-repo.org/packages/")
			 ))

;; Local load-path
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

(require 'use-package)

;; Reduce modeline (set it early because we need it for internal package too)
(require 'diminish)


(defun my-emacs-dir (file)
  "Shortcut for (expand-file-name FILE user-emacs-directory)."
  (expand-file-name file user-emacs-directory))


;; Personals
(setq user-mail-address "xakz@rxsoft.eu")
(setq user-full-name "Maxime Chatelle")
(setq user-organization "RXSoft")

;;;;;;;;;;;;;;;;;;;
;;; UI settings ;;;
;;;;;;;;;;;;;;;;;;;
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'deeper-blue t)

;; start on *scratch*
(setq inhibit-startup-screen t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; prefs
(column-number-mode 1)
(line-number-mode 1)
(global-linum-mode 1)
(size-indication-mode 1)
(show-paren-mode 1)
(blink-cursor-mode -1)
(setq make-pointer-invisible t)

;; display time
;(display-time)
;(setq display-time-24hr-format t)
;(setq display-time-day-and-date t)
;(setq display-time-mail-file (quote none))
;(setq display-time-mode t)

;; no beep
(setq visible-bell t)
(setq ring-bell-function 'ignore)
(setq bell-volume 0)

;; Gain some space
(if (fboundp 'fringe-mode)
    (fringe-mode '(4 . 1)))

;; Better WM window title
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
					    (abbreviate-file-name (buffer-file-name))
					  "%b"))))
;; Full font-lock
(setq font-lock-maximum-decoration t)

;; Set some faces
(set-face-attribute 'help-argument-name nil :inherit 'font-lock-function-name-face)


;; some colors ^_^
(set-cursor-color "#880000")
(set-mouse-color "OrangeRed")
(add-to-list 'initial-frame-alist '(cursor-color . "#880000"))
(add-to-list 'initial-frame-alist '(mouse-color . "OrangeRed"))
(add-to-list 'default-frame-alist '(cursor-color . "#880000"))
(add-to-list 'default-frame-alist '(mouse-color . "OrangeRed"))

;; (set-frame-font "-*-fixed-medium-*-*-*-*-200-*-*-*-*-iso10646-*")
;; (add-to-list 'initial-frame-alist '(font . "-*-fixed-medium-*-*-*-*-200-*-*-*-*-iso10646-*"))
;; (add-to-list 'default-frame-alist '(font . "-*-fixed-medium-*-*-*-*-200-*-*-*-*-iso10646-*"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core and internal package settings ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; save point position
(setq-default save-place t)
(require 'saveplace)

;; Auto-revert
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(global-auto-revert-mode 1)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Change some defaults
(defalias 'yes-or-no-p 'y-or-n-p)       ; y or n is enough

;; Some command aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'rs 'replace-string)
(defalias 'rr 'replace-regexp)
(defalias 'equit 'save-buffers-kill-emacs)
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'dtw 'delete-trailing-whitespace)

;; UTF-8 support
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; self documenting..
(auto-compression-mode 1)
(auto-encryption-mode 1)
(setq require-final-newline nil)
(setq imenu-auto-rescan t)
(setq dired-auto-revert-buffer t)
(setq-default fill-column 80)
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; backup
(setq backup-inhibited nil)
(setq backup-directory-alist `(("." . ,(my-emacs-dir "backups"))))
(setq backup-by-copying t)
(setq vc-make-backup-files t)
(setq delete-old-versions t)
(setq version-control t)
(setq kept-new-versions 10)
(setq kept-old-versions 0)
(setq dired-kept-versions 2)

;; Enable all functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
;(put 'scroll-left 'disabled nil)
;(put 'scroll-right 'disabled nil)



;; ignore case
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completion-ignore-case t)
(setq case-fold-search t)

;; I like log
(setq message-log-max 2000)

;; abbrev-mode
(abbrev-mode 1)
;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)
;; my personal abbrev
(define-abbrev-table 'global-abbrev-table '(
    ;; math/unicode symbols
    ("8in" "∈")
    ("8nin" "∉")
    ("8inf" "∞")
    ("8luv" "♥")
    ("8smly" "☺")
    ))
;; no modeline please
(ignore-errors
  (diminish 'abbrev-mode))



;; Use view-mode for unwriteable files
(setq view-read-only t)
;; And stay in view mode...
(defmacro do-not-exit-view-mode-unless-writable-advice (f)
  `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
     (if (and (buffer-file-name)
              (not (file-writable-p (buffer-file-name))))
         (message "File is unwritable, so stay in view-mode.")
       ad-do-it)))
;; apply macro
(do-not-exit-view-mode-unless-writable-advice view-mode-exit)
(do-not-exit-view-mode-unless-writable-advice view-mode-disable)

;; setting for add-log package
(setq add-log-full-name user-full-name)
(setq add-log-mailing-address user-mail-address)
(setq add-log-keep-changes-together t) ;; keep new entries for the same file together

;; make scripts executable automatically
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; fill correctly with bulleted lists
(setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-*] +.*$\\|[ \t]*[0-9]\\{1,3\\}\. +.*$"
      paragraph-separate "$")

;; Tramp
(setq tramp-default-method "ssh")

;; Never insert tabs
(setq-default indent-tabs-mode nil)

;; uniquify buffer name
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-strip-common-suffix nil)
(setq uniquify-min-dir-content 2)
(require 'uniquify)

;; auto-save
(setq auto-save-list-file-prefix (my-emacs-dir "auto-save-list-"))
(setq auto-save-file-name-transforms
      `(("\\`/\\([^/]*/\\)*\\([^/]*\\)\\'" ,(expand-file-name "\\2" temporary-file-directory) t)))

;; Bookmarks
(setq bookmark-save-flag 1)
(setq bookmark-use-annotations nil)
(setq bookmark-default-file (my-emacs-dir "bookmarks"))

;; create missing parent directories
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
	       (yes-or-no-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

;; Midnight mode
(setq midnight-period (* 60 60 1)) ; cleanup interval in seconds
(setq midnight-delay 0) ; no delay, uses simply `midnight-period'
(require 'midnight)
(setq clean-buffer-list-delay-general 60) ; days before a buffer is eligible to autokilling
(setq clean-buffer-list-delay-special (* 60 60 24 7)) ; seconds before a 'special' buffer is eligible to autokilling (see C-h f clean-buffer-list)
(add-to-list 'clean-buffer-list-kill-never-regexps "\\bmain\\.org\\b")


;; Save minibuffer history
(setq savehist-file (my-emacs-dir "history"))
(savehist-mode 1)
(setq history-length 200)

;; Easily navigate sillycased words
(global-subword-mode 1)
(ignore-errors ; without modeline
  (diminish 'subword-mode))

;; tweak Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; I use a tiling
;; only hilight current diff
(setq-default ediff-highlight-all-diffs nil)
;; turn off whitespace checking
(setq ediff-diff-options "-w")
;; How to split
(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))
;; Automatically restore window configuration
(add-hook 'ediff-load-hook
          (lambda ()
            (add-hook 'ediff-before-setup-hook
                      (lambda ()
                        (setq ediff-saved-window-configuration (current-window-configuration))))
            (let ((restore-window-configuration
                   (lambda ()
                     (set-window-configuration ediff-saved-window-configuration))))
              (add-hook 'ediff-quit-hook restore-window-configuration 'append)
              (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

;; Manual page reader in elisp
(require 'woman)
(set-face-attribute 'woman-bold nil :inherit 'font-lock-keyword-face)
(set-face-attribute 'woman-italic nil :inherit 'font-lock-function-name-face)
(setq woman-fill-column 100)
(setq woman-imenu t)
(defalias 'man 'woman)

;; Use X clipboard
(setq x-select-enable-clipboard t)





;;;;;;;;;;;;;;;;;;;;;
;;;     CEDET     ;;;
;;;;;;;;;;;;;;;;;;;;;

(global-ede-mode 1)
;;(ede-cpp-root-project "emacs.d"
;;(ede-enable-generic-projects)



;; (defun MY-FILE-FOR-DIR (&optional dir)
;;   "Return a full file name to the project file stored in DIR."
;;   <write your code here, or return nil>
;;   )

;; (defun MY-ROOT-FCN ()
;;   "Return the root fcn for `default-directory'"
;;   ;; You might be able to use `ede-cpp-root-project-root'
;;   ;; and not write this at all.
;;   )

;; (defun MY-LOAD (dir)
;;   "Load a project of type `cpp-root' for the directory DIR.
;;      Return nil if there isn't one."
;;   ;; Use your preferred construction method here.
;;   (ede-cpp-root-project "NAME" :file (expand-file-name "FILE" dir)
;;                         :locate-fcn 'MYFCN)
;;   )

;; (add-to-list 'ede-project-class-files
;;              (ede-project-autoload "cpp-root"
;;                                    :name "CPP ROOT"
;;                                    :file 'ede-cpp-root
;;                                    :proj-file 'MY-FILE-FOR-DIR
;;                                    :proj-root 'MY-ROOT-FCN
;;                                    :load-type 'MY-LOAD
;;                                    :class-sym 'ede-cpp-root)
;;              t)




(use-package semantic
  :defer 1
  :init
  (setq semantic-case-fold t)
  (setq semantic-idle-scheduler-work-idle-time 20) ; idle parsing delay
  (setq semantic-idle-work-parse-neighboring-files-flag t) ; parse files in the same directory
  :config
  (require 'semantic/bovine/gcc)
  (semantic-mode 1)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-idle-summary-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (global-semantic-show-unmatched-syntax-mode 1)
  ;;(global-semantic-idle-completions-mode 1) ; Disabled because we use company
  ;;(global-semantic-decoration-mode 1)       ; dont like too much decoration
  ;;(global-semantic-highlight-func-mode 1)   ; same
  (global-semantic-mru-bookmark-mode 1)
  (global-semantic-idle-breadcrumbs-mode 1)
  (global-semantic-idle-local-symbol-highlight-mode 1)
  )








;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;      General packages      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Recent file history
(use-package recentf
  :defer 1
  :init
  (setq recentf-save-file (my-emacs-dir "recentf"))
  (setq recentf-max-saved-items 400)
  :config
  (recentf-mode 1)
  )

(use-package desktop
  :init
  (setq desktop-save t)
  (setq desktop-auto-save-timeout 3)
  (setq desktop-load-locked-desktop t)
  (setq desktop-base-file-name "desktop")
  (setq desktop-base-lock-name "desktop.lock")
  (setq desktop-path `(,user-emacs-directory "~"))
  (setq desktop-restore-frames nil)
  (setq desktop-dirname user-emacs-directory)
  :config
  (desktop-save-mode 1)
  )

(use-package windmove
  :defer 1
  :bind (("M-s-<left>" . windmove-left)
         ("M-s-<right>" . windmove-right)
         ("M-s-<up>" . windmove-up)
         ("M-s-<down>" . windmove-down))
  :config
  (setq windmove-wrap-around t)
  )

;; dired settings
(use-package dired
  :demand
  :config
  (setq dired-listing-switches "-la")
  ;; More colors and features in dired buffers
  (use-package dired+
    :init
    (setq diredp-hide-details-initially-flag nil)
    )
  )

;; more usable yank-pop
(use-package browse-kill-ring
  :defer 11
  :bind (("M-y" . browse-kill-ring))
  )

;; Snippet insertion
(use-package yasnippet
  :defer 1
  :config
  (setq yas-prompt-functions
        '(yas-ido-prompt yas-x-prompt yas-completing-prompt))
  (setq yas-snippet-dirs `(,(my-emacs-dir "snippets")))
  (setq yas-wrap-around-region t)
  (dolist (d yas-snippet-dirs)
    (add-to-list 'auto-mode-alist
                 `(,(regexp-quote (expand-file-name d)) . snippet-mode)))
  (yas-global-mode 1)
  :diminish yas-minor-mode
  )

;; highlight-parentheses-mode everywhere
(use-package highlight-parentheses
  :defer 1
  :config
  (global-highlight-parentheses-mode 1)
  :diminish highlight-parentheses-mode
  )

;; Project management
(use-package projectile
  :defer 1
  :bind (("M-s" . projectile-switch-to-buffer)
         ("H-M-s" . projectile-switch-to-buffer-other-window))
  :config
  (setq projectile-project-root-files
        (nconc projectile-project-root-files
               '("configure.ac" "configure.in" "build.xml")))
  (setq projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name))))
  (projectile-global-mode)

  )

;; Completion
(use-package company
  :defer 1
  :diminish company-mode
  :config
  (setq company-etags-ignore-case t)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 20)  ; bigger popup window
  (setq company-idle-delay .3)     ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)      ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  (global-company-mode 1)
  )

;; Move/duplicate line/region of text.
(use-package move-dup
  :defer 19
  :bind (("M-<up>" . md/move-lines-up)
         ("M-<down>" . md/move-lines-down)
	 ("M-S-<down>" . md/duplicate-down))
  )

;; Minibuffer prototype presentation
(use-package eldoc
  :defer 1
  :init
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-mode-hook 'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'eldoc-mode)
  (add-hook 'ielm-mode-hook 'eldoc-mode)
  (add-hook 'eshell-mode-hook 'eldoc-mode)
  :diminish eldoc-mode
  )

;; Git from Emacs
(use-package magit
  :defer 17
  :bind ("<f8> <f8>" . magit-status)
  )

;; Ido
(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-use-filename-at-point nil)
  (setq ido-use-url-at-point nil)
  (setq ido-use-virtual-buffers t)
  (setq ido-save-directory-list-file (my-emacs-dir "ido.last"))
  (ido-mode 1)

  ;; More Ido everywhere
  (setq org-completion-use-ido t)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq gnus-completing-read-function 'gnus-ido-completing-read)

  ;; faster flex matching algorithm usin the flx package
  (use-package flx-ido
    :config
    (setq ido-use-faces nil) ; uses flx faces instead of ido faces
    (flx-ido-mode 1)
    )

  ;; ido-mode for M-x
  (use-package smex
    :defer 7
    :init
    (setq smex-save-file (my-emacs-dir "smex-items"))
    :bind (("M-x" . smex)
           ("M-X" . smex-major-mode-commands))
    )

  ;; imenu with Ido
  (use-package idomenu
    :defer 13
    :bind (("M-i" . idomenu))
    )

  (use-package ido-vertical-mode
    :init
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    :config
    (ido-vertical-mode 1)
    )

  (use-package ido-ubiquitous
    :config (ido-ubiquitous-mode)
    )

  )

;; Optimized regexp generator (now Emacs has regexp-opt but some packages still
;; use make-regexp)
(use-package make-regexp
  :commands (make-regexp make-regexps)
  )

;; better grep (apt-get install silversearch-ag)
(use-package ag
  :defer 61
  :bind (("<f8> g" . ag-files))
  )

;; Better buffer list
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-default-sorting-mode 'filename/process)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-expert t)                 ; no confirm
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1024) (format "%7.1fk" (/ (buffer-size) 1024.0)))
     ((> (buffer-size) 1048576) (format "%7.1fM" (/ (buffer-size) 1048576.0)))
     (t (format "%8d" (buffer-size)))))
  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 20 20 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))
  ;; auto update
  (add-hook 'ibuffer-mode-hook (lambda ()
                                 (ibuffer-auto-mode 1)))
  )

(use-package eshell
  :defer 31
  :config
  (add-hook 'eshell-first-time-mode-hook
            (lambda ()
              (set-face-attribute 'eshell-prompt nil :foreground "deep sky blue" :weight 'bold)
              (define-key eshell-mode-map (kbd "C-M-<delete>") 'eshell-kill-input)
                ;; Add some "visual" commands (commands that use ncurses-like UI)
              (add-to-list 'eshell-visual-commands "mutt")
              (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
              ))

  ;; Pop completion list automatically
  (setq eshell-cmpl-autolist t)
  ;; ignore case
  (setq eshell-cmpl-ignore-case t)
  ;; use woman
  (setq eshell-cmpl-man-function 'woman)
  ;; short buffer name
  (setq eshell-buffer-shorthand t)
  ;; Copy to curdir if destination argument
  (setq eshell-default-target-is-dot t)
  ;; Need confirmation
  (setq eshell-cp-interactive-query t)
  (setq eshell-ln-interactive-query t)
  (setq eshell-mv-interactive-query t)
  (setq eshell-rm-interactive-query t)
  ;; Delete directory with rm
  (setq eshell-rm-removes-directories t)
  ;; eshell-term
  (setq eshell-term-name "xterm-256color")
  (setq eshell-destroy-buffer-when-process-dies t)
  )

;;;;;;;;;;;;;;;
;;;   Org   ;;;
;;;;;;;;;;;;;;;

(use-package org
  :defer 31
  :bind (("C-c l" . org-store-link)
         ("C-c C-l" . org-insert-link-global)
         ("C-c C-o" . org-open-at-point-global)
         ("C-c o" . org-open-at-point-global))
  :init
  ;; Settings
  (setq org-support-shift-select t)
  (setq my-org-dir "~/notes")
  (setq org-agenda-files `(,my-org-dir)) ; use all files in this dir to generate agenda buffer
  (setq my-main-org-file (expand-file-name "main.org" my-org-dir))
  (setq org-todo-keywords
        '((sequence "TODO" "VALIDATING" "|" "DONE")))
  ;; capture templates
  (setq org-capture-templates
        '(
          ("n" "new Note" entry (file+headline my-main-org-file "Notes")
           "* %?\n  %U - %a\n  %i\n" :empty-lines 1)
          ("t" "new Task Entry" entry (file+headline my-main-org-file "Tasks")
           "* TODO %?\n  %^t - %a\n  %i\n" :empty-lines 1)
          ("l" "new Link" entry (file+headline my-main-org-file "Links")
           "* %a\n  %U\n  %i\n  %?\n" :empty-lines 1)
          ("j" "new Journal Entry" entry (file+datetree (concat my-org-dir "/journal.org"))
           "* %U %?\n %i\n %a\n" :empty-lines 1)
          ))

  :config
  ;; org-mode support for ChangeLog (C-x 4 a)
  (defun org-log-current-defun ()
    (save-excursion
      (org-back-to-heading)
      (if (looking-at org-complex-heading-regexp)
          (match-string 4))))
  (add-hook 'org-mode-hook
            (lambda ()
              (make-variable-buffer-local 'add-log-current-defun-function)
              (setq add-log-current-defun-function 'org-log-current-defun)))

  )

(use-package guide-key
  :config
  ;; Set prefix key that fire guide-key
  ;; Can be set by mode
  (setq guide-key/guide-key-sequence '("<f1>" "<f2>" "<f3>" "<f4>" "<f5>" "<f6>" "<f7>" "<f8>" "<f9>" "<f10>" "<f11>" "C-x r" "C-c" "M-s"
                                       ;;(foo-mode . "prefix key")
                                       ))
  (setq guide-key/popup-window-position 'right)
  (setq guide-key/idle-delay 0.5)
  ;; font-lock by regexp
  (setq guide-key/highlight-command-regexp
        '(
          ("\\bregister\\b" . 'font-lock-keyword-face)
          ("\\brectangle\\b" . 'font-lock-string-face)
          ("\\bbookmark\\b" . 'font-lock-preprocessor-face)
          ("projectile" . 'font-lock-constant-face)
          ("helm-info" . 'font-lock-doc-face)
          ("helm.*?\\(?:grep\\|occur\\|woman\\|find\\)" . font-lock-constant-face)
          ))
  ;; if t allow to show popup for C-c C-x if only C-c is configured
  (setq guide-key/recursive-key-sequence-flag t)
  ;; Start it
  (guide-key-mode 1)
  :diminish guide-key-mode
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     Language specific packages     ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; flex-mode (TODO: make a better mode)
(use-package flex-mode
  :commands (flex-mode)
  :mode "\\.l\\'"
  :mode "\\.flex\\'"
  :mode "\\.lex\\'"
  )

;; Edit QML (Qt) files
(use-package qml-mode
  :commands (qml-mode)
  :mode "\\.qml\\'"
  )

;; TODO nxml
(setq nxml-slash-auto-complete-flag t)
(setq nxml-auto-insert-xml-declaration-flag nil)
(eval-after-load 'rng-loc
  '(add-to-list 'rng-schema-locating-files "~/.emacs.d/schemas/schemas.xml"))

;; TODO scss-mode and sass-mode
(setq scss-sass-command "/usr/bin/sass")
(setq scss-compile-at-save nil)

;; TODO php-mode
(setq php-mode-coding-style "WordPress")
(add-hook 'php-mode-hook 'column-enforce-n120)

;; Preprocessor directive indentation
(use-package ppindent
  :commands (ppindent)
  :config (setq ppindent-increment 1)
  )

(use-package python
  :commands (python-mode)
  :mode ("\\.py\\'" . python-mode)
  :mode ("[Ss][Cc]onstruct\\'" . python-mode)
  :mode ("[Ss][Cc]onscript\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (setq imenu-create-index-function 'python-imenu-create-index)))
  )

(use-package ruby-mode
  :commands (ruby-mode)
  :mode ("\\.rb\\'" "[Rr]akefile\\'" "\\.rk\\'" "\\.gemspec\\'")
  :interpreter "ruby"
  )

;; cperl has more features and can make eldoc available for perl. Does not use
;; use-package because it make Emacs buggy (Not understanding why)
;; TODO: prefer autoloading with use-package
(require 'cperl-mode)
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.t\\'" . perl-mode))
;; Enable Eldoc for perl
(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))
(add-hook 'cperl-mode-hook
          (lambda ()
            (set (make-local-variable 'eldoc-documentation-function)
                 'my-cperl-eldoc-documentation-function)))
(add-hook 'cperl-mode-hook 'eldoc-mode)
(cperl-set-style "PerlStyle")

(use-package cmake-mode
  :commands (cmake-mode)
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\.in\\'" "\\.cmake\\'")
  )

(use-package apache-mode
  :commands (apache-mode)
  :mode ("\\.htaccess\\'" "httpd\\.conf\\'" "apache\\.conf\\'" "srm\\.conf\\'"
         "access\\.conf\\'" "sites-\\(available\\|enabled\\)/")
  )

(use-package markdown-mode
  :commands (markdown-mode)
  :mode ("\\.md\\'" "\\.mdown\\'")
  :init
  ;; add autoloads for Github Flavored Markdown
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  )

;; Mutt related
(use-package muttrc-mode
  :commands (muttrc-mode)
  :init
  ;; mutt -- Add this at the end because some regexp doesn't match end
  ;; of string.
  (add-to-list 'auto-mode-alist '("muttrc\\'" . muttrc-mode) 'append)
  (add-to-list 'auto-mode-alist '("\\.mutt/" . muttrc-mode) 'append)
)

;; The C* mode
(use-package cc-mode
  :demand
  :config

  ;; Adds more C/C++ styles if installed.
  (use-package cc-mode-styles
    :config
    (cc-mode-styles)
    )

  ;; continue setting cc-mode...
  (setq c-tab-always-indent t)
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-toggle-hungry-state 1)
              (define-key c-mode-base-map (kbd "RET") 'reindent-then-newline-and-indent)
              ;; never indent in extern "C" { blocks.
              (c-set-offset 'inextern-lang 0)
              ;; Same for namespace
              (c-set-offset 'innamespace 0)
              ))

  ;; default styles
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (c-mode . "google")
          (c++-mode . "google")))

  (use-package function-args
    :diminish function-args-mode
    :config
    (defun function-args-cycle ()
      "Cycle through no overlay and function-args available prototypes."
      (interactive)
      (cond
       ((and fa-overlay (= fa-idx (+ (length fa-lst) -1)))
        (fa-abort))
       ((not fa-overlay)
        (fa-show))
       (t
        (fa-idx-cycle-down))))

    ;; change the bindings to use less keys
    ;; 7 _direct_ keybindings for only one package is too much for me.
    (let ((map function-args-mode-map))
      (define-key map (kbd "M-o") 'function-args-cycle)
      (define-key map (kbd "M-i") nil)
      (define-key map (kbd "M-n") nil)
      (define-key map (kbd "M-h") nil)
      (define-key map (kbd "M-u") nil)
      (define-key map (kbd "M-j") 'fa-jump-maybe)
      (define-key map (kbd "C-M-j") nil))

    (fa-config-default)
    )
  )

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook (lambda ()
                            (local-set-key (kbd "C-c d") 'godoc-at-point)
                            (local-set-key (kbd "M-j") 'godef-jump-other-window)
                            ))
  (add-hook 'before-save-hook 'gofmt-before-save)
  (use-package go-eldoc
    :config
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    )
  ;; Adds some projectile features for Go
  (use-package go-projectile)
  (use-package company-go
    :config
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode 1)))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Mail handling   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; beautiful messages
(add-hook 'message-mode-hook 'turn-on-auto-fill)
(add-hook 'message-mode-hook (lambda ()
                                (footnote-mode 1)))

;; Use local sendmail command
(setq message-send-mail-function 'sendmail-send-it)

;; Do not keep the buffer on send
(setq message-kill-buffer-on-exit t)

;; use sendmail
(setq send-mail-function 'sendmail-send-it)

;; Add that for using emacs as external editor for mutt
(add-to-list 'auto-mode-alist '("^/tmp/mutt-" . message-mode) 'append)



;;;;;;;;;;;;;;;;;;;;;
;;;   The Final   ;;;
;;;;;;;;;;;;;;;;;;;;;

;; The custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Always load that for handling from external programs
(use-package org-protocol
  :defer 1
  )


;; always start server
(require 'server)
(unless (server-running-p)
  (server-start))

;; TODO split that in packages
(require 'xkz-custom-functions)
;; TODO make that more clean
(require 'xkz-bindings)
