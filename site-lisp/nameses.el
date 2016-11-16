;;; Nameses enables named sessions.
;; Nameses is largely based on my-desktop.el by Scott Frazer
;; Modification by Jannis Teunissen
;; Version 0.02 - 21 Nov 2014 (added extra check)

;;; *** Usage ***
;; Note: if 'name' is not given, you'll be asked for a name
;;
;; (nameses-load prefix &optional name) -> without prefix: load session
;; (nameses-load prefix &optional name) -> with prefix: save session
;; (nameses-remove &optional name)      -> remove session
;; (nameses-reset)                      -> clear session without saving
;; (nameses-prev)                       -> load previous session
;; (nameses-current-name)               -> get current session name

;;; *** Example configuration in .emacs ***
;; (require 'desktop)
;; (require 'nameses)
;; (require 'ido) or (setq nameses-ido-mode nil)
;; (global-set-key (kbd "<f9>")     'nameses-load)
;; (global-set-key (kbd "C-<f9>")   'nameses-prev)
;; (global-set-key (kbd "C-S-<f9>") 'nameses-save)

(defvar nameses-dir
  (concat (getenv "HOME") "/.emacs.d/nameses-sessions/")
  "*Directory to save desktop sessions in")

(defvar nameses-prev-session nil
  "The previous desktop session")

(defvar nameses-ido-mode t
  "Whether to use ido-mode")

(defun nameses-save (&optional name)
  "Save desktop by name."
  (interactive)
   (unless name (setq name (read-string "Save session as: ")))
   (when (nameses-current-name)
     (desktop-release-lock))
   (make-directory (concat nameses-dir name) t)
   (desktop-lazy-complete)               ; Load all buffers before saving
   (desktop-save (concat nameses-dir name) nil))

(defun nameses-prev ()
  "Switch to previous session"
  (interactive)
  (when nameses-prev-session
    (nameses-load nil nameses-prev-session)))

(defun nameses-remove (&optional name)
  "Remove desktop by name."
  (interactive)
  (unless name
    (setq name (nameses-select "Remove session: ")))
  (unless (nameses-detect-problems name)
    (when (yes-or-no-p (concat "Really remove session '" name "' ?"))
      (progn
	(delete-directory (concat nameses-dir name) t)
	(when (string= nameses-prev-session name)
	  (setq nameses-prev-session nil))
	(when (string= (nameses-current-name) name) ; Current session is removed
	  (setq desktop-dirname nil))))))           ; so reset session name

(defun nameses-reset ()
  "Reset session without saving."
  (interactive)
  (save-some-buffers)
  (desktop-release-lock)
  (desktop-clear)
  (setq desktop-dirname nil))

(defun nameses-detect-problems (name)
  "Check whether a session is unlocked and stored correctly"
  (catch 'err
    (let ((dirname (file-name-as-directory (concat nameses-dir name))))
      (when (not (file-directory-p dirname))
	(throw 'err (concat dirname " does not exist")))
      (when (file-exists-p (concat dirname desktop-base-lock-name))
	(if (y-or-n-p (concat name " is locked, remove lock?"))
	    (desktop-release-lock dirname)
	  (throw 'err (concat name " is locked"))))
      (let ((dirfiles (delete "." (delete ".." (directory-files dirname)))))
	(when (not (equal dirfiles (list desktop-base-file-name)))
	  (throw 'err (concat dirname " contains extra files")))))))

(defun nameses-load (prefix &optional name)
  "Load session by name. With universal argument, create new session."
  (interactive "P")
  (let ((prev-name (nameses-current-name)))
    (if (equal prefix '(4))
	(progn
	  (unless name (setq name (read-string "Create new session: ")))
          (when prev-name (nameses-save prev-name))
	  (nameses-reset)
	  (nameses-save name))
      (progn
	(unless name (setq name (nameses-select "Load session: ")))
        (when prev-name (nameses-save prev-name))
	(nameses-reset)
        (nameses-detect-problems name)
        (desktop-read (concat nameses-dir name))))
    (unless (and nameses-prev-session (string= nameses-prev-session prev-name))
      (setq nameses-prev-session prev-name))))

(defun nameses-current-name ()
  "Get the current desktop name."
  (interactive)
  (when desktop-dirname
    (let ((dirname (substring desktop-dirname 0 -1))) ; Remove trailing /
      (when (string= (file-name-directory dirname) nameses-dir)
	(file-name-nondirectory dirname)))))

(defun nameses-list-all ()
  "Get all stored session names"
  (when (file-exists-p nameses-dir)
    (delete "." (delete ".." (directory-files nameses-dir)))))

(defun nameses-select (message)
  "Select an existing session by name"
  (if nameses-ido-mode
      (ido-completing-read message (nameses-list-all) nil t nil nil nameses-prev-session)
    (completing-read message (nameses-list-all) nil t nil nil nameses-prev-session)))

(defun nameses-kill-emacs-hook ()
  "Save desktop before killing emacs."
  (when (nameses-current-name)
    (nameses-save (nameses-current-name))))

(add-hook 'kill-emacs-hook 'nameses-kill-emacs-hook)
(provide 'nameses)
