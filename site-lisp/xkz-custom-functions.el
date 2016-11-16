;; indirect buffer with mode
(defvar indirect-mode-name nil
  "Mode to set for indirect buffers.")
(make-variable-buffer-local 'indirect-mode-name)
(defun indirect-region (start end)
  "Edit the current region in another buffer.
    If the buffer-local variable `indirect-mode-name' is not set, prompt
    for mode name to choose for the indirect buffer interactively.
    Otherwise, use the value of said variable as argument to a funcall."
  (interactive "r")
  (let ((buffer-name (generate-new-buffer-name "*indirect*"))
        (mode
         (if (not indirect-mode-name)
             (setq indirect-mode-name
                   (intern
                    (completing-read
                     "Mode: "
                     (mapcar (lambda (e)
                               (list (symbol-name e)))
                             (apropos-internal "-mode$" 'commandp))
                     nil t)))
           indirect-mode-name)))
    (pop-to-buffer (make-indirect-buffer (current-buffer) buffer-name))
    (funcall mode)
    (narrow-to-region start end)
    (goto-char (point-min))
    (shrink-window-if-larger-than-buffer)))

;; toggle shell buffer
(defun sx-toggle-shell-buffer ()
  "Toggle between shell and previous buffer"
  (interactive)
  (if (string-match "^\*eshell" (buffer-name))
      (previous-buffer)
    (eshell)))

(defun sx-comment-multi-line-region ()
  "Use comment style 'extra-line' and comment region"
  (interactive)
  (let ((comment-style 'extra-line))
    (comment-region (region-beginning) (region-end))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun kill-buffer-other-window ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

(defun quit-window-other-window ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (quit-window)
    (select-window win-curr)))

(defun forward-thing-smart (&optional arg)
  "TOFINISH
Find a way to move forward to an interresting thing."
  (interactive "N")
  (unless (ignore-errors (forward-button (or arg 1) 'wrap))
    (forward-paragraph (or arg 1))))

(defun backward-thing-smart (&optional arg)
  "TOFINISH
Find a way to move backward to an interresting thing."
  (interactive "N")
  (forward-thing-smart arg))

;; (push-button)


;; (defun end-of-buffer-other-window (arg)
;;   "Move point to the end of the buffer in the other window.
;; Leave mark at previous position.
;; With arg N, put point N/10 of the way from the true end."
;;   (interactive "P")
;;   ;; See beginning-of-buffer-other-window for comments.
;;   (let ((orig-window (selected-window))
;; 	(window (other-window-for-scrolling)))
;;     (unwind-protect
;; 	(progn
;; 	  (select-window window)
;; 	  (with-no-warnings
;; 	   (end-of-buffer arg))
;; 	  (recenter '(t)))
;;       (select-window orig-window))))


(defun delete-file-and-kill-current-buffer ()
  "Delete the current buffer's file and kill the buffer."
  (interactive)
  (let ((file (buffer-file-name))
        (buffer (current-buffer)))
    (if (not file)
        (user-error "This buffer do not visit any file !")
      (if (not (y-or-n-p (format "Really delete the file %s ?" file)))
          (message "Cancelled ! Nothing deleted.")
        (unless (condition-case err
                    (delete-file file t)
                  (error
                   (user-error "Error during delete operation: %s" (error-message-string err))))
          (kill-buffer buffer)
          (message "File %s deleted !" file))))))

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly 2 windows to do this.")
    (let* ((w1 (car (window-list)))
           (w2 (cadr (window-list)))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (s1 (window-start w1))
           (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 s2)
      (set-window-start w2 s1))))


(defun my-eshell-execute-current-line ()
  "Insert text of current line in eshell and execute."
  (interactive)
  (require 'eshell)
  (let ((command (buffer-substring
                  (save-excursion
                    (beginning-of-line)
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point)))))
    (let ((buf (current-buffer)))
      (unless (get-buffer eshell-buffer-name)
        (eshell))
      (display-buffer eshell-buffer-name t)
      (switch-to-buffer-other-window eshell-buffer-name)
      (end-of-buffer)
      (eshell-kill-input)
      (insert command)
      (eshell-send-input)
      (end-of-buffer)
      (switch-to-buffer-other-window buf))))

(defun backward-kill-line (arg)
  "Kill ARG lines backward."
  (interactive "p")
  (kill-line (- 1 arg)))

(provide 'xkz-custom-functions)
