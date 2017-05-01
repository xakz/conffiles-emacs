;; Disable some unused binding.
;; Many are based on the fact that old keyboards doesn't have keys that modern keyboards have.
;; (arrow, delete, pgup, home, end, ...)
;; Others are removed because I do not use it or remapped it.
(global-unset-key (kbd "C-f")) ; exists with <right>
(global-unset-key (kbd "C-b")) ; exists with <left>
(global-unset-key (kbd "C-p")) ; exists with <up>
(global-unset-key (kbd "C-n")) ; exists with <down>
(global-unset-key (kbd "C-d")) ; exists with <delete>
(global-unset-key (kbd "C-v")) ; exists with <next>
(global-unset-key (kbd "M-v")) ; exists with <prior>
;(global-unset-key (kbd "M-b")) ; exists with M-<right>
;(global-unset-key (kbd "M-f")) ; exists with M-<left>

;; Some rebinding
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Only one hit please !

;; Remap in ctl-x-map
(define-key ctl-x-map (kbd "C-q") 'view-mode)



;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fun with function key
;;;;;;;;;;;;;;;;;;;;;;;


;;(global-set-key (kbd "<f1>") 'nil)
;;(global-set-key (kbd "<f2>") 'nil)
;;(global-set-key (kbd "<f3>") 'nil)
;;(global-set-key (kbd "<f4>") 'nil)
;;(global-set-key (kbd "<f5>") 'nil)
;;(global-set-key (kbd "<f6>") 'nil)
;;(global-set-key (kbd "<f7>") 'nil)
(global-set-key (kbd "<f8> <f8>") 'magit-status)
;(global-set-key (kbd "<f8> <f5>") 'mu4e)
(global-set-key (kbd "<f8> <f9>") 'org-capture)
(global-set-key (kbd "<f8> <f10>") 'org-agenda-list)
(global-set-key (kbd "<f8> <f11>") 'org-agenda)
(global-set-key (kbd "<f8> c") 'calculator)
(global-set-key (kbd "<f8> a") 'calendar)

;;(global-set-key (kbd "<f9>") 'nil)
;;(global-set-key (kbd "<f10>") 'menu-bar-open)
;;(global-set-key (kbd "<f11>") 'nil)
(global-set-key (kbd "<f12>") 'sx-toggle-shell-buffer)



;;;;;;;;;;;;;;;;;;
;; Fun with arrow keys
;;;;;;;;;;;;;;;;;;
;; DO NOT REDEFINE :p
;;(global-set-key (kbd "<up>") 'previous-line)
;;(global-set-key (kbd "<down>") 'next-line)
;;(global-set-key (kbd "<left>") 'left-char)
;;(global-set-key (kbd "<right>") 'right-char)

;; Not binded but combined with Shift selection, do not redefine
;;(global-set-key (kbd "S-<up>") 'nil)
;;(global-set-key (kbd "S-<down>") 'nil)
;;(global-set-key (kbd "S-<left>") 'nil)
;;(global-set-key (kbd "S-<right>") 'nil)

;;(global-set-key (kbd "C-<up>") 'backward-paragraph)
;;(global-set-key (kbd "C-<down>") 'forward-paragraph)
;;(global-set-key (kbd "C-<left>") 'backward-word)
;;(global-set-key (kbd "C-<right>") 'forward-word)

;;(global-set-key (kbd "M-<up>") 'nil) ;; Defined in init.el for move line up
;;(global-set-key (kbd "M-<down>") 'nil) ;; and move line down (move-dup package)
;;(global-set-key (kbd "M-<left>") 'nil)
;;(global-set-key (kbd "M-<right>") 'nil)

;; Not binded but combined with Shift selection, do not redefine
;;(global-set-key (kbd "C-S-<up>") 'nil)
;;(global-set-key (kbd "C-S-<down>") 'nil)
;;(global-set-key (kbd "C-S-<left>") 'nil)
;;(global-set-key (kbd "C-S-<right>") 'nil)

;;(global-set-key (kbd "M-S-<up>") 'nil)
;;(global-set-key (kbd "M-S-<down>") 'nil)   ;; also definied by move-dup package (dup line)
;;(global-set-key (kbd "M-S-<left>") 'nil)
;;(global-set-key (kbd "M-S-<right>") 'nil)

;; Default is navigation in sexp or like in various mode
;;(global-set-key (kbd "C-M-<up>") 'backward-up-list) 
;;(global-set-key (kbd "C-M-<down>") 'down-list)
;;(global-set-key (kbd "C-M-<left>") 'forward-sexp)
;;(global-set-key (kbd "C-M-<right>") 'backward-sexp)

;;(global-set-key (kbd "C-M-S-<up>") 'nil)
;;(global-set-key (kbd "C-M-S-<down>") 'nil)
;;(global-set-key (kbd "C-M-S-<left>") 'nil)
;;(global-set-key (kbd "C-M-S-<right>") 'nil)

;;;;;;;;;;;;;;;;;;
;; Fun with <prior> and <next>
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<prior>") (lambda () (interactive) (previous-line 15)))
(global-set-key (kbd "<next>") (lambda () (interactive) (next-line 15)))

(global-set-key (kbd "S-<prior>") 'scroll-down-command)
(global-set-key (kbd "S-<next>") 'scroll-up-command)

(global-set-key (kbd "C-<prior>") 'backward-page)
(global-set-key (kbd "C-<next>") 'forward-page)

;;(global-set-key (kbd "M-<prior>") 'scroll-other-window-down)
;;(global-set-key (kbd "M-<next>") 'scroll-other-window)

(global-set-key (kbd "C-M-<prior>") (lambda () (interactive) (scroll-down-command 1)))
(global-set-key (kbd "C-M-<next>") (lambda () (interactive) (scroll-up-command 1)))

;;(global-set-key (kbd "M-S-<prior>") 'nil)
;;(global-set-key (kbd "M-S-<next>") 'nil)

;;(global-set-key (kbd "C-S-<prior>") 'nil)
;;(global-set-key (kbd "C-S-<next>") 'nil)

;;(global-set-key (kbd "C-M-S-<prior>") 'nil)
;;(global-set-key (kbd "C-M-S-<next>") 'nil)


;;;;;;;;;;;;;;;;;;
;; Fun with <home>, <end>
;;;;;;;;;;;;;;;;;;
;;(global-set-key (kbd "<home>") 'move-beginning-of-line)
;;(global-set-key (kbd "<end>") 'move-end-of-line)

;;(global-set-key (kbd "S-<home>") 'nil)
;;(global-set-key (kbd "S-<end>") 'nil)

;;(global-set-key (kbd "C-<home>") 'beginning-of-buffer)
;;(global-set-key (kbd "C-<end>") 'end-of-buffer)

;;(global-set-key (kbd "M-<home>") 'beginning-of-buffer-other-window)
;;(global-set-key (kbd "M-<end>") 'end-of-buffer-other-window)

;;(global-set-key (kbd "C-S-<home>") 'nil)
;;(global-set-key (kbd "C-S-<end>") 'nil)

;;(global-set-key (kbd "M-S-<home>") 'nil)
;;(global-set-key (kbd "M-S-<end>") 'nil)

;;(global-set-key (kbd "C-M-<home>") 'nil)
;;(global-set-key (kbd "C-M-<end>") 'nil)

;;(global-set-key (kbd "C-M-S-<home>") 'nil)
;;(global-set-key (kbd "C-M-S-<end>") 'nil)



;;;;;;;;;;;;;;;;;;
;; Fun with <backspace>, <delete>
;;;;;;;;;;;;;;;;;;
;;(global-set-key (kbd "<backspace>") 'backward-delete-char-untabify)
;;(global-set-key (kbd "<delete>") 'delete-forward-char)

;;(global-set-key (kbd "S-<backspace>") 'nil)
;;(global-set-key (kbd "S-<delete>") 'nil)

(global-set-key (kbd "C-<backspace>") 'backward-kill-line)
(global-set-key (kbd "C-<delete>") 'kill-line)

(global-set-key (kbd "M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<delete>") 'kill-word)

;;(global-set-key (kbd "C-S-<backspace>") 'kill-whole-line)
;;(global-set-key (kbd "C-S-<delete>") 'nil)

;;(global-set-key (kbd "M-S-<backspace>") 'nil)
;;(global-set-key (kbd "M-S-<delete>") 'nil)

;;(global-set-key (kbd "C-M-<backspace>") 'nil) ;; DONT, its kill X11 :-p
(global-set-key (kbd "C-M-<delete>") 'kill-whole-line)

;;(global-set-key (kbd "C-M-S-<backspace>") 'nil)
;;(global-set-key (kbd "C-M-S-<delete>") 'nil)


;;;;;;;;;;;;;;;;;;
;; Fun with <insert>, <print> and <pause>.
;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<insert>") 'other-window)
(global-set-key (kbd "<print>") 'winner-undo)
(global-set-key (kbd "<pause>") 'winner-redo)

;;(global-set-key (kbd "S-<insert>") 'yank)
;;(global-set-key (kbd "S-<print>") 'nil)
;;(global-set-key (kbd "S-<pause>") 'nil)

(global-set-key (kbd "C-<insert>") 'swap-windows)
;;(global-set-key (kbd "C-<print>") 'nil)
;;(global-set-key (kbd "C-<pause>") 'nil)

;;(global-set-key (kbd "M-<insert>") 'nil)
;;(global-set-key (kbd "M-<print>") 'nil)
;;(global-set-key (kbd "M-<pause>") 'nil)

;;(global-set-key (kbd "C-S-<insert>") 'nil)
;;(global-set-key (kbd "C-S-<print>") 'nil)
;;(global-set-key (kbd "C-S-<pause>") 'nil)

;;(global-set-key (kbd "M-S-<insert>") 'nil)
;;(global-set-key (kbd "M-S-<print>") 'nil)
;;(global-set-key (kbd "M-S-<pause>") 'nil)

;;(global-set-key (kbd "C-M-<insert>") 'nil)
;;(global-set-key (kbd "C-M-<print>") 'nil)
;;(global-set-key (kbd "C-M-<pause>") 'nil)

;;(global-set-key (kbd "C-M-S-<insert>") 'nil)
;;(global-set-key (kbd "C-M-S-<print>") 'nil)
;;(global-set-key (kbd "C-M-S-<pause>") 'nil)


;;;;;;;;;;;;;;;
;; Others, more classic bindings
;;;;;;;;;;;;;;;

;; Rapid buffer kill, and mark buffer as done in server mode.
(global-set-key (kbd "M-k") 'kill-this-buffer)
(add-hook 'server-switch-hook
          (lambda ()
            (local-set-key (kbd "M-k") '(lambda ()
                                          (interactive)
                                          (if server-buffer-clients
                                              (server-edit)
                                            (kill-this-buffer))))))

;; custom funcs
(global-set-key (kbd "C-M-;") 'sx-comment-multi-line-region)
(global-set-key (kbd "C-S-k") 'kill-buffer-other-window)
(global-set-key (kbd "M-d") 'quit-window-other-window)




;;;; AZERTY keyboards

;; some binding for azerty keyboard
(global-set-key (kbd "C-x &") 'delete-other-windows) ; C-x 1
(global-set-key (kbd "C-x é") 'split-window-vertically) ;  C-x 2
(global-set-key (kbd "C-x \"") 'split-window-horizontally) ; C-x 3
(global-set-key (kbd "C-x à") 'delete-window)              ; C-x 0

;; azerty mod for C-x 4 serie...
(global-unset-key (kbd "C-x '")) ; do not need that
(global-set-key (kbd "C-x ' b") 'switch-to-buffer-other-window) ; C-x 4 b
(global-set-key (kbd "C-x ' .") 'find-tag-other-window) ; C-x 4 .
(global-set-key (kbd "C-x ' f") 'find-file-other-window) ; C-x 4 f
(global-set-key (kbd "C-x ' a") 'add-change-log-entry-other-window) ; C-x 4 a
(global-set-key (kbd "C-x ' C-o") 'display-buffer) ; C-x 4 C-o
(global-set-key (kbd "M-:") 'dabbrev-expand) ; M-/


;; fast switch buffer
(global-set-key "\C-b" 'switch-to-buffer)
(global-set-key "\C-x\C-d" (lambda (&optional arg)
                               "Open a dired buffer for the current buffer directory."
                               (interactive)
                               (dired (file-name-directory (or (buffer-file-name)
                                                               default-directory)))))

;;;;;; Hyper bindings (all is about control windows without leaving current window) ;;;;;;
;; move between windows
(global-set-key (kbd "H-<up>") 'windmove-up)
(global-set-key (kbd "H-<down>") 'windmove-down)
(global-set-key (kbd "H-<left>") 'windmove-left)
(global-set-key (kbd "H-<right>") 'windmove-right)

(global-set-key (kbd "H-c") 'delete-window)
(global-set-key (kbd "M-c") 'delete-window)

(global-set-key (kbd "H-s") 'ispell-word)
(global-set-key (kbd "M-s") 'ispell-word)

;; switch buffer other window
(global-set-key (kbd "H-C-b") 'switch-to-buffer-other-window)



(provide 'xkz-bindings)
