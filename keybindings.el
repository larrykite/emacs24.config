;; Keyboard
;; ============
;;
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c a") 'auto-fill-mode)
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [(control x) (k)] 'kill-this-buffer)

;; C-z should never iconify Emacs, only suspend it when in a terminal.
;; I mean, who even iconifies programs any more?  Not me.
(if (eq window-system 'x)
    (global-set-key [(control z)] 'suspend-emacs))
;; Make it easy to ask about a character.  Useful for obscure Unicode.
(global-set-key "\M-?" 'describe-char)

(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x C") 'ibuffer)
(global-set-key (kbd "C-x B") 'ibuffer)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")   'buf-move-right)
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
(global-set-key (kbd "M-;") 'win-swap)
(global-set-key (kbd "C-`") 'push-mark-no-activate)
(global-set-key (kbd "M-`") 'jump-to-mark)
(global-set-key (kbd "C-S-v") 'scroll-down-command)

(global-set-key [C-tab]     'yic-next-buffer)
(global-set-key [C-S-tab]     'yic-prev-buffer)

(global-set-key (kbd "C-c c") 'comment-region)   ;; make C-c C-c and C-c C-u work
(global-set-key (kbd "C-c C-c") 'comment-region) ;; make C-c C-c and C-c C-u work
(global-set-key (kbd "C-c m") 'remember)
(global-set-key (kbd "C-c u") 'uncomment-region) ;; for comment/uncomment region in all modes
(global-set-key (kbd "C-c C-u") 'uncomment-region) ;; for comment/uncomment region in all modes
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
;;(global-set-key (kbd "C-z") 'shell)
;;(global-set-key (kbd "C-x p") 'compile)
(global-set-key (kbd "C-x y") 'upcase-word)

;; Meta
;; (global-set-key "\M- " 'set-mark-command) ;; commented-out shadowed by Alfred activate Mac OS system keybinding
(global-set-key "\M-\C-h" 'backward-kill-word)
(global-set-key "\M-\C-r" 'query-replace)
(global-set-key "\M-r" 'replace-string)
(global-set-key "\M-g" 'goto-line)


;; Function keys
(global-set-key [f1] 'manual-entry)
(global-set-key [f2] 'info)
(global-set-key [f3] 'repeat-complex-command)
;;(global-set-key [f4] 'advertised-undo)
(global-set-key [f5] "\C-u1\C-v")
(global-set-key [f6] "\C-u1\M-v")
(global-set-key [f7] 'other-window)
;;(global-set-key [f8] 'find-file)
(global-set-key [f9] 'kmacro-start-macro-or-insert-counter)
;; (global-set-key [f11] 'fullscreen) ;; commented-out: shadowed by Mac OS show desktop command
(global-set-key [f12] 'grep)

;; Keypad bindings
(global-set-key [up] "\C-p")
(global-set-key [down] "\C-n")
(global-set-key [left] "\C-b")
(global-set-key [right] "\C-f")
(global-set-key [home] "\C-a")
(global-set-key [end] "\C-e")
(global-set-key [prior] "\M-v")
(global-set-key [next] "\C-v")
(global-set-key [C-up] "\M-\C-b")
(global-set-key [C-down] "\M-\C-f")
(global-set-key [C-left] "\M-b")
(global-set-key [C-right] "\M-f")
(global-set-key [C-home] "\M-<")
(global-set-key [C-end] "\M->")
(global-set-key [C-prior] "\M-<")
(global-set-key [C-next] "\M->")
(global-set-key "\M-u" 'previous-line)
(global-set-key "\M-e" 'next-line)
(global-set-key "\M-n" 'backward-char)
(global-set-key "\M-i" 'forward-char)
(global-set-key "\M-N" 'backward-kill-word)
(global-set-key "\M-I" 'kill-word)
(global-set-key "\M-U" 'kill-start-of-line)
(global-set-key "\M-E" 'kill-line)

;; disable kill-emacs and minimize window if emacs started in daemon mode
(if (daemonp)
    (global-unset-key (kbd "C-x C-c")))
(if (daemonp)
    (global-unset-key (kbd "C-x C-z")))

;; Mouse
(global-set-key [mouse-3] 'imenu)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))


;; kill-other-buffers      key: C-c k
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(global-set-key (kbd "C-c k") 'kill-other-buffers)

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
         (buffer-substring (region-beginning) (region-end))
         (read-string "Google: "))))))

(global-set-key (kbd "C-c g") 'google)

(defun indent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning) (region-end))))

(global-set-key (kbd "C-M-z") 'indent-defun)

(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(global-set-key [(shift return)] 'smart-open-line)
(global-set-key [f8] 'deft)
;; Fixing a key binding bug in elpy
;;(define-key yas-minor-mode-map (kbd "C-c ;") 'yas-expand)
;; Fixing another key binding bug in iedit mode
;; (define-key global-map (kbd "C-c o") 'iedit-mode)
(global-set-key (kbd "C-x o") 'switch-window)
