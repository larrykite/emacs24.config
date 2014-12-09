;; Time-stamp: <Last changed 09-12-2014 15:13:34 by Larry Kite, larry>

(setenv "PYTHONPATH" "/usr/bin/python")
(set-default-font "Source Code Pro")
(require 'package)
;; Configure elpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-bindings
		      auto-complete
		      ein
		      jedi
		      undo-tree
                      buffer-move
                      dash
                      deft
                      dired+
                      discover
                      dropdown-list
                      ess
                      ido-ubiquitous
                      key-chord
                      magit
                      multiple-cursors
                      rainbow-delimiters
                      smex
                      solarized-theme
                      switch-window
                      zenburn-theme
                      )

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'sh-mode-hook (lambda () (auto-fill-mode -1)))
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:key-complete (kbd "C-c ."))

(global-set-key (kbd "C-c a") 'auto-fill-mode)
(global-auto-revert-mode 1)
(delete-selection-mode t)
(require 'ess-site)
(set-language-environment "utf-8")
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-pattern nil)
(setq
 time-stamp-active t          ; do enable time-stamps
 time-stamp-line-limit 10     ; check first 10 buffer lines for Time-stamp: <>
 time-stamp-format "Last changed %02d-%02m-%04y %02H:%02M:%02S by %L, %u") ; date format
(add-hook 'write-file-hooks 'time-stamp) ; update when saving

(setq-default default-tab-width 4)

(setq lmk-functions-file "~/.emacs.d/functions.el")
(setq lmk-keybindings-file "~/.emacs.d/keybindings.el")
(setq temporary-file-directory "~/.emacs.d/backups")
(load lmk-functions-file)
(load lmk-keybindings-file)

(load-theme 'zenburn t)

;; commented out multiple-cursors because same functionality exists as
;; part of elpy
;; (require 'multiple-cursors)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)
(ido-mode 1)
;; commented out doremi because I have no idea wtf it does.

(require 'switch-window)
(require 'buffer-move)
(require 'dired-x)
(auto-fill-mode -1)
(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'end-of-buffer)
(key-chord-define-global "gb" 'beginning-of-buffer)
(key-chord-define-global "FF" 'find-file)
(key-chord-define-global "jj" 'undo-tree-visualize)
(key-chord-define-global "JJ" 'switch-to-previous-buffer)
(key-chord-define-global ",." 'ibuffer)
(key-chord-define-global "AF" 'auto-fill-mode)
(add-hook 'before-save-hook 'whitespace-cleanup)


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; (require 'recentf)
;; (setq recentf-max-saved-items 200
;;       recentf-max-menu-items 15)
;; (recentf-mode +1)

(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)

;; (global-hl-line-mode +1)

(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(setq scroll-preserve-screen-position t)
(global-set-key (kbd "RET") 'newline-and-indent)

(defalias 'qrr 'query-replace-regexp)

;; (set-frame-height (selected-frame) 52)
;; (set-frame-width (selected-frame) 130)

;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
            ;; We remove Which Function Mode from the mode line, because it's mostly
            ;; invisible here anyway.
            (assq-delete-all 'which-func-mode mode-line-misc-info))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-auto-complete t)
 '(custom-safe-themes
   (quote
    ("442c946bc5c40902e11b0a56bd12edc4d00d7e1c982233545979968e02deb2bc" "d143750cb9fadb9ea9a3a27e0632418d2ad09788e115a61a64dd5404fedfe178" "d809ca3cef02087b48f3f94279b86feca896f544ae4a82b523fba823206b6040" "a507b9ca4a605d5256716da70961741b9ef9ec3246041a4eb776102e8df18418" "fa942713c74b5ad27893e72ed8dccf791c9d39e5e7336e52d76e7125bfa51d4c" "56cb99174fad69feba8edd6663c592e77d77332fb4c4bb5be058ef459a426463" "c3e567dedaa800e869d879c4df8478237d6ea31fd04464086fd674c864fe4d71" "f0a99f53cbf7b004ba0c1760aa14fd70f2eabafe4e62a2b3cf5cabae8203113b" "efa048d8a9f9a8340a2f6382f3b8b8f4549cba38aa226803ff5b6a9b3a2d5f4b" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "16248150e4336572ff4aa21321015d37c3744a9eb243fbd1e934b594ff9cf394" "1c6693b96aab150f9739f19fc423770e0eb0b4cb8e2a95c8c6c48abcae719521" "572caef0c27b100a404db8d540fd5b31397f90ab660ef5539ff0863ff9bee26a" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "7694bfb46ec19cfc47093783633d8cd4df208d620104910bf5c1c840528a8dd1" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "8c5ffc9848db0f9ad4e296fa3cba7f6ea3b0e4e00e8981a59592c99d21f99471" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(deft-directory "/home/larry/Dropbox/deft/")
 '(dired-listing-switches "-alX")
 '(ein:use-auto-complete t)
 '(ein:use-auto-complete-superpack t)
 '(fci-rule-color "#383838")
 '(ido-create-new-buffer (quote never))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-file-extensions-order (quote (".py" ".el" ".sh")))
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)

(fset 'resize-main-frame
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108917 67108915 3 104 67108913 67108915 67108912 3 119] 0 "%d")) arg)))
(global-set-key (kbd "C-c z") 'resize-main-frame)

(fset 'testmacro
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("" 0 "%d")) arg)))
