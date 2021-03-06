;; Time-stamp: <Last changed 22-07-2015 14:52:08 by Larry Kite, larrykite>

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

;; Configure el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/dev/makey")
(add-to-list 'load-path "~/dev/discover.el")
(add-to-list 'load-path "/home/larry/.emacs.d/elpa/yasnippet-20140314.255/snippets")
;;(add-to-list 'load-path "~/dev/smart-scan")
(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files")
;; (set-default-font "Source Code Pro")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; list all packages you want installed
(setq my-el-get-packages
      (append
       '(auto-complete
         ein
         jedi
         undo-tree
         doremi)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)
(require 'package)
;; Configure elpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setenv "PYTHONPATH" "/usr/bin/python")
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(ace-jump-mode
                      ack
                      browse-kill-ring
                      buffer-move
                      deft
                      dired+
                      discover
                      dropdown-list
                      elpy
                      expand-region
                      helm
                      git-gutter
                      git-gutter-fringe
                      idle-highlight-mode
                      ido-ubiquitous
                      ido-vertical-mode
                      jinja2-mode
                      key-chord
                      magit
                      multiple-cursors
                      naquadah-theme
                      rainbow-delimiters
                      smartscan
                      smex
                      solarized-theme
                      switch-window
                      yasnippet
                      zenburn-theme
                      web-mode
                      ess)

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(elpy-enable)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:key-complete (kbd "C-c ."))
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(global-auto-revert-mode 1)
(delete-selection-mode t)

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
(load lmk-functions-file)
(load lmk-keybindings-file)

(load-theme 'zenburn t)
;;(load-theme 'base16-mocha t)
;;(load-theme 'base16-chalk t)
(global-git-gutter-mode t)
(require 'git-gutter-fringe)
;; commented out multiple-cursors because same functionality exists as
;; part of elpy
;;(require 'multiple-cursors)
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;(global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;(require 'expand-region)
;;(global-set-key (kbd "C-=") 'er/expand-region)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)
(ido-mode 1)
;; commented out doremi because I have no idea wtf it does.
;;(require 'doremi)
(require 'switch-window)
(require 'buffer-move)
(require 'dired-x)
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
;;(add-hook 'prog-mode-hook 'whitespace-mode)
(global-whitespace-mode +1)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'end-of-buffer)
(key-chord-define-global "gb" 'beginning-of-buffer)
(key-chord-define-global "FF" 'find-file)
(key-chord-define-global "jj" 'undo-tree-visualize)
(key-chord-define-global "JJ" 'switch-to-previous-buffer)
(key-chord-define-global ",." 'ibuffer)
(key-chord-define-global "hx" 'helm-M-x)
(key-chord-define-global "hm" 'helm-mini)
(add-hook 'before-save-hook 'whitespace-cleanup)

(require 'yasnippet)
(yas-global-mode 1)

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

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)
(helm-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)

(global-hl-line-mode +1)

(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(setq scroll-preserve-screen-position t)
(global-set-key (kbd "RET") 'newline-and-indent)

(defalias 'qrr 'query-replace-regexp)

(fset 'resize-main-frame
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([67108917 67108915 3 104 67108913 67108915 67108912 3 119] 0 "%d")) arg)))
(global-set-key (kbd "C-c z") 'resize-main-frame)


(require 'discover)
(global-discover-mode 1)
(smartscan-mode 1)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
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

 '(custom-safe-themes
   (quote
	("e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "7694bfb46ec19cfc47093783633d8cd4df208d620104910bf5c1c840528a8dd1" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "3d6b08cd1b1def3cc0bc6a3909f67475e5612dba9fa98f8b842433d827af5d30" "8c5ffc9848db0f9ad4e296fa3cba7f6ea3b0e4e00e8981a59592c99d21f99471" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(deft-directory "/home/larry/Dropbox/deft/")
 '(ein:use-auto-complete t)
 '(ein:use-auto-complete-superpack t)
 '(fci-rule-color "#383838")
 '(ido-create-new-buffer (quote never))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-file-extensions-order (quote (".py" ".el" ".sh")))
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess))
 '(package-selected-packages
   (quote
	(ess web-mode zenburn-theme switch-window solarized-theme smex smartscan rainbow-delimiters naquadah-theme multiple-cursors magit key-chord jinja2-mode ido-vertical-mode ido-ubiquitous idle-highlight-mode git-gutter-fringe git-gutter helm expand-region elpy dropdown-list discover dired+ deft buffer-move browse-kill-ring ack ace-jump-mode)))
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
