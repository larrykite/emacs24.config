;; Time-stamp: <Last changed 03-07-2015 17:05:19 by Larry Kite, larrykite>

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

(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      ace-jump-mode
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
 '(package-selected-packages
   (quote
    (ess web-mode zenburn-theme switch-window solarized-theme smartscan rainbow-delimiters naquadah-theme multiple-cursors key-chord jinja2-mode ido-vertical-mode git-gutter-fringe git-gutter helm expand-region elpy dropdown-list discover dired+ deft buffer-move browse-kill-ring ace-jump-mode starter-kit-bindings))))
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
