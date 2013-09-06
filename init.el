;; Time-stamp: <Last changed 15-08-2013 17:16:31 by Larry Kite, larry>

;; Configure el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))


; list all packages you want installed
(setq my-el-get-packages
      (append
       '(auto-complete
         ein
         jedi
         undo-tree)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-el-get-packages)

;; Configure elpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      yasnippet
                      ace-jump-mode
                      ack-and-a-half
                      magit
                      melpa
                      solarized-theme
                      switch-window
                      smex
                      key-chord
                      buffer-move
                      ido-ubiquitous
                      buffer-move
                      idle-highlight-mode
                      browse-kill-ring
                      deft
                      dired+
                      expand-region
                      ido-vertical-mode
                      naquadah-theme
                      rainbow-delimiters
                      soothe-theme
                      dropdown-list
                      zenburn-theme)


  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


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
(setq-default default-tab-width 4)

(setq lmk-functions-file "~/.emacs.d/functions.el")
(setq lmk-keybindings-file "~/.emacs.d/keybindings.el")
(load lmk-functions-file)
(load lmk-keybindings-file)

(load-theme 'zenburn t)
(require 'undo-tree)
(global-undo-tree-mode)
(ido-mode 1)

(require 'switch-window)
(require 'buffer-move)

(require 'whitespace)
(setq whitespace-line-column 96)
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

(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)
(global-set-key (kbd "C-c D") 'delete-file-and-buffer)

(global-hl-line-mode +1)

(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

(global-set-key (kbd "RET") 'newline-and-indent)

;;(electric-indent-mode +1)
(defalias 'qrr 'query-replace-regexp)
(set-frame-height (selected-frame) 52)
(set-frame-width (selected-frame) 130)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ein:use-auto-complete t)
 '(ein:use-auto-complete-superpack t)
 '(ido-create-new-buffer (quote never))
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(ido-file-extensions-order (quote (".py" ".el" ".sh")))
 '(ido-show-dot-for-dired t)
 '(ido-use-filename-at-point (quote guess)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
