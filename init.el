(setq lmk-emacs-init-file load-file-name)
(setq lmk-emacs-config-dir
      (file-name-directory lmk-emacs-init-file))

;; Set up 'custom' system
(setq custom-file (expand-file-name "emacs-customizations.el" lmk-emacs-config-dir))
(load custom-file)

(setq user-emacs-directory lmk-emacs-config-dir)

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (defvar my-packages '(starter-kit
;;                       starter-kit-bindings)
;;   "A list of packages to ensure are installed at launch.")

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))

;; (defalias 'qrr 'query-replace-regexp)

;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'sh-mode-hook 'turn-off-auto-fill)
