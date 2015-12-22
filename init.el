;; Time-stamp: <Last changed 04-03-2015 15:57:59 by Larry Kite, larry>

(setenv "PYTHONPATH" "/usr/bin/python")
;; (set-default-font "Source Code Pro")
(set-frame-font "Inconsolata-10" t t)
;; I have spent far too much of my life answering "y" to the prompt
;; "Active processes exist; kill them and exit anyway?"
(add-hook 'shell-mode-hook
          (lambda ()
            (process-kill-without-query
             (get-buffer-process (current-buffer)) nil)))

(require 'package)
;; Configure elpa
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(better-defaults
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
		      idle-highlight-mode
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

(require 'auto-complete-config)
(ac-config-default)

;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'sh-mode-hook (lambda () (auto-fill-mode -1)))
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 200)
(setq jedi:tooltip-method nil)
(setq jedi:key-complete (kbd "C-c ."))

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
;; (pdf-tools-install)
(if (file-exists-p "~/.emacs.d/functions.el")
    (load-library "~/.emacs.d/functions.el"))
(if (file-exists-p "~/.emacs.d/keybindings.el")
    (load-library "~/.emacs.d/keybindings.el"))

(load-theme 'zenburn t)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

(ido-mode 1)
;; (require 'ecb)
;; (setq ecb-layout-name "left1")
;; (setq ecb-show-sources-in-directories-buffer 'always)
;; (setq ecb-compile-window-height 12)

;; ;;; activate and deactivate ecb
;; (global-set-key (kbd "C-x C-;") 'ecb-activate)
;; (global-set-key (kbd "C-x C-'") 'ecb-deactivate)
;; ;;; show/hide ecb window
;; (global-set-key (kbd "C-;") 'ecb-show-ecb-windows)
;; (global-set-key (kbd "C-'") 'ecb-hide-ecb-windows)
;; ;;; quick navigation between ecb windows
;; (global-set-key (kbd "C-)") 'ecb-goto-window-edit1)
;; (global-set-key (kbd "C-!") 'ecb-goto-window-directories)
;; (global-set-key (kbd "C-@") 'ecb-goto-window-sources)
;; (global-set-key (kbd "C-#") 'ecb-goto-window-methods)
;; (global-set-key (kbd "C-$") 'ecb-goto-window-compilation)
;; (add-hook 'ecb-deactivate-hook
;; 	  (lambda () (modify-all-frames-parameters '((width . 80)))))
;; ;; resize the ECB window to be default (order matters here)
;; (add-hook 'ecb-activate-hook (lambda () (ecb-redraw-layout)))
;; (add-hook 'ecb-activate-hook
;; 	  (lambda () (modify-all-frames-parameters '((width . 120)))))

;; (require 'ecb-autoloads)

;; (setq load-path (cons "/usr/share/emacs/site-lisp/global" load-path))
;; (autoload 'gtags-mode "gtags" "" t)
;; (when window-system
;;   (speedbar t))
(require 'switch-window)
(require 'buffer-move)
(require 'dired-x)
;; (setq dired-omit-files
;;       (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
;;               (seq "~" eol)                 ;; backup-files
;;               (seq ".pyc" eol)
;;               (seq ".pyo" eol)
;;               )))
;; (setq dired-omit-extensions
;;       (append dired-latex-unclean-extensions
;;               dired-bibtex-unclean-extensions
;;               dired-texinfo-unclean-extensions))
;; (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
;; (put 'dired-find-alternate-file 'disabled nil)

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

(require 'cl)                       ; for Common List remove-if function

(defun contains-space-p (s)
  "Determine whether a given string contains spaces."
  (string-match-p " " s))

(defadvice ispell-parse-output (after remove-multi-words activate)
  "Remove multi-word suggestions from ispell-style output."
  (if (listp ad-return-value)
      (setq ad-return-value
            (list (nth 0 ad-return-value) ;; original word
                  (nth 1 ad-return-value) ;; offset in file
                  (remove-if 'contains-space-p (nth 2 ad-return-value))
                  (remove-if 'contains-space-p (nth 3 ad-return-value))
                  ))))

;; Spell-check the whole buffer upon entry (thanks, Ryan McGuire!)
;; unless it is really huge.

(defadvice flyspell-mode
  (after advice-flyspell-check-buffer-on-start activate)
  (if (< (buffer-size) 40000)
      (flyspell-buffer)))

;; Use an alternative dictionary, if available (see ./SETUP-spell.sh).
;; The "sug-mode" suggested by http://emacswiki.org/emacs/InteractiveSpell

(if (file-exists-p "~/.emacs.d/aspell-huge")
    (progn
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args
            (list
             (concat "--master=" (expand-file-name "~/.emacs.d/aspell-huge"))
             " --sug-mode=ultra"))))

;; store all backup and autosave files in the tmp dir
(setq temporary-file-directory "~/.emacs.d/backups")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

(global-hl-line-mode +1)

(setq scroll-preserve-screen-position t)

(defalias 'qrr 'query-replace-regexp)

(defun insert-dunder-main ()
  (interactive)
  (insert "import argparse\nif __name__ == '__main__':\n    "))

(global-set-key [(control _)] 'insert-dunder-main)

(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'rst-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'js-mode-hook (lambda () (flyspell-prog-mode)))

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'sh-mode-hook 'turn-off-auto-fill)

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
 '(package-selected-packages
   (quote
    (ecb scala-mode2 ensime zenburn-theme undo-tree switch-window starter-kit-bindings solarized-theme rainbow-delimiters pdf-tools multiple-cursors key-chord jedi ess ein dropdown-list discover dired+ deft buffer-move))))
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
