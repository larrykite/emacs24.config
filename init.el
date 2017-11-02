(setq lmk-emacs-init-file load-file-name)
(setq lmk-emacs-config-dir
      (file-name-directory lmk-emacs-init-file))

;; Set up 'custom' system
(setq custom-file (expand-file-name "emacs-customizations.el" lmk-emacs-config-dir))
(setq functions-file (expand-file-name "functions.el" lmk-emacs-config-dir))
(setq keybinding-file (expand-file-name "keybindings.el" lmk-emacs-config-dir))
(load custom-file)
(load functions-file)
(load keybinding-file)

(setq user-emacs-directory lmk-emacs-config-dir)

(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))

(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(package-initialize)

(defvar local-packages '(
                         auto-complete
                         avy
                         better-defaults
                         bind-key
                         buffer-move
                         dash
                         deft
                         dired+
                         discover
                         dropdown-list
                         ein
                         elpy
                         ess
                         flycheck
                         idle-highlight-mode
                         ido-ubiquitous
                         jedi
                         key-chord
                         leuven-theme
                         magit
                         multiple-cursors
                         naquadah-theme
                         paradox
                         projectile
                         rainbow-delimiters
                         smartscan
                         smex
                         solarized-theme
                         switch-window
                         sx
                         undo-tree
                         use-package
                         which-key
                         zenburn-theme
                         exec-path-from-shell
                         )
  "A list of packages to ensure are installed at launch.")

(fset 'yes-or-no-p 'y-or-n-p)

(defun uninstalled-packages (packages)
  (delq nil
        (mapcar (lambda (p) (if (package-installed-p p nil) nil p)) packages)))

;; This delightful bit adapted from:
;; http://batsov.com/articles/2012/02/19/package-management-in-emacs-the-good-the-bad-and-the-ugly/

(let ((need-to-install (uninstalled-packages local-packages)))
  (when need-to-install
    (progn
      (package-refresh-contents)
      (dolist (p need-to-install)
        (package-install p)))))


(elpy-enable)
(load-theme 'naquadah t)
;; (require 'ess-site)
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode)
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(ido-mode 1)
(ido-ubiquitous-mode)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)

;; (global-flycheck-mode)

(global-hl-line-mode +1)

(setq scroll-preserve-screen-position t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'sh-mode-hook 'turn-off-auto-fill)

(if (display-graphic-p)
    (set-frame-to-my-size))
;; Show the current function name in the header line
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      ;; We remove Which Function Mode from the mode line, because it's mostly
      ;; invisible here anyway.
      (assq-delete-all 'which-func-mode mode-line-misc-info))

(setq column-number-mode t)
(which-key-mode)


(key-chord-define-global "KE" 'kill-emacs)
(key-chord-define-global "FF" 'find-file)
(key-chord-define-global "jk" 'beginning-of-buffer)

(key-chord-mode +1)



;; Global Jedi config vars

(defvar jedi-config:use-system-python nil)
"Will use system python and active environment for Jedi server.
May be necessary for some GUI environments (e.g., Mac OS X)"

(defvar jedi-config:with-virtualenv "mypythonenv"
  "Set to non-nil to point to a particular virtualenv.")

(defvar jedi-config:vcs-root-sentinel ".git")

(defvar jedi-config:python-module-sentinel "__init__.py")

(setq paradox-github-token "437628cb8c9f5b064a1ca3b83eacaea7378304a5")

;; Small helper to scrape text from shell output
(defun get-shell-output (cmd)
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

;; Ensure that PATH is taken from shell
;; Necessary on some environments without virtualenv
;; Taken from: http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable

(exec-path-from-shell-initialize)

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell."
  (interactive)
  (let ((path-from-shell (get-shell-output "$SHELL --login -i -c 'echo $PATH'")))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defalias 'qrr 'query-replace-regexp)
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))


;; ugly hack to remove "." from load-path. Can't figure out where it's
;; being added.

(setq load-path (remove "." load-path))
;; (eval-after-load "python"
;;  '(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
;; (add-hook 'jedi-mode-hook 'jedi-direx:setup)
(setq magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-w") 'ace-window)

;; nearly all of this is the default layout
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 30 30 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))


(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)
(delete-selection-mode t)
(global-auto-revert-mode t)
(defalias 'workon 'pyvenv-workon)

(provide 'init)
;;; init.el ends here
