;;; init.el --- Armin's Emacs configuration
;;

;; After loading, you can use the following to see all key bindings:
;; M-x describe-personal-keybindings

;; Proxy Authentication - Enable if you're behind an authenticated proxy.
;;(setq url-proxy-services
;;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;     ("http" . "proxy.com:8080")
;;     ("https" . "proxy.com:8080")))
;;
;;(setq url-http-proxy-basic-auth-storage
;;    (list (list "proxy.com:8080"
;;                (cons "Input your LDAP UID !"
;;                      (base64-encode-string "LOGIN:PASSWORD")))))

;; Package setup.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Always load newest compiled files.
(setq load-prefer-newer t)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB).
(setq gc-cons-threshold 50000000)

;; Warn when opening files bigger than 100MB.
(setq large-file-warning-threshold 100000000)

;; Disable the toolbar, waste of space.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable blink cursor.
(blink-cursor-mode -1)

;; Disable annoying bell ring.
(setq ring-bell-function 'ignore)

;; Misc settings.
(cua-mode)
(global-hl-line-mode 1)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq create-lockfiles nil)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Scroll one line at a time with mouse scroll wheel, no acceleration
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Mode line settings.
(line-number-mode t)
(column-number-mode t)

;; Shorthand for questions.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Set buffer title.
(setq-default frame-title-format
   (list '((buffer-file-name " %f"
             (dired-directory
              dired-directory
              (revert-buffer-function " %b"
              ("%b - Dir:  " default-directory)))))))

;; Coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; Easier window navigation
(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))

(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))

;; Theme setup.
(load-theme 'deeper-blue t)

;; Window font and size
;; Font size is in 1/10pt, so 100 will give you 10pt.
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (set-frame-size (selected-frame) 140 45)
        (if (and (> (x-display-pixel-width) 2000) (> (x-display-pixel-height) 1400))
            (set-face-attribute 'default nil :height 140) ;; Cinema Display
          (set-face-attribute 'default nil :height 105)))))

;; Fontify current and future frames
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)

;; Find emacs init file. You can also use: describe-variable user-init-file
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; Enable recentf minor mode to track recently opened files.
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Auto Completion.
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; Ido.
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Set Command key to Meta on MacOS.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))

;; Useful keybindings.
(global-set-key [f1] (lambda () (interactive) (switch-to-buffer "*tmpscratch*")))
(global-set-key [f2] (lambda () (interactive) (find-file "~/notes.txt")))
(global-set-key [f3] 'query-replace)
(global-set-key [f4] 'goto-line)
;;(global-set-key [f5] 'projectile-find-file) ; Open file within project.
;;(global-set-key (kbd "C-<f5>") 'projectile-toggle-between-implementation-and-test)
(global-set-key [f6] 'start-kbd-macro)
(global-set-key [f7] 'end-kbd-macro)
(global-set-key [f8] 'call-last-kbd-macro)
(global-set-key [f9] 'whitespace-mode)
;;(global-set-key [f12] 'dumb-jump-go) ; Jump to definition.
;;(global-set-key (kbd "C--") 'dumb-jump-back) ; Jump back from definition.
;;(global-set-key (kbd "C-0") 'ace-jump-mode)
(global-set-key (kbd "C-8") 'move-cursor-previous-pane)
(global-set-key (kbd "C-9") 'move-cursor-next-pane)
;;(global-set-key (kbd "C-S-f") 'projectile-ag) ; Search for symbol within project.

;; SCSS mode.
(use-package scss-mode
  :ensure t
  :config
  (setq scss-compile-at-save nil))

;; Render RGB strings with color.
(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  :config
  (rainbow-mode))

;; REST testing.
(use-package restclient
  :ensure t)

;; Workgroups - support for loading & saving window/buffer config.
(use-package workgroups
  :ensure t
  :config
  (unless (file-directory-p "~/.emacs_files")
    (mkdir "~/.emacs_files"))
  (setq wg-prefix-key (kbd "C-x w")
        wg-restore-associated-buffers t ; restore all buffers opened in this WG?
        wg-use-default-session-file t  ; turn off for "emacs --daemon"
        wg-default-session-file "~/.emacs_files/workgroups"
        wg-use-faces nil
        wg-morph-on nil)
  (workgroups-mode 1)
  (when (file-exists-p wg-default-session-file)
    (wg-load wg-default-session-file)))

;; Project interaction library.
(use-package projectile
  :ensure t
  :init
  (add-hook 'after-init-hook 'projectile-global-mode)
  :bind (("<f5>" . projectile-find-file)
         ("C-<f5>" . projectile-toggle-between-implementation-and-test)
         ("C-S-f" . projectile-ag)))

;; Dumb-jump jumps to definition of symbols.
(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-force-searcher 'ag)
  :bind (("<f12>" . dumb-jump-go)
         ("C--" . dumb-jump-back)))

;; Completion framework.
(use-package company
  :ensure t
  :config
  (global-company-mode))

;; Fast cursor movement.
(use-package ace-jump-mode
  :ensure t
  :bind ("C-0" . ace-jump-mode))

;; Configure Ag. Ensure you have the silversearcher-ag package installed.
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers 't)
  (setq ag-arguments (list "--smart-case" "--column")))

;; Configure org-present for presentation of slides using Emacs.
;; M-x org-present
;; C-c C-q    (org-present-quit)
(use-package org-present
  :ensure t
  :init
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

;; Make Emacs use the $PATH set up by the user's shell.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

;; Syntax checking.
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))

;; Provides a REPL buffer connected to a Ruby subprocess.
;; C-x C-q : Switch the compilation buffer mode for interactive debugging
(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(use-package sml-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package json-reformat
  :ensure t)

(use-package web-mode
  :ensure t)

;; Enable iedit minor mode - allows editing of one occurance of some text in buffer.
;; C-; search/replace
(use-package iedit
  :ensure t)

(use-package jump-char
  :ensure t)

(use-package s
  :ensure t)

(use-package haml-mode
  :ensure t)

(use-package rspec-mode
  :ensure t)

(use-package smartparens
  :ensure t)

;; Clojure IDE that rocks.
(use-package cider
  :pin melpa-stable
  :ensure t)

;; Intelligent auto-completion extension for Emacs. Required by ac-cider.
(use-package auto-complete
  :ensure t
  :config
  (ac-config-default))

;; Auto-complete backend for CIDER.
(use-package ac-cider
  :ensure t)

;; Collection of Clojure refactoring functions.
(use-package clj-refactor
  :pin melpa-stable
  :ensure t)

;; --

(use-package my-clojure
  :load-path "modules/"
  :defer 2)

(use-package my-smartparens
  :load-path "modules/"
  :defer 3)

;; EOF
