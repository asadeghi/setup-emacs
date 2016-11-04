;;; init.el --- Armin's Emacs configuration
;;

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

;; Install packages
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)

;; Pinned packages require Emacs 24.4+ to work.
(setq package-pinned-packages '((cider        . "melpa-stable")
                                (clj-refactor . "melpa-stable")))

(package-initialize)
;; Update package archive if required.
(when (not package-archive-contents)
  (package-refresh-contents))
 
(defvar my-packages '(use-package
                      smartparens
                      company ;; Completion framework
                      projectile ;; Project interaction
                      ag ;; Silver-Searcher-Ag
                      sml-mode
                      markdown-mode
                      yaml-mode
                      scss-mode
                      json-reformat
                      rainbow-mode ;; Render RGB strings with color
                      web-mode
                      auto-complete ;; Required by ac-cider
                      cider
                      ac-cider
                      clj-refactor
                      restclient
                      iedit
                      ace-jump-mode
                      jump-char
                      s
                      workgroups
                      org-present
                      exec-path-from-shell
                      dumb-jump
                      haml-mode)
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Additional package path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules"))

;; Always load newest compiled files.
(setq load-prefer-newer t)

;; Reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; Warn when opening files bigger than 100MB
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
(rainbow-mode)
(global-hl-line-mode 1)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq create-lockfiles nil)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(add-hook 'after-init-hook 'projectile-global-mode)

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

;; Disable SASS auto-compilation, and integrate with rainbow-mode.
(setq scss-compile-at-save nil)
(add-hook 'css-mode-hook 'rainbow-mode)

;; Window font and size
;; Font size is in 1/10pt, so 100 will give you 10pt.
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (set-frame-size (selected-frame) 140 45)
        (if (and (> (x-display-pixel-width) 2000) (> (x-display-pixel-height) 1400))
            (set-face-attribute 'default nil :height 150) ;; Cinema Display
          (set-face-attribute 'default nil :height 110)))))

;; Fontify current and future frames
(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)

;; Find emacs init file. You can also use: describe-variable user-init-file
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; VB.NET Mode
(autoload 'vbnet-mode "vbnet-mode.el" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                              vbnet-mode)) auto-mode-alist))

;; Enable recentf minor mode to track recently opened files.
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Enable iedit minor mode - allows editing of one occurance of some text in buffer.
(require 'iedit) ;; C-; search/replace

;; Support for loading & saving window/buffer config
(require 'workgroups)
(setq wg-prefix-key (kbd "C-x w")
      wg-restore-associated-buffers t ; restore all buffers opened in this WG?
      wg-use-default-session-file t   ; turn off for "emacs --daemon"
      wg-default-session-file "~/.emacs_files/workgroups"
      wg-use-faces nil
      wg-morph-on nil)

(workgroups-mode 1)     ; Activate workgroups
(unless (file-directory-p "~/.emacs_files")
  (mkdir "~/.emacs_files"))

(when (file-exists-p wg-default-session-file)
  (wg-load wg-default-session-file))

;; Auto Completion
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Configure Auto-Complete
;;(require 'auto-complete-config)
(ac-config-default)
;;(setq ac-ignore-case nil)
;;(add-to-list 'ac-modes 'enh-ruby-mode)
;;(add-to-list 'ac-modes 'web-mode)

;; Set Command key to Meta on MacOS.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'meta))

;; Useful keybindings
(global-set-key [f1] (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [f2] (lambda () (interactive) (find-file "~/notes.txt")))
(global-set-key [f3] 'query-replace)
(global-set-key [f4] 'goto-line)
(global-set-key [f5] 'projectile-find-file) ; Open file within project.
(global-set-key [f6] 'start-kbd-macro)
(global-set-key [f7] 'end-kbd-macro)
(global-set-key [f8] 'call-last-kbd-macro)
(global-set-key [f9] 'whitespace-mode)
(global-set-key [f12] 'dumb-jump-go) ; Jump to definition.
(global-set-key (kbd "C-<f12>") 'dumb-jump-back) ; Jump back from definition.
(global-set-key (kbd "C-8") 'move-cursor-previous-pane)
(global-set-key (kbd "C-9") 'move-cursor-next-pane)
;(global-set-key (kbd "C-0") 'ace-jump-mode)
(global-set-key (kbd "C-S-f") 'projectile-ag) ; Search for symbol within project.

; Dumb-jump jumps to definition of symbols: C-M g , and back: C-M p
(use-package dumb-jump
             :defer 4
             :config
             (dumb-jump-mode))

(use-package company
             :defer 3
             :config
             (global-company-mode))

(use-package ace-jump-mode
             :defer t
             :bind ("C-0" . ace-jump-mode))

;; Configure Ag. Ensure you have the silversearcher-ag package installed.
(use-package ag
             :defer t
             :config
             (setq ag-highlight-search t)
             (setq ag-reuse-buffers 't)
             (setq ag-arguments (list "--smart-case" "--column"))
             )

;; Configure org-present for presentation of slides using Emacs.
;; M-x org-present
;; C-c C-q (org-present-quit)
(use-package org-present
             :defer 3
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

(use-package exec-path-from-shell
             :defer 1
             :config (when (memq window-system '(mac ns))
                           (exec-path-from-shell-initialize)))

(use-package my-clojure
             :defer 1)

(use-package setup-smartparens
             :defer 1)

;; EOF
