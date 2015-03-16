;; Proxy Authentication - Enable if required
;;(setq url-proxy-services
;;   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;     ("http" . "proxy.com:8080")
;;     ("https" . "proxy.com:8080")))
;;
;;(setq url-http-proxy-basic-auth-storage
;;    (list (list "proxy.com:8080"
;;                (cons "Input your LDAP UID !"
;;                      (base64-encode-string "LOGIN:PASSWORD")))))

;; Add package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(smartparens company
                      projectile
                      sml-mode
                      markdown-mode yaml-mode
                      scss-mode rainbow-mode web-mode
                      ace-jump-mode
                      solarized-theme
                      go-mode 
                      cider)
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Modes
(cua-mode)
(global-hl-line-mode 1)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq column-number-mode t)
(tool-bar-mode -1)
;;(load-theme 'solarized-[light|dark] t)
(load-theme 'solarized-light t)
(setq inhibit-startup-message t
  inhibit-startup-echo-area-message t) 
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'projectile-global-mode)

;; Shortcuts
;;(global-set-key [(control \5)] (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [f1] (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [f5] 'projectile-find-file)
(global-set-key [f7] 'start-kbd-macro)
(global-set-key [f8] 'end-kbd-macro)
(global-set-key [f9] 'call-last-kbd-macro)
(define-key global-map (kbd "C-0") 'ace-jump-mode)

;; Scroll one line at a time with mouse scroll wheel, no acceleration
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Tramp setup
;; C-x C-f /username@remoteserver:/path/to/file
(setq tramp-default-method "ssh")
;;(setq tramp-debug-buffer t)
;;(setq tramp-verbose 10)

;; Disable SASS auto-compilation
(setq scss-compile-at-save nil)

;; Set buffer title
(setq-default frame-title-format
   (list '((buffer-file-name " %f"
             (dired-directory
              dired-directory
              (revert-buffer-function " %b"
              ("%b - Dir:  " default-directory))))))) 

;; Window size
(when window-system (set-frame-size (selected-frame) 140 47))

;; -- Find emacs init file. You can also use: describe-variable user-init-file
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; -- Additional packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; -- VB.NET Mode
(autoload 'vbnet-mode "vbnet-mode.el" "Mode for editing VB.NET code." t)
(setq auto-mode-alist (append '(("\\.\\(frm\\|bas\\|cls\\|vb\\)$" .
                              vbnet-mode)) auto-mode-alist))

;; -- Smartparens setup
(require `smartparens-config)
(smartparens-global-mode)

;; -- Cider (clojure-emacs) setup
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq nrepl-hide-special-buffers t)
(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
