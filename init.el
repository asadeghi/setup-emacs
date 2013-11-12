(require 'package)

;; Add package repos
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(auto-complete smartparens
                      helm helm-ls-git
                      clojure-mode clojure-test-mode
                      sml-mode scala-mode
                      markdown-mode yaml-mode
                      scss-mode rainbow-mode web-mode
                      solarized-theme)
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
;;(helm-mode 1)
;;(load-theme 'solarized-[light|dark] t)
(load-theme 'solarized-light t)
(setq inhibit-startup-message t
  inhibit-startup-echo-area-message t) 

;; Shortcuts
;;(global-set-key [(control \5)] (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [f1] (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [f5] 'helm-mini)
(global-set-key [f7] 'start-kbd-macro)
(global-set-key [f8] 'end-kbd-macro)
(global-set-key [f9] 'call-last-kbd-macro)

;; Scroll one line at a time with mouse scroll wheel, no acceleration
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

;; Set buffer title
(setq-default frame-title-format
   (list '((buffer-file-name " %f"
             (dired-directory
              dired-directory
              (revert-buffer-function " %b"
              ("%b - Dir:  " default-directory))))))) 

;; Window size
(when window-system (set-frame-size (selected-frame) 140 47))

;; Redefine helm-mini
(require 'helm-config)
(require 'helm-ls-git)

(defun helm-mini ()
  "My additions (all git file) to Preconfigured `helm' lightweight version \(buffer -> recentf\)."
  (interactive)
  (require 'helm-files)
  (helm-other-buffer '(
    helm-c-source-buffers-list
    helm-c-source-recentf
    helm-c-source-ls-git)
  "*helm mini*"))
