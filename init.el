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
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t) ;; Only required if running Emacs v25.
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(smartparens
                      company ;; Completion framework
                      projectile ;; Project interaction
                      sml-mode
                      markdown-mode
                      yaml-mode
                      scss-mode
                      rainbow-mode ;; Render RGB strings with color
                      web-mode
                      solarized-theme
                      auto-complete ;; Required by ac-cider
                      cider
                      ac-cider
;;                      go-mode
;;                      magit
;;                      clj-refactor
;;                      itail                     
                      iedit
                      ace-jump-mode
                      jump-char
                      s
                      workgroups)
  "A list of packages to ensure are installed at launch.")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Modes
(cua-mode)
(rainbow-mode)
(global-hl-line-mode 1)
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq column-number-mode t)
(tool-bar-mode -1)
(setq inhibit-startup-message t
  inhibit-startup-echo-area-message t) 
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'after-init-hook 'projectile-global-mode)

;; Shortcuts
;;(global-set-key [(control \5)] (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [f1] (lambda () (interactive) (switch-to-buffer "*scratch*")))
(global-set-key [f2] (lambda () (interactive) (find-file "~/Desktop/notes.txt")))
(global-set-key [f3] 'query-replace)
(global-set-key [f4] 'projectile-grep)
(global-set-key [f5] 'projectile-find-file)
(global-set-key [f6] 'start-kbd-macro)
(global-set-key [f7] 'end-kbd-macro)
(global-set-key [f8] 'call-last-kbd-macro)
(global-set-key (kbd "<f9>") 'toggle-dark-light-theme)
(global-set-key (kbd "C-8") 'move-cursor-previous-pane)
(global-set-key (kbd "C-9") 'move-cursor-next-pane)
(global-set-key (kbd "C-0") 'ace-jump-mode)

;; Easier window navigation
(defun move-cursor-next-pane ()
  "Move cursor to the next pane."
  (interactive)
  (other-window 1))

(defun move-cursor-previous-pane ()
  "Move cursor to the previous pane."
  (interactive)
  (other-window -1))

;; Solarized theme setup. Enable toggling between light and dark themes.
;;(load-theme 'solarized-[light|dark] t)
(load-theme 'solarized-dark t)

(defun toggle-dark-light-theme ()
  (interactive)
  (let ((is-light (find 'solarized-light custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme (if is-light 'solarized-dark 'solarized-light) t)))

;; Scroll one line at a time with mouse scroll wheel, no acceleration
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

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
(when window-system (set-frame-size (selected-frame) 140 45))

;; Shorthand for questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Find emacs init file. You can also use: describe-variable user-init-file
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;; Additional package path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/modules"))

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

;; Configure Clojure
(require 'my-clojure)
(require 'setup-smartparens)

