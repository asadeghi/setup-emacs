;;; Smartparens
;; Original Smartparens author's setup can be found here:
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el
;;
(require 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; Add smartparens-strict-mode to all sp--lisp-modes hooks. C-h v sp--lisp-modes
;; to customize/view this list.
(mapc (lambda (mode)
        (add-hook (intern (format "%s-hook" (symbol-name mode))) 'smartparens-strict-mode))
      sp--lisp-modes)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management
;;
;; Conveniently set keys into the sp-keymap, limiting the keybinding to buffers
;; with SP mode activated
(mapc (lambda (info)
        (let ((key (kbd (car info)))
              (function (car (cdr info))))
          (define-key sp-keymap key function)))
      '(("C-M-f" sp-forward-sexp)
        ("C-M-b" sp-backward-sexp)

        ("C-M-d" sp-down-sexp)
        ("C-M-a" sp-backward-down-sexp)
        ("C-S-a" sp-beginning-of-sexp)
        ("C-S-d" sp-end-of-sexp)

        ("C-M-e" sp-up-sexp)

        ("C-M-u" sp-backward-up-sexp)
        ("C-M-t" sp-transpose-sexp)

        ("C-M-n" sp-next-sexp)
        ("C-M-p" sp-previous-sexp)

        ("C-M-k" sp-kill-sexp)
        ("C-M-w" sp-copy-sexp)

        ("M-<delete>" sp-unwrap-sexp)
        ("M-<backspace>" sp-backward-unwrap-sexp)

        ("C-<right>" sp-forward-slurp-sexp)
        ("C-<left>" sp-forward-barf-sexp)
        ("C-M-<left>" sp-backward-slurp-sexp)
        ("C-M-<right>" sp-backward-barf-sexp)

        ("M-D" sp-splice-sexp)
        ("C-M-<delete>" sp-splice-sexp-killing-forward)
        ("C-M-<backspace>" sp-splice-sexp-killing-backward)
        ("C-S-<backspace>" sp-splice-sexp-killing-around)

        ("C-]" sp-select-next-thing-exchange)
        ("C-<left_bracket>" sp-select-previous-thing)
        ("C-M-]" sp-select-next-thing)

        ("M-F" sp-forward-symbol)
        ("M-B" sp-backward-symbol)

        ("H-t" sp-prefix-tag-object)
        ("H-p" sp-prefix-pair-object)
        ("H-s c" sp-convolute-sexp)
        ("H-s a" sp-absorb-sexp)
        ("H-s e" sp-emit-sexp)
        ("H-s p" sp-add-to-previous-sexp)
        ("H-s n" sp-add-to-next-sexp)
        ("H-s j" sp-join-sexp)
        ("H-s s" sp-split-sexp)

        ("M-q" sp-reindent-defun)))

;;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :wrap "C-*" :skip-match 'sp--gfm-skip-asterisk)
  (sp-local-pair "_" "_" :wrap "C-_")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

(defun sp--gfm-skip-asterisk (ms mb me)
  (save-excursion
    (goto-char mb)
    (save-match-data (looking-at "^\\* "))))

;;; org-mode
(sp-with-modes 'org-mode
  (sp-local-pair "*" "*" :actions '(insert wrap) :unless '(sp-point-after-word-p sp-point-at-bol-p) :wrap "C-*" :skip-match 'sp--org-skip-asterisk)
  (sp-local-pair "_" "_" :unless '(sp-point-after-word-p) :wrap "C-_")
  (sp-local-pair "/" "/" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "~" "~" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "=" "=" :unless '(sp-point-after-word-p) :post-handlers '(("[d1]" "SPC")))
  (sp-local-pair "«" "»"))

(defun sp--org-skip-asterisk (ms mb me)
  (or (and (= (line-beginning-position) mb)
           (eq 32 (char-after (1+ mb))))
      (and (= (1+ (line-beginning-position)) me)
           (eq 32 (char-after me)))))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil
                 :wrap "C-("
                 :pre-handlers '(my-add-space-before-sexp-insertion)
                 :post-handlers '(my-add-space-after-sexp-insertion)))

(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun my-add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (and (looking-back (sp--get-closing-regexp))
                     (not (eq (char-syntax (preceding-char)) ?'))))
        (insert " ")))))

;;; C++
(sp-with-modes '(malabar-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))
(sp-local-pair 'c++-mode "/*" "*/" :post-handlers '((" | " "SPC")
                                                    ("* ||\n[i]" "RET")))

;;; php-mode
(sp-with-modes '(php-mode)
  (sp-local-pair "/**" "*/" :post-handlers '(("| " "SPC")
                                             (my-php-handle-docstring "RET")))
  (sp-local-pair "/*." ".*/" :post-handlers '(("| " "SPC")))
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "(" nil :prefix "\\(\\sw\\|\\s_\\)*"))

(defun my-php-handle-docstring (&rest _ignored)
  (-when-let (line (save-excursion
                     (forward-line)
                     (thing-at-point 'line)))
    (cond
     ((string-match-p "function" line)
      (save-excursion
        (insert "\n")
        (let ((args (save-excursion
                      (forward-line)
                      (my-php-get-function-args))))
          (--each args
            (insert (format "* @param %s\n" it)))))
      (insert "* "))
     ((string-match-p ".*class\\|interface" line)
      (save-excursion (insert "\n*\n* @author\n"))
      (insert "* ")))
    (let ((o (sp--get-active-overlay)))
      (indent-region (overlay-start o) (overlay-end o)))))

;;; Alternative implementation of paredit-reindent-defun. Similar to indent-region.
;;; Credit to https://github.com/benswift/.dotfiles/blob/master/init.el
(defun sp-reindent-defun (&optional argument)
  "Reindent the definition that the point is on.
If the point is in a string or a comment, fill the paragraph
instead, and with a prefix argument, justify as well."
  (interactive "P")
  (if (or (sp-point-in-string)
          (sp-point-in-comment))
      (lisp-fill-paragraph argument)
    (save-excursion
      (end-of-defun)
      (beginning-of-defun)
      (indent-sexp))))

(provide 'my-smartparens)
