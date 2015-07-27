
;; Appearance and behaviour

;(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(setq inhibit-startup-screen t)

(defvar mac-p (string-equal system-type "darwin"))


(when (not (null window-system))
  (set-background-color "DarkSlateGrey")
  (set-foreground-color "White")
  (setq frame-background-mode 'dark)
;  (set-frame-font "-windows-proggy clean-medium-r-normal-sans-13-80-96-96-c-70-iso8859-1")
  (setq font-lock-comment-face '(:foreground "Pink"))
  (global-set-key [?\C-z] 'undo))

(setq mac-allow-anti-aliasing nil)

;; Keys
(defun switch-buffer-immediate ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(when mac-p
  (setq mac-command-modifier 'meta)
  (setq mac-allow-anti-aliasing nil))

(define-key function-key-map [delete] "\C-d")
(global-set-key [?\C-`] 'next-error)
(global-set-key [?\C-,] 'call-last-kbd-macro)
(global-set-key "\M-m" 'set-mark-command)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-j" 'switch-buffer-immediate)
(global-set-key "\C-\M-y" 'clipboard-yank)
(global-set-key "\C-\M-w" 'clipboard-kill-ring-save)


(global-set-key (kbd "C-S-j") (lambda () ;; Join-forward Intellij-style
				(interactive)
				(join-line 1)))

;; Default modes
(show-paren-mode)
(global-font-lock-mode t)
(auto-compression-mode t)
(add-to-list 'completion-ignored-extensions ".svn/")

(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-mode))

(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages setup
;; Note; we're managing initialisation ourselves so we can do
;; everything in init.el

(require 'package)
(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
;        ("melpa"        . "http://melpa.milkbox.net/packages/")
        ("org"          . "http://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)

(defvar my-packages '(exec-path-from-shell
                      company
		      magit
		      clojure-mode
		      cider
		      midje-mode
		      rainbow-delimiters
		      paredit
		      paredit-menu
                      yasnippet
		      markdown-mode
                      web-mode
		      jinja2-mode
		      undo-tree
		      mustache-mode
		      puppet-mode
		      yaml-mode
		      lua-mode
                      go-mode
                      company-go
                      anaconda-mode
;                      company-anaconda
		      rust-mode
                      toml-mode
		      popup
		      color-theme-solarized
		      hide-lines
		      wc-mode
                      ssh-file-modes))

;;; auto-complete only seems to work as a manual install, however that
;;; manual install relies on popup being available
 
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc mode setup

(setq default-directory (expand-file-name "~"))

(if mac-p
    (exec-path-from-shell-initialize))

;; Misc. editing/saving settings

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)

(require 'saveplace)
(setq-default save-place t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;(require 'auto-complete-config)
;(ac-config-default)

;(require 'yaml-mode)
;(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))


;; Colour match parens and other structure characters to make code
;; easy to follow
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
;(require 'rainbow-delimiters)
;(global-rainbow-delimiters-mode)

;;; Enable undo-tree for everything, so you can M - _ to redo
(global-undo-tree-mode)

;; Interactive-do mode, mostly for file find
(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-auto-merge-work-directories-length -1
      ido-file-extensions-order '(".md" ".txt" ".py" ".xml" ".el" ".ini" ".cfg" ".cnf"))


;; (require 'helm-config)
;; (helm-mode 1)


;; Yasnippet mode
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets" t)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-dropdown-prompt
                             yas-x-prompt
                             yas-completing-prompt
                             yas-no-prompt))
(global-set-key "\M-i" 'yas/insert-snippet)


;; Paredit mode
(require 'paredit)
(require 'paredit-menu)
(define-key paredit-mode-map (kbd "<C-right>") nil)
(define-key paredit-mode-map (kbd "<C-left>") nil)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;; Company mode auto-completion setup
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 15)))))
   `(company-scrollbar-bg ((t (:background ,(color-darken-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-darken-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-comment-face :background "MidnightBlue"))))
   `(company-tooltip-common ((t (:inherit font-lock-doc-face :background "DarkBlue"))))))

;; Python setup
;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")

;(add-hook 'python-mode-hook 'jedi:setup)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-to-list 'company-backends 'company-anaconda)


;; Web-mode
(add-to-list 'auto-mode-alist '("\\.us\\'" . web-mode))
(setq web-mode-engines-alist
      '(("underscore"  . "\\.us\\.")))



;; Clojure setup
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(setq cider-prompt-for-symbol nil)

;; (require 'ac-nrepl)
;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'cider-repl-mode))


;; Go setup
;;
;;    go get -v -u code.google.com/p/rog-go/exp/cmd/godef
;;    go get -v -u github.com/nsf/gocode
;;    go get -v -u golang.org/x/tools/cmd/goimports

(setq gofmt-command "goimports")
(add-hook 'before-save-hook #'gofmt-before-save)

(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (local-set-key  "\M-." 'godef-jump)))

;; Rust setup
(setq racer-rust-src-path "~/software/rust/src/")
(setq racer-cmd "~/software/racer/target/release/racer")
(add-to-list 'load-path "~/software/racer/editors/emacs")
(require 'rust-mode)
(add-hook 'rust-mode-hook (lambda ()
                            ;(modify-syntax-entry ?_ "_" rust-mode-syntax-table)
                            (require 'racer)
                            (racer-activate)
                            (setq-local company-idle-delay 0.5)
                            (define-key rust-mode-map (kbd "TAB") #'racer-complete-or-indent)
                            (define-key rust-mode-map (kbd "M-.") #'racer-find-definition)))


;; Jinja2
;; (require 'jinja2-mode)
;; (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))


;; Gnus
(setq gnus-select-method '(nntp "news.eu.supernews.com"))


(require 'wc-mode)
(add-hook 'markdown-mode-hook 'wc-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(when (not (null window-system))
  (setq proggy (if (eq window-system 'x) "ProggyCleanTT" "ProggyClean"))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "DarkSlateGrey" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 72 :width normal :foundry "unknown" :family "Input Mono"))))
   '(highlight ((t (:background "#244444"))))))
