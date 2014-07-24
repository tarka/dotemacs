
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


;; Keys
(defun switch-buffer-immediate ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(if mac-p
    (setq mac-command-modifier 'meta))

(define-key function-key-map [delete] "\C-d")
(global-set-key [?\C-`] 'next-error)
(global-set-key [?\C-,] 'call-last-kbd-macro)
(global-set-key "\M-m" 'set-mark-command)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-j" 'switch-buffer-immediate)
(global-set-key "\M-i" 'yas/insert-snippet)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages setup

(require 'package)
(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("melpa"        . "http://melpa.milkbox.net/packages/")
        ("org"          . "http://orgmode.org/elpa/")))
(package-initialize)

(defvar my-packages '(exec-path-from-shell
		      magit
		      yasnippet
		      clojure-snippets
		      clojure-mode
		      cider ac-nrepl
		      rainbow-delimiters
		      paredit
		      markdown-mode
		      jinja2-mode
		      undo-tree
		      auto-complete
		      mustache-mode
		      puppet-mode
		      yaml-mode
		      lua-mode
		      rust-mode
		      jedi
		      popup
		      color-theme-solarized
		      hide-lines))

;;; auto-complete only seems to work as a manual install, however that
;;; manual install relies on popup being available
 
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(add-to-list 'load-path "~/.emacs.d/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc mode setup

(if mac-p
    (exec-path-from-shell-initialize))

(require 'saveplace)
(setq-default save-place t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'recentf)
(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(require 'auto-complete-config)
(ac-config-default)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))


;; Colour match parens and other structure characters to make code
;; easy to follow
(global-rainbow-delimiters-mode)

;;; Enable undo-tree for everything, so you can M - _ to redo
(global-undo-tree-mode)

;; Interactive-do mode, mostly for file find
(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always)

;; Paredit mode
(require 'paredit)
(define-key paredit-mode-map (kbd "<C-right>") nil)
(define-key paredit-mode-map (kbd "<C-left>") nil)

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)


;; Python setup
;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")

(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)

;; Clojure setup
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(require 'ac-nrepl)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))


;; Yasnippet
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets" t)
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt
			     yas-dropdown-prompt
			     yas-x-prompt
			     yas-completing-prompt
			     yas-no-prompt))

;; Jinja2
(require 'jinja2-mode)
(add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))

;; Gnus
(setq gnus-select-method '(nntp "news.eu.supernews.com"))



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
   `(default ((t (:inherit nil :stipple nil :background "DarkSlateGrey" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family ,proggy))))
   '(highlight ((t (:background "#244444"))))))
