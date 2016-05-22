
;; Appearance and behaviour

(menu-bar-mode 1)
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
(global-set-key "\C-c\C-j" 'switch-buffer-immediate)
(global-set-key "\C-x\C-o" 'other-window)
(global-set-key "\C-c\C-o" 'other-window)
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
(setq show-trailing-whitespace t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages setup
;; Note; we're managing initialisation ourselves so we can do
;; everything in init.el

(require 'package)
(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ;("melpa"        . "http://melpa.org/packages/")
        ("org"          . "http://orgmode.org/elpa/")))

(setq package-enable-at-startup nil)
(package-initialize)

(defvar my-packages '(exec-path-from-shell
                      company
                      projectile
                      grizzl
                      flx-ido
		      magit
		      clojure-mode
		      midje-mode
		      rainbow-delimiters
		      paredit
		      paredit-menu
                      yasnippet
                      web-mode
		      jinja2-mode
                      less-css-mode
		      undo-tree
                      json-mode
		      mustache-mode
		      puppet-mode
		      yaml-mode
		      lua-mode
                      go-mode
                      company-go
                      anaconda-mode
;                      company-anaconda
                      toml-mode
		      popup
		      color-theme-solarized
		      hide-lines
		      wc-mode
                      ssh-file-modes
                      dockerfile-mode
                      haskell-mode
                      ghc
                      coffee-mode
                      js2-mode))

;;; auto-complete only seems to work as a manual install, however that
;;; manual install relies on popup being available

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

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
(setq save-place-file "~/.emacs.d/saved-places")


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

;; Cleanups

(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)))

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
(require 'flx-ido)
(flx-ido-mode 1)


;; (require 'helm-config)
;; (helm-mode 1)


;; Yasnippet mode
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets" t)
;(yas-global-mode 1)
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

;; Projectile
(setq projectile-completion-system 'grizzl)
(projectile-global-mode)


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

;; Javascript
(setq js-indent-level 2)

;; JS2 Mode
;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Web-mode
(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.us\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.selmer\\'" . web-mode))
(setq web-mode-engines-alist
      '(("underscore"  . "\\.us\\'")
        ("django"  . "\\.selmer\\'")))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))

(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)


;; Clojure setup
(require 'clojure-mode)
(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (enable-paredit-mode)
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c C-r")))

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
;(add-hook 'before-save-hook #'gofmt-before-save)

(require 'company-go)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          (local-set-key  "\M-." 'godef-jump)))

;; Haskell setup
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; Rust setup
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(setq racer-rust-src-path "~/software/rust/src/")
(setq racer-cmd "~/software/racer/target/release/racer")
;(require 'rust-mode)
(add-hook 'rust-mode-hook (lambda ()
                                        ;(modify-syntax-entry ?_ "_" rust-mode-syntax-table)
                            (require 'racer)
                            (racer-mode)
                            (setq-local company-idle-delay 0.5)
                            (define-key rust-mode-map (kbd "TAB") #'racer-complete-or-indent)
                            (define-key rust-mode-map (kbd "M-.") #'racer-find-definition)
                            (setq company-tooltip-align-annotations t)))

(add-hook 'racer-mode-hook (lambda ()
                             (company-mode)))

;; Jinja2
;; (require 'jinja2-mode)
;; (add-to-list 'auto-mode-alist '("\\.j2\\'" . jinja2-mode))


;; Gnus
(setq gnus-select-method '(nntp "news.eu.supernews.com"))

;; Markdown
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(require 'wc-mode)
(add-hook 'markdown-mode-hook (lambda ()
                                (wc-mode)
                                (auto-fill-mode)
                                (set-fill-column 80)
                                (flyspell-mode)))

;; Docker support
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(safe-local-variable-values
   (quote
    ((web-mode-content-types-alist
      ("jsx" . ".*\\.js[x]?\\'"))
     (eval add-to-list
           (quote auto-mode-alist)
           (quote
            ("\\.js\\'" . web-mode)))
     (whitespace-line-column . 80)
     (lexical-binding . t))))
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visible-bell nil))

(when (not (null window-system))
  (setq proggy (if (eq window-system 'x) "ProggyCleanTT" "ProggyClean"))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   `(default ((t (:inherit nil :stipple nil :background "DarkSlateGrey" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family ,proggy))))
   '(highlight ((t (:background "#244444"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "DarkSlateGrey" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "ProggyClean"))))
 '(company-scrollbar-bg ((t (:background "#1b2f2f"))))
 '(company-scrollbar-fg ((t (:background "#253f3f"))))
 '(company-tooltip ((t (:inherit default :background "#121f1f"))))
 '(company-tooltip-common ((t (:inherit font-lock-doc-face :background "DarkBlue"))))
 '(company-tooltip-selection ((t (:inherit font-lock-comment-face :background "MidnightBlue"))))
 '(highlight ((t (:background "#244444")))))
