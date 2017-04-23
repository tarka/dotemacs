;; Appearance and behaviour

(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(setq inhibit-startup-screen t)

(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(server-start)


(defvar mac-p (string-equal system-type "darwin"))

(when (not (null window-system))
  (set-background-color "DarkSlateGrey")
  (set-foreground-color "White")
  (setq frame-background-mode 'dark)
  (set-frame-font "-*-Scr11-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
  (setq font-lock-comment-face '(:foreground "Pink"))
  (global-set-key [?\C-z] 'undo))

(setq mac-allow-anti-aliasing nil)

(defun monitor-mode ()
  (interactive)
  (set-frame-font "-unknown-ProggyCleanTTSZBP-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1" nil t))
(defun lcd-mode ()
  (interactive)
  (set-frame-font "-adobe-Source Code Pro-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1" nil t))
(defun lcd2-mode ()
  (interactive)
  (set-frame-font "-adobe-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1" nil t))

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
(global-set-key "\M-[" 'backward-paragraph)
(global-set-key "\M-]" 'forward-paragraph)
(global-set-key "\M-p" 'backward-paragraph)
(global-set-key "\M-n" 'forward-paragraph)

(global-set-key (kbd "C-S-j") (lambda () ;; Join-forward Intellij-style
				(interactive)
				(join-line 1)))


;; Default modes
(show-paren-mode)
(global-font-lock-mode t)
(auto-compression-mode t)
(add-to-list 'completion-ignored-extensions ".svn/")

(setq-default indent-tabs-mode nil)
(setq show-trailing-whitespace t)

(global-set-key (kbd "<f12>") 'compile)
(setq compilation-window-height 30)

(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)))

(setq js-indent-level 2)

(setq ispell-program-name "hunspell")


;; Misc. editing/saving settings

(setq default-directory (expand-file-name "~"))
(setq require-final-newline t)

(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages setup

(require 'package)
(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("marmalade"    . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

; Manual install packages
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package setup

(setq use-package-always-ensure t)
(setq use-package-verbose t)
(setq use-package-always-pin "melpa-stable")


(use-package exec-path-from-shell
  :if mac-p
  :config (exec-path-from-shell-initialize))


(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))


(use-package uniquify
  :ensure f
  :config
  (setq uniquify-buffer-name-style 'forward))


(use-package recentf
  :config
  (recentf-mode 1)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files))


(use-package magit)


(use-package json-mode
  :mode "\\.json\\'")


(use-package yaml-mode
  :mode ("\\.yml$"
         "\\.yaml$"))


(use-package toml-mode
  :pin melpa
  :mode "\\.toml\'")


(use-package ssh-file-modes
  :pin marmalade
  :mode (".ssh/authorized_keys2?\\'"
         ".ssh/known_hosts\\'"
         "ssh_known_hosts\\'"))


(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


(use-package string-inflection)


(use-package undo-tree
  :pin melpa
  :config
  (global-undo-tree-mode))


(use-package ido
  :config
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-auto-merge-work-directories-length -1
        ido-file-extensions-order '(".md" ".txt" ".py" ".xml" ".el" ".ini" ".cfg" ".cnf")))

(use-package flx-ido
  :config
  (flx-ido-mode 1))


(use-package paredit
  :pin melpa
  :config
  (define-key paredit-mode-map (kbd "<C-right>") nil)
  (define-key paredit-mode-map (kbd "<C-left>") nil)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))
(use-package paredit-menu
  :pin melpa)


(use-package projectile
  :config
  (setq projectile-completion-system 'grizzl)
  (projectile-global-mode))

(use-package grizzl
  :pin melpa)


(use-package popup)
(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))


(use-package color
  :config
  (let ((bg (face-attribute 'default :background)))
    (custom-set-faces
     `(company-tooltip ((t (:inherit default :background ,(color-darken-name bg 15)))))
     `(company-scrollbar-bg ((t (:background ,(color-darken-name bg 10)))))
     `(company-scrollbar-fg ((t (:background ,(color-darken-name bg 5)))))
     `(company-tooltip-selection ((t (:inherit font-lock-comment-face :background "MidnightBlue"))))
     `(company-tooltip-common ((t (:inherit font-lock-doc-face :background "DarkBlue")))))))


(use-package web-mode
  :mode ("\\.html?\\'"
         "\\.us\\'"
         "\\.selmer\\'"
         "\\.jsx\\'")
  :config
  (setq web-mode-engines-alist
        '(("underscore"  . "\\.us\\'")
          ("django"  . "\\.selmer\\'")))

  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t))


(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (js2-mode-hide-warnings-and-errors))


(use-package clojure-mode
  :mode (("\\.clj\\'"  . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode)
         ("\\.cljc\\'" . clojurec-mode)
         ("\\.cljx\\'" . clojurex-mode))
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (require 'clojure-mode-extra-font-locking)
                                 (enable-paredit-mode)
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-r")))
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-display-help-banner nil))
(use-package cider
  :commands (cider-jack-in cider-connect)
  :config
  (setq cider-repl-history-file "~/.cider-repl-history"))
(use-package clj-refactor
  :commands clj-refactor-mode
  :config
  (setq cljr-favor-prefix-notation nil))
(use-package clojure-mode-extra-font-locking
  :defer t)


(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setenv "PATH" (concat (getenv "PATH") ":~/.cargo/bin"))
  (setq exec-path (append exec-path '("~/.cargo/bin/")))
  (setenv "CARGO_HOME" (expand-file-name "~/.cargo/"))
  (add-hook 'rust-mode-hook (lambda ()
                              ;; (modify-syntax-entry ?_ "_" rust-mode-syntax-table)
                              (racer-mode)
                              (setq-local compile-command "cargo test"))))

(use-package racer
  :commands racer-mode
  :config
  (setq rust-root (car (split-string (shell-command-to-string "rustc --print sysroot"))))
  (setq racer-rust-src-path (format "%s/lib/rustlib/src/rust/src/" rust-root))
  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (add-hook 'racer-mode-hook (lambda ()
                               (company-mode)
                               (setq-local company-idle-delay 0.5)
                               (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
                               (define-key rust-mode-map (kbd "M-.") #'racer-find-definition)
                               (setq company-tooltip-align-annotations t))))


(use-package fsharp-mode
  :mode "\\.fs\\'"
  :config
  (setenv "MONO_MANAGED_WATCHER" "disabled"))


(use-package jinja2-mode
  :mode "\\.j2\\'")


(use-package wc-mode)
(use-package markdown-mode
  :mode ("\\.markdown\\'"
         "\\.md\\'")
  :config
  (add-hook 'markdown-mode-hook (lambda ()
                                  (wc-mode)
                                  (auto-fill-mode)
                                  (set-fill-column 80)
                                  (flyspell-mode))))


(use-package dockerfile-mode
  :mode ("Dockerfile\\'"
         "\\.dockerfile\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom config
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
