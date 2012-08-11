
;; Appearance

;(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(setq inhibit-startup-screen t)

(when (not (null window-system))
  (set-background-color "DarkSlateGrey")
  (set-foreground-color "White")
;  (set-frame-font "-windows-proggy clean-medium-r-normal-sans-13-80-96-96-c-70-iso8859-1")
  (setq font-lock-comment-face '(:foreground "Pink"))
  (global-set-key [?\C-z] 'undo))

;; Keys
(defun switch-buffer-immediate ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(define-key function-key-map [delete] "\C-d")
(global-set-key [?\C-`] 'next-error)
(global-set-key [?\C-,] 'call-last-kbd-macro)
(global-set-key "\M-m" 'set-mark-command)
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-x\C-j" 'switch-buffer-immediate)
(global-set-key "\M-i" 'yas/insert-snippet)
(global-set-key "\C-\M-y" 'clipboard-yank)
(global-set-key "\C-\M-w" 'clipboard-kill-ring-save)
(if (string-equal system-type "darwin")
    (setq mac-command-modifier 'meta))

;; Default modes
(show-paren-mode)
(global-font-lock-mode t)
(auto-compression-mode t)
(add-to-list 'completion-ignored-extensions ".svn/")


;; Packages setup

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") )
(package-initialize)

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-eshell starter-kit-bindings
		      clojure-mode clojure-test-mode
		      rainbow-delimiters
		      ac-slime
		      markdown-mode
		      popup
		      undo-tree
                      auto-complete))

;;; auto-complete only seems to work as a manual install, however that
;;; manual install relies on popup being available
 
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(add-to-list 'load-path "~/.emacs.d/")
(require 'auto-complete-config)
(ac-config-default)

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<C-right>") nil)
     (define-key paredit-mode-map (kbd "<C-left>") nil)))


;; Colour match parens and other structure characters to make code easy to follow
(global-rainbow-delimiters-mode)

;;; Enable undo-tree for everything, so you can M - _ to redo
(global-undo-tree-mode)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "DarkSlateGrey" :foreground "White" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "apple" :family "ProggyClean"))))
 '(highlight ((t (:background "#244444")))))
