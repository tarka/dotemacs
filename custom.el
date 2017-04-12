(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(package-selected-packages
   (quote
    (clojure-mode-extra-font-locking groovy-mode yaml-mode web-mode wc-mode undo-tree toml-mode ssh-file-modes rainbow-delimiters puppet-mode projectile paredit-menu mustache-mode magit lua-mode less-css-mode json-mode js2-mode jinja2-mode hide-lines grizzl go-mode ghc fsharp-mode flx-ido exec-path-from-shell dockerfile-mode company-go color-theme-solarized coffee-mode clj-refactor anaconda-mode)))
 '(safe-local-variable-values
   (quote
    ((js-indent-level . 4)
     (web-mode-content-types-alist
      ("jsx" . ".*\\.js[x]?\\'"))
     (eval add-to-list
           (quote auto-mode-alist)
           (quote
            ("\\.js\\'" . web-mode)))
     (whitespace-line-column . 80)
     (lexical-binding . t))))
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
