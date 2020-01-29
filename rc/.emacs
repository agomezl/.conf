;; .emacs

;;;;;;;;;;;;;;;;;;;;;
;; Package manager ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(dolist (source '(("melpa" . "https://melpa.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda-input-user-translations (quote (("blkb" "▪") ("bool" "𝔹") ("brt" "▸") ("blt" "◂"))))
 '(agda2-include-dirs (quote ("." "/home/agomezl/opt/agda-stdlib/src")))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("" . "~/.save/"))))
 '(column-number-mode t)
 '(company-ghc-show-info t)
 '(default-frame-alist
    (quote
     ((vertical-scroll-bars)
      (font . "DejaVu Sans Mono-12"))))
 '(global-hl-line-mode nil)
 '(global-linum-mode t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(hexl-bits 8)
 '(ibuffer-show-empty-filter-groups nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "english")
 '(keyboard-coding-system (quote utf-8-unix))
 '(magit-diff-refine-hunk t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/Documents/TODO.org")))
 '(package-selected-packages
   (quote
    (ibuffer-tramp ibuffer-vc docker-tramp free-keys helm-ag helm-projectile hc-zenburn-theme zenburn-theme helm sml-mode multi-term ag flycheck yaml-mode web-mode s pcache multiple-cursors marshal markdown-mode magit logito fill-column-indicator dockerfile-mode company-ghc auctex ac-haskell-process)))
 '(safe-local-variable-values (quote ((org-todo-keyword-faces ("HOLD" . "yellow")))))
 '(scroll-bar-mode nil)
 '(select-enable-primary t)
 '(show-paren-mode t)
 '(sml-indent-level 2)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.save/"))

;; Install any missing package
(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(diff-refine-added ((t (:background "#188618" :foreground "#AFD8AF"))))
 '(diff-refine-removed ((t (:background "#AA1313" :foreground "#ECB3B3"))))
 '(fringe ((t (:foreground "#DCDCCC" :background "#3F3F3F"))))
 '(hol-free-variable ((t (:foreground "deep sky blue" :weight bold))))
 '(holscript-definition-syntax ((t (:inherit holscript-theorem-syntax))))
 '(holscript-quoted-material ((t (:slant oblique))))
 '(holscript-smlsyntax ((t (:inherit font-lock-keyword-face))))
 '(holscript-theorem-syntax ((t (:inherit font-lock-keyword-face))))
 '(holscript-thmname-syntax ((t (:inherit font-lock-type-face :weight bold))))
 '(magit-diff-added-highlight ((t (:background "#3F5F3F" :foreground "#AFD8AF"))))
 '(magit-diff-removed-highlight ((t (:background "#7C4343" :foreground "#ECB3B3")))))

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; backups
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Org
(setq org-log-done t)

;; default web browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;;up-case down-case enable
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;
;; Modules ;;
;;;;;;;;;;;;;

(load "~/.conf/elisp/modules.el")

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;

(load "~/.conf/elisp/theme.el")

;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

(load "~/.conf/elisp/keys.el")

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

(load "~/.conf/elisp/hooks.el")

;;;;;;;;;;;;
;; server ;;
;;;;;;;;;;;;


(message "ALL DONE")
