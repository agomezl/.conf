;; .emacs

;;;;;;;;;;;;;;;;;;;;;
;; Package manager ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")))
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
 '(fci-rule-column 80)
 '(fci-rule-use-dashes t)
 '(global-linum-mode t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(ibuffer-show-empty-filter-groups nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "~/Documents/TODO.org")
 '(ispell-dictionary "english")
 '(keyboard-coding-system (quote utf-8-unix))
 '(magit-diff-refine-hunk t)
 '(menu-bar-mode nil)
 '(org-agenda-files (quote ("~/Documents/TODO.org")))
 '(package-selected-packages
   (quote
    (free-keys helm-ag helm-projectile projectile hc-zenburn-theme zenburn-theme helm sml-mode multi-term ag flycheck yasnippet yaml-mode web-mode s pcache multiple-cursors marshal markdown-mode magit logito fill-column-indicator edit-server-htmlize dockerfile-mode company-ghc auctex ac-mozc ac-haskell-process)))
 '(safe-local-variable-values (quote ((org-todo-keyword-faces ("HOLD" . "yellow")))))
 '(scroll-bar-mode nil)
 '(select-enable-primary t)
 '(show-paren-mode t)
 '(sml-indent-level 2)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.save/"))

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

(when (and (daemonp) (locate-library "edit-server"))
   (require 'edit-server)
   (setq edit-server-new-frame nil)
   (edit-server-start))

(message "ALL DONE")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
