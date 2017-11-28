;; .emacs

;;;;;;;;;;;;;;;;;;;;;
;; Package manager ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/isar")

;;;;;;;;;;;;;
;; Modules ;;
;;;;;;;;;;;;;

(load "~/.conf/elisp/modules.el")

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

;; Miscellaneous
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda-input-user-translations (quote (("blkb" "▪") ("bool" "𝔹") ("brt" "▸") ("blt" "◂"))))
 '(agda2-include-dirs (quote ("." "/home/agomezl/opt/agda-stdlib/src")))
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
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
    (hc-zenburn-theme zenburn-theme helm sml-mode multi-term ag flycheck yasnippet yaml-mode web-mode s pcache multiple-cursors marshal markdown-mode magit logito fill-column-indicator edit-server-htmlize dockerfile-mode company-ghc auctex ac-mozc ac-haskell-process)))
 '(safe-local-variable-values (quote ((org-todo-keyword-faces ("HOLD" . "yellow")))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(select-enable-primary t)
 '(show-paren-mode t)
 '(sml-indent-level 2)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.save/"))

;; Theme
(defvar zenburn-override-colors-alist
  '(("zenburn-fg+1"     . "#FFFFEF")
    ("zenburn-fg"       . "#DCDCCC")
    ("zenburn-fg-1"     . "#70705E")
    ("zenburn-bg-2"     . "#000000")
    ("zenburn-bg-1"     . "#202020")
    ("zenburn-bg-05"    . "#2D2D2D")
    ("zenburn-bg"       . "#313131")
    ("zenburn-bg+05"    . "#383838")
    ("zenburn-bg+1"     . "#3E3E3E")
    ("zenburn-bg+2"     . "#4E4E4E")
    ("zenburn-bg+3"     . "#5E5E5E")
    ("zenburn-red+1"    . "#E9B0B0")
    ("zenburn-red"      . "#D9A0A0")
    ("zenburn-red-1"    . "#C99090")
    ("zenburn-red-2"    . "#B98080")
    ("zenburn-red-3"    . "#A97070")
    ("zenburn-red-4"    . "#996060")
    ("zenburn-orange"   . "#ECBC9C")
    ("zenburn-yellow"   . "#FDECBC")
    ("zenburn-yellow-1" . "#EDDCAC")
    ("zenburn-yellow-2" . "#DDCC9C")
    ("zenburn-green-1"  . "#6C8C6C")
    ("zenburn-green"    . "#8CAC8C")
    ("zenburn-green+1"  . "#9CBF9C")
    ("zenburn-green+2"  . "#ACD2AC")
    ("zenburn-green+3"  . "#BCE5BC")
    ("zenburn-green+4"  . "#CCF8CC")
    ("zenburn-cyan"     . "#A0EDF0")
    ("zenburn-blue+1"   . "#9CC7FB")
    ("zenburn-blue"     . "#99DDE0")
    ("zenburn-blue-1"   . "#89C5C8")
    ("zenburn-blue-2"   . "#79ADB0")
    ("zenburn-blue-3"   . "#699598")
    ("zenburn-blue-4"   . "#597D80")
    ("zenburn-blue-5"   . "#436D6D")
    ("zenburn-magenta"  . "#E090C7")))
(load-theme 'zenburn t)



;;up-case down-case enable
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-z"))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-S-SPC") 'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-s") 'save-buffer-clean)
(global-set-key (kbd "C-x C-S-s") 'save-buffer)
(global-set-key (kbd "<S-delete>") 'delete-region)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-S-<left>") 'previous-buffer)
(global-set-key (kbd "C-S-<right>") 'next-buffer)
(global-set-key (kbd "C-M-S-g" ) 'magit-status)
(global-set-key (kbd "C-M-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-S-<down>") 'shrink-window)
(global-set-key (kbd "C-M-S-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-S-z")  'window-configuration-to-register)
(global-set-key (kbd "C-S-z")  'jump-to-register)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers and Backups  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ibuffer custom group
(setq ibuffer-saved-filter-groups
      '(("alien"
         ("Latex" (or (filename . ".tex")
                      (filename . ".bib")))
         ("Isabelle" (or (filename . "ROOT")
                         (filename . ".thy")))
         ("SML" (filename . ".sml"))
         ("Git" (or (mode . magit-status-mode)
                    (mode . magit-mode)
                    (mode . git-commit-mode)))
         ("Dired" (mode . dired-mode))
         ("Haskell" (or (mode . haskell-mode)
                        (mode . literate-haskell-mode)))
         ("JavaScript" (filename . ".js"))
         ("Bash"(filename . ".sh" ))
         ("MarkDown" (filename . ".md"))
         ("Org" (filename . ".org"))
         ("Java" (filename . ".java"))
         ("Helm" (predicate string-match "Hmm" mode-name))
         ("Other" (mode . fundamental))
         ("C++" (mode . c-mode)))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "alien")))



;; backups
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color and style stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
;; '(agda2-highlight-field-face ((t (:foreground "#ad7fa8"))))
;; '(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
;; '(agda2-highlight-inductive-constructor-face ((t (:foreground "#8ae234"))))
;; '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
;; '(agda2-highlight-module-face ((t (:inherit font-lock-builtin-face))))
;; '(agda2-highlight-number-face ((t (:inherit font-lock-constant-face))))
;; '(agda2-highlight-postulate-face ((t (:inherit font-lock-type-face))))
;; '(agda2-highlight-primitive-face ((t (:inherit font-lock-type-face))))
;; '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face))))
;; '(agda2-highlight-record-face ((t (:inherit font-lock-type-face))))
;; '(agda2-highlight-string-face ((t (:inherit font-lock-string-face))))
;; '(column-marker-1 ((t (:underline (:color "green" :style wave)))) t)
;; '(column-marker-2 ((t (:underline (:color "yellow" :style wave)))) t)
;; '(column-marker-3 ((t (:underline (:color "orange" :style wave)))) t)
;; '(cursor ((t (:background "white"))))
;; '(error ((t (:background "firebrick2" :foreground "white" :weight bold))))
;; '(mode-line ((t (:foreground "#ffffff" :background "#696969" :box nil))))
;; '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
;; '(org-todo ((t (:foreground "red" :weight bold))))
;; '(powerline-active1 ((t (:inherit mode-line :background "white" :foreground "black"))))
;; '(powerline-active2 ((t (:inherit mode-line :background "grey20"))))
;; '(powerline-inactive1 ((t (:inherit mode-line-inactive :background "grey22"))))
;; '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey40"))))
;; '(show-paren-match ((t (:foreground "lime green" :weight bold))))
;; '(show-paren-mismatch ((t (:foreground "red1" :weight bold))))
;; '(warning ((t (:background "light sea green" :foreground "white" :weight bold))))
;; '(web-mode-current-element-highlight-face ((t (:underline "white"))))
;; '(web-mode-html-tag-face ((t (:inherit font-lock-keyword-face)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and dirty magic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-buffer-clean ()
  "save current buffer after cleaning up whitespaces"
  (interactive)
  (whitespace-cleanup)
  (save-buffer))

;; full-screen buffer toggle
(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
    (jump-to-register '_)
    (progn
      (set-register '_ (list (current-window-configuration)))
      (delete-other-windows))))

;; HOL4
(load "~/opt/HOL/tools/hol-mode")

(add-hook 'sml-mode-hook
          (lambda ()
            (set-input-method "Agda")
            (setq electric-indent-chars'())))

;; General hook
(defun general-hook ()
  (interactive)
  (fci-mode)
  (yas-minor-mode)
  (flycheck-mode)
  (local-set-key (kbd "C-<tab>") 'yas-expand)
  (local-set-key (kbd "C-+") 'yas-insert-snippet))

;; Org
(setq org-log-done t)

;; aspell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; default web browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
(define-key web-mode-map (kbd "C-c ]") 'web-mode-element-close)

;; Agda mode
;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;;up-case down-case enable
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

;; sml-mode
(add-hook 'sml-mode-hook
          (lambda ()
            (general-hook)))


;; c-mode
(add-hook 'c-mode-common-hook
          (lambda ()
            (general-hook)))

(add-hook 'c-mode-hook
          (lambda ()
            (general-hook)))

;; latex-mode

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (flyspell-mode)
            (tex-pdf-mode)
            (general-hook)))

;;(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (general-hook)
            (interactive-haskell-mode)
            (set-input-method "Agda")
            (company-mode)
            (haskell-indent-mode)
            (flyspell-prog-mode)
            ))

;; Org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))

;; Magit-commit-mode
(add-hook 'magit-commit-mode-hook
          (lambda ()
            (general-hook)))

;; Git-commit-mode
(add-hook 'git-commit-mode-hook
          (lambda ()
            (general-hook)
            (flyspell-mode)))

(add-hook 'sh-mode-hook
          (lambda ()
            (general-hook)))

(add-hook 'python-mode-hook
          (lambda ()
            (general-hook)))


;; web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))

;;edit server

(when (and (daemonp) (locate-library "edit-server"))
   (require 'edit-server)
   (setq edit-server-new-frame nil)
   (edit-server-start))

(message "ALL DONE")
(put 'dired-find-alternate-file 'disabled nil)
