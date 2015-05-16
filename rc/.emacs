;; .emacs

;;;;;;;;;;;;;;;;;;;;;
;; Package manager ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/")

;;;;;;;;;;;;;
;; Require ;;
;;;;;;;;;;;;;

(require 'agda-input)
(require 'company)
(require 'flymake)
(require 'package)
(require 'org)
(require 'magit)
(require 'ghc)
(require 'gist)
(require 'flyspell)
(require 'fill-column-indicator)
(require 'yasnippet)
(require 'multiple-cursors)
;; (require 'gccsense)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;

;; Miscellaneous
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda-input-user-translations (quote (("bool" "𝔹"))))
 '(agda2-highlight-face-groups (quote default-faces))
 '(agda2-include-dirs (quote ("." "/home/alien/opt/agda-stdlib-0.9/src")))
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("" . "~/.save/"))))
 '(before-save-hook (quote (whitespace-cleanup)))
 '(column-number-mode t)
 '(company-ghc-show-info t)
 '(custom-enabled-themes (quote (wombat)))
 '(display-battery-mode t)
 '(fci-rule-column 80)
 '(fci-rule-use-dashes t)
 '(global-linum-mode t)
 '(ibuffer-show-empty-filter-groups nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(ispell-dictionary "english")
 '(keyboard-coding-system (quote utf-8-unix))
 '(menu-bar-mode nil)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.save/"))

;; whitespace-mode
(setq-default indent-tabs-mode nil)

;; Org
(setq org-log-done t)

;; aspell
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

;; default web browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; ibuffer custom group
(setq ibuffer-saved-filter-groups
      '(("alien"
         ("Latex" (or (filename . ".tex")
                      (filename . ".bib")))
         ("Git" (or (mode . magit-status-mode)
                    (mode . magit-mode)
                    (mode . git-commit-mode)))
         ("Dired" (mode . dired-mode))
         ("Haskell" (mode . haskell-mode))
         ("JavaScript" (filename . ".js"))
         ("Bash"(filename . ".sh" ))
         ("MarkDown" (filename . ".md"))
         ("Org" (filename . ".org"))
         ("Java" (filename . ".java"))
         ("C++" (mode . c-mode)))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "alien")))

;; backups
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;; Yasnipet
(require 'yasnippet)
(yas-reload-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and dirty magic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General hook
(defun general-hook ()
  (interactive)
  (fci-mode)
  (yas-minor-mode)
  (local-set-key (kbd "C-<tab>") 'yas-expand)
  (local-set-key (kbd "C-+") 'yas-insert-snippet))

;; Flymake and LaTex
(defun flymake-get-tex-args (file-name)
  (list "pdflatex"
        (list
         "-file-line-error"
         "-draftmode"
         "-interaction=nonstopmode" file-name)))


;; Flymake only checks on save
(eval-after-load "flymake"
  '(progn
     (defun flymake-after-change-function (start stop len)
       "Start syntax check for current buffer if it isn't already running."
       ;; Do nothing, don't want to run checks until I save.
       )))

;;Agda
(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))

;;up-case down-case enable
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

(global-unset-key (kbd "C-z"))
(define-key flyspell-mode-map (kbd "C-M-i") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)
(define-key flyspell-mode-map (kbd "C-.") nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)
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

;;company mode
(add-to-list 'company-backends 'company-ghc)



;;multiple cursors

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;Org

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color and style stuff ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 ((t (:underline (:color "green" :style wave)))) t)
 '(column-marker-2 ((t (:underline (:color "yellow" :style wave)))) t)
 '(column-marker-3 ((t (:underline (:color "orange" :style wave)))) t)
 '(cursor ((t (:background "white"))))
 '(error ((t (:background "firebrick2" :foreground "white" :weight bold))))
 '(flyspell-duplicate ((t (:underline (:color "deep sky blue" :style wave)))))
 '(flyspell-incorrect ((t (:underline (:color "orange red" :style wave)))))
 '(ghc-face-error ((t (:underline "orangered"))))
 '(idris-prover-processed-face ((t nil)) t)
 '(idris-semantic-bound-face ((t (:foreground "tomato"))) t)
 '(idris-semantic-data-face ((t (:foreground "DarkOliveGreen1"))) t)
 '(idris-semantic-type-face ((t (:foreground "light steel blue"))) t)
 '(show-paren-match ((t (:foreground "lime green" :weight bold))))
 '(show-paren-mismatch ((t (:foreground "red1" :weight bold))))
 '(warning ((t (:background "light sea green" :foreground "white" :weight bold)))))

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

;; Haskell-mode
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (general-hook)
            (set-input-method "Agda")
            (ghc-init)
            (company-mode)
            (haskell-indent-mode)
;;            (haskell-indentation-disable-show-indentations)
            (flyspell-prog-mode)
            ))

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

;; Makefile-mode
(add-hook 'makefile-mode-hook 'indent-tabs-mode)

;; Agda-mode
(add-hook 'agda-mode
          (lambda ()
            (general-hook)))


(add-hook 'sh-mode-hook
          (lambda ()
            (general-hook)))

;;edit server
(when (and (daemonp) (locate-library "edit-server"))
   (require 'edit-server)
   (setq edit-server-new-frame nil)
   (edit-server-start))

(autoload 'edit-server-maybe-dehtmlize-buffer
  "edit-server-htmlize" "edit-server-htmlize" t)
(autoload 'edit-server-maybe-htmlize-buffer
  "edit-server-htmlize" "edit-server-htmlize" t)
(add-hook 'edit-server-start-hook
          (lambda ()
            (edit-server-maybe-dehtmlize-buffer)
            (fci-mode)
            (flyspell-mode)
            ))

(add-hook 'edit-server-done-hook  'edit-server-maybe-htmlize-buffer)
