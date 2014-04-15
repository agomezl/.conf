;; .emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-face-groups (quote default-faces))
 '(agda2-include-dirs (quote ("." "/home/alien/.cabal/lib/lib-0.7/src")))
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("" . "~/.save/"))))
 '(before-save-hook (quote (whitespace-cleanup)))
 '(column-number-mode t)
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

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)

(setq-default indent-tabs-mode nil)

(add-hook 'makefile-mode
          ( lambda ()
            (setq indent-tabs-mode t)
            (setq whitespace-style '(empty face trailing lines))))

;; Yasnipet

(require 'yasnippet)
(yas-reload-all)

;; backups

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; configuraciones de haskell


(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
            (ghc-init)
            (turn-on-haskell-indentation)
            (general-hook)
            (yas-minor-mode)
            (local-set-key (kbd "C-<tab>") 'yas-expand)
            (local-set-key (kbd "C-+") 'yas-insert-snippet)
            ))

;; General hook
(defun general-hook ()
  (fci-mode)
  (auto-complete-mode)
  (custom-set-faces
    '(error ((t (:background "firebrick2" :foreground "white" :weight bold))))
    '(warning ((t (:background "light sea green" :foreground "white" :weight bold)))))
  )



;; configuraciones de C

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'exec-path "/opt/gccsense-0.1/bin/")
(require 'gccsense)

(add-hook 'c-mode-common-hook
          (lambda ()
            (gccsense-flymake-setup)
            (flymake-mode)
            (local-set-key [C-tab] 'ac-complete-gccsense)
            (local-set-key (kbd "C-?") 'flymake-display-err-menu-for-current-line)
            (general-hook)))

;; atajos de teclado y cosas raras generales
(put 'downcase-region 'disabled nil)
(global-unset-key (kbd "C-z"))

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

;;multiple cursors
;;(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; configuraciones de ibuffer
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

;; configuracion de latex

(require 'flymake)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (tex-pdf-mode)
            (flyspell-mode)
            (general-hook)
            (local-set-key (kbd "C-?") 'flymake-display-err-menu-for-current-line)
            (yas-minor-mode)
            (local-set-key (kbd "C-<tab>") 'yas-expand)
            (local-set-key (kbd "C-+") 'yas-insert-snippet)))

(defun flymake-get-tex-args (file-name)
  (list "pdflatex"
        (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-marker-1 ((t (:underline (:color "green" :style wave)))))
 '(column-marker-2 ((t (:underline (:color "yellow" :style wave)))))
 '(column-marker-3 ((t (:underline (:color "orange" :style wave)))))
 '(cursor ((t (:background "white"))))
 '(error ((t (:background "firebrick2" :foreground "white" :weight bold))))
 '(idris-prover-processed-face ((t nil)))
 '(idris-semantic-bound-face ((t (:foreground "tomato"))))
 '(idris-semantic-data-face ((t (:foreground "DarkOliveGreen1"))))
 '(idris-semantic-type-face ((t (:foreground "light steel blue"))))
 '(show-paren-match ((t (:foreground "lime green" :weight bold))))
 '(show-paren-mismatch ((t (:foreground "red1" :weight bold))))
 '(warning ((t (:background "light sea green" :foreground "white" :weight bold)))))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)


;; flymake
(eval-after-load "flymake"
  '(progn
     (defun flymake-after-change-function (start stop len)
       "Start syntax check for current buffer if it isn't already running."
       ;; Do nothing, don't want to run checks until I save.
       )))

;;Org-mode

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'magit-commit-mode-hook
          (lambda ()
            (general-hook)))

(add-hook 'git-commit-mode-hook
          (lambda ()
            (general-hook)
            (flyspell-mode)))

(add-hook 'agda-mode
          (lambda ()
            (general-hook)))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
(put 'upcase-region 'disabled nil)
