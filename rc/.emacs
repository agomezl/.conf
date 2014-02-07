;; .emacs

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("" . "~/.save/"))))
 '(before-save-hook (quote (whitespace-cleanup)))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(display-battery-mode t)
 '(ecb-layout-window-sizes (quote (("left5" (ecb-directories-buffer-name 0.23809523809523808 . 0.2807017543859649) (ecb-sources-buffer-name 0.23809523809523808 . 0.3508771929824561) (ecb-history-buffer-name 0.23809523809523808 . 0.3508771929824561)))))
 '(ecb-options-version "2.40")
 '(fci-rule-column 80)
 '(fci-rule-use-dashes t)
 '(global-linum-mode t)
 '(ibuffer-show-empty-filter-groups nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(ispell-dictionary "english")
 '(keyboard-coding-system (quote utf-8-unix))
 '(menu-bar-mode nil)
 '(nxhtml-autoload-web nil t)
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
            (setq-local indent-tabs-mode t)
            (setq-local whitespace-style '(empty face trailing lines))))

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
            (flymake-mode)
            (turn-on-haskell-indentation)
            (local-set-key (kbd "C-?") 'flymake-display-err-menu-for-current-line)
            (general-hook)
            ))

;; General hook
(defun general-hook ()
  (fci-mode)
  (auto-complete-mode)
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

(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; configuraciones de ibuffer
(setq ibuffer-saved-filter-groups
      '(("alien"
         ("Latex" (or (filename . ".tex")
                      (filename . ".bib")))
         ("C++" (or ( filename . ".cpp")
                    ( filename . ".c" )))
         ("Git" (or (mode . magit-status-mode)
                    (mode . magit-mode)
                    (mode . git-commit-mode)))
         ("Dired" (mode . dired-mode))
         ("Haskell" (filename . ".hs"))
         ("JavaScript" (filename . ".js"))
         ("Bash"(filename . ".sh" ))
         ("MarkDown" (filename . ".md"))
         ("Org" (filename . ".org"))
         ("Java" (filename . ".java")))))

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-switch-to-saved-filter-groups "alien")))

;; configuracion de latex

(require 'flymake)

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (tex-pdf-mode)
            (flymake-mode)
            (flyspell-mode)
            (general-hook)
            (local-set-key (kbd "C-?") 'flymake-display-err-menu-for-current-line)))

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
 '(show-paren-match ((t (:background "lime green"))))
 '(show-paren-mismatch ((t (:background "red1" :foreground "white"))))
 '(warning ((t (:background "light sea green" :foreground "white" :weight bold)))))

;; UTF-8 as default encoding
(set-language-environment "UTF-8")

;;; activate ecb
;;(require 'ecb)
;;(require 'ecb-autoloads)
(setq ecb-layout-name "left5")
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-compile-window-height 12)

;;; Emacs is not a package manager, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;;JavaScript shit
(require 'flymake-jslint)
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)
            (flymake-jslint-load)
            (fci-mode)))

;; hide code
(global-set-key (kbd "C-c SPC") 'hs-toggle-hiding)


;; flymake
(eval-after-load "flymake"
  '(progn
     (defun flymake-after-change-function (start stop len)
       "Start syntax check for current buffer if it isn't already running."
       ;; Do nothing, don't want to run checks until I save.
       )))


;; ARDUINO
;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode t)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode t)
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)

;; Enable Semantic
;; (semantic-mode 1)

;; Enable EDE (Project Management) features
;; (global-ede-mode 1)

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
