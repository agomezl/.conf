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
 '(current-language-environment "Spanish")
 '(custom-enabled-themes nil)
 '(display-battery-mode t)
 '(ecb-layout-window-sizes (quote (("left5" (ecb-directories-buffer-name 0.23809523809523808 . 0.2807017543859649) (ecb-sources-buffer-name 0.23809523809523808 . 0.3508771929824561) (ecb-history-buffer-name 0.23809523809523808 . 0.3508771929824561)))))
 '(ecb-options-version "2.40")
 '(flyspell-default-dictionary "castellano")
 '(global-linum-mode t)
 '(haskell-mode-hook (quote (turn-on-haskell-indent turn-on-haskell-indentation (lambda nil (ghc-init) (flymake-mode) (turn-on-haskell-indentation) (auto-complete-mode) (local-set-key (kbd "C-?") (quote flymake-display-err-menu-for-current-line))))) t)
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(ibuffer-show-empty-filter-groups nil)
 '(initial-buffer-choice t)
 '(keyboard-coding-system (quote utf-8-unix))
 '(nxhtml-autoload-web nil t)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tramp-auto-save-directory "~/.save/"))

;;; uncomment for CJK utf-8 support for non-Asian users
;; (require 'un-define)

;; backups

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; configuraciones de haskell

(add-to-list 'exec-path "~/.cabal/bin/")
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (ghc-init)
	    (flymake-mode)
	    (turn-on-haskell-indentation)
	    (auto-complete-mode)
	    (local-set-key (kbd "C-?") 'flymake-display-err-menu-for-current-line)))

;; configuraciones de C

(add-to-list 'load-path "~/.emacs.d/")
(require 'gccsense)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (gccsense-flymake-setup)
	    (flymake-mode)
	    (local-set-key [C-tab] 'ac-complete-gccsense)
	    (local-set-key (kbd "C-?") 'flymake-display-err-menu-for-current-line)))

;; atajos de teclado y cosas raras generales
(put 'downcase-region 'disabled nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-S-<left>") 'previous-buffer)
(global-set-key (kbd "C-S-<right>") 'next-buffer)



;; configuraciones de ibuffer
(setq ibuffer-saved-filter-groups
      '(("alien"
	 ("Latex" (or (filename . ".tex")
		      (filename . ".bib")))
	 ("Haskell" (filename . ".hs"))
	 ("JavaScript" (filename . ".js"))
	 ("Bash"(filename . ".sh" ))
	 ("MarkDown" (filename . ".md"))
	 ("Java" (filename . ".java")))))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "alien")))

;; configuracion de latex

(require 'flymake)

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	  (tex-pdf-mode)
	  (auto-complete-mode)))

(defun flymake-get-tex-args (file-name)
(list "pdflatex"
(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
(add-hook 'js-mode-hook
	  (lambda ()
	    ;; Scan the file for nested code blocks
	    (imenu-add-menubar-index)
	    ;; Activate the folding mode
	    (hs-minor-mode t)))

;; hide code
(global-set-key (kbd "C-c SPC") 'hs-toggle-hiding)
