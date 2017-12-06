;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and dirty magic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;
;; Hooks ;;
;;;;;;;;;;;

;; General hook
(defun general-hook ()
  (interactive)
  (fci-mode)
  (yas-minor-mode)
  (hs-minor-mode)
  (projectile-mode)
  (helm-projectile-on)
  (flycheck-mode)
  (local-set-key (kbd "C-<tab>") 'yas-expand))

(add-hook 'sml-mode-hook
          (lambda ()
            (set-input-method "Agda")
            (general-hook)
            (setq electric-indent-chars'())))

;; c-mode-common
(add-hook 'c-mode-common-hook
          (lambda ()
            (general-hook)))

;; c-mode
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
            (flyspell-prog-mode)))

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
