;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions and dirty magic ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ibuffer custom group
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (setq ibuffer-filter-groups
                  (append
                   (ibuffer-vc-generate-filter-groups-by-vc-root)
                   (ibuffer-tramp-generate-filter-groups-by-tramp-connection)
                   '(("Helm" (predicate string-match "Hmm" mode-name))
                     ("Dired" (mode . dired-mode)))))))

(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              vc-relative-file)))

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
  (company-mode)
  (hs-minor-mode)
  (projectile-mode)
  (helm-projectile-on))


(add-hook 'prog-mode-hook
          (lambda ()
            (general-hook)))

(add-hook 'sml-mode-hook
          (lambda ()
            (set-input-method "Agda")
            (font-lock-add-keywords 'sml-mode
              '(("Theorem" . font-lock-keyword-face)))


            (setq electric-indent-chars'())))

;; c-mode-common
(add-hook 'c-mode-common-hook
          (lambda ()
            (general-hook)))

;; c-mode

;; latex-mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (flyspell-mode)
            (tex-pdf-mode)))

;;(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda ()
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



;; web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (flyspell-prog-mode)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))
