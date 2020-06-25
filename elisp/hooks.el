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
  (set-fontset-font "fontset-default" (quote unicode) "DejaVuSansMonoForPowerline Nerd Font")
  (flyspell-prog-mode)
  (company-mode)
  (hs-minor-mode)
  (projectile-mode)
  (helm-projectile-on))


(add-hook 'prog-mode-hook
          (lambda ()
            (general-hook)))

(add-hook 'holscript-mode-hook
          (lambda ()
            (set-input-method "Agda")
            (general-hook)))

;; latex-mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (flyspell-mode)
            (tex-pdf-mode)))

;; Haskell-mode
(add-hook 'haskell-mode-hook
          (lambda ()
            ;;(general-hook)
            (interactive-haskell-mode)
            (set-input-method "Agda")
            (haskell-indent-mode)))

;; Org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))

;; Magit-commit-mode
(add-hook 'magit-commit-mode-hook
          (lambda ()
            (flyspell-mode)))

;; Git-commit-mode
(add-hook 'git-commit-mode-hook
          (lambda ()
            (flyspell-mode)))

;; web-mode
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)))
