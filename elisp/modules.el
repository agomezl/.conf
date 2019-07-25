;;;;;;;;;;;;;
;; Modules ;;
;;;;;;;;;;;;;


;; Conditionally load modules and settings

;;;;;;;;;;;;;;;
;; flymake   ;;
;;;;;;;;;;;;;;;

(if (require 'flymake nil 'noError)
    ;; Flymake only checks on save
    (eval-after-load "flymake"
      '(progn
         (defun flymake-after-change-function (start stop len)
           "Start syntax check for current buffer if it isn't already running."
           ;; Do nothing, don't want to run checks until I save.
           )))
  (message "[ERROR] flymake not loaded correctly"))

;;;;;;;;;;;
;; helm  ;;
;;;;;;;;;;;
(if (and (require 'helm)
         (require 'helm-config))
    (progn
      (global-set-key (kbd "C-c h") 'helm-command-prefix)
      (global-unset-key (kbd "C-x c"))
      ;; rebind tab to run persistent action
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      ;; make TAB work in terminal
      (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
      ;; list actions using C-z
      (define-key helm-map (kbd "C-z")  'helm-select-action)

      (when (executable-find "curl")
        (setq helm-google-suggest-use-curl-p t))

      ;; open helm buffer inside current window, not occupy whole other window
      (setq helm-split-window-in-side-p           t
            ;; move to end or beginning of source when reaching top or bottom of source.
            helm-move-to-line-cycle-in-source     t
            ;; search for library in `require' and `declare-function' sexp.
            helm-ff-search-library-in-sexp        t
            ;; scroll 8 lines other window using M-<next>/M-<prior>
            helm-scroll-amount                    8
            helm-ff-file-name-history-use-recentf t
            helm-echo-input-in-header-line t
            helm-autoresize-max-height 0
            helm-autoresize-min-height 20)

      (helm-autoresize-mode 1)

      (global-set-key (kbd "M-x") 'helm-M-x)
      (global-set-key (kbd "C-x b") 'helm-mini)
      (global-set-key (kbd "M-y") 'helm-show-kill-ring)
      (global-set-key (kbd "C-x C-f") 'helm-find-files)
      (global-set-key (kbd "C-s") 'helm-occur)
      (global-set-key (kbd "C-c C-SPC") 'helm-all-mark-rings)

      (helm-mode 1)

      (eval-after-load "helm"
        '(progn
           (define-key helm-find-files-map (kbd "C-s") 'helm-ff-run-grep-ag))))
  (message "[ERROR] helm not loaded correctly"))

;;;;;;;;;;;;;;;;
;; agda-input ;;
;;;;;;;;;;;;;;;;
(unless (require 'agda-input)
  (message "[ERROR] agda-input not loaded correctly"))

;;;;;;;;;;;;;;;;
;;   agda     ;;
;;;;;;;;;;;;;;;;
(unless (load-file (let ((coding-system-for-read 'utf-8))
                     (shell-command-to-string "agda-mode locate")))
  (message "[ERROR] agda-mode not loaded correctly"))


;;;;;;;;;;;;;;;;
;; projectile ;;
;;;;;;;;;;;;;;;;
(if (and (require 'projectile)
             (require 'helm-projectile))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (message "[ERROR] projectile not loaded correctly"))


;;;;;;;;;;;;;;;
;;  company  ;;
;;;;;;;;;;;;;;;
(if (require 'company nil 'noError)
    (add-to-list 'company-backends 'company-ghc)
  (message "[ERROR] company not loaded correctly"))

;;;;;;;;;;;;;;;
;;   magit   ;;
;;;;;;;;;;;;;;;
(if (require 'magit nil 'noError)
    (progn
      (setq magit-auto-revert-mode nil)
      (setq magit-last-seen-setup-instructions "1.4.0"))
  (message "[ERROR] magit not loaded correctly"))

;;;;;;;;;;;;;;;;
;;  flyspell  ;;
;;;;;;;;;;;;;;;;
(if (require 'flyspell nil 'noError)
    (progn
      (define-key flyspell-mode-map (kbd "C-M-i") nil)
      (define-key flyspell-mode-map (kbd "C-,") nil)
      (define-key flyspell-mode-map (kbd "C-.") nil)
      (setq ispell-program-name "aspell"
            ispell-extra-args '("--sug-mode=ultra")))
  (message "[ERROR] flyspell not loaded correctly"))

;;;;;;;;;;;;;;;
;;   fci     ;;
;;;;;;;;;;;;;;;
(require 'fill-column-indicator nil 'noError)

;;;;;;;;;;;;;;;
;; yasnipet  ;;
;;;;;;;;;;;;;;;
(when (require 'yasnippet nil 'noError)
    (yas-reload-all))

;;;;;;;;;;;;;;;;;;;;;;
;; multiple cursors ;;
;;;;;;;;;;;;;;;;;;;;;;
(if (require 'multiple-cursors nil 'noError)
  (progn
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))
  (message "[ERROR] multiple-cursors not loaded correctly"))

;;;;;;;;;;;;;;;
;;  web-mode ;;
;;;;;;;;;;;;;;;
(if (require 'web-mode nil 'noError)
  (progn
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-current-column-highlight t)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
    (define-key web-mode-map (kbd "C-n") 'web-mode-tag-match)
    (define-key web-mode-map (kbd "C-c ]") 'web-mode-element-close))
  (message "[ERROR] multiple-cursors not loaded correctly"))

;;;;;;;;;;;;;;;;;;;;;
;; whitespace-mode ;;
;;;;;;;;;;;;;;;;;;;;;
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;
;; HOL 4 ;;
;;;;;;;;;;;
(unless (load "~/.emacs.d/lisp/hol-mode" 'noError)
  (message "[ERROR] HOL4 emacs mode can't be found"))

;;;;;;;;;;;;;;;
;; save-mode ;;
;;;;;;;;;;;;;;;
(if (require 'saveplace nil 'noError)
    (setq save-place t)
    (message "[ERROR] save-place can't be found"))
