;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;


;; un-fill lines
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(global-set-key (kbd "M-S-q") 'unfill-paragraph)

;; Unset
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "M-h a"))

;; Set
(global-set-key (kbd "C-x C-b")       'ibuffer)
(global-set-key (kbd "C-S-SPC")       'toggle-maximize-buffer)
(global-set-key (kbd "C-x C-s")       'save-buffer-clean)
(global-set-key (kbd "C-x C-S-s")     'save-buffer)
(global-set-key (kbd "<S-delete>")    'delete-region)
(global-set-key (kbd "C-x <up>")      'windmove-up)
(global-set-key (kbd "C-x <down>")    'windmove-down)
(global-set-key (kbd "C-x <right>")   'windmove-right)
(global-set-key (kbd "C-x <left>")    'windmove-left)
(global-set-key (kbd "C-S-<left>")    'previous-buffer)
(global-set-key (kbd "C-S-<right>")   'next-buffer)
(global-set-key (kbd "C-M-S-g" )      'magit-status)
(global-set-key (kbd "C-M-S-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "C-M-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-S-<down>")  'shrink-window)
(global-set-key (kbd "C-M-S-<up>")    'enlarge-window)
(global-set-key (kbd "C-M-S-z")       'window-configuration-to-register)
(global-set-key (kbd "C-S-z")         'jump-to-register)

;;Org
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
