;;;;;;;;;;;;;;;;;;
;; Key bindings ;;
;;;;;;;;;;;;;;;;;;

;; Unset
(global-unset-key (kbd "C-z"))

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
