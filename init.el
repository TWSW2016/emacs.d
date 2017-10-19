(package-initialize)

;;;;;;;;;;;;;;;;;;;;; Basic ;;;;;;;;;;;;;;;;;;;;;;;;

;; Key-binding to ~/.emacs.d/init.el
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-my-init-file)

;;;;;;;;;;;;;;;;;;;;; company ;;;;;;;;;;;;;;;;;;;;;;

(require 'company)

;; Enable company-mode
(global-company-mode t)


;;;;;;;;;;;;;;;;;;;;; org-mode ;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

;; Enable fontify natively
(setq org-src-fontify-natively t)

;;;;;;;;;;;;;;;;;;;;; recentf ;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)

;; Enable recentf-mode
(recentf-mode t)

;; Set maximum size of items of recentf menu
(setq recentf-max-menu-items 25)

;; Shortcuts for open recent files
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;; Common ;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable tool-bar
(setq tool-bar-mode nil)

;; Disable scroll-bar
(setq scroll-bar-mode nil)

;; Show line number
(setq linum-mode t)

;; Disable splash page
(setq inhibit-splash-screen t)

;; Disable backup files
(setq make-backup-files nil)

;; Maximize screen
(setq initial-frame-alist 
      (quote ((fullscreen . maximized))))

;; Highlight current line
(global-hl-line-mode t)
