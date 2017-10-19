(package-initialize)

;;;;;;;;;;;;;;;;;;;;; Basic ;;;;;;;;;;;;;;;;;;;;;;;;

;; Key-binding to ~/.emacs.d/init.el
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-my-init-file)

;;;;;;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(require 'cl)

;; add whatever packages you want
(defvar wxx/packages '(company
		       monokai-theme
		       hungry-delete
		       counsel
		       swiper
		       ) "Default packages")
(defun wxx/packages-installed-p ()
  (loop for pkg in wxx/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))
(unless (wxx/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg wxx/packages)
    (when (not (package-installed-p pkg))
        (package-install pkg))))

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

;;;;;;;;;;;;;;;;;;;;; company ;;;;;;;;;;;;;;;;;;;;;;

(require 'company)

;; Enable company-mode
(global-company-mode t)

;;;;;;;;;;;;;;;;;;;;; Theme ;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'monokai t)

;;;;;;;;;;;;;;;;;; hungry-delete ;;;;;;;;;;;;;;;;;;;;

(require  'hungry-delete)
(global-hungry-delete-mode t)

;;;;;;;;;;;;;;;;;;;;; swiper ;;;;;;;;;;;;;;;;;;;;;;;;

(ivy-mode t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Shortcuts for swiper
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

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

;; Enable paren-mode
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
