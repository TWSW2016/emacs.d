(package-initialize)

;; Key-binding to ~/.emacs.d/init.el
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-my-init-file)

;; Enable company-mode
(setq global-company-mode t)

;; Disable tool-bar
(setq tool-bar-mode nil)

;; Disable scroll-bar
(setq scroll-bar-mode nil)

;; Show line number
(setq linum-mode t)

;; Disable splash page
(setq inhibit-splash-screen t)

;; Enable recentf-mode
(setq recentf-mode t)

