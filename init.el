
;; Add MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun require-package (package)
  "Install given PACKAGE if it was not installed before."
  (if (package-installed-p package)
      t
    (progn
      (unless (assoc package package-archive-contents)
	(package-refresh-contents))
      (package-install package))))

;; (package-refresh-contents)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet-snippets company-lsp lsp-ui lsp-mode lsp-python-ms magit all-the-icons neotree helm-rg helm-swoop elpy jedi dired-sidebar afternoon-theme helm-projectile helm golden-ratio atom-one-dark-theme flycheck-rust racer company cargo rust-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; enable company mode at start
(add-hook 'after-init-hook 'global-company-mode)

;; company complete on C-c TAB
(global-set-key (kbd "C-c TAB") 'company-complete)

;; quickly open up init.el in new window
;;;###autoload
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c i") #'find-user-init-file)

;; kill all running processes by default on exit
(setq confirm-kill-processes nil)

;; Bind C-c C-p to package install
(global-set-key (kbd "C-c C-p") #'package-install)

;;
;; Display Settings
;;

(require-package 'afternoon-theme)
(require-package 'golden-ratio)

(require 'golden-ratio)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(set-face-attribute 'default nil :height 165)
(setq-default line-spacing 0.4)

(setq
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(load-theme 'afternoon t)

(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(set-cursor-color "#cccccc")
(setq ring-bell-function 'ignore)

(golden-ratio-mode 1)

;; Enable rainbow delimiters
(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)
;;
;; neotree
;;
(require 'neotree)
(global-set-key (kbd "C-c C-e") 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme 'nerd)

;;
;; Shell
;;

;; Bind Open shell to C-c C-t 
(global-set-key (kbd "C-c C-t") 'shell)

;;
;; HELM
;;
(require 'helm)

;; To make helm-mode start with Emacs init file
(helm-mode 1)

;; To bind to M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;;
;; RUST configs
;;

;; rustfmt on save
(setq rust-format-on-save t)

;; rustfmt on C-c C-s
(add-hook 'rust-mode-hook
          (lambda () (local-set-key (kbd "C-c C-s") #'rust-format-buffer)))

;; enable cargo
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; enable racer
(setq racer-cmd "/Users/evincent/.cargo/bin/racer")
;;(setq racer-rust-src-path "/Users/evincent/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/")
(setq racer-rust-src-path "/Users/evincent/Code/rust/rust/src")
	      
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; enable flycheck
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

;; Take user input in cargo run
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-r") 'my-cargo-run))
(defun my-cargo-run ()
  "Build and run Rust code."
  (interactive)
  (cargo-process-run)
  (let (
      (orig-win (selected-window))
      (run-win (display-buffer (get-buffer "*Cargo Run*") nil 'visible))
    )
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (select-window orig-win)
  )
 )

;; Take user input in cargo clippy
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-l") 'my-cargo-clippy))
(defun my-cargo-clippy ()
  "Run Clippy on Rust code."
  (interactive)
  (cargo-process-clippy)
  (let (
      (orig-win (selected-window))
      (run-win (display-buffer (get-buffer "*Cargo Clippy*") nil 'visible))
    )
    (select-window run-win)
    (comint-mode)
    (read-only-mode 0)
    (select-window orig-win)
  )
 )


;;
;; Python
;;

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'lsp))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t)
