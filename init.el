
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
 '(doom-modeline-buffer-encoding nil)
 '(line-number-mode nil)
 '(package-selected-packages
   '(ag perspective diff-hl treemacs which-key git-gutter doom-modeline doom-themes yasnippet-classic-snippets py-autopep8 yapfify yasnippet-snippets company-lsp lsp-ui lsp-mode lsp-python-ms magit all-the-icons neotree helm-rg helm-swoop elpy jedi dired-sidebar helm-projectile helm golden-ratio flycheck-rust racer company cargo rust-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:background "#50fa7b"))))
 '(git-gutter:deleted ((t (:background "#ff79c6"))))
 '(git-gutter:modified ((t (:background "#f1fa8c")))))

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

;; Bind C-c p to package install
(global-set-key (kbd "C-c p") #'package-install)

;; Bind C-c f to projectile find file
(global-set-key (kbd "C-c f") #'projectile-find-file)

;; kill all running processes by default on exit
(setq confirm-kill-processes nil)

;;
;; Display Settings
;;

(require-package 'doom-themes)
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

(load-theme 'doom-one t)

;; (set-background-color "#1f5582")

;;
;; modeline
;;

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-vcs-max-length 40)

(blink-cursor-mode 0)
(setq-default cursor-type 'bar)
(set-cursor-color "#cccccc")
(setq ring-bell-function 'ignore)

(golden-ratio-mode 1)

;; Enable rainbow delimiters
(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)

(show-paren-mode 1)
;;
;; git-gutter
;;
(use-package git-gutter
    :custom
    (git-gutter:modified-sign "~")
    (git-gutter:added-sign    "+")
    (git-gutter:deleted-sign  "-")
    :custom-face
    (git-gutter:modified ((t (:background "#f1fa8c"))))
    (git-gutter:added    ((t (:background "#50fa7b"))))
    (git-gutter:deleted  ((t (:background "#ff79c6"))))
    :config
    (global-git-gutter-mode +1))

;;
;; neotree
;;
(require 'neotree)
(require 'all-the-icons)
(global-set-key (kbd "C-c C-a") 'neotree-toggle)
(setq neo-smart-open t)
(setq neo-theme 'icons)
(setq neo-vc-integration '(face))
(setq neo-window-width 30)

;;
;; Linum 
;;
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d \u2502 ")

;; ;; Highlight current line 
;; (global-hl-line-mode +1)
;; (set-face-background 'hl-line "#333333")

;; Highlight current line number
(require 'hlinum)
(hlinum-activate)

;;
;; Centaur tabs
;;
;; (require 'centaur-tabs)
;; (centaur-tabs-mode t)
;; (global-set-key (kbd "C-`")  'centaur-tabs-backward)
;; (global-set-key (kbd "C-1") 'centaur-tabs-forward)

;;
;; Shell
;;

;; Bind Open EShell to C-c C-t 
(global-set-key (kbd "C-t") 'eshell)

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

(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)    ;;  global-flycheck-mode
  (setq flycheck-display-errors-delay .3))

(use-package company
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config (setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
          (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'lsp))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t)

;; M-Ret to go to definition
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "M-RET") #'lsp-find-definition)))

;; yapf on C-c C-s
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c C-s") #'yapfify-buffer)))

;; yapf on save
(add-hook 'before-save-hook #'yapfify-buffer)
