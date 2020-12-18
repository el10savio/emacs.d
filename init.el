
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
 '(helm-completion-style 'emacs)
 '(line-number-mode nil)
 '(package-selected-packages
   '(marginalia dired-filter dashboard multiple-cursors helm-ag pyimpsort pyimport ag perspective diff-hl treemacs which-key git-gutter doom-modeline doom-themes yasnippet-classic-snippets py-autopep8 yapfify yasnippet-snippets company-lsp lsp-ui lsp-mode lsp-python-ms magit all-the-icons neotree helm-rg helm-swoop elpy jedi dired-sidebar helm-projectile helm golden-ratio flycheck-rust racer company cargo rust-mode))
 '(recentf-mode t))
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

;; Bind C-c return to eval-buffer
(global-set-key (kbd "C-c RET") 'eval-buffer)

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

;; Set up which-key
(which-key-mode 1)
(which-key-setup-side-window-right)

;
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

(set-face-attribute 'default nil :height 200)
(setq-default line-spacing 0.0)

(setq
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(load-theme 'doom-vibrant t)

;;
;; Font
;;

;; Set default font
(set-face-attribute 'default nil
                    :family "Monaco"
                    :weight 'normal
                    :width 'normal)

;;
;; modeline
;;

(require 'doom-modeline)
(doom-modeline-mode 1)
(display-time-mode 1)
(setq doom-modeline-vcs-max-length 40)

(blink-cursor-mode 1)
(setq-default cursor-type 'bar)
(set-cursor-color "#cccccc")
(setq ring-bell-function 'ignore)

(golden-ratio-mode 1)

;;
;; Dashboard
;;

(require 'dashboard)
(dashboard-setup-startup-hook)

;; Set the title
(setq dashboard-init-info "Hi Elton, Welcome back to Emacs")

;; Set the banner
(setq dashboard-startup-banner 'logo)
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever image/text you would prefer

(setq dashboard-items '((recents  . 5)
                        (projects . 5)
                        (bookmarks . 3)
                        (agenda . 3)
                        (registers . 3)))

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
;; (require 'neotree)
;; (require 'all-the-icons)
;; (global-set-key (kbd "C-c C-a") 'neotree-toggle)
;; (setq neo-smart-open t)
;; (setq neo-theme 'icons)
;; (setq neo-vc-integration '(face))
;; (setq neo-window-width 30)

;;
;; treemacs
;;
(use-package treemacs
	:ensure t
	:bind
	(:map global-map
	      ("C-c t" . treemacs)
	      ("C-c /" . treemacs-visit-node-vertical-split)
	      ("C-c -" . treemacs-visit-node-horizontal-split)
	      )
	:config
	(progn
	  (setq treemacs-is-never-other-window t
		treemacs-follow-after-init t
		treemacs-recenter-after-file-follow t
		treemacs-width 30
		treemacs-collapse-dirs (if (executable-find "python") 3 0)
		treemacs-silent-refresh t
		treemacs-silent-filewatch t
		treemacs-sorting 'alphabetic-asc
		treemacs-indentation 1
		)))

(setq treemacs-follow-mode t)

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

;; Delete trailing whitespaces on save
(add-hook 'before-save-hook 'prog-delete-trailing-whitespace)

(defun prog-delete-trailing-whitespace ()
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

;;
;; Centaur tabs
;;
;; (require 'centaur-tabs)
;; (centaur-tabs-mode t)
;; (global-set-key (kbd "C-`")  'centaur-tabs-backward)
;; (global-set-key (kbd "C-1") 'centaur-tabs-forward)

;;
;; Dired
;;

(use-package dired-subtree :ensure t
  :after dired
  :config
  (bind-key "<tab>" #'dired-subtree-cycle dired-mode-map))

;;
;; Marginalia
;;

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :init

  (marginalia-mode)

  ;; Prefer richer, more heavy, annotations over the lighter default variant.
  ;; E.g. M-x will show the documentation string additional to the keybinding.
  ;; By default only the keybinding is shown as annotation.
  ;; Note that there is the command `marginalia-cycle-annotators` to
  ;; switch between the annotators.
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

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

;; Global search bind to C-c s
(global-set-key (kbd "C-c s") #'helm-projectile-rg)

;; Local replce bind to C-c r
(global-set-key (kbd "C-c r") #'replace-string)

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

;; yapf on C-C-s
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "C-c C-s") #'yapfify-buffer)))

;; yapf on save
(add-hook 'before-save-hook #'yapfify-buffer)

;; flake8 checks
;; (setq flycheck-python-flake8-executable "flake8")
;; (flycheck-select-checker 'python-flake8)
;; (flycheck-mode t)

;; Hs Minor mode
(use-package hideshow
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))
