
;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

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
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(helm-completion-style 'emacs)
 '(line-number-mode nil)
 '(package-selected-packages
   '(elisp-format duplicate-thing term-run zig-mode org-ac smartparens writegood-mode howdoyou howdoi windresize bm yaml-mode ecb go-imenu imenu-list peep-dired magit-delta shell-pop vimish-fold rg sr-speedbar kaolin-themes blacken markdown-mode markdown-mode+ ace-jump-buffer vc-msg git-lens company-go exec-path-from-shell go-imports autopair go-autocomplete go-complete go-mode importmagic 2048-game transpose-frame mood-line marginalia dired-filter dashboard multiple-cursors helm-ag pyimpsort pyimport ag perspective diff-hl treemacs which-key git-gutter doom-themes yasnippet-classic-snippets py-autopep8 yapfify yasnippet-snippets company-lsp lsp-ui lsp-mode lsp-python-ms magit all-the-icons helm-rg elpy jedi helm-projectile helm golden-ratio flycheck-rust racer company cargo rust-mode))
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
;; (add-hook 'after-init-hook 'global-company-mode)

;; company complete on C-c TAB
;; (global-set-key (kbd "C-c TAB") 'company-complete)

;; Bind C-c return to eval-buffer
(global-set-key (kbd "C-c RET") 'eval-buffer)

;; quickly open up init.el in new window
;;;###autoload
(defun find-user-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c i") #'find-user-init-file)

;; Bind s-> to end-of-buffer
(global-set-key (kbd "s->") #'end-of-buffer)

;; Bind s-< to begining-of-buffer
(global-set-key (kbd "s-<") #'beginning-of-buffer)

;; Bind C-c p to package install
(global-set-key (kbd "C-c p") #'package-install)

;; Bind C-c f to projectile find file
(global-set-key (kbd "C-c f") #'projectile-find-file)

;; Local search bind to C-c o
(global-set-key (kbd "C-c o") #'helm-occur)

;; Local replce bind to C-c r
(global-set-key (kbd "C-c r") #'replace-string)

;; kill all running processes by default on exit
(setq confirm-kill-processes nil)

;; Set up which-key
(which-key-mode 1)
(which-key-setup-side-window-right)

;; Global search bind to C-c s
(global-set-key (kbd "C-c s") #'rgrep)

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

(set-face-attribute 'default nil :height 200)
(setq-default line-spacing 0.0)

(setq
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

(setq-default truncate-lines t)

;; overwrite selected text
(delete-selection-mode t)

(load-theme 'doom-dark+ t)

;;
;; Scroll
;;
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-conservatively 100000)
(setq scroll-error-top-bottom t)
(setq scroll-preserve-screen-position t)

;;
;; Font
;;

;; Set default font
(set-face-attribute 'default nil
                    :family "Monaco"
		    :height 180
                    :weight 'normal
                    :width 'normal)

;; Default tab size
(setq default-tab-width 1)

;;
;; modeline
;;

(require 'mood-line)
(mood-line-mode)

(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)

;;(set-face-background 'header-line "#18191f")
;;(set-face-background 'header-line "#444858")
(set-face-background 'header-line "#393c49")


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

(setq dashboard-items '((recents  . 8)))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)

(setq dashboard-set-navigator t)

;; Format: "(icon title help action face prefix suffix)"
(setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-faicon "server" :height 1.1 :v-adjust 0.0)
         "GoDCApp"
         "Open GoDCApp Repo"
         (lambda (&rest _) (find-file "/Users/evincent/EN/efa-branch/GoDCApp")))
         (,(all-the-icons-faicon "wifi" :height 1.1 :v-adjust 0.0)
         "networking-extreme"
         "Open OS Plugin Repo"
         (lambda (&rest _) (find-file "/Users/evincent/EN/efa-branch/networking-extreme")))
         (,(all-the-icons-faicon "check-circle" :height 1.1 :v-adjust 0.0)
         "todo.org"
         "Open todo.org"
         (lambda (&rest _) (find-file "/Users/evincent/todo.org")))
         (,(all-the-icons-faicon "desktop" :height 1.1 :v-adjust 0.0)
         "emacs.d"
         "Open Emacs Config"
         (lambda (&rest _) (find-file "/Users/evincent/.emacs.d")))
         )))


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
;; Duplicate Line
;;

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-d") 'duplicate-line)

;;
;; Magit
;;

;; Find Commits Affecting a Function M-x magit-log-trace-definition
;; Find Commits Affecting a File M-x magit-log-buffer-file

;; Magit Ediff
(setq magit-ediff-dwim-show-on-hunks t)
(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

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
		treemacs-width 25
		treemacs-indentation 2
		treemacs-indentation-string " "
  		treemacs-collapse-dirs (if (executable-find "python") 3 0)
		treemacs-silent-refresh t
		treemacs-silent-filewatch t
		treemacs-sorting 'alphabetic-asc
		treemacs-indentation 1
		)))

(treemacs-resize-icons 15)
(setq treemacs-follow-mode t)

;;
;; Linum
;;
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d \u2502")
(set-face-background 'fringe nil)

;; Highlight current line number
(require 'hlinum)
(hlinum-activate)

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

;; Open zsh terminal in new window
(defun term-other-window ()
  "Open `term` in a new window."
  (interactive)
  (let ((buf (term "/bin/zsh")))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

;; Open terminal in new window
;; and execute watch command
(defun term-watch-other-window (command)
  "Open `term` in a new window and execute watch command."
  (interactive "sWatch: ")
  (term-run-shell-command (concat "watch " command)))

;; Bind Open term to C-t
(global-set-key (kbd "C-t") 'term-other-window)

;; Bind Open term watch command to C-t
(global-set-key (kbd "M-t") 'term-watch-other-window)

;; Bind Run Shell Command to C-p
(global-set-key (kbd "C-p") 'shell-command)


;;
;; YAML Mode
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;
;; HELM
;;
(require 'helm)

;; To make helm-mode start with Emacs init file
(helm-mode 1)

;; To bind to M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; Helm Fuzzy Search
(setq helm-completion-style 'emacs)
(setq completion-styles `(basic partial-completion emacs22 initials
                                ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

;; Fix for helm-rg to work
(setq  helm-rg-ripgrep-executable "/usr/local/bin/rg")
(setq helm-rg-default-directory 'git-root)


;;
;; bm
;;
(require 'bm)

(global-set-key (kbd "C-c b") 'bm-toggle)
(global-set-key (kbd "C-c ]")   'bm-next)
(global-set-key (kbd "C-c [") 'bm-previous)

;;
;; Avy
;;

;; Global avy find char bind to M-s c
(global-set-key (kbd "M-s c") #'avy-goto-char)

;; Global avy find line bind to M-s l
(global-set-key (kbd "M-s l") #'avy-goto-line)

;;
;; Ace Jump Buffer
;;

;; Global ace-jump-buffer-other-window bind to M-s b
(global-set-key (kbd "M-s b") #'ace-jump-buffer-other-window)

;;
;; Org Mode
;;

;; Autocomplete org mode
(require 'org-ac)
(org-ac/config-default)

;;
;; Code
;;

;;
;; RUST
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
  :init (add-hook 'python-mode-hook 'company-mode)
  :config (setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
          (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'lsp)
  (setq lsp-file-watch-threshold 10000))

(use-package lsp-ui
  :ensure t)

(use-package company-lsp
  :ensure t)

(use-package blacken
  :ensure t
  :hook ((python-mode . blacken-mode))
  :config
  (setq blacken-line-length 80)
  (setq blacken-skip-string-normalization t))

(defun py-save-commands ()
  "Linter on Save for Python"
  (interactive)
  (yapfify-buffer)
  (pyimpsort-buffer))

;; Enable python yasnippets
;; (add-hook 'python-mode-hook 'yas-minor-mode)
;; (add-hook 'python-mode-hook 'company-yas)

;; M-Ret to go to definition
(add-hook 'python-mode-hook
          (lambda () (local-set-key (kbd "M-RET") #'lsp-find-definition)))

;; yapf and pyimport on C-c C-s
(add-hook 'python-mode-hook
          (lambda () (interactive) (local-set-key (kbd "C-c C-s") 'py-save-commands)))

;; flake8 checks
;; (setq flycheck-python-flake8-executable "flake8")
;; (flycheck-select-checker 'python-flake8)
;; (flycheck-mode t)

;; Hs Minor mode
(use-package hideshow
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;;
;; Bash
;;
(use-package lsp-mode
  :commands lsp
  :hook
  (sh-mode . lsp))

;;
;; Go
;;

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(add-to-list 'exec-path "~/.go/bin")
(add-to-list 'exec-path "/usr/local/bin/")

;; Init the auto complete modules
(require 'go-autocomplete)

(defun auto-complete-for-go ()
  (auto-complete-mode 1)
  (yas-minor-mode-on))

(require 'smartparens-config)

(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'yas-minor-mode)
(add-hook 'go-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))


;;(add-hook 'go-mode-hook 'auto-complete-for-go)

;; Just to make sure go tools are enabled
(add-to-list 'exec-path "~/go/bin")

;; Automatically format code on save
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;;
;; Zig
;;

;; Setup lsp-mode as desired.
;; See https://emacs-lsp.github.io/lsp-mode/page/installation/ for more information.
(require 'lsp-mode)

;; Either place zls in your PATH or add the following:
(setq lsp-zig-zls-executable "/Users/evincent/zls")
