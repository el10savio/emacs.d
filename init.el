;; Make startup faster by reducing the frequency of garbage
;; collection.  The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; The rest of the init file.

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Move all backup files to "~/.emacs-trash"
(setq backup-directory-alist '((".*" . "~/.emacs-trash")))

;; Enable Global Clipboard Use
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-selection-value)
(xclip-mode 1)

;; Auto Refresh All The Buffers From Disk 
(global-auto-revert-mode t)

;; Add MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))
;; (eval-and-compile
;;   (setq use-package-always-ensure t
;;         use-package-expand-minimally t))

(require 'use-package)

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
	 '(sml-mode geiser racket-mode rainbow-delimiters esup xcscope terraform-mode noccur projectile-ripgrep ripgrep peep-dired avy-zap avy selectric-mode nlinum wgrep fzf rustic xclip origami flycheck-inline engine-mode hideshow-org godoctor dired-subtree fancy-dabbrev bats-mode insert-shebang lispy dockerfile-mode gitignore-mode peek-mode load-theme-buffer-local gh-md grip-mode fira-code-mode duplicate-thing term-run org-ac smartparens writegood-mode howdoyou howdoi windresize bm yaml-mode ecb go-imenu imenu-list magit-delta shell-pop rg sr-speedbar kaolin-themes markdown-mode markdown-mode+ ace-jump-buffer vc-msg git-lens company-go exec-path-from-shell go-imports autopair go-autocomplete go-complete go-mode transpose-frame mood-line marginalia dired-filter dashboard multiple-cursors helm-ag ag perspective diff-hl which-key git-gutter doom-themes yasnippet-classic-snippets yasnippet-snippets company-lsp lsp-ui lsp-mode magit all-the-icons helm-rg helm-projectile helm company))
 '(recentf-mode t)
 '(warning-suppress-types '((use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:background "#50fa7b"))))
 '(git-gutter:deleted ((t (:background "#ff79c6"))))
 '(git-gutter:modified ((t (:background "#f1fa8c")))))

;;
;; Keybindings
;;

;; quickly open up init.el in new window
;;;###autoload
(defun find-user-init-file ()
	"Edit the `user-init-file', in another window."
	(interactive)
	(find-file-other-window user-init-file))

;; Bind C-c i to open init file in new window
(global-set-key (kbd "C-c i") #'find-user-init-file)

;; Bind C-k to kill whole line
(global-set-key (kbd "C-k") #'kill-whole-line)

;; Bind C-c return to eval-buffer
(global-set-key (kbd "C-c RET") 'eval-buffer)

;; Bind s-> to end-of-buffer
(global-set-key (kbd "s->") #'end-of-buffer)

;; Bind s-< to begining-of-buffer
(global-set-key (kbd "s-<") #'beginning-of-buffer)

;; Bind C-c p to package install
(global-set-key (kbd "C-c p") #'package-install)

;; Bind C-c f to fzf find file
(global-set-key (kbd "C-c f") #'fzf-projectile)

;; Bind C-/ to comment-or-uncomment-region
(global-set-key (kbd "C-/") #'comment-or-uncomment-region)

;; Bind C-c s to Global search
(global-set-key (kbd "C-c s") #'ripgrep-regexp)

;; Local search bind to C-c o
(global-set-key (kbd "C-c o") #'helm-occur)

;; Local replace bind to C-c r
(global-set-key (kbd "C-c r") #'replace-string)

;; kill all running processes by default on exit
(setq confirm-kill-processes nil)

;; Set up which-key
(which-key-mode 1)
(which-key-setup-side-window-right)

;;
;; God Mode
;;

;; (require 'god-mode)
;; (god-mode)
;; (global-set-key (kbd "") #'god-local-mode)

;;
;; Display Settings
;;

(use-package doom-themes)

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

;; (load-theme 'doom-vibrant t)
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
										:family "Fira Code Medium"
										:height 120
										:weight 'normal
										:width 'normal)

;; Default tab size
(setq default-tab-width 1)
(setq-default tab-width 2)

;;
;; modeline
;;

(use-package mood-line
	:defer t
	:init
	(mood-line-mode)
	(setq-default header-line-format mode-line-format)
	(setq-default mode-line-format nil)
	(set-face-background 'header-line "#393c49")
	(blink-cursor-mode 1)
	(setq-default cursor-type 'bar)
	(set-cursor-color "#cccccc")
	(setq ring-bell-function 'ignore)
)

;;
;; Dashboard
;;

(use-package dashboard
	:defer t
	:init
	(dashboard-setup-startup-hook)
  (setq dashboard-init-info "Hi Elton, Welcome back to Emacs")
	(setq dashboard-startup-banner 'logo)
	(setq dashboard-items '((recents  . 8)))
	(setq dashboard-set-heading-icons t)
	(setq dashboard-set-file-icons t)
	(setq dashboard-set-navigator t)
)

;; Enable rainbow delimiters
(use-package rainbow-delimiters
	:defer t
	:init
	(rainbow-delimiters-mode 1)
	(show-paren-mode 1)
)

;;
;; git-gutter
;;
(use-package git-gutter
		:defer t
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
;; Duplicate Line/Region
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

;; Bind Duplicate Line to C-d
(global-set-key (kbd "C-d") 'duplicate-line)

(defun duplicate-region-above()
	"Duplicate the selected region above the cursor."
	(interactive) (duplicate-region "above"))

(defun duplicate-region-below()
	"Duplicate the selected region below the cursor."
	(interactive) (duplicate-region "below"))

(defun duplicate-region(direction)
	"Perform the region duplication based on the direction given."
	(duplicate-region-extend-region-to-line-boundaries)
	(let
			((p1 (region-beginning))
			 (p2 (region-end)))
		(copy-region-as-kill p1 p2)
		(goto-char (region-end))
		(newline)
		(yank)
		(duplicate-region-highlight-region direction p1 p2)))

(defun duplicate-region-highlight-region (direction p1 p2)
	"Based on DIRECTION, activate the intended region at original P1 and P2."
	(let
			((region-length (- p2 p1)))
		(cond
		 ((equal direction "above")
			(goto-char p1)
			(push-mark p2))
		 ((equal direction "below")
			(goto-char (+ p2 1))
			(push-mark (+ (point) (+ region-length 1))))))
(setq deactivate-mark nil))

(defun duplicate-region-extend-region-to-line-boundaries ()
	"Given a selection or line, activate a region that extends to the beginning & end of each line in the region."
	(let
			((p1 (region-beginning))
			 (p2 (region-end)))
		(if mark-active
				(progn
					(goto-char p1)
					(push-mark (line-beginning-position))
					(goto-char p2)
					(goto-char (line-end-position)))
			(push-mark (line-beginning-position))
			(goto-char (line-end-position)))))

;;
;; Magit
;;

;; Find Commits Affecting a Function M-x magit-log-trace-definition
;; Find Commits Affecting a File M-x magit-log-buffer-file

;; gitignore-mode
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . gitignore-mode))

;; Magit Ediff
(setq magit-ediff-dwim-show-on-hunks t)
(add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))

;;
;; nLinum
;;

(add-hook 'prog-mode-hook 'nlinum-mode)
(setq nlinum-format "%4d \u2502")
(set-face-background 'fringe nil)

;;
;; Dired
;;

(use-package dired-subtree
	:defer
	:after dired
	:config
	(bind-key "<tab>" #'dired-subtree-cycle dired-mode-map)
)

;;
;; Treemacs
;;

;; (use-package treemacs
;;   :defer t
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;   (progn
;;     (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
;;           treemacs-deferred-git-apply-delay        0.5
;;           treemacs-directory-name-transformer      #'identity
;;           treemacs-display-in-side-window          t
;;           treemacs-eldoc-display                   'simple
;;           treemacs-file-event-delay                5000
;;           treemacs-file-extension-regex            treemacs-last-period-regex-value
;;           treemacs-file-follow-delay               0.2
;;           treemacs-file-name-transformer           #'identity
;;           treemacs-follow-after-init               t
;;           treemacs-expand-after-init               t
;;           treemacs-find-workspace-method           'find-for-file-or-pick-first
;;           treemacs-git-command-pipe                ""
;;           treemacs-goto-tag-strategy               'refetch-index
;;           treemacs-header-scroll-indicators        '(nil . "^^^^^^")
;;           treemacs-hide-dot-git-directory          t
;;           treemacs-indentation                     1
;;           treemacs-indentation-string              " "
;;           treemacs-is-never-other-window           nil
;;           treemacs-max-git-entries                 5000
;;           treemacs-missing-project-action          'ask
;;           treemacs-move-forward-on-expand          nil
;;           treemacs-no-png-images                   nil
;;           treemacs-no-delete-other-windows         t
;;           treemacs-project-follow-cleanup          nil
;;           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
;;           treemacs-position                        'left
;;           treemacs-read-string-input               'from-child-frame
;;           treemacs-recenter-distance               0.1
;;           treemacs-recenter-after-file-follow      nil
;;           treemacs-recenter-after-tag-follow       nil
;;           treemacs-recenter-after-project-jump     'always
;;           treemacs-recenter-after-project-expand   'on-distance
;;           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
;;           treemacs-show-cursor                     nil
;;           treemacs-show-hidden-files               t
;;           treemacs-silent-filewatch                nil
;;           treemacs-silent-refresh                  nil
;;           treemacs-sorting                         'alphabetic-asc
;;           treemacs-select-when-already-in-treemacs 'move-back
;;           treemacs-space-between-root-nodes        t
;;           treemacs-tag-follow-cleanup              t
;;           treemacs-tag-follow-delay                1.5
;;           treemacs-text-scale                      nil
;;           treemacs-user-mode-line-format           nil
;;           treemacs-user-header-line-format         nil
;;           treemacs-wide-toggle-width               70
;;           treemacs-width                           35
;;           treemacs-width-increment                 1
;;           treemacs-width-is-initially-locked       t
;;           treemacs-workspace-switch-cleanup        nil
;; 					)
;; 		)
;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (treemacs-fringe-indicator-mode 'always)
;; )

;; (use-package treemacs-projectile
;;   :after (treemacs projectile))

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once))

;; (use-package treemacs-magit
;;   :after (treemacs magit))

;;
;; Marginalia
;;

;; Enable richer annotations using the Marginalia package
(use-package marginalia
	:defer t
	:init
	(marginalia-mode)
	(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
)

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

;; Change term to buffer to copy text
;; M-x term-line-mode (C-c C-j)

;;
;; YAML Mode
;;

(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;
;; Evil Mode
;;

;; (use-package evil)
;; (evil-mode 1)

;;
;; HELM
;;
(use-package helm)

;; To make helm-mode start with Emacs init file
(helm-mode 1)

;; To bind to M-x
(global-set-key (kbd "M-x") 'helm-M-x)

;; Helm Fuzzy Search
(setq helm-completion-style 'emacs)
(setq completion-styles `(basic partial-completion emacs22 initials ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))

;; Fix for helm-rg to work
(setq  helm-rg-ripgrep-executable "/usr/local/bin/rg")
(setq helm-rg-default-directory 'git-root)

;;
;; bm
;;

(use-package bm
	:defer t
	:init
	(global-set-key (kbd "C-c b") 'bm-toggle)
	(global-set-key (kbd "C-c ]") 'bm-next)
	(global-set-key (kbd "C-c [") 'bm-previous)
)

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
(use-package org-ac)
(org-ac/config-default)

;;
;; Engine Mode
;;

;; Enable engine mode
(use-package engine-mode
	:defer t
	:init
	(engine-mode t)
	;; Set firefox as browser
	;; (setq engine/browser-function 'browse-url-firefox)

	;; Define duckduckgo Search
	(defengine duckduckgo
		"https://duckduckgo.com/?q=%s"
		:keybinding "d")

	;; Bind C-c = to engine mode duckduckgo search
	(global-set-key (kbd "C-c =") #'engine/search-duckduckgo)

	;; Set browser
	(setq browse-url-browser-function 'browse-url-chrome)
	(setq browse-url-chrome-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")

	;; Bind M-= to browse-url
	(global-set-key (kbd "M-=") #'browse-url)
)

;;
;; Code
;;

;;
;; Bash
;;

(use-package lsp-mode
	:defer t
	:commands lsp
	:hook
	(sh-mode . lsp))

;;
;; Rust
;;

(use-package rustic
	:defer t
	:init
	(setq rust-format-on-save t)
)

;;
;; Go
;;

(setq exec-path (append exec-path '("/Users/e.savio/go/bin")))

(setq company-idle-delay t)

(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

(defun my-go-mode-hook ()
	(local-set-key (kbd "M-.") 'godef-jump))
	(set (make-local-variable 'company-backends) '(company-go))

(use-package cl)

(add-hook 'go-mode-hook 'my-go-mode-hook)
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'yas-global-mode)
(add-hook 'go-mode-hook 'flycheck-mode)

(with-eval-after-load 'flycheck
	(add-hook 'flycheck-mode-hook #'flycheck-inline-mode))
(put 'upcase-region 'disabled nil)

;;
;; Startup profiling
;;

;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
 (lambda ()
  (message "Emacs ready in %s with %d garbage collections."
  (format "%.2f seconds"
  (float-time (time-subtract after-init-time before-init-time)))
  gcs-done))
)
