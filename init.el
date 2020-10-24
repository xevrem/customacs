(setq package-user-dir "~/repos/customacs/packages")
(setq inhibit-startup-message t)
(scroll-bar-mode -1)    ;; disable vis scrollbar
(tool-bar-mode -1)      ;; disable the toolbar
(tooltip-mode -1)       ;; disable tooltip
(set-fringe-mode 10)    ;; 'breathing' room
(menu-bar-mode -1)
;;(setq visible-bell t)   ;; visual bell

(set-face-attribute 'default 'nil :font "Fira Code" :height 180)

;; make escape qui prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; init srouces
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; init use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; better line info
(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		ansi-term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; packages
(use-package swiper)
(use-package counsel)
(use-package ivy
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; load all-the-icons only if in GUI mode
;; and install them if not present
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

;; add a better modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; enable better themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italics t)
  (load-theme 'doom-palenight t))

;; make it easier to keep track of parens and braces
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; setup a special menu that tells us what keys are available
;; based on the current mode, set pop-up delay to 0.1s
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq whick-key-idle-delay 0.1))

;; provide more helpful info in ivy panels
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; use counsel for completions over
;; default M-x and some other things
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; more better help menus
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
  

(defun custo-evil-hook ()
  (dolist (mode '(custom-mode
		  eshell-mode
		  git-rebase-mode
		  term-mode
		  ansi-term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; the very best mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  ;;:hook (evil-mode . custo-evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
 )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; better key binding
(use-package general)
 ;; :config
 ;; (setq general-default-prefix "SPC")
 ;; (general-create-definer custo-leader-keys
 ;;   ;;:keymaps '(normal insert visual emacs)
 ;;   :prefix "SPC"
 ;;   ;;:global-prefix "C-SPC"
 ;;   )

 ;; (custo-leader-keys
 ;;  "t" '(:ignore t :which-key "toggles")
 ;;  "t t" '(counsel-load-theme :which-key "choose theme")))
