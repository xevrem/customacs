(setq package-user-dir "~/repos/customacs/packages")
(setq inhibit-startup-message t)
(scroll-bar-mode -1)    ;; disable vis scrollbar
(tool-bar-mode -1)      ;; disable the toolbar
(tooltip-mode -1)       ;; disable tooltipt
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

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package doom-themes)

(load-theme 'doom-palenight)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq whick-key-idle-delay 0.1))

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([rema describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
  
