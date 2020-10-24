(setq package-user-dir "~/repos/customacs/packages")
(setq inhibit-startup-message t)
(scroll-bar-mode -1)    ;; disable vis scrollbar
(tool-bar-mode -1)      ;; disable the toolbar
(tooltip-mode -1)       ;; disable tooltip
(set-fringe-mode 10)    ;; 'breathing' room
(menu-bar-mode -1)
;;(setq visible-bell t)   ;; visual bell
(setq-default indent-tabs-mode nil) ;; uses spaces and not tabs

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
(menu-bar--display-line-numbers-mode-relative)
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
  (setq which-key-idle-delay 0.1)
  )

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
         ("C-r" . 'counsel-minibuffer-history)
	 )
  )

;; more better help menus
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )
  

(defun custo-evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode
                  ansi-term-mode))
    (add-to-list 'evil-emacs-state-modes mode)
    )
  )

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

;; better evil stuff
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; better key binding
(use-package general
  :config
  ;;(setq general-default-prefix "SPC")
  (general-create-definer custo/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (custo/leader-keys
    "TAB" '(evil-switch-to-windows-last-buffer :which-key "switch to previous buffer")
    "a" '(:ignore t :which-key "apps")
    "a e" '(eww :which-key "eww")
    "t" '(:ignore t :which-key "toggles")
    "t t" '(counsel-load-theme :which-key "choose theme")
    "q" '(:ignore q :which-key "quit")
    "q q" '(save-buffers-kill-emacs :which-key "save and quit")
    "q Q" '(kill-emacs :which-key "quit no-save")
    "f" '(:ignore f :which-key "file")
    "f f" '(counsel-find-file :which-key "find file")
    "f s" '(save-buffer :which-key "save file")
    "s" '(:ignore f :which-key "search")
    "s s" '(swiper :which-key "search buffer")
    "b" '(:ignore f :which-key "buffer")
    "b b" '(counsel-switch-buffer :which-key "switch buffers")
    "b d" '(kill-current-buffer :which-key "destroy buffer")
    "b i" '(ibuffer-list-buffers :which-key "ibuffer")
    "w" '(:ignore w :which-key "window")
    "w w" '(other-window :which-key "other window")
    "w d" '(delete-window :which-key "delete window")
    "w h" '(evil-window-vsplit :which-key "split window horizontally")
    "w v" '(evil-window-split :which-key "delete window vertically")
    )
  )

;; hydra to build menus
(use-package hydra
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t)
    )
  ;; since custo leader keys are defined, we can bind to them now :D
  (custo/leader-keys
    "t s" '(hydra-text-scale/body :which-key "scale text")
    )
  )

;; setup project management
(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-completion-system 'ivy)
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode)
  (custo/leader-keys
    "p" '(projectile-command-map :which-key "projectile"))
  )
  
;; add counsel capability
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode)
  )

;; make dired more like ranger
(use-package ranger
  :config
  (custo/leader-keys
   "f d" '(ranger :which-key "file directory")
   )
  )

;; magit
(use-package magit
  ;;:commands (magit-status magit-get-current-branch)
  ;;:custom
  ;;(magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (custo/leader-keys
    "g" '(:ignore g :which-key "magit")
    "g s" '(magit-status :whick-key "magit status")
    )

;; evil keys with magit  
(use-package evil-magit
  :after magit
  )

;; magit integration with github and gitlab
(use-package forge
  :after magit
  )
