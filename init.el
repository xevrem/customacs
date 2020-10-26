;; (setq package-user-dir "~/repos/customacs/packages")
(setq inhibit-startup-message t)
(scroll-bar-mode -1)    ;; disable vis scrollbar
(tool-bar-mode -1)      ;; disable the toolbar
(tooltip-mode -1)       ;; disable tooltip
(set-fringe-mode 10)    ;; 'breathing' room
(menu-bar-mode -1)
(setq visible-bell t)   ;; visual bell
(setq-default indent-tabs-mode nil) ;; uses spaces and not tabs

;; for performance
(setq gc-cons-threshold 100000000)
;; (setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(set-face-attribute 'default 'nil :font "Fira Code" :height 180)

;; make escape qui prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; better line info
(column-number-mode)
(global-display-line-numbers-mode t)
(menu-bar--display-line-numbers-mode-relative)
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                ansi-term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; setup straight for package management
(setq straight-use-package-by-default t)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

;;
;; PACKAGE CONFIGURATION
;;

;; ivy trio
(use-package swiper)
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

(use-package prescient)
(use-package ivy-prescient
 :init (ivy-prescient-mode 1))

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

;; yasnippet
(use-package yasnippet
  :hook
  '((text-mode-hook . yas-minor-mode-on)
    (prog-mode-hook . yas-minor-mode-on)
    (conf-mode-hook . yas-minor-mode-on)
    (snippet-mode-hook . yas-minor-mode-on)
    )
  )

(use-package smartparens
  :init (smartparens-global-mode 1)
  :config
  ;; don't interfere with yasnippets
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)
  )


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
  (general-create-definer custo/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer custo/local-leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m")
  )



;; define default keybinds
(custo/leader-key
    "TAB" '(evil-switch-to-windows-last-buffer :which-key "switch to previous buffer")
    "a" '(:ignore t :which-key "apps")
    "a e" '(eww :which-key "eww")
    "b" '(:ignore b :which-key "buffer")
    "b b" '(counsel-switch-buffer :which-key "switch buffers")
    "b d" '(kill-current-buffer :which-key "destroy buffer")
    "b i" '(ibuffer-list-buffers :which-key "ibuffer")
    "f" '(:ignore f :which-key "file")
    "f f" '(counsel-find-file :which-key "find file")
    "f s" '(save-buffer :which-key "save file")
    "m" '(:ignore m :which-key "local-leader")
    "q" '(:ignore q :which-key "quit")
    "q q" '(save-buffers-kill-emacs :which-key "save and quit")
    "q Q" '(kill-emacs :which-key "quit no-save")
    "s" '(:ignore f :which-key "search")
    "s b" '(swiper :which-key "search buffer")
    "s p" '(counsel-projectile-rg :which-key "search project")
    "t" '(:ignore t :which-key "toggles")
    "t t" '(counsel-load-theme :which-key "choose theme")
    "w" '(:ignore w :which-key "window")
    "w w" '(other-window :which-key "other window")
    "w d" '(delete-window :which-key "delete window")
    "w o" '(delete-other-windows :which-key "delete other windows")
    "w h" '(evil-window-vsplit :which-key "split window horizontally")
    "w v" '(evil-window-split :which-key "delete window vertically")
    )

(custo/local-leader-key
  "=" '(:ignore = :which-key "format")
  "d" '(:ignore d :which-key "documentation")
  "l" '(:ignore l :which-key "lsp")
  "i" '(:ingore i :which-key "insert")
  "c" '(:ignore c :which-key "comment")
  "c c" '(comment-line :which-key "comment line")
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
  (custo/leader-key
    "t s" '(hydra-text-scale/body :which-key "scale text")
    )
  )

(use-package undo-tree
  :init (global-undo-tree-mode 1)
  :config
  (defhydra hydra-undo-tree (:timeout 4)
    "undo / redo"
    ("u" undo-tree-undo "undo")
    ("r" undo-tree-redo "redo")
    ("t" undo-tree-visualize "undo-tree visualize" :exit t)
    )
  (custo/leader-key
    "u" '(hydra-undo-tree/body :which-key "undo/redo")
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
  (custo/leader-key
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
  (custo/leader-key
   "f d" '(ranger :which-key "file directory")
   )
  )

;; magit
(use-package magit
  ;;:commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (custo/leader-key
    "g" '(:ignore g :which-key "magit")
    "g s" '(magit-status :whick-key "magit status")
    )
  )

;; evil keys with magit  
(use-package evil-magit
  :after magit
  )

;; magit integration with github and gitlab
(use-package forge
  :after magit
  )

;; completion mini buffers
(use-package company
  :hook
  (after-init-hook . global-company-mode)
  :config
  (setq company-backends '(company-capf))
  )

(use-package company-prescient
  :init (company-prescient-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; better javascript mode
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq js-indent-level 2)
  )

;; teach js2-mode how to jsx
(use-package rjsx-mode
  :diminish
  :after js2-mode
  :config
  ;; (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("\\js\\'" . rjsx-mode))
  )

;; (use-package js-doc
;;   :after js2-mode
;;   :config
;;   (custo/local-leader-key
;;    "d" '(:ignore d :which-key "jsdoc")
;;    "d f" '(js-doc-insert-function-doc-snippet :which-key "jsdoc function")
;;    )
;;   )

(use-package js-react-redux-yasnippets
  :after yasnippet
  :config
  (custo/local-leader-key
    "i s" '(yas-insert-snippet :which-key "insert snippet")
    )
  )

;; format js and jsx
(use-package prettier
  :after js2-mode
  :config
  (custo/local-leader-key
    "= =" '(prettier-prettify :which-key "format with prettier")
    )
  )

(use-package web-mode)

;; lsp-mode
(use-package lsp-mode
  :hook ((js-mode . lsp)
         (scss-mode . lsp)
         (web-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (setq lsp-completion-provider :capf)
  (custo/local-leader-key
    "l f" '(:ignore f :which-key "find")
    "l f r" '(lsp-ui-peek-find-references :which-key "find references")
    "l f d" '(lsp-find-definition :which-key "find definition")
    "l o" '(lsp-ui-imenu :which-key "overview")
    "l r" '(lsp-rename :which-key "rename")
    "l =" '(:ignore = :which-key "format")
    "l = l" '(lsp-format-buffer :which-key "format with lsp")
    )
  )

;; prettier lsp
(use-package lsp-ui
  :commands lsp-ui-mode
  )

;; better lsp
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  )

;; error checking
(use-package flycheck
  ;; :commands flycheck-list-errors flycheck-buffer
  :hook (after-init-hook . global-flycheck-mode)
  :config
  (custo/local-leader-key
    "e" '(:ignore t :which-key "errors")
    "e l" '(flycheck-list-errors :which-key "list errors")
    )
  )


;; org stuff
(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "todo.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  )

(use-package org-superstar
  :hook
  (org-mode-hook . org-superstar-mode)
  )
