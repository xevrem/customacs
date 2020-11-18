;; (setq package-user-dir "~/repos/customacs/packages")

;; update path variables because stuff may not be set yet
;; (let '(path
;;        (shell-command-to-string "/usr/bin/env zsh -c \"source ~/.zshrc && env | grep \\^PATH | tr -d PATH=\""))
;;        ;; (shell-command-to-string "/usr/bin/env fish -c \"source && env | grep \\^PATH | tr -d PATH=\""))
;;   (setenv "PATH" path)
;;   (setq exec-path
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)
;;         ))

;; for performance
;; (setq gc-cons-threshold (* 100 1024 1024))
;; what doom does, but we want lower but this is also an option
;; (setq gc-cons-threshold most-positive-fixnum)

;; read process output in 1mb chunks
;; (setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq inhibit-startup-message t)
(scroll-bar-mode -1)    ;; disable vis scrollbar
(tool-bar-mode -1)      ;; disable the toolbar
(tooltip-mode -1)       ;; disable tooltip
(set-fringe-mode 10)    ;; 'breathing' room
(menu-bar-mode -1)      ;; turn off menus
(setq ring-bell-function 'ignore) ;; disable all visual and audible bells
(setq-default indent-tabs-mode nil) ;; uses spaces and not tabs
(setq create-lockfiles nil)

;; adjust the startup size of emacs
(setq initial-frame-alist
      `((width . 120) ; chars
        (height . 45) ; lines
        )
      )

;; better line info
(column-number-mode) ;; show column info
(global-display-line-numbers-mode t) ;; display line numbers to the left
(menu-bar--display-line-numbers-mode-relative) ;; make those line numbers relative

;; turn of line numbers in the following modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                ansi-term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



;; create some font size defaults
;; may need to customize
(defvar custo/default-font-size 93)
(defvar custo/default-variable-font-size 93)


;; if in macOS, set size appropriately
;; otherwise assume linux
(if (eq system-type 'darwin)
    (progn
      (setq custo/default-font-size 192)
      (setq custo/default-variable-font-size 192)
      ;; set default font
      (set-face-attribute 'default 'nil :font "FiraCode NF" :height custo/default-font-size)
      ;; Set the fixed pitch face
      (set-face-attribute 'fixed-pitch nil :font "FiraCode NF" :height custo/default-font-size)
      )
    (progn
      ;; set default font
      (set-face-attribute 'default 'nil :font "FiraCode Nerd Font" :height custo/default-font-size)
      ;; Set the fixed pitch face
      (set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height custo/default-font-size)
      )
    )

;; Set the variable pitch face which is the same for mac and linux
(set-face-attribute 'variable-pitch nil :font "Arial" :height custo/default-variable-font-size :weight 'regular)


;; setup straight for package management, its much better than use-package
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

;; get shell variables
(use-package exec-path-from-shell
  ;; :init
  ;; (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    )
  )

;; restart
(use-package restart-emacs)

;; ivy trio
(use-package swiper)
;; use counsel for completions over
;; default M-x and some other things
(use-package counsel
  ;; :bind (("M-x" . counsel-M-x)
  ;;        ("C-x b" . counsel-ibuffer)
  ;;        ("C-x C-f" . counsel-find-file)
  ;;        :map minibuffer-local-map
  ;;        ("C-r" . 'counsel-minibuffer-history)
  ;;        )
  )
(use-package ivy
  ;; :bind (("C-s" . swiper)
  ;;        :map ivy-minibuffer-map
  ;;        ("TAB" . ivy-alt-done)
  ;;        ("C-l" . ivy-alt-done)
  ;;        ("C-j" . ivy-next-line)
  ;;        ("C-k" . ivy-previous-line)
  ;;        :map ivy-switch-buffer-map
  ;;        ("C-k" . ivy-previous-line)
  ;;        ("C-l" . ivy-done)
  ;;        ("C-d" . ivy-switch-buffer-kill)
  ;;        :map ivy-reverse-i-search-map
  ;;        ("C-k" . ivy-previous-line)
  ;;        ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-sort-max-size 7500
        ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        ;; disable magic slash on non-match
        ivy-magic-slash-non-match-action nil
        ;; don't show recent files in switch-buffer
        ivy-use-virtual-buffers nil
        ;; ...but if that ever changes, show their full path
        ivy-virtual-abbreviate 'full
        ;; don't quit minibuffer on delete-error
        ivy-on-del-error-function #'ignore
        ;; enable ability to select prompt (alternative to `ivy-immediate-done')
        ivy-use-selectable-prompt t)
  (ivy-mode 1)
  )

(use-package prescient)
(use-package ivy-prescient
  :init (ivy-prescient-mode 1)
  :config
  ;;good ideas from doom:
  (defun +ivy-prescient-non-fuzzy (str)
    (let ((prescient-filter-method '(literal regexp)))
      (ivy-prescient-re-builder str)))
  (let ((standard-search-fn
         #'+ivy-prescient-non-fuzzy)
        (alt-search-fn
         ;; Ignore order for non-fuzzy searches by default
         #'ivy--regex-ignore-order))
    (setq ivy-re-builders-alist
          `((counsel-rg     . ,standard-search-fn)
            (swiper         . ,standard-search-fn)
            (swiper-isearch . ,standard-search-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))
  )

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
  :commands (yas-minor-mode-on
             yas-lookup-snippet)
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
  

(defun custo/evil-hook ()
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
  :hook (evil-mode . custo/evil-hook)
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
  (general-auto-unbind-keys)
  (general-create-definer custo/leader-key
    :states '(normal insert visual emacs)
    ;;:keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer custo/local-leader-key
    :states '(normal insert visual emacs)
    ;;:keymaps '(normal insert visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m")
  )



;; define default keybinds
(custo/leader-key
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "switch to previous buffer")
  ":" '(counsel-M-x :which-key "M-x")
  "a" '(:ignore t :which-key "apps")
  "a e" '(eww :which-key "eww")
  "a t" '(ansi-term :which-key "terminal")
  "b" '(:ignore t :which-key "buffer")
  "b b" '(counsel-switch-buffer :which-key "switch buffers")
  "b d" '(kill-current-buffer :which-key "destroy buffer")
  "b i" '(counsel-ibuffer :which-key "ibuffer")
  "c" '(:ignore t :which-key "cursor")
  "c c" '(comment-line :which-key "comment line")
  "f" '(:ignore f :which-key "file")
  "f f" '(counsel-find-file :which-key "find file")
  "f s" '(save-buffer :which-key "save file")
  "h" '(:ignore t :which-key "custo help")
  "h s" '(:ignore t :which-key "straight")
  "h s p" '(straight-pull-all :which-key "straight pull packages")
  "h s b" '(straight-rebuild-all :which-key "straight build packages")
  "m" '(:ignore t :which-key "local-leader")
  "o" '(:ignore t :which-key "org")
  "q" '(:ignore t :which-key "quit")
  "q q" '(save-buffers-kill-emacs :which-key "save and quit")
  "q Q" '(kill-emacs :which-key "quit no-save")
  "q r" '(restart-emacs :which-key "restart emacs")
  "s" '(:ignore t :which-key "search")
  "s s" '(swiper :which-key "search buffer")
  "s p" '(counsel-projectile-rg :which-key "search project")
  "t" '(:ignore t :which-key "toggles")
  "t t" '(toggle-truncate-lines :which-key "toggle truncate lines")
  "t T" '(counsel-load-theme :which-key "choose theme")
  "w" '(:ignore t :which-key "window")
  "w w" '(other-window :which-key "other window")
  "w d" '(delete-window :which-key "delete window")
  "w o" '(delete-other-windows :which-key "delete other windows")
  "w h" '(evil-window-vsplit :which-key "add window horizontally")
  "w v" '(evil-window-split :which-key "add window vertically")
  )

(custo/local-leader-key
  ;; :keymaps 'prog-mode
  "=" '(:ignore t :which-key "format")
  "d" '(:ignore t :which-key "documentation")
  "g" '(:ignore t :which-key "goto")
  "i" '(:ingore t :which-key "insert")
  )

;; hydra to build menus
(use-package hydra
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" (text-scale-adjust 0.1) "in")
    ("k" (text-scale-adjust -0.1) "out")
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

;; (use-package multiple-cursors
(use-package evil-mc
  :init
  (global-evil-mc-mode 1)
  :config
  (custo/leader-key
    "c n" '( evil-mc-make-and-goto-next-match :which-key "mc-mark and next")
    "c p" '(evil-mc-make-and-goto-prev-match :which-key "mc-mark and prev")
    "c u" '(evil-mc-undo-all-cursors :which-key "mc-mark undo all"))
  )

;; setup project management
(use-package projectile
  :diminish projectile-mode
  :custom
  (projectile-completion-system 'ivy)
  :init
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (custo/leader-key
    "p" '(projectile-command-map :which-key "projectile")
    "p a" '(projectile-add-known-project :which-key "add project"))
  (projectile-mode 1)
  )
  
;; add counsel capability
(use-package counsel-projectile
  :after projectile
  :bind
  ([remap projectile-find-file] . counsel-projectile-find-file)
  :config (counsel-projectile-mode)
  )

;; make dired more like ranger
(use-package ranger
  :after dired
  :config
  (setq ranger-preview-file nil
        ranger-show-literal t
        ranger-max-parent-width 0.12
        ranger-width-preview 0.45
        ranger-max-preview-size 1
        ranger-dont-show-binary t)
  (custo/leader-key
   "f d" '(ranger :which-key "file directory")
   )
  )

;; prettier dired
(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

;; magit
(use-package magit
  ;;:commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (custo/leader-key
    "g" '(:ignore t :which-key "magit")
    "g s" '(magit-status :which-key "magit status")
    "g b" '(magit-branch :which-key "magit branch")
    "g B" '(magit-blame :which-key "magit blame")
    )
  )

;; evil keys with magit  
(use-package evil-magit
  :after magit
  )

;; magit integration with github and gitlab
;; (use-package forge
;;   :after magit
;;   )

;; completion mini buffers
(use-package company
  :hook
  (lsp-mode . company-mode)
  :config
  (setq company-backends '(company-capf)
        company-idle-delay 0.0
        company-minimum-prefix-length 1)
  )

(use-package company-prescient
  :init (company-prescient-mode 1))

(use-package company-box
  :hook (company-mode . company-box-mode))


;; better javascript mode
(use-package js2-mode
  :mode "\\/.*\\.js\\'"
  :config
  (setq js-indent-level 2)
  :hook
  (js-mode . yas-minor-mode)
  )

;; teach js2-mode how to jsx
(use-package rjsx-mode
  :mode "components\\/.*\\.js\\'"
  )

;; auto-docs :D
(use-package js-doc
  :after js2-mode
  :config
  (custo/local-leader-key
    :keymaps '(js2-mode-map rsjx-mode)
    "d" '(:ignore t :which-key "jsdoc")
    "d f" '(js-doc-insert-function-doc :which-key "jsdoc function"))
  )

(use-package js-react-redux-yasnippets
  :after (yasnippet js2-mode)
  :config
  (custo/local-leader-key
    :keymaps '(js2-mode-map rsjx-mode)
    "i s" '(yas-insert-snippet :which-key "insert snippet"))
  )

;; format js and jsx
(use-package prettier
  :after js2-mode
  :config
  (custo/local-leader-key
    :keymaps '(js2-mode-map rsjx-mode)
    "= =" '(prettier-prettify :which-key "format with prettier"))
  )


(use-package web-mode
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2)
  )

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)
  )

(use-package rustic 
  :config
  (setq indent-tabs-mode nil
        rustic-lsp-server 'rust-analyzer
        rustic-indent-offset 2
        rust-format-on-save t)
  (custo/local-leader-key
    :keymaps 'rustic-mode-map
    "= =" '(rustic-format-buffer :which-key "format with rustfmt"))
  )


(use-package csharp-mode
  :hook
  (csharp-mode . rainbow-delimiters-mode)
  (csharp-mode . company-mode)
  (csharp-mode . flycheck-mode)
  )

(use-package omnisharp
  :after company
  :commands omnisharp-install-server
  :hook
  (csharp-mode . omnisharp-mode)
  :config
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 2
        tab-width 2
        evil-shift-width 2)
  (add-to-list 'company-backends 'company-omnisharp)
  (custo/local-leader-key
    :keymaps '(csharp-mode-map omnisharp-mode-map)
    "o" '(:ignore t :which-key "omnisharp")
    "o r" '(omnisharp-run-code-action-refactoring :which-key "omnisharp refactor")
    "o b" '(recompile :which-key "omnisharp build/recompile")
    )
  )

;; lsp-mode
(use-package lsp-mode
  :hook ((js2-mode . lsp-deferred)
         (rsjx-mode . lsp-deferred)
         (scss-mode . lsp-deferred)
         (web-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (rustic-mode . lsp-deferred)
         (csharp-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-rust-server 'rust-analyzer)
  (custo/local-leader-key
    :keymaps '(js2-mode-map
               rjsx-mode-map
               rustic-mode-map
               typescript-mode-map
               csharp-mode
               lsp-mode-map
               lsp-ui-mode-map)
    "g r" '(lsp-ui-peek-find-references :which-key "goto references")
    "g g" '(lsp-find-definition :which-key "goto definition")
    "o" '(lsp-ui-imenu :which-key "overview")
    "r" '(:ignore t :which-key "refactor")
    "r r" '(lsp-rename :which-key "rename")
    "=" '(:ignore t :which-key "format")
    "= l" '(lsp-format-buffer :which-key "format with lsp")
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
  :hook
  ;; FIXME we should call these based on mode not globally
  (after-init-hook . global-flycheck-mode)
  :config
  (custo/local-leader-key
    ;; FIXME keymaps probably wont work in global mode so try after you fix
    ;;       the above
    ;; :keymaps '(js2-mode rsjx-mode typescript-mode rustic-mode csharp-mode)
    "e" '(:ignore t :which-key "errors")
    "e l" '(flycheck-list-errors :which-key "list errors")
    )
  )

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO" . ,(face-foreground 'warning))
          ("DONT" . ,(face-foreground 'error))
          ("DANGER" . ,(face-foreground 'error ))
          ("DONE" . ,(face-foreground 'success))
          ("NOTE" . ,(face-foreground 'warning))
          ("HACK" . ,(face-foreground 'warning))
          ("FIXME" . ,(face-foreground 'error ))
          ("WARNING" . ,(face-foreground 'warning))
          ))
  (global-hl-todo-mode 1)
  )

;; org stuff
(defun custo/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  ;; (auto-fill-mode 0)
  (visual-line-mode 1)
  ;; (display-line-numbers-mode 0)
  ;; (setq org-directory "~/org/")
  ;; (setq evil-auto-indent nil)
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
  )

(use-package org
  :straight
  `(org
    :local-repo nil
    )
  :hook
  (org-mode . custo/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t
        ;; org-startup-indented nil
        )
  (setq org-agenda-files
        `(
          "~/org/tasks.org"
          "~/org/ideas.org"
          "~/org/journal.org"
          )
        )
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence
           "TODO"
           "DOING"
           "DELAYED"
           "IN REVIEW"
           "|"
           "DONE"
           "PARTIAL"
           "CANCELLED"
           "OBE"
           "MOVED")))
  (setq org-todo-keyword-faces
        `(("TODO" . "#88ff88")
          ("DOING" . "#ffff88")
          ("DELAYED" . "#ffbb88")
          ("IN REVIEW" . "#bb55ff")
          ("DONE" . "#8888ff")
          ("PARTIAL" . "#8855ff")
          ("CANCELLED" . "#ff8888")
          ("OBE" . "#ffbb88")
          ("MOVED" . "#88ffbb")))
  (setq org-tag-alist
        '((:startgroup)
          ;; mutually exclusive tags here
          (:endgroup)
          ("errand" . ?e)
          ("chore" . ?c)
          ("appointment" . ?a)
          ("note" . ?n)
          ("idea" . ?i)
          ("followup" . ?f)
          )
        )
  (setq org-refile-targets
        '(("~/org/archive.org" :maxlevel . 1)
          ("~/org/tasks.org" :maxlevel . 1)
          ("~/org/ideas.org" :maxlevel . 1)
          ))
  ;; safety save all org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; capture templates
  (setq org-capture-templates
        '(("t" "Tasks")
          ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
           "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
          
          )
        )
  ;; global key bindings
  (custo/leader-key
    "o a" '(org-agenda :wk "agenda")
    "o c" '(org-capture :wk "capture")
    "o t" '(org-todo-list :wk "list todos"))
  ;; org mode specific bindings
  (custo/local-leader-key
    :keymaps 'org-mode-map
    "a" '(:ignore t :wk "add")
    "a t" '(org-time-stamp :wk "timetamp")
    "a s" '(org-schedule :wk "schedule")
    "a d" '(org-deadline :wk "deadline")
    "a p" '(org-set-property :wk "property")
    "a T" '(org-set-tags-command :wk "tag")
    "i" '(:ignore t :wk "insert")
    "i RET" '(org-insert-structure-template :wk "insert template")
    "i s" '((lambda ()
                (interactive)
                (org-insert-structure-template "src")) :wk "insert source block")
    "s" '(:ignore t :wk "subtree") 
    "s r" '(org-refile :wk "refile")
    "s c" '(org-copy-subtree :wk "copy")
    "s x" '(org-cut-subtree :wk "cut")
    "s p" '(org-paste-subtree :wk "paste")
    "s n" '(org-toggle-narrow-to-subtree :wk "toggle narrow")
    "s <right>" '(org-demote-subtree :wk "demote")
    "s <left>" '(org-promote-subtree :wk "promote")
    "s <up>" '(org-move-subtree-up :wk "move up")
    "s <down>" '(org-move-subtree-down :wk "move down")
    "t" '(org-babel-tangle :wk "tangle file")
    )
  )

(require 'ob-js)
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)
    (js . t)
    )
  )

(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("js" . "src js"))

;; (use-package org-projectile
;;   :config
;;   (org-projectile-per-project)
;;   (setq org-projectile-per-project-filepath "todo.org")
;;   (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
;;   )

;; make org look nicer
(use-package org-superstar
  :hook
  (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets)
  ;; (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  )

(dolist (face '((org-level-1 . 1.3)
                (org-level-2 . 1.25)
                (org-level-3 . 1.20)
                (org-level-4 . 1.15)
                (org-level-5 . 1.10)
                (org-level-6 . 1.05)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  ;; (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height (cdr face)))
  (set-face-attribute (car face) nil :font "Arial" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(defun custo/mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook
  (org-mode . custo/mode-visual-fill)
  ;; (prog-mode . custo/mode-visual-fill)  
  )


(use-package centaur-tabs
  :config
  (setq centaur-tabs-height 32)
  (setq centaur-tabs-bar-height 43)
  (setq centaur-tabs-set-bar 'under)
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-greyout-icons t)
  (setq centaur-tabs-icon-scale-factor 0.75)
  ;; (setq centaur-tabs-icon-v-adjust -0.1)
  (setq x-underline-at-descent-line t)
  (centaur-tabs-mode 1)
  )

(straight-use-package
 '(global-term-cursor
   :host github :repo "h0d/term-cursor.el")
  )

(load "~/.private.el")

(use-package circe
  :config
  (setq circe-network-options
        `(("irc.chat.twitch.tv"
           :tls t
           :port 6697
           :nick ,private/circe-nick
           :pass ,private/circe-pass
           ))
        )
  (require 'circe-color-nicks)
  (setq circe-color-nicks-min-constrast-ratio 4.5
        circe-color-nicks-everywhere t)
  (custo/leader-key
    "a c" '(circe :wk "circe")
    )
  :hook (circe-channel-mode . enable-circe-color-nicks)
  )


