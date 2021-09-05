;;; init --- "Summary"
;; customacs initialization & configuration file
;;; Commentary:
;;; Code:

;; lets us know how long it takes to startup
(defun custo/display-startup-time ()
  "Display the startup time for customacs."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'custo/display-startup-time)

(setq inhibit-startup-message t)
(set-fringe-mode 10)    ;; 'breathing' room
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
;; (global-display-line-numbers-mode t) ;; display line numbers to the left
(menu-bar--display-line-numbers-mode-relative) ;; make those line numbers relative

;; turn of line numbers in the following modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                ansi-term-mode-hook
                ;; treemacs-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)
                   )
            )
  )

(dolist (mode '(prog-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1)
                   )
            )
  )

;; set user caching directory
(unless (file-directory-p (expand-file-name ".cache/" user-emacs-directory))
  (make-directory (expand-file-name ".cache/" user-emacs-directory))
  )
(setq user-emacs-directory (expand-file-name ".cache/" user-emacs-directory))

;; set global backup directory
(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; setup auto-save
(unless (file-directory-p (expand-file-name "auto-saves/" user-emacs-directory))
  (make-directory (expand-file-name "auto-saves/" user-emacs-directory))
  )

(setq auto-save-list-file-prefix (expand-file-name "auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))

;; native comp insanity
;; if native comp is used, cache compiled code
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name "eln-cache/" user-emacs-directory)))

;; disable lockfiles
;;(setq create-lockfiles nil)

;; if in macOS, set size appropriately
;; otherwise assume linux
(defun custo/setup-font-faces ()
  "Setup all customacs font faces."
  ;;re-disable GUI stuff we don't care about
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (when (display-graphic-p)
    ;; set default font
    (set-face-attribute 'default nil :font (font-spec :family "FiraCode Nerd Font" :size 20 :weight 'regular))
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "FiraCode Nerd Font" :size 20 :weight 'regular))
    ;; Set the variable pitch face which is the same for mac and linux
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Arial" :size 20 :weight 'regular))
    ;; after org-mode we want to adjust font sizes
    (with-eval-after-load 'org
      (dolist (face '((org-level-1 . 1.3)
                      (org-level-2 . 1.25)
                      (org-level-3 . 1.20)
                      (org-level-4 . 1.15)
                      (org-level-5 . 1.10)
                      (org-level-6 . 1.05)
                      (org-level-7 . 1.0)
                      (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil :font "Arial" :weight 'regular :height (cdr face))
        )
      
      ;; Ensure that anything that should be fixed-pitch in Org files appears that way
      (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
      (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
      (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
      )
    ;; set current frame to 120x45 characters
    (set-frame-width (frame-focus) 120)
    (set-frame-height (frame-focus) 45)
    )
  )
;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'custo/setup-font-faces)
;; re-run this hook if we create a new frame from daemonized Emacs
(add-hook 'server-after-make-frame-hook 'custo/setup-font-faces)


;; setup straight for package management, its much better than use-package
(setq straight-use-package-by-default t
      straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5)
      )
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)
      )
    )
  (load bootstrap-file nil 'nomessage)
  )
(straight-use-package 'use-package)

;;
;; PACKAGE CONFIGURATION
;;


;; get shell variables
(use-package exec-path-from-shell
  :init
  (unless (not 'display-graphic-p))
  (exec-path-from-shell-initialize)
  )


;; restart
(use-package restart-emacs
  :defer t
  :commands restart-emacs
  )


(use-package savehist
  :defer t
  :hook
  (after-init . savehist-mode)
  )

(use-package dashboard
  :defer t
  :hook
  (after-init . dashboard-setup-startup-hook)
  :config
  (setq dashboard-center-content t
        dashboard-items '((recents  . 5)
                          (projects . 5)
                          (agenda . 5)
                          )
        dashboard-startup-banner 'logo
        dashboard-banner-logo-title "Welcome to Customacs!"
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        )
  )


;; minad' and oantolin's awesome packages:
(use-package vertico
  :straight '(:type git :host github
              :repo "minad/vertico"
              :branch "main")
  :defer t
  :hook
  (after-init . vertico-mode)
  :config
  (setq vertico-cycle t
        completion-in-region-function #'consult-completion-in-region)
  )


(use-package consult
  :defer t
  :after vertico
  :commands (consult-xref
             consult-line
             consult-recent-file
             consult-flymake)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-project-root-function #'projectile-project-root)
  )

(use-package consult-flycheck
  :defer t
  :commands consult-flycheck)


(use-package marginalia
  :defer t
  :after vertico
  :hook
  (vertico-mode . marginalia-mode)
  )


(use-package orderless
  :config
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))
                                        (command (styles orderless))
                                        (symbol (styles orderless))
                                        (variable (styles orderless)))
        )
  )


(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3

        ;; Enable indentation+completion using the TAB key.
        ;; Completion is often bound to M-TAB.
        tab-always-indent 'complete
        )
  )


;; show recently used files
(use-package recentf
  :defer t
  :commands recentf-open-files
  )

;; used by projectile-ripgrep
(use-package ripgrep)

;; load all-the-icons only if in GUI mode
;; and install them if not present
(use-package all-the-icons
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)
    )
  )

;; add a better modeline
(use-package doom-modeline
  :defer t
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 32)
  )

;; enable better themes
(use-package one-themes
  :after doom-modeline
  )

(use-package challenger-deep-theme
  :after doom-modeline
  ;; :config
  ;; (load-theme 'challenger-deep t)
  )

(use-package doom-themes
  :after doom-modeline
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-challenger-deep t)
  )

;; for certian versions of emacs, we need to change the backgroun
(add-hook 'tty-setup-hook
          (lambda ()
            (unless (> emacs-major-version 27)
              (add-hook 'emacs-startup-hook
                        (lambda ()
                          (set-background-color "black")))
              (add-hook 'server-after-make-frame-hook
                        (lambda ()
                          (set-background-color "black")))
              )
            )
          )

(use-package rainbow-identifiers
  :defer t
  :after (:any lsp-mode
               prog-mode
               emacs-lisp-mode)
  :hook
  ;; (eglot--managed-mode . rainbow-identifiers-mode)
  (lsp-mode . rainbow-identifiers-mode)
  (emacs-lisp-mode . rainbow-identifiers-mode)
  )

;; make it easier to keep track of parens and braces
(use-package rainbow-delimiters
  :defer t
  :after (:any lsp-mode
               prog-mode
               emacs-lisp-mode)
  :hook
  ;; (eglot--managed-mode . rainbow-delimiters-mode)
  (lsp-mode . rainbow-delimiters-mode)
  (emacs-lisp-mode . rainbow-delimiters-mode)
  )

(defun custo/smart-parens ()
                 (require 'smartparens-config)
                 (smartparens-mode))

(use-package smartparens
  :defer t
  :after (:any lsp-mode
               prog-mode
               emacs-lisp-mode)
  :hook
  ;; (eglot--managed-mode . custo/smart-parens)
  (lsp-mode . custo/smart-parens)
  (emacs-lisp-mode . custo/smart-parens)
  :config
  ;; don't interfere with yasnippets
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)
  )


;; highlight matching delimiters
(use-package paren
  :defer t
  :after (:any lsp-mode
               prog-mode
               emacs-lisp-mode)
  :hook
  ;; (eglot--managed-mode . show-paren-mode)
  (lsp-mode . show-paren-mode)
  (emacs-lisp-mode . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        blink-matching-paren t)
  )


(use-package clipetty
  :defer t
  )

(use-package xclip
  :defer t
  )

(unless (display-graphic-p)
  (add-hook 'prog-mode-hook 'xclip-mode)
  (add-hook 'prog-mode-hook 'clipetty-mode)
  )


;; setup a special menu that tells us what keys are available
;; based on the current mode, set pop-up delay to 0.1s
(use-package which-key
  :defer t
  :hook (after-init . which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1)
  )



;; more better help menus
(use-package helpful
  :defer t
  :after which-key
  :commands (helpful-callable
             helpful-command
             helpful-function
             helpful-key
             helpful-variable
             helpful-symbol)
  :bind
  ([remap describe-callable] . helpful-callable)
  ([remap describe-function] . helpful-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  )
  

(defun custo/evil-hook ()
  "Custom hook to use Emacs mode."
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode
                  ansi-term-mode
                  vterm-mode
                  )
                )
    (add-to-list 'evil-emacs-state-modes mode)
    )
  )

;; the very best mode
(use-package evil
  :defer t
  :hook
  (after-init . evil-mode)
  ;; (evil-mode . custo/evil-hook)
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t)
  :bind (:map evil-insert-state-map
              ("C-g" . evil-normal-state)
              )
  :config
  (custo/evil-hook)
 )

;; better evil stuff
(use-package evil-collection
  :defer t
  :after evil
  :hook
  (evil-mode . evil-collection-init)
  )


(use-package evil-commentary
  :defer t
  :after evil
  :hook
  (evil-mode . evil-commentary-mode)
  )


;; better key binding
(use-package general
  :config
  (general-auto-unbind-keys)
  (general-create-definer custo/leader-key
    :states '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer custo/local-leader-key
    :states '(normal insert visual emacs)
    :prefix "SPC m"
    :global-prefix "C-SPC m")
  )


;; define default keybinds
(custo/leader-key
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "switch to previous buffer")
  ":" '(execute-extended-command :wk "M-x")
  "X" '(execute-extended-command-for-buffer :wk "M-x for buffer")
  "a" '(:ignore t :wk "apps")
  "a c" '(circe :wk "circe")
  "a e" '(eww :wk "eww")
  "a p" '(prodigy :wk "prodigy")
  "a t" '(eshell :wk "eshell")
  "a T" '(vterm :wk "vterm")
  "b" '(:ignore t :which-key "buffer")
  "b b" '(switch-to-buffer :which-key "switch buffers")
  "b d" '(kill-current-buffer :which-key "destroy buffer")
  "c" '(:ignore t :which-key "cursor")
  "c c" '(comment-line :which-key "comment line")
  "f" '(:ignore f :which-key "file")
  "f d" '(ranger :which-key "file directory")
  "f f" '(find-file :which-key "find file")
  "f p" '(find-file-in-project :wk "find file in project")
  "f r" '(consult-recent-file :wk "recent files")
  "f R" '(recentf-open-files :wk "full recentf files")
  "f s" '(save-buffer :which-key "save file")
  "g" '(:ignore t :which-key "magit")
  "g g" '(magit-status :which-key "magit status")
  "g b" '(magit-branch :which-key "magit branch")
  "g B" '(magit-blame :which-key "magit blame")
  "g s" '(:ignore s :wk "smerge")
  "g s n" '(smerge-next :wk "goto next conflict")
  "g s p" '(smerge-prev :wk "goto prev conflict")
  "g s u" '(smerge-keep-upper :wk "keep upper")
  "g s l" '(smerge-keep-lower :wk "keep lower")
  "g s b" '(smerge-keep-all :wk "keep both")
  "h" '(:ignore t :which-key "custo help")
  "h d" '(:ignore t :wk "devdocs")
  "h d i" '(devdocs-install :wk "install devdocs")
  "h d l" '(devdocs-lookup :wk "lookup devdocs")
  "h h" '(:ignore h :wk "helpful docs")
  "h h c" '(helpful-callable :wk "helpful callable")
  "h h f" '(helpful-function :wk "helpful function")
  "h h v" '(helpful-variable :wk "helpful variable")
  "h h k" '(helpful-key :wk "helpful key")
  "h h s" '(helpful-symbol :wk "helpful symbol")
  "h s" '(:ignore t :which-key "straight")
  "h s p" '(straight-pull-all :which-key "straight pull packages")
  "h s b" '(straight-rebuild-all :which-key "straight build packages")
  "m" '(:ignore t :which-key "local-leader")
  "o" '(:ignore t :which-key "org")
  "p" '(projectile-command-map :wk "projectile")
  "q" '(:ignore t :which-key "quit")
  "q f" '(delete-frame :wk "delete frame")
  "q q" '(save-buffers-kill-emacs :which-key "save and quit")
  "q Q" '(kill-emacs :which-key "quit no-save")
  "q r" '(restart-emacs :which-key "restart emacs")
  "s" '(:ignore t :which-key "search")
  ;; "s p" '(projectile-ripgrep :wk "search project")
  "s p" '(consult-ripgrep :which-key "search project")
  "s s" '(consult-line :wk "search buffer")
  "t" '(:ignore t :which-key "toggles")
  "t l" '(display-line-numbers-mode :wk "toggle line numbers")
  "t r" '((lambda ()
            (interactive)
            (custo/setup-font-faces))
          :wk "reset font-faces")
  "t t" '(toggle-truncate-lines :which-key "toggle truncate lines")
  ;; "t T" '(consult-theme :wk "choose theme")
  "t T" '(load-theme :wk "choose theme")
  "w" '(:ignore t :which-key "window")
  "w w" '(other-window :which-key "other window")
  "w d" '(delete-window :which-key "delete window")
  "w o" '(delete-other-windows :which-key "delete other windows")
  "w h" '(evil-window-vsplit :which-key "add window horizontally")
  "w v" '(evil-window-split :which-key "add window vertically")
  )

(custo/local-leader-key
  :keymaps 'prog-mode-map
  "=" '(:ignore t :which-key "format")
  "d" '(:ignore t :which-key "documentation")
  "g" '(:ignore t :which-key "goto")
  "i" '(:ingore t :which-key "insert")
  )


(use-package yasnippet-snippets)

;; yasnippet
(use-package yasnippet
  :defer t
  :after yasnippet-snippets
  :commands (yas-reload-all
             yas-minor-mode-on)
  :hook
  (after-init . yas-reload-all)
  (text-mode . yas-minor-mode-on)
  (prog-mode . yas-minor-mode-on)
  (conf-mode . yas-minor-mode-on)
  (snippet-mode . yas-minor-mode-on)
  :config
  (custo/local-leader-key
    "i s" '(yas-insert-snippet :which-key "insert snippet"))
  )


;; hydra to build menus
(use-package hydra
  :defer t
  :after which-key
  :commands (defhydra)
  :config
  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" (text-scale-adjust 0.1) "in")
    ("k" (text-scale-adjust -0.1) "out")
    ("f" nil "finished" :exit t)
    )
  (defhydra hydra-window-resize (:timeout 4)
    "resize window"
    ("l" enlarge-window "enlarge window")
    ("h" shrink-window "shrink window")
    ("q" nil "done" :exit t)
    )
  
  ;; since custo leader keys are defined, we can bind to them now :D
  (custo/leader-key
    "t s" '(hydra-text-scale/body :wk "scale text")
    "w r" '(hydra-window-resize/body :wk "resize window")
    )
  )

(use-package undo-tree
  :defer t
  :after hydra
  :hook
  (prog-mode . undo-tree-mode)
  (org-mode . undo-tree-mode)
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

(use-package symbol-overlay
  :defer t
  :after hydra
  :hook
  (prog-mode . symbol-overlay-mode)
  :config
  (defhydra hydra-symbol-overlay (:timeout nil)
    "symbol find and replace"
    ("q" "quit" :exit t)
    ("d" symbol-overlay-remove-all "unmark all symbols" :exit t)
    ("m" symbol-overlay-put "toggle symbol")
    ("r" symbol-overlay-rename "rename symbol")
    ("Q" symbol-overlay-query-replace "find and replace symbol")
    ("t" symbol-overlay-toggle-in-scope "toggle scope")
    ("n" symbol-overlay-jump-next "next symbol")
    ("p" symbol-overlay-jump-prev "prev symbol")
    )
  (custo/leader-key
    "s o" '(hydra-symbol-overlay/body :wk "symbol overlay")
    )
  )

(use-package evil-mc
  :defer t
  :after evil
  :hook
  (prog-mode . evil-mc-mode)
  :config
  (custo/leader-key
    :keymaps 'prog-mode-map
    "c n" '(evil-mc-make-and-goto-next-match :which-key "mc-mark and next")
    "c p" '(evil-mc-make-and-goto-prev-match :which-key "mc-mark and prev")
    "c u" '(evil-mc-undo-all-cursors :which-key "mc-mark undo all")
    )
  )

;; setup project management
(use-package projectile
  :defer t
  :after vertico
  ;; :diminish projectile-mode
  :commands (projectile-command-map
             projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-project-root)
  :hook
  (vertico-mode . projectile-mode)
  :custom
  ;; this allows projectile to use orderless
  (projectile-completion-system 'default)
  :config
  (custo/leader-key
    "p" '(projectile-command-map :wk "projectile")
    :keymaps 'projectile-mode-map
    "p a" '(projectile-add-known-project :which-key "add project"))
  )

(use-package find-file-in-project
  :defer t
  :after projectile
  :commands find-file-in-project
  :config
  (setq ffip-use-rust-fd t)
  (custo/leader-key
    :keymaps 'projectile-mode-map
    "p f" '(find-file-in-project :wk "find file in project")
    )
  )

(use-package all-the-icons-dired
  :defer t
  :after dired
  :hook (dired-mode . all-the-icons-dired-mode)
  )

;; make dired more like ranger
(use-package ranger
  :defer t
  :commands ranger
  :after dired
  :config
  (setq ranger-preview-file nil
        ranger-show-literal t
        ranger-max-parent-width 0.12
        ranger-width-preview 0.45
        ranger-max-preview-size 1
        ranger-dont-show-binary t)
  )

;; prettier dired
(use-package diredfl
  :defer t
  :after dired
  :hook
  (dired-mode . diredfl-mode)
  )

(use-package git-gutter-fringe
  :defer t
  :hook
  (prog-mode . git-gutter-mode)
  )

;; magit
(use-package magit
  :defer t
  :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(use-package forge
  :after magit)


;; completion mini buffers
(use-package company
  :defer t
  :after (:any lsp-mode emacs-lisp-mode)
  :hook
  ;; (eglot--managed-mode . company-mode)
  (lsp-mode . company-mode)
  (emacs-lisp-mode . company-mode)
  :bind (;; only active when trying to complete a selection
         (:map company-active-map
               ;; complete the currently chosen selection
               ("RET" . company-complete-selection)
               ;; goto next selection
               ("<tab>" . company-select-next)
               ("TAB" . company-select-next)
               ;; goto previous selection
               ("<backtab>" . company-select-previous)
               ("S-TAB" . company-select-previous)
               )
         (:map emacs-lisp-mode-map
               ;; start the completion process
               ("<tab>" . company-indent-or-complete-common)
               ("TAB" . company-indent-or-complete-common)
               )
         ;; only make tab start completions if lsp is active
         (:map lsp-mode-map
               ;; start the completion process
               ("<tab>" . company-indent-or-complete-common)
               ("TAB" . company-indent-or-complete-common)
               )
         )
  :config
  (setq company-idle-delay nil
  ;; (setq company-idle-delay 0.5
        company-backends '(company-capf)
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-limit 25
        ;;
        ;; Good Ideas from DOOM:
        ;;
        ;; These auto-complete the current selection when
        ;; `company-auto-complete-chars' is typed. This is too magical. We
        ;; already have the much more explicit RET and TAB.
        company-auto-complete nil
        company-auto-complete-chars nil

        ;; Only search the current buffer for `company-dabbrev' (a backend that
        ;; suggests text your open buffers). This prevents Company from causing
        ;; lag once you have a lot of buffers open.
        company-dabbrev-other-buffers nil
        company-dabbrev-code-other-buffers nil
        ;; Make `company-dabbrev' fully case-sensitive, to improve UX with
        ;; domain-specific words with particular casing.
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        )
  )

(use-package company-box
  :defer t
  :after company
  :hook (company-mode . company-box-mode)
  )

;; better javascript mode
(use-package js2-mode
  :defer t
  :after prog-mode
  ;; :mode "\\/.*\\.js\\'"
  :mode "\\.js\\'"
  :config
  (setq js-indent-level 2)
  :hook
  (js2-mode . yas-minor-mode)
  )

;; teach js2-mode how to jsx
(use-package rjsx-mode
  :defer t
  :after prog-mode
  :mode ("components\\/.*\\.js\\'" "\\.jsx\\'")
  )

;; auto-docs :D
(use-package js-doc
  :after (:any js2-mode rjsx-mode typescript-mode)
  :config
  (custo/local-leader-key
    :keymaps '(js2-mode-map rjsx-mode-map typescript-mode-map)
    "d" '(:ignore t :which-key "jsdoc")
    "d f" '(js-doc-insert-function-doc :which-key "jsdoc function"))
  )

;; format js and jsx
(use-package prettier
  :after (:any js2-mode rsjx-mode typescript-mode web-mode)
  :config
  (custo/local-leader-key
    :keymaps '(js2-mode-map rsjx-mode-map typescript-mode-map web-mode-map)
    "= =" '(prettier-prettify :wk "format with prettier"))
  )


(use-package web-mode
  :defer t
  :mode ("\\.html\\'" "\\.scss\\'" "\\.css\\'")
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2)
  )

(use-package typescript-mode
  :defer t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  (progn
      (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
      (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))
  )

(use-package rustic
  :defer t
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq indent-tabs-mode nil
        rustic-lsp-client 'lsp
        rustic-lsp-server 'rust-analyzer
        lsp-rust-analyzer-proc-macro-enable t
        rustic-indent-offset 4
        rustic-format-on-save nil)
  (custo/local-leader-key
    :keymaps 'rustic-mode-map
    "= =" '(rustic-format-buffer :wk "format with rustfmt")
    "c" '(:ignore t :wk "cargo commands")
    "c b" '(rustic-cargo-build :wk "cargo build")
    "c c" '(rustic-cargo-check :wk "cargo check")
    "c C" '(rustic-cargo-clippy :wk "cargo clippy")
    "c r" '(rustic-cargo-run :wk "cargo run")
    "c t" '(rustic-cargo-test :wk "cargo test")
    )
  )

(use-package csharp-mode
  :defer t
  :mode "\\.cs\\'"
  :hook
  (csharp-mode . rainbow-delimiters-mode)
  )

(use-package omnisharp
  :defer t
  :mode ("\\.cs\\'" . omnisharp-mode)
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

(use-package elixir-mode
  :mode ("\\.ex\\'"
          "\\.eex\\'"
          "\\.exs\\'")
  :hook
  (elixir-mode . yas-minor-mode)
  )

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  )

(use-package pyenv-mode
  :straight (:type git :host github
                   :repo "pythonic-emacs/pyenv-mode"
                   :branch "master"
                   )
  :defer t
  :hook (python-mode . pyenv-mode)
  )

(use-package json-mode
  :defer t
  :mode "\\.json\\'"
  :hook
  (json-mode . yas-minor-mode)
  :config
  (setq json-indent-offset 2)
  )

(use-package yaml-mode
  :defer t
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :hook
  (yaml-mode . yas-minor-mode)
  :config
  (setq yaml-indent-offset 2)
  )

(use-package gdscript-mode
  :defer t
  :mode "\\.gd\\'")

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :config
  (setq tab-width 2)
  )

(use-package plantuml-mode
  :straight '(:type git :host github
              :repo "skuro/plantuml-mode"
              :branch "develop")
  :defer t
  :mode ("\\.puml\\'" "\\.pml\\'")
  :config
  (setq plantuml-default-exec-mode 'server
        plantuml-server-url "http://localhost:8080"
        plantuml-indent-level 2)
  )

;; eglot helper functions
(defun custo/xref-goto-xref ()
  "Customacs goto xref and quit xref buffer."
  (interactive)
  (xref-goto-xref t)
  )


(use-package jsonrpc)
(use-package flymake)
(use-package project)
(use-package xref)
(use-package eldoc)

;; (use-package eglot
;;   :defer t
;;   :after (:all yasnippet jsonrpc flymake project xref eldoc)
;;   :hook ((js2-mode . eglot-ensure)
;;          (rsjx-mode . eglot-ensure)
;;          (scss-mode . eglot-ensure)
;;          (web-mode . eglot-ensure)
;;          (typescript-mode . eglot-ensure)
;;          (rustic-mode . eglot-ensure)
;;          (csharp-mode . eglot-ensure)
;;          (elixir-mode . eglot-ensure)
;;          (yaml-mode . eglot-ensure)
;;          (json-mode . eglot-ensure)
;;          (go-mode . eglot-ensure)
;;          )
;;   :bind
;;   ([remap xref-goto-xref] . custo/xref-goto-xref)
;;   :config
;;   (custo/local-leader-key
;;     :keymaps '(js2-mode-map
;;                rjsx-mode-map
;;                rustic-mode-map
;;                typescript-mode-map
;;                typescript-tsx-mode-map
;;                csharp-mode-map
;;                elixir-mode-map
;;                yaml-mode-map
;;                json-mode-map
;;                web-mode-map
;;                go-mode-map
;;                python-mode-map
;;                gdscript-mode-map)
;;     "a" '(eglot-code-actions :wk "excute code action")
;;     "g r" '(xref-find-references :wk "goto references")
;;     ;; "g g" '(eglot-find-implementation :wk "goto definition")
;;     "g g" '(xref-find-definitions :wk "goto definition")
;;     "r" '(:ignore t :wk "refactor")
;;     "r r" '(eglot-rename :wk "rename")
;;     "=" '(:ignore t :wk "format")
;;     "= l" '(eglot-format-buffer :wk "format with eglot")
;;     "e" '(:ignore t :wk "errors")
;;     "e l" '(consult-flymake :wk "list errors")
;;     "h" '(:ignore t :wk "help")
;;     "h i" '(eldoc :wk "info about symbol")
;;     )
;;   )


;; lsp-mode
(use-package lsp-mode
   :defer t
   :hook ((js2-mode . lsp-deferred)
          (rsjx-mode . lsp-deferred)
          (scss-mode . lsp-deferred)
          (web-mode . lsp-deferred)
          (typescript-mode . lsp-deferred)
          (typescript-tsx-mode . lsp-deferred)
          (rustic-mode . lsp-deferred)
          (csharp-mode . lsp-deferred)
          (elixir-mode . lsp-deferred)
          (yaml-mode . lsp-deferred)
          (json-mode . lsp-deferred)
          (go-mode . lsp-deferred)
          (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands (lsp lsp-deferred)
  :bind
  ([remap xref-goto-xref] . custo/xref-goto-xref)
  :config
  (setq lsp-completion-provider :capf
        lsp-file-watch-threshold 100
        lsp-headerline-breadcrumb-enable nil
        ;; lsp-headerline-breadcrumb-segments '(project file symbols)
        lsp-ui-doc-enable nil
        lsp-idle-delay 0.500
        lsp-log-io nil
        )
  (custo/local-leader-key
    :keymaps '(js2-mode-map
               rjsx-mode-map
               rustic-mode-map
               typescript-mode-map
               typescript-tsx-mode-map
               csharp-mode-map
               elixir-mode-map
               yaml-mode-map
               json-mode-map
               web-mode-map
               go-mode-map
               python-mode-map
               gdscript-mode-map)
    "a" '(lsp-execute-code-action :wk "excute code action")
    "g g" '(lsp-find-definition :which-key "goto definition")
    "g p" '(lsp-ui-peek-find-references :which-key "peek references")
    "g r" '(lsp-find-references :wk "goto references")
    "l" '(:ignore t :wk "lsp")
    "l g" '(lsp-ui-doc-glance :wk "glance symbol")
    "l d" '(lsp-describe-thing-at-point :wk "describe symbol")
    "o" '(lsp-ui-imenu :which-key "overview")
    "r" '(:ignore t :which-key "refactor")
    "r r" '(lsp-rename :which-key "rename")
    "=" '(:ignore t :which-key "format")
    "= l" '(lsp-format-buffer :which-key "format with lsp")
    )
  )

(use-package lsp-pyright
  :after (:all lsp-mode python)
  :hook (python-mode .
                     (lambda ()
                       (require 'lsp-pyright)
                       (lsp-deferred)))
  )

;; prettier lsp
(use-package lsp-ui
  :defer t
  :after lsp-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  )

;; error checking
(use-package flycheck
  :defer t
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (setq flycheck-temp-prefix ".flycheck")
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  (custo/leader-key
    "e" '(:ignore t :wk "errors")
    "e l" '(consult-flycheck :wk "list errors")
    )
  (custo/local-leader-key
    :keymaps '(js2-mode-map
               rsjx-mode-map
               web-mode-map
               typescript-mode-map
               typescript-tsx-mode-map
               rustic-mode-map
               elixir-mode-map
               csharp-mode-map
               go-mode-map
               gdscript-mode-map)
    "e" '(:ignore t :wk "errors")
    "e l" '(consult-flymake :wk "list errors")
    )
  )




(use-package hl-todo
  :defer t
  :hook
  (prog-mode . hl-todo-mode)
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
  )


;; org stuff
(defun custo/org-mode-setup ()
  "Setup various org-mdoe settings whenever org mode is launched in a buffer."
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
  '(org
    :local-repo nil
    )
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :commands (org-capture org-agenda)
  :hook
  (org-mode . custo/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t
        org-startup-indented nil
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
           "|"
           "PARTIAL"
           "COMPLETE"
           "IN REVIEW"
           "DONE"
           "CANCELLED"
           "OBE"
           "MOVED")))
  (setq org-todo-keyword-faces
        `(("TODO" . "#88ff88")
          ("DOING" . "#ffff88")
          ("DELAYED" . "#ffbb88")
          ("PARTIAL" . "#8855ff")
          ("COMPLETE" . "#ff55bb")
          ("IN REVIEW" . "#bb55ff")
          ("DONE" . "#8888ff")
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
    "e" '(:ignore t :wk "export")
    "e d" '(org-export-dispatch :wk "export dispatch")
    "e m" '(org-md-export-as-markdown :wk "export as markdown")
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

(with-eval-after-load 'org
  (require 'ob-js)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (js . t)
     (plantuml . t)
     )
   )

  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
)
;; we'll setup this directory so that ox-gfm doesnt freak out when
;; it doesnt see an actual `org` directory
(unless (file-directory-p (expand-file-name "straight/repos/org/" user-emacs-directory))
  (make-directory (expand-file-name "straight/repos/org/" user-emacs-directory))
  )

(use-package ox-gfm
  :after org)

;; make org look nicer
(use-package org-superstar
  :if (display-graphic-p)
  :defer t
  :after org
  :hook
  (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets)
  )

(defun custo/visual-fill ()
  "When in GUI mode, enable visual fill column."
  (when (display-graphic-p)
    (visual-fill-column-mode 1)))

(use-package visual-fill-column
  :defer t
  :hook
  (org-mode . custo/visual-fill)
  (markdown-mode . custo/visual-fill)
  :config
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (custo/leader-key
    "t v" '((lambda () (interactive) (visual-fill-column-mode 1)) :wk "visual-fill mode on")
    "t V" '((lambda () (interactive) (visual-fill-column-mode -1)) :wk "visual-fill mode off"))
  )



;; terminal related packages
(use-package term-cursor
  :straight '(:type git :host github
              :repo "h0d/term-cursor.el"
              :branch "master"
              :file "term-cursor.el")
  :defer t
  :commands (term-cursor-mode)
  )
;; if not in a graphical environment, add term cursor to prog mode hook
(unless (display-graphic-p)
  (add-hook 'prog-mode-hook 'term-cursor-mode)
  )


;; (use-package evil-terminal-cursor-changer
;;   :defer t
;;   :hook
;;   (tty-setup . evil-terminal-cursor-changer-activate)
;;   :config
;;   (setq evil-motion-state-cursor 'box  ; █
;;         evil-visual-state-cursor 'box  ; █
;;         evil-normal-state-cursor 'box  ; █
;;         evil-insert-state-cursor 'bar  ; ⎸
;;         evil-emacs-state-cursor  'hbar) ; _
;;   )

;; ;; ensure we only set these hooks if we're not in a graphical environment
;; (unless (display-graphic-p)
;;   ;; run this hook after we have initialized the first time
;;   (add-hook 'after-init-hook 'evil-terminal-cursor-changer-activate)
;;   ;; re-run this hook if we create a new frame from daemonized Emacs
;;   (add-hook 'server-after-make-frame-hook 'evil-terminal-cursor-changer-activate)
;;   )


(defconst private-file (expand-file-name "~/.private.el"))
(unless (file-exists-p private-file)
  (with-temp-buffer (write-file private-file))
  )

;; IRC
(with-eval-after-load 'circe
  (load private-file)
  )

(use-package circe
  :defer t
  :commands circe
  :config
  (setq circe-network-options
        `(("irc.chat.twitch.tv"
           :tls t
           :port 6697
           :nick ,private/circe-nick
           :pass ,private/circe-pass
           )
          ("irc.libera.chat"
           :tls t
           :port 6697
           :nick ,private/libera-nick
           :pass ,private/libera-pass
           )
          )
        )
  (require 'circe-color-nicks)
  (setq circe-color-nicks-min-constrast-ratio 4.5
        circe-color-nicks-everywhere t)
  :hook (circe-channel-mode . enable-circe-color-nicks)
  )


(use-package vterm
  :defer t
  :commands vterm
  )

(defun custo/launch-vterm ()
  "Launches vterm and switch to evil-emacs-state."
  (interactive)
  (vterm)
  (evil-emacs-state)
  )

(use-package prodigy
  :defer t
  :commands prodigy
  :config
  (prodigy-define-service
    :name "Megalith"
    :command "npm"
    :cwd "/Users/erikajonell/repos/megalith"
    :args '("run" "start")
    )
  (prodigy-define-service
    :name "Nucleo"
    :command "docker-compose"
    :cwd "/Users/erikajonell/repos/nucleo"
    :args '("up" "-d")
    )
  )

(use-package devdocs
  :defer t
  :commands (devdocs-install
             devdocs-lookup)
  )

(use-package gcmh
  :defer t
  :hook (after-init . gcmh-mode)
  :config
  (setq gcmh-high-cons-threashold (* 1024 1024 100)
        gcmh-idle-delay 60)
  )

(provide 'init)
;;; init ends here
