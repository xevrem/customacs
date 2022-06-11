;;; init --- "Summary"
;;; customacs initialization & configuration file
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

(setq-default inhibit-startup-message t ;; dont show startup message
              ring-bell-function 'ignore ;; disable all visual and audible bells
              indent-tabs-mode nil ;; uses spaces and not tabs
              create-lockfiles nil ;; do not create lockfiles
              truncate-lines 1 ;; do not truncate lines by default
              ;; disable until we actually call it
              recentf-auto-cleanup 'never
              ;; adjust the startup size of emacs
              default-frame-alist `((width . 120) ;; chars
                                    (height . 39) ;; lines
                                    )
              initial-frame-alist `((width . 120) ;; chars
                                    (height . 39) ;; lines
                                    )
              ;; Resizing the Emacs frame can be a terribly expensive part of changing the
              ;; font. By inhibiting this, we halve startup times, particularly when we use
              ;; fonts that are larger than the system default (which would resize the frame).
              frame-inhibit-implied-resize t
              ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
              idle-update-delay 1.0
              ;; Font compacting can be terribly expensive, especially for rendering icon
              ;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
              ;; hasn't been determined, but do it there anyway, just in case. This increases
              ;; memory usage, however!
              inhibit-compacting-font-caches t
              ;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
              ;; receiving input, which should help a little with scrolling performance.
              redisplay-skip-fontification-on-input t
              ;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
              ;; is more than enough.
              inhibit-startup-screen t
              inhibit-startup-echo-area-message user-login-name
              inhibit-default-init t
              ;; Shave seconds off startup time by starting the scratch buffer in
              ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
              ;; pull in a ton of packages. `doom/open-scratch-buffer' provides a better
              ;; scratch buffer anyway.
              initial-major-mode 'fundamental-mode
              initial-scratch-message nil
              )
(set-fringe-mode 10) ;; 'breathing' room
;; better line info
(column-number-mode) ;; show column info
(menu-bar--display-line-numbers-mode-relative) ;; make those line numbers relative

;; set global backup directory
(setq-default backup-directory-alist `(("." . ,(expand-file-name "backups/" user-emacs-directory))))

;; setup auto-save
(unless (file-directory-p (expand-file-name "auto-saves/" user-emacs-directory))
  (make-directory (expand-file-name "auto-saves/" user-emacs-directory))
  )
(setq-default auto-save-list-file-prefix (expand-file-name "auto-saves/sessions/" user-emacs-directory)
              auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))


;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
;;      it disabled will have nasty side-effects, so we simply delay it instead,
;;      and invoke it later, at which point it runs quickly; how mysterious!
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun doom-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))

;; turn of line numbers in the following modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                ansi-term-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)
                   )
            )
  )

(dolist (mode '(prog-mode-hook
                conf-mode-hook
                text-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1)
                   )
            )
  )

;; setup fonts and sizing regardless of OS
(defun custo/setup-font-faces ()
  "Setup all customacs font faces."
  ;;re-disable GUI stuff we don't care about
  (push '(menu-bar-lines . 0) default-frame-alist)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (push '(vertical-scroll-bars) default-frame-alist)
  (when (display-graphic-p)
    ;; set default font
    (set-face-attribute 'default nil :font (font-spec :family "MesloLGS Nerd Font Mono" :size 20 :weight 'regular))
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "MesloLGS Nerd Font Mono" :size 20 :weight 'regular))
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
    (set-frame-height (frame-focus) 39)
    (doom-modeline-refresh-font-width-cache)
    )
  )
;; run this hook after we have initialized the first time
(add-hook 'after-init-hook 'custo/setup-font-faces)
;; re-run this hook if we create a new frame from daemonized Emacs
(add-hook 'server-after-make-frame-hook 'custo/setup-font-faces)

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
;;      it disabled will have nasty side-effects, so we simply delay it instead,
;;      and invoke it later, at which point it runs quickly; how mysterious!
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (defun custo-init-tty-h ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))



;; setup straight for package management, its much better than use-package
(setq straight-use-package-by-default t
      straight-repository-branch "develop"
      ;; single file for caching autoloads
      straight-cache-autoloads t
      ;; NOTE: requires python3 and watchexec
      ;; straight-check-for-modifications '(watch-files find-when-checking)
      ;; NOTE: requires no watchexec
      straight-find-executable "fd"
      straight-check-for-modifications '(check-on-save find-when-checking)
      )

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
;; CUSTOM HOOKS
;;

(defvar custo/after-general-load-hook nil
  "Hook called after general is loaded.")

(defvar custo/after-consult-load-hook nil
  "Hook called after consult is loaded.")

(defvar custo/after-wk-load-hook nil
  "Hook called after which key is loaded.")

(defvar custo/after-load-hook nil
  "Hook called after general-load hook.")

;;
;; PACKAGE CONFIGURATION
;;

;; get shell variables
(use-package exec-path-from-shell
  :defer t
  :commands (exec-path-from-shell-initialize)
  :hook
  (after-init . (lambda ()
                  (when (memq window-system '(mac ns x))
                    (exec-path-from-shell-copy-env "LSP_USE_PLISTS")
                    (exec-path-from-shell-initialize)
                    )
                  (when (daemonp)
                    (exec-path-from-shell-copy-env "LSP_USE_PLISTS")
                    (exec-path-from-shell-initialize)
                    )
                  )
              )
  )


;; restart
(use-package restart-emacs
  :defer t
  :commands restart-emacs
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "q r" '(restart-emacs :wk "restart emacs")
                                  )
                                )
                            )
  )


(use-package savehist
  :defer t
  :hook
  (after-init . (lambda ()
                  (savehist-mode t)
                  )
              )
  )

;; setup a special menu that tells us what keys are available
;; based on the current mode, set pop-up delay to 0.1s
(use-package which-key
  :defer t
  :hook (after-init . (lambda ()
                        (which-key-mode)
                        )
                    )
  :config
  (setq which-key-idle-delay 0.1)
  (message "wk hook")
  (run-hooks 'custo/after-wk-load-hook)
  )

;; better key binding
(use-package general
  :defer t
  :commands (general-auto-unbind-keys
             general-create-definer)
  :hook
  (custo/after-wk-load . (lambda ()
                           (general-auto-unbind-keys)
                           (general-create-definer custo/leader-key
                             :states '(normal insert visual emacs)
                             :prefix "SPC"
                             :global-prefix "C-SPC")
                           (general-create-definer custo/local-leader-key
                             :states '(normal insert visual emacs)
                             :prefix "SPC m"
                             :global-prefix "C-SPC m")
                           ;; define default keybinds
                           (custo/leader-key
                             "SPC" '(execute-extended-command :wk "M-x") 
                             "TAB" '(evil-switch-to-windows-last-buffer :wk "switch to previous buffer")
                             ":" '(execute-extended-command :wk "M-x")
                             "X" '(execute-extended-command-for-buffer :wk "M-x for buffer")
                             "a" '(:ignore t :wk "apps")
                             "a e" '(eww :wk "eww")
                             "a t" '(eshell :wk "eshell")
                             "b" '(:ignore t :wk "buffer")
                             "b d" '(kill-current-buffer :wk "destroy buffer")
                             "b i" '(ibuffer :wk "ibuffer")
                             "b r" '(revert-buffer-quick :wk "revert buffer")
                             "c" '(:ignore t :wk "cursor")
                             "c c" '(comment-line :wk "comment line")
                             "f" '(:ignore f :wk "file")
                             "f f" '(find-file :wk "find file")
                             "f P" '(find-file-in-project-by-selected :wk "find file in project w/ regex")
                             "f s" '(save-buffer :wk "save file")
                             "h" '(:ignore t :wk "custo help")
                             "h s" '(:ignore t :wk "straight")
                             "h s p" '(straight-pull-all :wk "pull packages")
                             "h s P" '(straight-pull-package-and-deps :wk "pull package")
                             "h s b" '(straight-rebuild-all :wk "build packages")
                             "h s B" '(straight-rebuild-package :wk "build package")
                             "h s c" '(:ingore t :wk "cleaning")
                             "h s c p" '(straight-prune-build :wk "prune builds")
                             "h s c c" '(straight-prune-build-cache :wk "prune build cache only")
                             "j" '(:ignore t :wk "jump")
                             "j f" '(evil-jump-forward :wk "jump forward")
                             "j b" '(evil-jump-backward :wk "jump forward")
                             "m" '(:ignore t :wk "local-leader")
                             "o" '(:ignore t :wk "org")
                             "p" '(projectile-command-map :wk "projectile")
                             "q" '(:ignore t :wk "quit")
                             "q f" '(delete-frame :wk "delete frame")
                             "q q" '(save-buffers-kill-emacs :wk "save and quit")
                             "q Q" '(kill-emacs :wk "quit no-save")
                             "s" '(:ignore t :wk "search")
                             "t" '(:ignore t :wk "toggles")
                             "t l" '(display-line-numbers-mode :wk "toggle line numbers")
                             "t r" '((lambda ()
                                       (interactive)
                                       (custo/setup-font-faces))
                                     :wk "reset font-faces")
                             "t t" '(toggle-truncate-lines :wk "toggle truncate lines")
                             "w" '(:ignore t :wk "window")
                             "w w" '(other-window :wk "other window")
                             "w d" '(delete-window :wk "delete window")
                             "w o" '(delete-other-windows :wk "delete other windows")
                             "w h" '(evil-window-vsplit :wk "add window horizontally")
                             "w v" '(evil-window-split :wk "add window vertically")
                             )

                           (custo/local-leader-key
                             :keymaps 'prog-mode-map
                             "=" '(:ignore t :wk "format")
                             "d" '(:ignore t :wk "documentation")
                             "g" '(:ignore t :wk "goto")
                             "i" '(:ingore t :wk "insert")
                             )
                           (message "general hook")
                           (run-hooks 'custo/after-general-load-hook)
                           (run-hooks 'custo/after-load-hook)
                           )
                       )
  )


;; minad' and oantolin's awesome packages:
(use-package vertico
  :straight '(:type git :host github
                    :repo "minad/vertico"
                    :branch "main")
  :defer t
  :hook
  (custo/after-general-load . (lambda ()
                                (vertico-mode 1)
                                )
                            )
  :config
  (setq vertico-cycle t
        completion-in-region-function #'consult-completion-in-region)
  )


(use-package consult
  :defer t
  :commands (consult-buffer
             consult-completion-in-region
             consult-customize
             consult-imenu
             consult-line
             consult-project-buffer
             consult-recent-file
             consult-ripgrep
             consult-theme
             consult-xref
             )
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "b b" '(consult-buffer :wk "switch buffers")
                                  "f r" '(consult-recent-file :wk "recent files")
                                  "s i" '(consult-imenu :wk "imenu")
                                  "s p" '(consult-ripgrep :wk "search project")
                                  "s s" '(consult-line :wk "search buffer")
                                  "t T" '(consult-theme :wk "choose theme")
                                  "p b" '(consult-project-buffer :wk "project buffers")
                                  )
                                )
                            )
  (projectile-mode . (lambda ()
                       (setq consult-project-root-function #'projectile-project-root)
                       )
                   )
  :config
  (message "consult config")
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file
   consult--source-bookmark consult-buffer consult-project-buffer
   :preview-key (kbd "C-p")
   )
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.5
        consult-async-input-throttle 0.5
        consult-async-input-debounce 0.5
        )
  (message "consult hook")
  (run-hooks 'custo/after-consult-load-hook)
  )

(use-package consult-flycheck
  :defer t
  :commands consult-flycheck
  :hook
  (custo/after-consult-load . (lambda ()
                                (custo/leader-key
                                  "e l" '(consult-flycheck :wk "list errors")
                                  )
                                )
                            )
  :config
  (custo/local-leader-key
    "e l" '(consult-flycheck :wk "list errors")
    )
  )


(use-package marginalia
  :defer t
  :hook
  (vertico-mode . marginalia-mode)
  )


(use-package orderless
  :defer t
  :hook
  (after-init . (lambda ()
                  (setq completion-styles '(orderless partial-completion basic)
                        completion-category-defaults nil
                        completion-category-overrides '((file (styles orderless partial-completion))
                                                        (command (styles orderless))
                                                        (symbol (styles orderless))
                                                        (variable (styles orderless)))
                        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
                        )
                  )
              )
  )

(use-package embark
  :defer t
  :commands embark-act
  :bind(("C-e" . embark-act)
        :map vertico-map
        ("C-e" . embark-act)
        )
  :config
  (use-package embark-consult)
  )


(defun custo/lsp-mode-setup-completion ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless))) ;;

(defun corfu-move-to-minibuffer ()
  "Allows corfu to be moved into the minibuffer for better orderless completions."
  (interactive)
  (let ((completion-extra-properties corfu--extra)
        completion-cycle-threshold completion-cycling)
    (apply #'consult-completion-in-region completion-in-region--data)
    )
  )

(use-package corfu
  :defer t
  :hook
  (prog-mode . corfu-mode)
  (lsp-completion-mode . custo/lsp-mode-setup-completion)
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("<tab>" . corfu-next)
              ("S-TAB" . corfu-previous)
              ("<backtab>" . corfu-previous)
              ("SPC" . corfu-move-to-minibuffer)
              ("<space>" . corfu-move-to-minibuffer)
              )
  :custom
  (corfu-cycle t)
  :config
  ;; since we use orderless
  (setq corfu-quit-at-boundary nil
        corfu-auto nil
        )
  (use-package cape
    :straight (:type git
                     :host github
                     :repo "minad/cape")
    :config
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    (add-to-list 'completion-at-point-functions #'cape-symbol)
    )
  )



(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 1

        ;; Enable indentation+completion using the TAB key.
        ;; Completion is often bound to M-TAB.
        tab-always-indent 'complete
        )
  )


;; show recently used files
(use-package recentf
  :defer t
  :commands recentf-open-files
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "f R" '(recentf-open-files :wk "full recentf files")
                                  )
                                )
                            )
  )


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
  :commands (doom-modeline-mode
             doom-modeline-refresh-font-width-cache)
  :hook
  (custo/after-load . doom-modeline-mode)
  (server-after-make-frame . doom-modeline-refresh-font-width-cache)
  :config
  (setq doom-modeline-height 26
        doom-modeline-unicode-fallback t
        doom-modeline-project-detection 'ffip
        )
  (doom-modeline-refresh-font-width-cache)
  )


(use-package doom-themes
  :defer t
  :hook
  (doom-modeline-mode . (lambda ()
                          (setq doom-themes-enable-bold t
                                doom-themes-enable-italic t)
                          (setq doom-challenger-deep-brighter-comments t
                                doom-challenger-deep-comment-bg t)
                          (consult-theme 'doom-challenger-deep)
                          )
                      )
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


(defun custo/setup-dashboard ()
  "Setup dashboard to be default initial buffer."
  )

;; nice dashboard
(use-package dashboard
  :defer t
  :hook
  (after-init . dashboard-setup-startup-hook)
  (server-after-make-frame . dashboard-setup-startup-hook)
  (dashboard-after-initialize . (lambda ()
                                  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
                                  )
                              )
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


(use-package rainbow-identifiers
  :defer t
  :hook
  (prog-mode . rainbow-identifiers-mode)
  :config
  (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
        rainbow-identifiers-cie-l*a*b*-lightness 75
        rainbow-identifiers-cie-l*a*b*-saturation 50)
  )

;; make it easier to keep track of parens and braces
(use-package rainbow-delimiters
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  )

(defun custo/smart-parens ()
  (require 'smartparens-config)
  (smartparens-mode))

(use-package smartparens
  :defer t
  :hook
  (prog-mode . custo/smart-parens)
  (markdown-mode . custo/smart-parens)
  (org-mode . custo/smart-parens)
  :config
  ;; don't interfere with yasnippets
  (advice-add #'yas-expand :before #'sp-remove-active-pair-overlay)
  )


;; highlight matching delimiters
(use-package paren
  :defer t
  :hook
  (prog-mode . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t
        blink-matching-paren t)
  )


(use-package clipetty
  :defer t
  :commands clipetty-mode
  )

(use-package xclip
  :defer t
  :commands xclip-mode
  )

;; ensure that these are added to all the appropriat modes
(add-hook 'after-init-hook (lambda ()
                             (unless (display-graphic-p)
                               (dolist (mode '(prog-mode-hook
                                               conf-mode-hook
                                               text-mode-hook
                                               ))
                                 (add-hook mode 'xclip-mode)
                                 (add-hook mode 'clipetty-mode)
                                 )
                               )
                             ))




;; better help menus
(use-package helpful
  :defer t
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "h h" '(:ignore h :wk "helpful docs")
                                  "h h c" '(helpful-callable :wk "helpful callable")
                                  "h h f" '(helpful-function :wk "helpful function")
                                  "h h v" '(helpful-variable :wk "helpful variable")
                                  "h h k" '(helpful-key :wk "helpful key")
                                  "h h s" '(helpful-symbol :wk "helpful symbol")
                                  )
                                )
                            )
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

;; the very best mode
(use-package evil
  :defer t
  :hook
  (after-init . (lambda ()
                  (setq evil-want-integration t
                        evil-want-keybinding nil
                        evil-want-C-u-scroll t
                        evil-want-C-d-scroll t)
                  (evil-mode)
                  ))
  :custom
  (evil-undo-system 'undo-fu)
  :bind
  (:map evil-insert-state-map
        ("C-g" . evil-normal-state)
        )
  )


;; better evil stuff
(use-package evil-collection
  :defer t
  :hook
  (evil-mode . evil-collection-init)
  )


(use-package evil-commentary
  :defer t
  :hook
  (evil-mode . evil-commentary-mode)
  )


(use-package yasnippet-snippets
  :defer t
  :commands yasnippet-snippets-initialize
  )
(use-package js-react-redux-yasnippets
  :defer t
  :commands js-react-redux-yasnippets-initialize
  )

;; yasnippet
(use-package yasnippet
  :defer t
  :commands (yas-reload-all
             yas-insert-snippet)
  :hook
  (custo/after-wk-load . (lambda ()
                           (yasnippet-snippets-initialize)
                           (js-react-redux-yasnippets-initialize)
                           (yas-reload-all)
                           )
                       )
  (custo/after-general-load . (lambda ()
                                (custo/local-leader-key
                                  "i s" '(yas-insert-snippet :wk "insert snippet")
                                  )
                                )
                            )
  (text-mode . yas-minor-mode-on)
  (prog-mode . yas-minor-mode-on)
  (conf-mode . yas-minor-mode-on)
  (snippet-mode . yas-minor-mode-on)
  )


;; hydra to build menus
(use-package hydra
  :defer t
  :commands (defhydra)
  :hook
  (custo/after-general-load . (lambda ()
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
                            )
  )


(use-package undo-fu
  :defer t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all)
  )

(use-package vundo
  :defer t
  :straight (:type git :host github
                   :repo "casouri/vundo"
                   :branch "master"
                   :file "vundo.el"
                   )
  :commands (vundo)
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "u" '(vundo :wk "visualize undo / redo")
                                  )
                                )
                            )
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  )


(use-package evil-mc
  :defer t
  :after evil
  :hook
  (prog-mode . evil-mc-mode)
  (markdown-mode . evil-mc-mode)
  (org-mode . evil-mc-mode)
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  :keymaps '(prog-mode-map markdown-mode-map org-mode-map)
                                  "c a" '(evil-mc-make-all-cursors :wk "mc-mark and make all")
                                  "c n" '(evil-mc-make-and-goto-next-match :wk "mc-mark and next")
                                  "c p" '(evil-mc-make-and-goto-prev-match :wk "mc-mark and prev")
                                  "c u" '(evil-mc-undo-all-cursors :wk "mc-mark undo all")
                                  )
                                )
                            )
  )

(use-package iedit
  :defer t
  :commands (iedit-mode)
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "c i" '(iedit-mode :wk "iedit")
                                  )
                                )
                            )
  )

;; setup project management
(use-package projectile
  :defer t
  :commands (projectile-command-map
             projectile-find-file
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-project-root)
  :hook
  (vertico-mode . projectile-mode)
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "p" '(projectile-command-map :wk "projectile")
                                  :keymaps 'projectile-mode-map
                                  "p a" '(projectile-add-known-project :wk "add project")
                                  )
                                )
                            )
  :custom
  ;; this allows projectile to use orderless
  (projectile-completion-system 'default)
  :config
  ;; used by projectile-ripgrep
  (use-package ripgrep)
  )

(use-package find-file-in-project
  :defer t
  :commands (find-file-in-project
             ffip)
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "f p" '(ffip :wk "find file in project")
                                  :keymaps 'projectile-mode-map
                                  "p f" '(ffip :wk "find file in project")
                                  )
                                )
                            )
  :config
  (setq ffip-use-rust-fd t)
  )

(use-package all-the-icons-dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode)
  )

;; make dired more like ranger
(use-package ranger
  :defer t
  :commands ranger
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "f d" '(ranger :wk "file directory")
                                  )
                                )
                            )
  :config
  (setq ranger-preview-file nil
        ranger-show-literal t
        ranger-max-parent-width 0.12
        ranger-width-preview 0.45
        ranger-max-preview-size 1
        ranger-dont-show-binary t)
  )


(use-package git-gutter
  :defer t
  :hook
  (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 5.0)
  )


(use-package git-gutter-fringe
  :defer t
  :commands (git-gutter-fr:init)
  :hook
  (git-gutter-mode . (lambda ()
                       (git-gutter-fr:init)
                       (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
                       (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
                       (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom)
                       )
                   )
  )

;; magit
(use-package magit
  :defer t
  :commands (magit-status
             magit-branch
             magit-blame
             magit-fetch
             magit-pull
             magit-push
             magit-stash
             )
  :hook
  (custo/after-general-load . (lambda ()
                                (defhydra hydra-smerge (:timeout 4)
                                  "smerge conflicts"
                                  ("q" nil "done" :exit t)
                                  ("n" smerge-next "next conflict")
                                  ("p" smerge-prev "prev conflict")
                                  ("u" smerge-keep-upper "keep upper")
                                  ("l" smerge-keep-lower "keep lower")
                                  ("b" smerge-keep-all "keep both")
                                  )
                                (custo/leader-key
                                  "g" '(:ignore t :wk "magit")
                                  "g g" '(magit-status :wk "magit status")
                                  "g b" '(magit-branch :wk "magit branch")
                                  "g B" '(magit-blame :wk "magit blame")
                                  "g f" '(magit-fetch :wk "magit fetch")
                                  "g p" '(magit-pull :wk "magit pull")
                                  "g P" '(magit-push :wk "magit push")
                                  "g s" '(hydra-smerge/body :wk "smerge")
                                  "g S" '(magit-stash :wk "magit stash")
                                  )
                                )
                            )
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (setq magit-diff-refine-hunk t ; show granular diffs in selected hunk
        ;; Don't autosave repo buffers. This is too magical, and saving can
        ;; trigger a bunch of unwanted side-effects, like save hooks and
        ;; formatters. Trust the user to know what they're doing.
        magit-save-repository-buffers nil
        ;; Don't display parent/related refs in commit buffers; they are rarely
        ;; helpful and only add to runtime costs.
        magit-revision-insert-related-refs nil
        )
  )

;;enable super syntax highlighting
(use-package tree-sitter
  :straight (:type git :host github
                   :repo "emacs-tree-sitter/elisp-tree-sitter"
                   :branch "release"
                   )
  :defer t
  :after tree-sitter-langs
  :hook
  (lsp-mode . (lambda ()
                (tree-sitter-mode)
                (tree-sitter-hl-mode)
                )
            )
  )

(use-package tree-sitter-langs
  :straight (:type git :host github
                   :repo "emacs-tree-sitter/tree-sitter-langs"
                   :branch "release"
                   )
  :defer t
  :commands (tree-sitter-require)
  :hook
  (prog-mode . (lambda ()
                 (tree-sitter-require 'tsx)
                 (tree-sitter-require 'html)
                 (tree-sitter-require 'json)
                 (tree-sitter-require 'css)
                 (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
                 (add-to-list 'tree-sitter-major-mode-language-alist '(svelte-mode . html))
                 (add-to-list 'tree-sitter-major-mode-language-alist '(web-mode . html))
                 (add-to-list 'tree-sitter-major-mode-language-alist '(json-mode . json))
                 (add-to-list 'tree-sitter-major-mode-language-alist '(scss-mode . css))
                 )
             )
  )


;; better javascript mode
(use-package js2-mode
  :defer t
  :after prog-mode
  :mode ("\\.js\\'" "\\.cjs\\'")
  :config
  (setq js-indent-level 2)
  )

;; teach js2-mode how to jsx
(use-package rjsx-mode
  :defer t
  :after prog-mode
  :mode ("components\\/.*\\.js\\'" "\\.jsx\\'")
  )

(use-package svelte-mode
  :defer t
  :after prog-mode
  :mode ("\\.svelte\\'")
  )

;; auto-docs :D
(use-package js-doc
  :defer t
  :commands (js-doc-insert-function-doc)
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/local-leader-key
                                  :keymaps '(js2-mode-map
                                             rjsx-mode-map
                                             typescript-mode-map
                                             typescript-tsx-mode-map
                                             svelte-mode-map
                                             )
                                  "d" '(:ignore t :wk "jsdoc")
                                  "d f" '(js-doc-insert-function-doc :wk "jsdoc function")
                                  )
                                )
                            )
  )

(use-package eslint-fix
  :defer t
  :commands (eslint-fix)
  )

;; format js and jsx
(use-package prettier
  :defer t
  :commands (prettier-prettify)
  )

(add-hook 'custo/after-general-load-hook (lambda ()
                                           (custo/local-leader-key
                                             :keymaps '(js2-mode-map
                                                        rsjx-mode-map
                                                        typescript-mode-map
                                                        typescript-tsx-mode-map
                                                        svelte-mode-map
                                                        web-mode-map)
                                             "= =" '((lambda ()
                                                       (interactive)
                                                       (prettier-prettify)
                                                       (eslint-fix)
                                                       ) :wk "format with prettier and eslint")
                                             )
                                           )
          )

(use-package scss-mode
  :defer t
  :after prog-mode
  :mode ("\\.scss\\'" "\\.css\\'")
  :config
  (setq css-indent-offset 2)
  )

(use-package web-mode
  :defer t
  :after prog-mode
  :mode ("\\.html\\'" )
  :config
  (setq web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2)
  )

(use-package typescript-mode
  :defer t
  :after prog-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  )

(define-derived-mode typescript-tsx-mode typescript-mode "tsx")
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))

(use-package rustic
  :defer t
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq indent-tabs-mode nil
        rustic-lsp-client 'lsp-mode
        lsp-rust-server 'rust-analyzer
        rustic-lsp-server 'rust-analyzer
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-server-display-inlay-hints t
        lsp-rust-analyzer-inlay-hints-mode nil
        rustic-indent-offset 4
        rustic-format-on-save nil)
  (custo/local-leader-key
    :keymaps 'rustic-mode-map
    "= =" '(rustic-format-buffer :wk "format with rustfmt")
    "c" '(:ignore t :wk "cargo commands")
    "c b" '(rustic-cargo-build :wk "cargo build")
    "c C" '(rustic-cargo-check :wk "cargo check")
    "c c" '(rustic-cargo-clippy :wk "cargo clippy")
    "c r" '(rustic-cargo-run :wk "cargo run")
    "c t" '(rustic-cargo-test :wk "cargo test")
    "t" '(:ignore t :wk "toggles")
    "t i" '(lsp-rust-analyzer-inlay-hints-mode :wk "toggle inlay hints")
    )
  )

(use-package csharp-mode
  :defer t
  :after prog-mode
  :mode "\\.cs\\'"
  )

(use-package omnisharp
  :defer t
  ;; :mode ("\\.cs\\'" . omnisharp-mode)
  :after (corfu csharp-mode)
  :commands omnisharp-install-server
  :hook
  (csharp-mode . omnisharp-mode)
  :config
  (setq indent-tabs-mode nil
        c-syntactic-indentation t
        c-basic-offset 2
        tab-width 2
        evil-shift-width 2)
  (custo/local-leader-key
    :keymaps '(csharp-mode-map omnisharp-mode-map)
    "o" '(:ignore t :wk "omnisharp")
    "o r" '(omnisharp-run-code-action-refactoring :wk "omnisharp refactor")
    "o b" '(recompile :wk "omnisharp build/recompile")
    )
  )

(use-package elixir-mode
  :defer t
  :after prog-mode
  :mode ("\\.ex\\'"
         "\\.eex\\'"
         "\\.exs\\'"
         "\\.heex\\'"
         "\\.leex\\'")
  )

(use-package python-black
  :defer t
  :after python
  :commands (python-black-buffer)
  )

(use-package python
  :defer t
  :after prog-mode
  :mode ("\\.py\\'" . python-mode)
  :config
  (custo/local-leader-key
    :keymaps '(python-mode-map)
    "= =" '(python-black-buffer :wk "format with black"))
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
  :after prog-mode
  :mode "\\.json\\'"
  :config
  (setq json-indent-offset 2)
  )

(use-package yaml-mode
  :defer t
  :after prog-mode
  :mode ("\\.yml\\'" "\\.yaml\\'")
  :config
  (setq yaml-indent-offset 2)
  )

(use-package markdown-mode
  :defer t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  )

(use-package gdscript-mode
  :defer t
  :after prog-mode
  :mode "\\.gd\\'")

(use-package go-mode
  :defer t
  :after prog-mode
  :mode "\\.go\\'"
  )

(use-package plantuml-mode
  :straight '(:type git :host github
                    :repo "skuro/plantuml-mode"
                    :branch "develop")
  :defer t
  :after prog-mode
  :mode ("\\.puml\\'" "\\.pml\\'")
  :config
  (setq plantuml-default-exec-mode 'server
        plantuml-server-url "http://localhost:8080"
        plantuml-indent-level 2)
  )

(use-package dockerfile-mode
  :defer t
  :after prog-mode
  :mode ("\\Dockerfile\\'"))


;; lsp-mode
(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred lsp-mode-map)
  :hook 
  (js2-mode . lsp-deferred)
  (rsjx-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  (typescript-tsx-mode . lsp-deferred)
  (rustic-mode . lsp-deferred)
  (elixir-mode . lsp-deferred)
  (scss-mode . lsp-deferred)
  (yaml-mode . lsp-deferred)
  (json-mode . lsp-deferred)
  (web-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (svelte-mode . lsp-deferred)
  (csharp-mode . lsp-deferred)
  (gdscript-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :bind
  ([remap xref-goto-xref] . custo/xref-goto-xref)
  :config
  (setq lsp-completion-provider :none
        ;; lsp-file-watch-threshold 100
        lsp-headerline-breadcrumb-enable nil
        lsp-lens-enable t
        ;; lsp-headerline-breadcrumb-segments '(project file symbols)
        lsp-idle-delay 1.0
        lsp-log-io nil
        lsp-enable-snippet nil ;; disable snippet completion as it causes more problems than it helps
        lsp-modeline-diagnostics-enable t;; disable warnings that usually get in the way
        lsp-lense-debounce-interval 0.5 ;; set it to a more sane value
        lsp-lense-place-position 'above-line
        lsp-use-plists t
        )
  (custo/local-leader-key
    :keymaps '(
               js2-mode-map
               rjsx-mode-map
               typescript-mode-map
               typescript-tsx-mode-map
               rustic-mode-map
               elixir-mode-map
               yaml-mode-map
               json-mode-map
               scss-mode-map
               web-mode-map
               go-mode-map
               gdscript-mode-map
               svelte-mode-map
               csharp-mode-map
               python-mode-map
               )
    "a" '(lsp-execute-code-action :wk "excute code action")
    "g g" '(lsp-find-definition :wk "find definition")
    "g G" '(lsp-goto-implementation :wk "goto definition")
    "g R" '(lsp-ui-peek-find-references :wk "peek references")
    "g r" '(lsp-find-references :wk "find references")
    "g t" '(lsp-goto-type-definition :wk "goto type definition")
    "h" '(:ignore t :wk "help")
    "h g" '(lsp-ui-doc-glance :wk "glance symbol")
    "h d" '(lsp-describe-thing-at-point :wk "describe symbol")
    "h s" '(lsp-signature-activate :wk "show signature")
    "o" '(lsp-ui-imenu :wk "overview")
    "r" '(:ignore t :wk "refactor")
    "r r" '(lsp-rename :wk "rename")
    "=" '(:ignore t :wk "format")
    "= l" '(lsp-format-buffer :wk "format with lsp")
    )
  )

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred)))
  )

;; prettier lsp
(use-package lsp-ui
  :defer t
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-delay 1.0
        lsp-ui-sideline-enable t
        lsp-ui-sideline-delay 1.0
        )
  )

;; error checking
(use-package flycheck
  :defer t
  :hook
  (lsp-mode . flycheck-mode)
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
    )
  (custo/local-leader-key
    :keymaps '(
               js2-mode-map
               rjsx-mode-map
               typescript-mode-map
               typescript-tsx-mode-map
               rustic-mode-map
               elixir-mode-map
               yaml-mode-map
               json-mode-map
               scss-mode-map
               web-mode-map
               go-mode-map
               gdscript-mode-map
               svelte-mode-map
               csharp-mode-map
               python-mode-map
               )
    "e" '(:ignore t :wk "errors")
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
  :mode ("\\.org\\'" . (lambda ()
                         (require 'ob-js)
                         (require 'ox-gfm)
                         (require 'org-tempo)
                         (org-mode)
                         )
         )
  :commands (org-capture
             org-agenda
             org-todo-list
             org-time-stamp
             org-schedule
             org-deadline
             org-set-property
             org-set-tags-command
             org-export-dispatch
             org-md-export-as-markdown
             org-refile
             org-copy-subtree
             org-cut-subtree
             org-paste-subtree
             org-demote-subtree
             org-promote-subtree
             org-move-subtree-up
             org-move-subtree-down
             org-babel-tangleorg-toggle-narrow-to-subtree
             )
  :hook
  (custo/after-general-load . (lambda ()
                                ;; global key bindings
                                (custo/leader-key
                                  "o a" '(org-agenda :wk "agenda")
                                  "o c" '(org-capture :wk "capture")
                                  "o t" '(org-todo-list :wk "list todos")
                                  )
                                )
                            )
  (org-mode . (lambda ()
                (custo/org-mode-setup)
                (org-babel-do-load-languages
                 'org-babel-load-languages
                 '((emacs-lisp . t)
                   (python . t)
                   (js . t)
                   (plantuml . t)
                   )
                 )
                (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
                (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
                (add-to-list 'org-structure-template-alist '("py" . "src python"))
                (add-to-list 'org-structure-template-alist '("js" . "src js"))
                )
            )
  :config
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t
        org-startup-indented nil
        ;; org-agenda-files `("~/org/tasks.org"
        ;;                    "~/org/ideas.org"
        ;;                    "~/org/journal.org"
        ;;                    )
        org-agenda-start-with-log-mode t
        org-log-done 'time
        org-log-into-drawer t
        org-todo-keywords '((sequence
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
                             "MOVED"))
        org-todo-keyword-faces `(("TODO" . "#88ff88")
                                 ("DOING" . "#ffff88")
                                 ("DELAYED" . "#ffbb88")
                                 ("PARTIAL" . "#8855ff")
                                 ("COMPLETE" . "#ff55bb")
                                 ("IN REVIEW" . "#bb55ff")
                                 ("DONE" . "#8888ff")
                                 ("CANCELLED" . "#ff8888")
                                 ("OBE" . "#ffbb88")
                                 ("MOVED" . "#88ffbb"))
        org-tag-alist '((:startgroup)
                        ;; mutually exclusive tags here
                        (:endgroup)
                        ("errand" . ?e)
                        ("chore" . ?c)
                        ("appointment" . ?a)
                        ("note" . ?n)
                        ("idea" . ?i)
                        ("followup" . ?f)
                        )
        org-refile-targets '(("~/org/archive.org" :maxlevel . 1)
                             ("~/org/tasks.org" :maxlevel . 1)
                             ("~/org/ideas.org" :maxlevel . 1)
                             )
        ;; capture templates
        org-capture-templates '(("t" "Tasks")
                                ("tt" "Task" entry (file+olp "~/org/tasks.org" "Inbox")
                                 "* TODO %?\n %U\n %a\n %i" :empty-lines 1)
                                )
        )
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
  ;; safety save all org buffers after refiling
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  )

;; we'll setup this directory so that ox-gfm doesnt freak out when
;; it doesnt see an actual `org` directory
(unless (file-directory-p (expand-file-name "straight/repos/org/" user-emacs-directory))
  (make-directory (expand-file-name "straight/repos/org/" user-emacs-directory))
  )


(use-package ox-gfm
  :defer t
  )

;; make org look nicer
(use-package org-superstar
  :if (display-graphic-p)
  :defer t
  :hook
  (org-mode . org-superstar-mode)
  :config
  (org-superstar-configure-like-org-bullets)
  )

(use-package visual-fill-column
  :defer t
  :commands (visual-fill-column-mode)
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "t v" '((lambda () (interactive) (call-interactively 'visual-fill-column-mode)) :wk "toggle visual-fill")
                                  )
                                )
                            )
  :config
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  )

(use-package centaur-tabs
  :defer t
  :commands centaur-tabs-mode
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "t c" '(centaur-tabs-mode :wk "toggle centaur tabs")
                                  )
                                )
                            )
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-height 28
        centaur-tabs-set-icons t
        centaur-tabs-icon-v-adjust 0.0
        centaur-tabs-icon-scale-factor 0.7
        centaur-tabs-set-bar 'under
        x-underline-at-descent-line t
        )
  (centaur-tabs-group-by-projectile-project)
  )




;; terminal related packages
(use-package term-cursor
  :if (not (display-graphic-p))
  :straight '(:type git :host github
                    :repo "h0d/term-cursor.el"
                    :branch "master"
                    :file "term-cursor.el")
  :defer t
  :commands (term-cursor-mode)
  :hook
  (prog-mode . term-cursor-mode)
  )


;; (defconst private-file (expand-file-name "~/.private.el"))
;; (unless (file-exists-p private-file)
;;   (with-temp-buffer (write-file private-file))
;;   )

;; ;; IRC
;; (with-eval-after-load 'circe
;;   (load private-file)
;;   )

(use-package circe
  :defer t
  :commands circe
  :hook
  (circe-channel-mode . enable-circe-color-nicks)
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "a c" '(circe :wk "circe")
                                  )
                                )
                            )
  :config
  (defconst private-file (expand-file-name "~/.private.el"))
  (unless (file-exists-p private-file)
    (with-temp-buffer (write-file private-file))
    )
  (load private-file)
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
  )


(use-package vterm
  :defer t
  :commands vterm
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "a T" '((lambda ()
                                            (interactive)
                                            (call-interactively 'vterm)) :wk "vterm")
                                  )
                                )
                            )
  :config
  (setq vterm-timer-delay 0.1
        vterm-shell "fish")
  )

(use-package prodigy
  :defer t
  :commands prodigy
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "a p" '(prodigy :wk "prodigy")
                                  )
                                )
                            )
  :config
  (prodigy-define-service
    :name "Megalith"
    :command "yarn"
    :cwd "~/repos/megalith"
    :args '("start")
    :stop-signal 'sigterm
    ;; :kill-process-buffer-on-stop t
    )
  (prodigy-define-service
    :name "Vaerydian"
    :command "yarn"
    :cwd "~/repos/vaerydian-engine"
    :args '("start")
    :stop-signal 'sigterm
    ;; :kill-process-buffer-on-stop t
    )
  (prodigy-define-service
    :name "Nucleo"
    :command "docker"
    :cwd "~/repos/nucleo"
    :args '("compose" "up")
    :stop-signal 'sigterm
    )
  (prodigy-define-service
    :name "screen saver"
    :command "mpv"
    :cwd "~/Movies/Kaptures"
    :args '("--window-scale=0.5" "--loop-file" "textured-lines.mp4")
    :stop-signal 'sigterm
    )
  (prodigy-define-service
    :name "redshift"
    :command "redshift"
    :args '("-P" "-O" "2700" "-b" "0.75")
    :stop-signal 'sigterm
    )
  )

(use-package devdocs
  :defer t
  :commands (devdocs-install
             devdocs-lookup)
  :hook
  (custo/after-general-load . (lambda ()
                                (custo/leader-key
                                  "h d" '(:ignore t :wk "devdocs")
                                  "h d i" '(devdocs-install :wk "install devdocs")
                                  "h d l" '(devdocs-lookup :wk "lookup devdocs")
                                  )
                                )
                            )
  )

(use-package gcmh
  :defer t
  :hook (after-init . gcmh-mode)
  :config
  (setq-default gcmh-idle-delay 'auto  ; default is 15s
                gcmh-auto-idle-delay-factor 10
                gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
                ;; gcmh-verbose t
                )
  )

(provide 'init)
;;; init ends here
