;; based on this post: 

;; https://www.reddit.com/r/emacs/comments/jw19dy/emacs_271_earlyinit_file/gcno7i8?utm_source=share&utm_medium=web2x&context=3

;; Firstly I put this in my init.el to make sure the early init file is always loaded.
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")
        )
  )

;;Then some speed up tips from doom
(defvar default-gc-cons-threshold gc-cons-threshold)
(defvar default-gc-cons-percentage gc-cons-percentage)

;; for debugging
;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)    ; now you should get a backtrace

;; make garbage collector less invasive
(setq-default gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

(setq-default read-process-output-max (* 1024 1024)) ;; 1mb

;;Disabling some annoying GUI stuff emacs has enabled by default.
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; set user emacs directory
;;(make-directory "~/.cache/emacs")
;;(setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
;; set user caching directory
(unless (file-directory-p (expand-file-name ".cache/" user-emacs-directory))
  (make-directory (expand-file-name ".cache/" user-emacs-directory))
  )
(setq-default user-emacs-directory (expand-file-name ".cache/" user-emacs-directory))

;; native comp insanity
;; if native comp is used, cache compiled code
(when (boundp 'native-comp-eln-load-path)
  (setcar native-comp-eln-load-path
          (expand-file-name "eln-cache/" user-emacs-directory)))

;;Another tip from doom.
(setq-default default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

;; And then finally a hook to reset everything.
(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            ;; (message "startup hook was fired")
            (setq-default gc-cons-threshold default-gc-cons-threshold
                  gc-cons-percentage default-gc-cons-percentage
                  file-name-handler-alist default-file-name-handler-alist)

            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)
            )
          )

;; (setq frame-inhibit-implied-resize t)
(set-language-environment "UTF-8")

;;prevent package.el loading stuff too early
(setq-default package-enable-at-startup nil)

;; prevent bug with straight.el
;; OBE: (defvar comp-deferred-compilation-deny-list ())

;; use whatever is available, then replace it with native comp one
(when (boundp 'native-comp-deferred-compilation)
  (setq-default native-comp-deferred-compilation t)
  )

;; use level 2 optimizations
(when (boundp 'native-comp-speed)
  (setq-default native-comp-speed 2)
  )

;; dont report async compile warnings
(setq-default native-comp-async-report-warnings-errors nil)
