;;Then some speed up tips from doom
(defvar default-gc-cons-threshold gc-cons-threshold)
(defvar default-gc-cons-percentage gc-cons-percentage)

;; for debugging
;; (setq max-lisp-eval-depth 10000)
;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)    ; now you should get a backtrace

;; make garbage collector less invasive during startup
(setq-default gc-cons-threshold  most-positive-fixnum
              gc-cons-percentage 0.6)

(setq-default read-process-output-max (* 1024 1024)) ;; 1mb

;;Disabling some annoying GUI stuff emacs has enabled by default.
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; set user emacs directory
;; (make-directory "~/.cache/emacs")
;; (setq user-emacs-directory (expand-file-name "~/.cache/emacs"))
;; set user caching directory
(unless (file-directory-p (expand-file-name "~/.cache/emacs" user-emacs-directory))
  (make-directory (expand-file-name "~/.cache/emacs" user-emacs-directory))
  )
(setq-default user-emacs-directory (expand-file-name "~/.cache/emacs" user-emacs-directory))

;; native comp insanity
;; if native comp is used, cache compiled code

(when (version= emacs-version "28")
  (when (boundp 'native-comp-eln-load-path)
    (setcar native-comp-eln-load-path
            (expand-file-name "eln-cache/" user-emacs-directory)))
  )
(when (version= emacs-version "29")
  (when (boundp 'native-comp-eln-load-path)
    (startup-redirect-eln-cache
     (expand-file-name "eln-cache/" user-emacs-directory)))
  )

(defvar custo/after-startup-hook nil
  "Hook called after emacs has started.")

(defvar custo/after-window-hook nil
  "Hook called after emacs has setup a window.")


(setq-default
 ;; Resizing the Emacs frame can be a terribly expensive part of changing the
 ;; font. By inhibiting this, we halve startup times, particularly when we use
 ;; fonts that are larger than the system default (which would resize the frame).
 frame-inhibit-implied-resize t
 ;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
 idle-update-delay 1.0
 ;; prevent package.el loading stuff too early
 package-enable-at-startup nil
 ;; Premature redisplays can substantially affect startup times and produce
 ;; ugly flashes of unstyled Emacs.
 inhibit-redisplay t
 inhibit-message t
 ;;Another tip from doom.
 default-file-name-handler-alist file-name-handler-alist
 file-name-handler-alist nil
 native-comp-async-report-warnings-errors nil
 native-comp-warning-on-missing-source nil
 )

;; And then finally a hook to reset everything.
(add-hook 'custo/after-window-hook
          (lambda (&rest _)
            ;; (message "startup hook was fired")
            (setq-default 
             gc-cons-threshold default-gc-cons-threshold
             gc-cons-percentage default-gc-cons-percentage
             file-name-handler-alist default-file-name-handler-alist)

            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)
            (run-hooks 'custo/after-startup-hook)
            )
          )

(set-language-environment "UTF-8")

;; use whatever is available, then replace it with native comp one
(when (boundp 'native-comp-deferred-compilation)
  (setq-default native-comp-deferred-compilation t)
  )

;; use level 2 optimizations
(when (boundp 'native-comp-speed)
  (setq-default native-comp-speed 2)
  )

;; emacs 29 and 30 change the name of this variable, but some programs may still reference it
(unless (boundp 'native-comp-deferred-compilation-deny-list)
  (setq-default native-comp-deferred-compilation-deny-list '())
)


;; Site files tend to use `load-file', which emits "Loading X..." messages in
;; the echo area, which in turn triggers a redisplay. Redisplays can have a
;; substantial effect on startup times and in this case happens so early that
;; Emacs may flash white while starting up.
(define-advice load-file (:override (file) silence)
  (load file nil 'nomessage))


(add-hook 'window-setup-hook
          (lambda ()
            ;; reset redisplay 
            (setq-default inhibit-redisplay nil
                          inhibit-message nil
                          frame-inhibit-implied-resize nil
                          idle-update-delay 0.5
                          )
            ;; Undo our `load-file' advice above, to limit the scope of any edge cases it
            ;; may introduce down the road.
            (advice-remove #'load-file #'load-file@silence)
            (redisplay)
            (run-hooks 'custo/after-window-hook)
            )
          )


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
