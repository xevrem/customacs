;; based on this post: 
;; https://www.reddit.com/r/emacs/comments/jw19dy/emacs_271_earlyinit_file/gcno7i8?utm_source=share&utm_medium=web2x&context=3

;; Firstly I put this in my init.el to make sure the early init file is always loaded.
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

;;Then some speed up tips from doom
(defvar default-gc-cons-threshold 16777216 ; 16mb
  "my default desired value of `gc-cons-threshold'
during normal emacs operations.")

;; make garbage collector less invasive
(setq gc-cons-threshold  most-positive-fixnum
      gc-cons-percentage 0.6)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;;Disabling some annoying GUI stuff emacs has enabled by default.
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;Another tip from doom.
(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

;; And then finally a hook to reset everything.
(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            (setq gc-cons-threshold default-gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)

            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)))
