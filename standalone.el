;; This is to support loading from a non-standard .emacs.d
;; via emacs -q --load "/path/to/standalone.el"
;; see https://emacs.stackexchange.com/a/4258/22184

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
; Ensure that the custom file exists.
(write-region nil nil custom-file)

(load custom-file)
(require 'package)
; Let's just look at MELPA
; (add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

;; Work around an Emacs v26 bug.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/
(when (version< emacs-version "27")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Install use-package that we require for managing all other dependencies
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; I find these light-weight and helpful

(use-package which-key
  :ensure
  :init
  (which-key-mode))

; selectrum causes more trouble than it's worth
;(use-package selectrum
;  :ensure
;  :init
;  (selectrum-mode)
;  :bind(:map selectrum-minibuffer-map
;             ("C-m" . selectrum-submit-exact-input) )
;  :custom
;  (completion-styles '(flex substring partial-completion)))

(load-file (expand-file-name "scilla-mode/scilla-mode.el" user-emacs-directory))
(use-package flycheck :ensure)


;; Some common sense settings

					;(load-theme 'leuven t)
;; I don't like this - rrw
;; (fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))



(load-file (expand-file-name "monokai-rrw.el" user-emacs-directory))
;; set theme
(enable-theme 'monokai)
(cond
; ((member "Latin Modern Mono" (font-family-list))
;  (set-face-attribute 'default nil :font "Latin Modern Mono" :height 140))
 ((member "Monaco" (font-family-list))
  (set-face-attribute 'default nil :font "Monaco-12"))
 ((member "Inconsolata" (font-family-list))
  (set-face-attribute 'default nil :font "Inconsolata-12"))
 ((member "Consolas" (font-family-list))
  (set-face-attribute 'default nil :font "Consolas-11"))
 ((member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-12")))

(load-file (expand-file-name "rust.el" user-emacs-directory))
(load-file (expand-file-name "prefs.el" user-emacs-directory))
(load-file (expand-file-name "kotlin.el" user-emacs-directory))

