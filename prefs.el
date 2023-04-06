;; rrw's custom bindings
(global-set-key         "\M-g"         'goto-line)
(global-set-key         "\M-d"         'neotree-find)
(global-set-key         "\M-r"         'recompile)
(global-set-key         (kbd "C-1")         (lambda () (interactive) (insert "\t")))
;; Allow us to insert a hash on Macs.
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

; Just don't, eh?
(setq vc-handled-backends nil)

; Show trailing whitespace
(setq-default show-trailing-whitespace 't)

; No more eldoc!
(setq global-eldoc-node nil)


; Confirm killing emacs, in case of quirkafleegs.
(setq confirm-kill-emacs 'y-or-n-p)

; Don't auto-save files or link files - it causes npm / react to become confused.
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Load some packages I like
;;  csharp-mode cuda-mode dart-mode flutter rust-mode gradle-mode typescript-mode ini-mode dockerfile-mode godoctor go-tag go-stacktracer go-rename go-imports go-guru go-gopath go-fill-struct swift3-mode swift-mode jinja2-mode markdown-mode+ kubernetes jdee tide google-c-style adoc-mode markdown-mode go-mode yaml-mode terraform-mode kotlin-mode js2-mode dash assess
(use-package csharp-mode :ensure)
(use-package cuda-mode :ensure)
(use-package dart-mode :ensure)
(use-package flutter :ensure)
(use-package gradle-mode :ensure)
(use-package typescript-mode :ensure)
(use-package dockerfile-mode :ensure)
(use-package godoctor :ensure)
(use-package swift3-mode :ensure)
(use-package swift-mode :ensure)
(use-package jinja2-mode :ensure)
(use-package markdown-mode :ensure)
(use-package kubernetes :ensure)
(use-package jdee :ensure)
(use-package tide :ensure)
(use-package google-c-style :ensure)
(use-package adoc-mode :ensure)
(use-package go-mode :ensure)
(use-package yaml-mode :ensure)
(use-package kotlin-mode :ensure)
(use-package js2-mode :ensure)
(use-package dash :ensure)
(use-package assess :ensure)
(use-package rjsx-mode :ensure)
(use-package java-imports :ensure)
(use-package neotree :ensure)
(use-package terraform-mode :ensure)
(use-package solidity-mode :ensure)
(use-package tuareg :ensure)
(use-package graphql-mode :ensure)
(use-package groovy-mode :ensure)
(use-package svelte-mode :ensure)
(if (version< emacs-version "27.1")
    ()
    (use-package bazel :ensure))



;; Make ivy use a fixed height mb
(setq selectrum-fix-vertical-window-height t)
(setq selectrum-max-window-height 4)


;; whatever you want to bind it to
(define-key java-mode-map (kbd "M-I") 'java-imports-add-import-dwim)

;; See customization below for where to put java imports
(setq java-imports-find-block-function 'java-imports-find-place-sorted-block)

(add-hook 'java-mode-hook 'java-imports-scan-file)
(add-hook 'kotlin-mode-hook 'java-imports-scan-file)


;; Don't use tabs for indentation (at all)
(setq-default indent-tabs-mode nil)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq csharp-want-flymake-fixup 'nil)
(setq auto-mode-alist 
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(setq auto-mode-alist 
      (append '(("\\.jsx$" .
                 rjsx-mode)) auto-mode-alist))
(setq auto-mode-alist 
      (append '(("\\.tsx$" .
                 typescript-mode)) auto-mode-alist))

;; repo indent styles
(defun js-indent ()
  "jsx/tsx indentation"
  (interactive)
  (setq typescript-indent-level 2)
  (setq javascript-indent-level 2)
  )

(add-hook 'typescript-mode-hook 'js-indent)
(add-hook 'javascript-mode-hook 'js-indent)

(defun old-indentation ()
  "Old indentation"
  (interactive)
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq show-trailing-whitespace t)
  )

;; For working with the kernel
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(defun kernel-indentation() 
  "Use kernel indentation mode"
  (interactive) 
  (setq indent-tabs-mode t)
  (setq show-trailing-whitespace t)
  (c-set-style "linux-tabs-only"))


;; Java style
(autoload 'google-set-c-style "google-c-style")
(autoload 'google-make-newline-indent "google-c-style")
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)
            ))

;; Kotlin style
(setq-default kotlin-tab-width 2)
                                        ;
                                        ; Shut jdee-maven up
(setq jdee-maven-disabled-p t)
; Always split windows horizontally
(setq split-width-threshold nil)

                                        ; Start neotree
; I kept turning this off, so that's the default now - rrw 2022-08-17
; (neotree)
(global-set-key [f8] 'neotree-toggle)

; Selectrum's file completion is rather rubbish and slows me down :-(
; (now disabled - it's just not very useful)
                                        ;(defun my-find-file()   (interactive)   (progn (selectrum-mode -1) (unwind-protect ( progn (call-interactively 'find-file) (selectrum-mode 1)) (selectrum-mode 1))))
;(global-set-key (kbd "C-x C-f") 'my-find-file)


; Set the default window size
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 180) ; chars
              (height . 50) ; lines
              ;(left . 50)
                                        ;(top . 50)
              ))
      (setq default-frame-alist
            '(
              (tool-bar-lines . 0)
              (width . 180)
              (height . 50)
              ;(left . 50)
                                        ;(top . 50)
              )))
  (progn
    (setq initial-frame-alist '( (tool-bar-lines . 0)))
    (setq default-frame-alist '( (tool-bar-lines . 0)))))




