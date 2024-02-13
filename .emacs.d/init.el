;;; init.el --- .emacs.d/init.el init file for emacs -*- lexical-binding: t -*-

;; Author: nasa9084

;;; Commentary:
;; init.el is setting initialize file for Emacs.
;; this is used on the newest Emacs w/GUI.

;;; Code:

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ general
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; workaround for the issue that the title bar becomes taller on macOS 14 (Sonoma)
;;
;; This commit will resolve the issue but has not yet released:
;; https://bitbucket.org/mituharu/emacs-mac/commits/5f6c306095c825eb01708e336f9d03c15271dfe9
(tool-bar-mode 1)
(tool-bar-mode 0)

;; Common Lisp
(require 'cl-lib)

;; generic config major mode
(require 'generic-x)

;; Configure *scratch* buffer
(setq initial-scratch-message ";; Welcome to emacs! This buffer is *scratch*.\n\n")
(defun scratch-mode ()
  "A lightweight major mode for *scratch* buffer."
  (interactive)
  (setq mode-name "scratch")
  (setq major-mode 'scratch-mode)
  ;; State doesn't enabled if the scratch buffer is recreated
  (state-mode)
  (cd "~"))
(setq initial-major-mode 'scratch-mode)

;; Disable startup echo area message
(custom-set-variables '(inhibit-startup-echo-area-message (user-login-name)))

;; Change title bar
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; show line numbers
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Use ricty 13.5 pt as default font
(add-to-list 'default-frame-alist '(font . "ricty-13.5"))

;; coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;; color for selected region
(set-face-background 'region "#555")

;; emphasize trailing whitespaces
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#393939")

;; use whitespaces as tab by default
(setq-default indent-tabs-mode nil)

;; custom
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; mode line format
(setq-default mode-line-format
      '(
        ""
        mode-line-mule-info mode-line-modified
        "-"
        mode-line-buffer-identification
        "%p / L%l:C%c ("
        mode-name mode-line-process minor-mode-alist ")"
        )
      )

;; tab width
(setq tab-width 4)

;; yes or no -> y or n
(setq use-short-answers t)

;; Automatically update buffers if the file is changed outside emacs
(global-auto-revert-mode t)

;; always follow symlinks without asking
(setq vc-follow-symlinks t)

;; I don't need backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; I don't need lock files
(setq create-lockfiles nil)

;; don't blink cursor
(blink-cursor-mode 0)

;; scrolling
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; no beep
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; create directory before opening file if the directory does not exist
(defun mkdir-before-open (orig &rest args)
  (let* ((directory (file-name-directory (car args))))
    (if (not (file-directory-p directory))
        (if (yes-or-no-p (format "Directory %s does not exist. Create? " directory))
            (make-directory directory t)))
    (apply orig args)))
(advice-add 'find-file :around 'mkdir-before-open)

;; use zsh
(cond ((string= system-type "darwin")
       (setq shell-file-name '"/opt/homebrew/bin/zsh"))
      (t ;; on linux
       (setq shell-file-name '"/usr/bin/zsh")))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ key-bind
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; show key storokes on minubuffer faster
(setq echo-keystrokes 0.1)

;; C-h -> backspace
(global-unset-key (kbd "C-h"))
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; word-based kill
(defun kill-word-at-point()
  "Function kill-word-at-point deletes a word where the cursor is."
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string=" " char) (delete-horizontal-space))
     ((string-match"[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))
(global-set-key "\M-d" 'kill-word-at-point)

;; never used set-fill-column and it's always typo of C-x C-f
(global-set-key (kbd "C-x f") #'find-file)

;; disable pinch-in/pinch-out action on TrackPad
(global-unset-key (kbd "<magnify-up>"))
(global-unset-key (kbd "<magnify-down>"))

;; disable swipe action on TrackPad
(global-unset-key (kbd "<swipe-left>"))
(global-unset-key (kbd "<swipe-right>"))

;; disable close keybind
(global-unset-key (kbd "C-x C-c"))

;; disable show menubar
(global-unset-key (kbd "C-<tab>"))

;; disable case edit
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-c"))
(global-unset-key (kbd "M-l"))

;; disable zap-to-char
(global-unset-key (kbd "M-z"))

;; disable zoom
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<wheel-up>"))

;; disable minimize
(global-unset-key (kbd "C-z"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ browse-at-remote
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; browse the line on the browser
(use-package browse-at-remote
  :ensure t
  :bind (("C-c b r" . browse-at-remote)
         ("C-c b k" . browse-at-remote-kill))
  :defines browse-at-remote-remote-type-regexps
  :custom (browse-at-remote-prefer-symbolic nil)
  :config
  (add-to-list 'browse-at-remote-remote-type-regexps
               `(:host ,(rx bol "git.")
                       :type "github")))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ consult
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package consult
  :ensure t
  :bind (("C-M-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ([remap goto-line] . consult-goto-line))
  :config
  (consult-customize
   consult-line :prompt "Search: ")
  (setq consult-buffer-sources '(
                                 consult--source-buffer
                                 consult--source-modified-buffer
                                 consult--source-project-buffer-hidden)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ cua-base
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in CUA-mode
;; for rectangle selection
(use-package cua-base
  :custom
  (cua-enable-cua-keys nil "Disable CUA keybindings e.g. C-x for cut")
  :config
  (cua-mode t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ cyphejor
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; The major mode name for go.mod file is go-dot-mod-mode which is
;; split into "go dot mod mode" internally in cyphejor--cypher function
;; and is difficult to be converted into other name as a "go-dot-mod"
;; group. To do the conversion easier, use advice for cyphejor--cypher
;; function.
(defun cyphejor--go-dot-mod-mode-advice (func old-name &rest rules)
  (if (string= old-name "go-dot-mod-mode") (setq old-name "go.mod"))
  (apply func old-name rules))

;; convert major mode names
(use-package cyphejor
  :ensure t
  :init
  (setq cyphejor-rules
        '(("emacs" "e")
          ("interaction" "i" :prefix)
          ("markdown" "md")
          ("mode" "" :postfix)
          ;; tautology rules for removing space
          ("js2" "js2")
          ("lisp" "lisp")
          ("org" "org")
          ("yaml" "yaml")))
  :config
  (advice-add 'cyphejor--cypher :around #'cyphejor--go-dot-mod-mode-advice)

  (cyphejor-mode 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ diminish
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; hide some minor modes
(use-package diminish
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ dired
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in dired
(use-package dired
  :custom
  ;; I might want to copy/move a file to another directory when I opening two dired
  (dired-dwim-target t)

  (dired-recursive-copies 'always))


(use-package ls-lisp
  :init
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-use-insert-directory-program nil))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ display-fill-column-indicator
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package display-fill-column-indicator
  :hook (git-commit-mode . (lambda ()
                             (setq display-fill-column-indicator-column 68)
                             (display-fill-column-indicator-mode))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ dockerfile-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package dockerfile-mode
  :ensure t
  :hook (dockerfile-mode . lsp-deferred)
  :mode ("Dockerfile\\'" . dockerfile-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ editorconfig
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ emmet-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :hook  ((sgml-mode . emmet-mode)
          (css-mode . emmet-mode)
          (web-mode . emmet-mode))
  :bind (:map emmet-mode-keymap
              ("C-j" . nil)
              ("H-i" . emmet-expand-line))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-indentation 2)

  (keyboard-translate ?\C-i ?\H-i))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ exec-path-from-shell
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package exec-path-from-shell
  :ensure t
  :hook (after-init . (lambda ()
                        (when (memq system-type '("darwin"))
                          (let ((envs '("PATH" "HOME" "GOPATH" "GOPRIVATE")))
                            (exec-path-from-shell-copy-envs envs))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ flycheck
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package flycheck-pos-tip
  :ensure t)

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :init
  ;; https://www.flycheck.org/en/latest/user/syntax-checkers.html#variable-flycheck-disabled-checkers
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :bind(("C-c C-n" . flycheck-next-error)
        ("C-c C-p" . flycheck-previous-error))
  :config
  (flycheck-pos-tip-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ git-gutter
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package git-gutter
  :ensure t
  :diminish
  :config (global-git-gutter-mode 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ git-modes
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package git-modes
  :ensure t
  :mode (("CODEOWNERS" . gitignore-mode)
         (".dockerignore" . gitignore-mode)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ GitHub Actions
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package flycheck-actionlint
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ go
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :hook ((go-mode . lsp-deferred)
         (go-mode . lsp-go-install-save-hooks)
         (go-dot-mod-mode . display-line-numbers-mode))
  :functions (lsp-format-buffer lsp-organize-imports)
  :config
  (defun lsp-go-install-save-hooks()
    "Add file save hook provided by lsp."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ groovy-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package groovy-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ highlight-indent-guides
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :hook ((prog-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode)) ; yaml-mode is not a prog-mode...
  :config
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-auto-even-face-perc 0)
  (setq highlight-indent-guides-auto-odd-face-perc 7))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ hiwin
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; visualize current active window
(use-package hiwin
  :ensure t
  :functions hiwin-create-ol-after
  :config
  (defun hiwin-create-ol-after () (set-face-extend 'hiwin-face t))
  (advice-add 'hiwin-create-ol :after #'hiwin-create-ol-after)
  (hiwin-activate))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ isearch
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in isearch
(use-package isearch
  :config
  (setq case-fold-search t)
  (setq isearch-case-fold-search t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ javascript
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; major mode for javascript
(use-package rjsx-mode
  :ensure t
  :mode ("\\.js\\'" . rjsx-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ jinja2-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package jinja2-mode
  :ensure t
  :mode ("\\.tmpl\\'" . jinja2-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ json-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package json-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ jsonnet-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package jsonnet-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ k8s-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package k8s-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ kotlin-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package kotlin-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ lsp-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Language Server
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :custom
  (gc-cons-threshold (* gc-cons-threshold 150))
  (lsp-auto-guess-root t)
  (lsp-completion-enable nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-document-sync-method lsp--sync-incremental)
  (lsp-enable-file-watchers nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-imenu-show-container-name nil)
  (lsp-lens-enable nil)
  (lsp-log-io nil)
  (lsp-print-performance nil)
  (lsp-response-timeout 5)
  (lsp-signature-auto-activate nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions))
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-deley 0.5)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-webkit nil))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ magit
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package magit
  :ensure t
  :defer t
  :commands (magit--completion-table)
  :functions (magit-completion-table-with-sort
              builtin-completing-read)
  :config
  (defun sort-preferred-remote-first (branches)
    (let ((preferred-push-remote-prefix "origin/"))
      (nconc (seq-filter (lambda (x) (string-prefix-p preferred-push-remote-prefix x)) branches)
             (seq-remove (lambda (x) (string-prefix-p preferred-push-remote-prefix x)) branches))))

  (defun magit-completion-table-with-sort (collection)
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (display-sort-function . sort-preferred-remote-first))
        (complete-with-action action collection string pred))))

  (defun builtin-completing-read
      (prompt choices &optional predicate require-match initial-input hist def)
    "Wrapper for standard `completing-read' function to be used by magit."
    (pcase this-command
      ('magit-push-current-to-upstream
       ;; use my sort function to sort candidate branches
       (setq choices (magit-completion-table-with-sort choices))
       ;; I don't want origin/master to be on top when push
       ;; ref. https://github.com/magit/magit/blob/7bef529ce9b07808d5c14692c5ab2d248e7b5bd1/lisp/magit-push.el#L141
       (when (string= def "origin/master") (setq def nil)))
      (_
       ;; otherwise use magit's default completion-table function
       (setq choices (magit--completion-table choices))))
    (completing-read prompt choices predicate require-match initial-input hist def))

  (setq magit-completing-read-function #'builtin-completing-read))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ marginalia
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; add more informations to the minibuffer completions
(use-package marginalia
  :ensure t
  :functions (marginalia--time-absolute--month-number)
  :config
  (defun marginalia--time-absolute--month-number (time)
    "Format TIME as an absolute age but use month number instead of month name."
    (let ((system-time-locale "C"))
      (format-time-string
       (if (> (decoded-time-year (decode-time (current-time)))
              (decoded-time-year (decode-time time)))
           " %Y-%m-%d"
         "%m-%d %H:%M")
       time)))
  (advice-add 'marginalia--time-absolute :override #'marginalia--time-absolute--month-number)
  (marginalia-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Markdown-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode (("\\.markdown\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.text\\'" . gfm-mode)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ nerd-icons
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package nerd-icons
  :ensure t
  :config
  ;; prefer markdown logo even for README.md file
  (delete '("^readme" nerd-icons-octicon "nf-oct-book" :face nerd-icons-lcyan) nerd-icons-regexp-icon-alist)
  ;; prefer Go logo instead of go gopher, as the font size is too small to see what it is in the completion
  (add-to-list 'nerd-icons-extension-icon-alist '("go" nerd-icons-mdicon "nf-md-language_go" :face nerd-icons-blue))
  ;; gradle icon for gradlew
  (add-to-list 'nerd-icons-regexp-icon-alist '("^gradlew\\'" nerd-icons-sucicon "nf-seti-gradle" :face nerd-icons-green)))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  ;; :hook does not work
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ nginx-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package nginx-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ orderless
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  (setq orderless-matching-styles '(orderless-flex orderless-regexp)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ org-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package org
  :config
  (setq org-startup-with-inline-images t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ paren
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ perl-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in perl-mode
(use-package perl
  :mode ("\\.psgi\\'" . perl-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ protobuf-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; protocolbuffers
(use-package protobuf-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ python-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in python.el
(use-package python
  :mode ("\\.wsgi\\'" . python-mode)
  :bind (:map python-mode-map
              ("C-m" . newline-and-indent)
              ("C-c s" . python-pep8)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rainbow-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :hook ((css-mode . rainbow-mode)
         (scss-mode . rainbow-mode)
         (web-mode . rainbow-mode))
  :config
  (setq rainbow-html-colors t)
  (setq rainbow-latex-colors t)
  (setq rainbow-x-colors t)
  (setq rainbow-ansi-colors t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Rego / OpenPolicyAgent
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package rego-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ review-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Major mode for Re:VIEW
(use-package review-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rust
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode))
  :hook (rust-mode . lsp-deferred)
  :custom (rust-format-on-save t))

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ savehist
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in savehist
(use-package savehist
  :custom (savehist-ignored-variables . (file-name-history))
  :init
  ;; Save minibuffer history
  (savehist-mode 1)
  (setq history-length 500))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ sh-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(defun sh-set-shell-tmp-path-advice (func &rest r)
  (let ((exec-path (cons "/bin/" exec-path))) (apply func r)))

(advice-add 'sh-set-shell
            :around 'sh-set-shell-tmp-path-advice)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ shell-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package comint
  :custom (comint-scroll-show-maximum-output t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ ssh-config-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package ssh-config-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ smart-newline
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package smart-newline
  :ensure t
  :bind (("C-j" . smart-newline)
         ("RET" . smart-newline)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ stash
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; persistent some variables
(use-package stash
  :ensure t
  :config
  (defstash kill-ring "kill-ring.el" nil (or stashed 'nil))
  (setq stash-directory "/tmp/stashes"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ state
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package state
  :ensure t
  :diminish state-mode
  :init
  (defvar state-keymap-prefix (kbd "C-c C-x s"))

  :config
  ;; [scratch state] Open *scratch* by prefix s
  (state-define-state
    scratch
    :key "s"
    :switch "*scratch*"
    :create (scratch-buffer))

  ;; [emacs state] Open init.el by prefix e
  (state-define-state
    emacs
    :key "e"
    :in "init.el"
    :create (find-file "~/.emacs.d/init.el"))

  (state-global-mode 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ sql-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package sql-indent
  :ensure t)

(use-package sql
  :hook (sql-mode . sqlind-minor-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ terraform-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package terraform-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ tree-sitter
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; define tree-sitter grammar source
(setq treesit-language-source-alist
      '((yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;; automatically install if the grammar has not been installed
(dolist (element treesit-language-source-alist)
  (let* ((lang (car element)))
    (if (treesit-language-available-p lang)
        (message "tree-sistter: %s is already installed" lang)
      (message "tree-sitter: %s is not installed" lang)
      (treesit-install-language-grammar lang))))
;; enable "TS"-powered modes
(setq major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ undo-fu
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package undo-fu
  :ensure t
  :bind (([remap undo] . undo-fu-only-undo)
         ([remap undo-redo] . undo-fu-only-redo))
  :init
  (setq undo-limit 67108864) ; 64mb
  (setq undo-strong-limit 100663296) ; 96mb.
  (setq undo-outer-limit 1006632960)) ; 960mb.

(use-package undo-fu-session
  :ensure t
  :init
  (undo-fu-session-global-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ uniquify
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in uniquify
;; add directory name when I open two or more files which have same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ vertico
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("<left>" . vertico-directory-up)
              ("<right>" . vertico-insert))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-count 32)
  (setq enable-recursive-minibuffers t)
  (dolist (ext '(".DS_Store")) (add-to-list 'completion-ignored-extensions ext)))

(use-package vertico-multiform
  :after vertico
  :defines vertico-multiform-categories
  :commands vertico-sort-alpha
  :init
  (defun sort-directories-first (files)
    (setq files (vertico-sort-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  (setq vertico-multiform-categories
        '((file (vertico-sort-function . sort-directories-first)))))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("<backspace>" . vertico-directory-delete-char)
              ("RET" . vertico-directory-enter)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ visual-regexp
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package visual-regexp
  :ensure t
  :bind ("M-5" . vr/query-replace))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ volatile-highlights
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; visualize paste/undo results
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ web-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.xhtml\\'" . web-mode)
         ("\\.shtml\\'" . web-mode)
         ("\\.tpl\\'" . web-mode)
         ("\\.tmpl\\'" . web-mode)
         ("\\.tag\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-attr-indent-offset nil)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-sql-indent-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ which-key
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ YAML
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in yaml-ts-mode
(use-package yaml-ts-mode
  :hook (yaml-ts-mode . lsp-deferred))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ theme
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "theme"))
             )
(load-theme 'gnupack-dark t)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; byte-compile-warnings: (not cl-functions obsolete) ; emmet-mode still uses cl
;; End:

;;; init.el ends here
