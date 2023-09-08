;;; init.el --- .emacs.d/init.el init file for emacs

;; Author: nasa9084

;;; Commentary:
;; init.el is setting initialize file for Emacs.
;; this is used on the newest Emacs w/GUI.

;;; Code:

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ general
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

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
  (setq major-mode 'scratch-mode))
(setq initial-major-mode 'scratch-mode)

;; Disable startup echo area message
(custom-set-variables '(inhibit-startup-echo-area-message (user-login-name)))

;; タイトルバー表示変更
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; フォント設定
(add-to-list 'default-frame-alist '(font . "ricty-13.5"))

;; coding system
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8-unix)

;; 選択領域の色
(set-face-background 'region "#555")

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#393939")

;; タブをスペースで扱う
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
(fset 'yes-or-no-p 'y-or-n-p)

;; 最近使ったファイルをメニューに表示
(recentf-mode t)
(defvar recentf-max-menu-items 10)
(defvar recentf-max-saved-items 100)
(defvar recentf-save-file "/var/tmp/recentf")

;; バックアップしない
(setq make-backup-files nil)
(setq auto-save-default nil)

;;スクロール指定
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)

;; ビープ音を出さない
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; shellをbashからzshに変更
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

;; キーストロークをミニバッファに早く表示
(setq echo-keystrokes 0.1)

;; C-h で[BS]
(global-unset-key (kbd "C-h"))
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; M-d でカーソル位置の単語を削除
(defun kill-word-at-point()
  "Function kill-word-at-point deletes a word where the cursor is."
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string=" " char) (delete-horizontal-space))
     ((string-match"[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))
(global-set-key "\M-d" 'kill-word-at-point)

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

;; disable upcase
(global-unset-key (kbd "M-u"))
(global-unset-key (kbd "M-c"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; カーソルの点滅をさせない
(blink-cursor-mode 0)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ロックファイルの生成を抑止
(setq create-lockfiles nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ autoinsert
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package autoinsert
  :hook (find-file . auto-insert)
  :config
  (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  (setq auto-insert-query nil))

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
  (if (equal old-name "go-dot-mod-mode") (setq old-name "go.mod"))
  (apply func old-name rules))

;; メジャーモード名を変換
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
          ("org" "org")))
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
  ;; 2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のディレクトリに
  (dired-dwim-target t)
  ;; ディレクトリのコピーをサブディレクトリについても実行
  (dired-recursive-copies 'always))

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

;; HTML/CSSの短縮入力
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
  :mode ("CODEOWNERS" . gitignore-mode))

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
;;; @ gradle
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package gradle-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ highlight-indent-guides
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package highlight-indent-guides
  :ensure t
  :diminish highlight-indent-guides-mode
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'fill)
  (setq highlight-indent-guides-auto-even-face-perc 0)
  (setq highlight-indent-guides-auto-odd-face-perc 7))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ hiwin
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 現在アクティブなウィンドウを可視化
(use-package hiwin
  :ensure t
  :config
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

;; javascript用モード
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
  (lsp-document-sync-method lsp--sync-incremental)
  (lsp-enable-file-watchers nil)
  (lsp-completion-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-print-performance nil)
  (lsp-log-io nil)
  (lsp-lens-enable nil)
  (lsp-imenu-show-container-name nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-signature-auto-activate nil)
  (lsp-response-timeout 5))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-deley 0.5)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-chidframe nil)
  (lsp-ui-doc-use-webkit nil))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ magit
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; gitクライアント
(use-package magit
  :ensure t
  :defer t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ marginalia-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; add more informations to the minibuffer completions
(use-package marginalia
  :ensure t
  :config
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
  (delete '("^readme" nerd-icons-octicon "nf-oct-book" :face nerd-icons-lcyan) nerd-icons-regexp-icon-alist))

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  ;; :hook does not work
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ nginx-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package nginx-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ org-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package org
  :config
  (setq org-startup-with-inline-images t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ paren
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 対応する括弧を光らせる
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
;;; @ popwin
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; popup window manager: used for gotest
(use-package popwin
  :ensure t
  :bind-keymap ("C-z" . popwin:keymap))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ prescient
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; sorting and filtering for completion
(use-package prescient
  :ensure t
  :commands prescient-persist-mode
  :config
  (setq prescient-filter-method 'fuzzy)
  (prescient-persist-mode))

(use-package vertico-prescient
  :ensure t
  :after prescient
  :config
  (vertico-prescient-mode))

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

;; 16進色文字列に色をつける
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

;; 改行を少し賢く
(use-package smart-newline
  :ensure t
  :bind (("C-j" . smart-newline)
         ("RET" . smart-newline)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ stash
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 変数永続化
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
;;; @ uniquify
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in uniquify
;; 同一バッファ名にディレクトリ付与
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
  ;(setq vertico-multiform-commands
  ;      '((t (vertico-sort-function . just-echo))))
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

;; 対話的正規表現置換
(use-package visual-regexp
  :ensure t
  :bind ("M-5" . vr/query-replace))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ volatile-highlights
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; undoやyankなどの操作をした部分を可視化
(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ web-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; HTML等用の編集モード
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

;; prefix-keyのあとの操作を教えてくれる
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-setup-side-window-bottom)
  (which-key-mode 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ yaml-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package yaml-mode
  :ensure t
  :hook (yaml-mode . lsp-deferred)
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

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
