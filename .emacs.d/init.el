;;; init.el --- .emacs.d/init.el init file for emacs

;; Author: nasa9084

;;; Commentary:
;; init.el is setting initialize file for Emacs.
;; this is used on the newest Emacs w/GUI.
;; by nasa9084

;;; Code:

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ general
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(setq initial-frame-alist
      (append (list
               '(width . 180)
               '(height . 75)
               )
              initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; Common Lisp
(require 'cl-lib)

;; generic config major mode
(require 'generic-x)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; スタートアップ非表示
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Welcome to emacs! This buffer is *scratch*.\n\n")

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; ツールバー非表示
(tool-bar-mode -1)
(menu-bar-mode -1)

;; タイトルバー表示変更
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;; 行番号表示
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; フォント設定
(add-to-list 'default-frame-alist '(font . "ricty-13.5"))

;; 優先文字コード
(prefer-coding-system 'utf-8)

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

;; ミニバッファの履歴を保存する
(savehist-mode 1)
(setq history-length 500)
(defvar savehist-file "/var/tmp/emacs-savehist-history")
(defvar savehist-ignored-variables '(file-name-history))

;; バックアップしない
(setq make-backup-files nil)
(setq auto-save-default nil)

;;スクロール指定
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(defvar comint-scroll-show-maximum-output t) ;;shell-mode

;; ビープ音を出さない
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; shellをbashからzshに変更
(cond ((string= system-type "darwin")
       (setq shell-file-name '"/usr/local/bin/zsh"))
      (t ;; on linux
       (setq shell-file-name '"/usr/bin/zsh")))

(use-package exec-path-from-shell
  :ensure t)
(cond ((string= system-type "darwin")
       (let ((envs '("PATH" "HOME" "GOPATH" "GOPRIVATE")))
         (exec-path-from-shell-copy-envs envs))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ key-bind
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; キーストロークをミニバッファに早く表示
(setq echo-keystrokes 0.1)

;; C-[Ret] で矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; C-h で[BS]
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; C-[Ret] M-Iでデクリメント
(defun cua-decr-rectangle (DECREMENT)
  "Decrement each line of CUA rectangle by prefix amount."
  (interactive "p")
  (cua-incr-rectangle (- DECREMENT)))
(define-key cua--rectangle-keymap (kbd "M-I") 'cua-decr-rectangle)

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

;; flycheck次のエラー・前のエラー
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)

;; multi-term
(global-set-key (kbd "C-c t") '(lambda ()
                                 (interactive)
                                 (if (get-buffer "*terminal<1>*")
                                     (switch-to-buffer "*terminal<1>*")
                                   (multi-term))))

;; TrackPadのピンチイン・ピンチアウトを無効化
(global-set-key (kbd "<magnify-up>") nil)
(global-set-key (kbd "<magnify-down>") nil)

;; disable close keybind
(global-set-key (kbd "C-x C-c") nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; デフォルトの言語環境
(set-language-environment "Japanese")

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)

;; 端末の文字コード
(set-terminal-coding-system 'utf-8-unix)

;; ファイル名の文字コード
;; windows環境ではshift-jis
;; linux環境ではutf-8-unix
(cond
 ((eq window-system 'w32)
  (setq file-name-coding-system 'sjis))
 ((eq system-type 'gnu/linux)
  (setq file-name-coding-system 'utf-8-unix)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; カーソルの点滅をさせない
(blink-cursor-mode 0)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 大文字・小文字を区別しないでサーチ
(setq case-fold-search t)
(setq isearch-case-fold-search t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ dired
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のディレクトリに
(defvar dired-dwim-target t)

;; ディレクトリのコピーをサブディレクトリについても実行
(defvar dired-recursive-copies 'always)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ロックファイルの生成を抑止
(setq create-lockfiles nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto-async-byte-compile
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 自動コンパイル
(use-package auto-async-byte-compile
  :ensure t
  :config
  ;; 自動コンパイルを無効にするファイル名の正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "init.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ autoinsert
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package autoinsert
  :config
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert-directory "~/.emacs.d/autoinsert/")
  (setq auto-insert-query nil))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ bury successful compilation
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; コンパイル成功時にcompilationバッファを自動で閉じる
(use-package bury-successful-compilation
  :ensure t
  :config
  (bury-successful-compilation 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ company
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 補完
(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode 1)

  ;; C-n, C-pで補完候補を次/前の候補を選択
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-search-map (kbd "C-n") 'company-select-next)
  (define-key company-search-map (kbd "C-p") 'company-select-previous)

  ;; C-sで絞り込む
  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)

  ;; TABで候補を設定
  (define-key company-active-map (kbd "C-i") 'company-complete-selection)

  :config
  (defvar company-dabbrev-downcase nil)
  (set-face-attribute 'company-tooltip nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "black" :background "lightgrey")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "white" :background "steelblue")
  (set-face-attribute 'company-tooltip-selection nil
                      :foreground "black" :background "steelblue")
  (set-face-attribute 'company-preview-common nil
                      :foreground "lightgrey" :background nil :underline t)
  (set-face-attribute 'company-scrollbar-fg nil
                      :background "gray40")
  (set-face-attribute 'company-scrollbar-bg nil
                      :background "gray"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ cyphejor
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; メジャーモード名を変換
(use-package cyphejor
  :ensure t
  :config
  (setq cyphejor-rules
        '(("emacs" "e")
          ("interaction" "i" :prefix)
          ("js2" "js2")
          ("lisp" "lisp")
          ("markdown" "md")
          ("org" "org")
          ("twittering" "tw")
          ("mode" "" :postfix)))
  (cyphejor-mode 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ diminish
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; hide some minor modes
(use-package diminish
  :ensure t)
(diminish 'auto-revert-modee)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ dockerfile-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; major mode for docker
(use-package dockerfile-mode
  :ensure t
  :hook
  (dockerfile-mode . lsp-deferred)
  :mode
  ("Dockerfile$" . dockerfile-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ editorconfig
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; editorconfig plugin
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
  :init
  ;; HTML, CSSでemmet-mode
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)

  ;; インデントは2文字分
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))

  :config
  ;; 展開語は最初のクォーテーションの間にカーソル
  (setq emmet-move-cursor-between-quotes t)

  ;; 展開のキーバインドをC-j -> C-i
  (eval-after-load "emmet-mode"
    '(define-key emmet-mode-keymap (kbd "C-j") nil))
  (keyboard-translate ?\C-i ?\H-i)
  (define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ flycheck
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package flycheck-pos-tip
  :ensure t)
(use-package flycheck
  :ensure t
  :init
  ;; 文法チェック
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; エラーをツールチップ表示
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode)))
(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))
(flycheck-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ git-gutter
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package git-gutter
  :ensure t
  :diminish
  :init (global-git-gutter-mode 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ gitconfig-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package gitconfig-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ go
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; for golang
(defun lsp-go-install-save-hooks()
  "Add file save hook provided by lsp."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode))
  :hook
  (go-mode . lsp-deferred)
  :init
  (add-hook 'go-mode-hook (lambda()
                            (local-set-key (kbd "M-.") 'godef-jump)))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-dot-mod-mode-hook #'display-line-numbers-mode))

(use-package gotest
  :ensure t
  :after (popwin)
  :config
  (setq go-test-verbose t)
  (define-key go-mode-map (kbd "C-c C-t") 'go-test-current-file)
  (define-key go-mode-map (kbd "C-c t") 'go-test-current-test)
  (push '("\*Go Test\*" :regexp t :height 0.5 :stick t) popwin:special-display-config))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ helm
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Helm.el
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (global-set-key (kbd "C-c h") 'helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)

  (define-key global-map (kbd "M-x") 'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "C-c C-s") 'helm-ghq)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "<left>") 'helm-previous-source)
  (define-key helm-map (kbd "<right>") 'helm-next-source)
  (customize-set-variable 'helm-ff-lynx-style-map t)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))
  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ hiwin
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 現在アクティブなウィンドウを可視化
(use-package hiwin
  :ensure t
  :config
  ;; hiwin-modeを有効化
  (hiwin-activate))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ javascript
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; javascript用モード
(use-package rjsx-mode
  :ensure t
  :mode
  (("\\.js\\'" . rjsx-mode)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ jinja2-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; for Jinja2 Template
(use-package jinja2-mode
  :ensure t
  :mode
  ("\\.tmpl\\'" . jinja2-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ json-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; json用モード
(use-package json-mode
  :ensure t
  :mode
  (("\\.json\\'" . json-mode)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ k8s-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; major mode for Kubernetes configuration
(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ lsp-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Language Server
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook(
        (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (gc-cons-threshold (* gc-cons-threshold 150))
  (lsp-document-sync-method lsp--sync-incremental)
  (lsp-enable-file-watchers nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-print-performance nil)
  (lsp-log-io nil)
  (lsp-imenu-show-container-name nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-signature-auto-activate nil)
  (lsp-response-timeout 5)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-deley 0.5)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-position 'top)
  (lsp-ui-doc-use-chidframe nil)
  (lsp-ui-doc-use-webkit nil)
)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ magit
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; gitクライアント
(use-package magit
  :ensure t
  :defer t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Markdown-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; markdown用モード
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode
  (("\\.markdown\\'" . gfm-mode)
   ("\\.md\\'" . gfm-mode)
   ("\\.text\\'" . gfm-mode)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ nginx-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; for nginx configurations
(use-package nginx-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ org-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(defvar org-startup-with-inline-images t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ paren
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 対応する括弧を光らせる
(use-package paren
  :hook
  (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ perl-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in perl-mode
(add-to-list 'auto-mode-alist
             '("\\.psgi$" . perl-mode) t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ popwin
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; popup window manager: used for gotest
(use-package popwin
  :ensure t
  :config
  (global-set-key (kbd "C-z") popwin:keymap))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ protobuf-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; protocolbuffers
(use-package protobuf-mode
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ python.el
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; built-in python.el
(add-to-list 'auto-mode-alist
             '("\\.wsgi$" . python-mode) t)
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "C-m") 'newline-and-indent)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ python-pep8
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; python-pep8
(add-hook 'python-mode-hook
          '(lambda ()
             (define-key python-mode-map (kbd "C-c s") 'python-pep8)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rainbow-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 16進色文字列に色をつける
(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'scss-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode)
  :config
  (setq rainbow-html-colors t)
  (setq rainbow-latex-colors t)
  (setq rainbow-x-colors t)
  (setq rainbow-ansi-colors t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ rust
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; rust
(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode))
  :hook
  (rust-mode . lsp-deferred)
  :custom rust-format-on-save t)

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

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
  :config
  (global-set-key (kbd "C-j") 'smart-newline)
  (global-set-key (kbd "RET") 'smart-newline))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ stash
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 変数永続化
(use-package stash
  :ensure t)
(defstash kill-ring "kill-ring.el" nil (or stashed 'nil))
(setq stash-directory "/tmp/stashes")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ state
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; stateのプレフィクスキーをC-c C-x sにする(読み込み前に指定)
(defvar state-keymap-prefix (kbd "C-c C-x s"))

(use-package state
  :ensure t
  :diminish state-mode
  :init
  ;; マイナーモードを有効にする
  (state-global-mode 1)

  :config
  ;; [scratch state]prefix sで*scratch*に切り替える
  (state-define-state
      scratch
    :key "s"
    :switch "*scratch*")

  ;; [twit state]prefix tでtwittering-modeに切替える
  (state-define-state
      twit
    :key "t"
    ;; この条件を満たすときにtwit stateとみなす
    :in (string= major-mode 'twittering-mode)
    ;; (twit) でtwittering-modeにする
    :create twit)

  ;; [emacsstate]prefix eでEmacs設定ファイルを編集する
  (state-define-state
    emacs
    :key "e"
    :in "init.el"
    ;; どれも見付からないときは init.el を開く
    :create (find-file "~/.emacs.d/init.el")))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ sql-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package sql-indent
  :ensure t)
(eval-after-load "sql"
  '(load-library "sql-indent"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ twittering-mode
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; emacs用twitterクライアント
(use-package twittering-mode
  :ensure t
  :defer t
  :config
  ;; 公式retweetキーバインド
  (add-hook 'twittering-mode-hook
            '(lambda ()
               (define-key twittering-mode-map (kbd "C-u C-c RET") 'twittering-native-retweet)))

  ;; マスターパスワードの使用->毎回PINを入力しなくてよくなる
  (setq twittering-use-master-password t)

  ;; アイコンの表示
  (setq twittering-icon-mode t)

  ;; アイコンをダウンロードする
  (setq twittering-icon-storage-limit t)

  ;; 起動時に自動で開くTL
  (setq twittering-initial-timeline-spec-string
        '(":mentions"
          ":home"))

  ;; 表示する書式
  (setq twittering-status-format
        "%i %S (@%s) - %@:
%T
// via %f%L%RT{ retweeted by %S(%s)}
")

  ;; 割り込みReply時に全員に返信、ハッシュタグ自動挿入
  (setq twittering-edit-skeleton 'inherit-any)

  ;; C-c <RET>で引用RT
  (setq twittering-retweet-format '(nil _ " %u"))

  ;; URLを開くときにブラウザを聞く
  (defadvice browse-url (before select-browser activate)
    (if (y-or-n-p "Open with eww? ")
        (setq-local browse-url-browser-function 'eww-browse-url)
      (setq-local browse-url-browser-function 'browse-url-default-browser))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ visual-regexp
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 対話的正規表現置換
(use-package visual-regexp
  :ensure t)
(global-set-key (kbd "M-5") 'vr/query-replace)

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
  (setq tab-width 2)
  )

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

;; YAML
(use-package yaml-mode
  :ensure t
  :hook
  (yaml-mode . lsp-deferred)
  :mode
  ("\\.yml\\'" . yaml-mode)
  ("\\.yaml\\'" . yaml-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ theme
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; テーマ格納ディレクトリのパス追加
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "theme"))
             )

;; テーマ選択
(load-theme 'gnupack-dark t)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; init.el ends here
