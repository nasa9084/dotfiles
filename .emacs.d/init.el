;;; init.el --- .emacs.d/init.el init file for emacs

;; Author: nasa9084

;;; Commentary:
;; init.el is setting initialize file for emacs.
;; by nasa9084

;;; Code:

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ load-path                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; elispフォルダをロード
(add-to-load-path "elisp")

;; elpaフォルダをロード
(add-to-load-path "elpa")

;; site-lispフォルダをロード
(add-to-load-path "site-lisp")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/") t)

(package-initialize)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ general                                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Common Lisp
(require 'cl)

;; use-package
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
(use-package linum
  :config
  (set-face-attribute 'linum nil
                      :height 0.9)
  (setq linum-format "%4d"))

;; フォント設定
(add-to-list 'default-frame-alist '(font . "ricty-13.5"))

;; 対応する括弧を光らせる
(show-paren-mode 1)
(setq show-paren-delay 0)

;; 予約語を色分けする
(global-font-lock-mode t)

;; 選択領域の色
(set-face-background 'region "#555")

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#393939")

;; 保存時に行末の空白を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; タブ幅
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(mode-line-format
   (quote
    ("" mode-line-mule-info mode-line-modified "-" mode-line-buffer-identification "%p / L%l:C%c (" mode-name mode-line-process minor-mode-alist ")")))
 '(package-selected-packages
   (quote
    (markdown-mode mode-compile smart-compile codic auto-complete hiwin yatex yasnippet web-mode volatile-highlights use-package undohist undo-tree twittering-mode stash smooth-scroll smart-newline multi-term haskell-mode flycheck emmet-mode bury-successful-compilation auto-install auto-compile auto-async-byte-compile)))
 '(tab-width 4))

;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 最近使ったファイルをメニューに表示
(recentf-mode t)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 100)

;; ミニバッファの履歴を保存する
(savehist-mode 1)
(setq history-length 500)

;; バックアップしない
(setq make-backup-files nil)

;;スクロール指定
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;;shell-mode

;; ビープ音を出さない
(setq visible-bell t)
(setq ring-bell-function 'ignore)

;; shellをbashからzshに変更
(setq hostname (system-name))
(cond ((string= (system-name) "nasa-ubuntu")
       (setq shell-file-name '"/usr/bin/zsh"))
      ((string= (system-name) "NASA-THINK")
       (setq shell-file-name '"C:\\msys32\\usr\\bin\\zsh.exe"))
      (t
       (setq shell-file-name '"~/.local/bin/zsh")))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ key-bind                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; キーストロークをミニバッファに早く表示
(setq echo-keystrokes 0.1)

;; C-[Ret] で矩形選択
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; M-d でカーソル位置の単語を削除
(defun kill-word-at-point()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string=" " char) (delete-horizontal-space))
     ((string-match"[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))
(global-set-key "\M-d" 'kill-word-at-point)

;; 画面内のカーソル移動
(global-set-key (kbd "C-M-t") (lambda () (interactive) (move-to-window-line 0)))
(global-set-key (kbd "C-M-m") (lambda () (interactive) (move-to-window-line nil)))
(global-set-key (kbd "C-M-l") (lambda () (interactive) (move-to-window-line -1)))

;; flycheck次のエラー・前のエラー
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)

;; multi-term
(global-set-key (kbd "C-c t") '(lambda ()
                                 (interactive)
                                 (if (get-buffer "*terminal<1>*")
                                     (switch-to-buffer "*terminal<1>*")
                                   (multi-term))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
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
;;; @ language - input method                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; windows使用時のIME設定
(cond
 ((eq window-system 'w32)
  ;; IME初期化
  (w32-ime-initialize)

  ;; デフォルトIME
  (setq default-input-method "W32-IME")))

;; 漢字/変換キー入力時のエラーメッセージ抑止
(global-set-key (kbd "<A-kanji>") 'ignore)
(global-set-key (kbd "<M-kanji>") 'ignore)
(global-set-key (kbd "<kanji>") 'ignore)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 同一バッファ名にディレクトリ付与
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq uniquify-ignore-buffers-re "*[^*]+*"))

;; ウィンドウの縦分割横分割を入れ替え
(defun window-toggle-division ()
  "ウィンドウ 2 分割時に、縦分割横分割"
  (interactive)
  (unless (= (count-windows 1) 2)
    (error "ウィンドウが 2 分割されていません。"))
  (let (before-height (other-buf (window-buffer (next-window))))
    (setq before-height (window-height))
    (delete-other-windows)
    (if (= (window-height) before-height)
        (split-window-vertically)
      (split-window-horizontally)
      )

    (switch-to-buffer-other-window other-buf)
    (other-window -1)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - cursor                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; カーソルの点滅
(blink-cursor-mode 0)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ search - isearch                                              ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 大文字・小文字を区別しないでサーチ
(setq case-fold-search t)
(setq isearch-case-fold-search t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ dired                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のディレクトリに
(setq dired-dwim-target t)

;; ディレクトリのコピーをサブディレクトリについても実行
(setq dired-recursive-copies 'always)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ eww                                                           ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; emacs組み込みブラウザ
(use-package eww
  :config
  (defvar eww-disable-colorize t)
  (defun shr-colorize-region--disable (orig start end fg &optional bg &rest _)
    (unless eww-disable-colorize
      (funcall orig start end fg)))
  (advice-add 'shr-colorize-region :around 'shr-colorize-region--disable)
  (advice-add 'eww-colorize-region :around 'shr-colorize-region--disable)
  (defun eww-disable-color ()
    "eww で文字色を反映させない"
    (interactive)
    (setq-local eww-disable-colorize t)
    (setq-default show-trailing-whitespace nil)
    (eww-reload))
  (defun eww-enable-color ()
    "eww で文字色を反映させる"
    (interactive)
    (setq-local eww-disable-colorize nil)
    (setq-default show-trailing-whitespace t)
    (eww-reload))
  (setq eww-search-prefix "http://www.google.co.jp/search?q="))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - hiwin                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 現在アクティブなウィンドウを可視化
(use-package hiwin
  :config
  ;; hiwin-modeを有効化
  (hiwin-activate))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ロックファイルの生成を抑止
(setq create-lockfiles nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto-async-byte-compile                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 自動コンパイル
;; http://www.emacswiki.org/emacs/auto-async-byte-compile.el
(use-package auto-async-byte-compile
  :ensure t
  :config
  ;; 自動コンパイルを無効にするファイル名の正規表現
  (setq auto-async-byte-compile-exclude-files-regexp "init.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto-complete                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (ac-config-default)
  (define-key ac-complete-mode-map "\r" nil)
  (push 'ac-source-filename ac-sources)
  (setq ac-ignore-case 'smart))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ auto-install                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package auto-install
  :ensure t
  :config
  (setq auto-install-directory "~/.emacs.d/elisp")
  ;;(auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ bury successful compilation                                   ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; コンパイル成功時にcompilationバッファを自動で閉じる
(use-package bury-successful-compilation
  :ensure t
  :config
  (bury-successful-compilation 1))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ codic                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; プログラマ用命名辞書
(use-package codic
  :ensure t)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ emmet-mode                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; HTML/CSSの短縮入力
(use-package emmet-mode
  :ensure t
  :commands emmet-mode
  :init
  ;; HTML, CSSでemmet-mode
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)

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
;;; @ flycheck                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package flycheck
  :ensure t
  :init
  ;; 文法チェック
  (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; エラーをツールチップ表示
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
(flycheck-mode)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ haskell-mode                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(use-package haskell-mode
  :ensure t
  :init
  (require 'haskell-cabal)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

  :mode
  (("\\.hs$" . haskell-mode)
   ("\\.lhs$" . literate-haskell-mode)
   ("\\.cabal\\'" . haskell-cabal-mode)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ Markdown-mode                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; markdown用モード
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode
  (("\\.markdown\\'" . markdown-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.text\\'" . markdown-mode)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ mode-compile                                                  ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; M-x compileを賢く->M-x mode-compile
(use-package mode-compile
  ;:ensure t
  :commands (mode-compile mode-compile-kill)
  :config
  (setq mode-compile-always-save-buffer-p t)
  (setq mode-compile-expert-p t)
  (setq mode-compile-reading-time 0)
  (defun compile-autoclose (buffer string)
    (cond ((string-match "finished" string)
         (message "Build maybe successful: closing window.")
         (run-with-timer 0.3 nil
                         'delete-window
                         (get-buffer-window buffer t)))
        (t (message "Compilation exited abnormally: %s" string))))
(setq compilation-finish-functions 'compile-autoclose))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ multi-term                                                    ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; term/ansi-termの拡張
;; termの複数起動ができるほか、いろいろ強化
(use-package multi-term
  :ensure t
  :init
  (add-hook 'term-mode-hook
         '(lambda ()
            ;; C-h を term 内文字削除にする
            (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
            ;; C-y を term 内ペーストにする
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            ;; yasnippetを無効にする
            (yas-minor-mode -1)))
  :config
  (setq multi-term-program shell-file-name)
  (add-to-list 'term-unbind-key-list '"M-x"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ sdic                                                          ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 英和・和英辞書(外部パッケージ)
(use-package sdic
  :config
  (global-set-key (kbd "C-c w") 'sdic))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ smart-newline                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 改行を少し賢く
(use-package smart-newline
  :ensure t
  :config
  (global-set-key (kbd "C-j") 'smart-newline)
  (global-set-key (kbd "RET") 'smart-newline))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ smooth-scroll                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; C-v M-vをなめらかに
(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ stash                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 変数永続化
(use-package stash
  :ensure t)
(defstash kill-ring "kill-ring.el" nil (or stashed 'nil))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ twittering-mode                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; emacs用twitterクライアント
(use-package twittering-mode
  :ensure t
  :commands (twittering-mode twit)
  :init
  ;; urlを開くブラウザをewwに変更
  (add-hook 'twittering-mode-hook
            (lambda()
              (setq-local browse-url-browser-function 'eww-browse-url)))

  :config
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
// via %f%L %RT{retweeted by %S(%s)}
")

  ;; 割り込みReply時に全員に返信
  (setq twittering-edit-skeleton 'inherit-mentions))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ undohist                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; undoの永続化
(use-package undohist
  :ensure t
  :config
  (undohist-initialize)
  ;; 永続化を無視するファイル名の正規表現
  (setq undohist-ignored-files
        '("/tmp/")))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ undo-tree                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; undoの拡張
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ volatile-highlights                                           ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; undoやyankなどの操作をした部分を可視化
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode t))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ web-mode                                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; HTML等用の編集モード
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.xhtml\\'" . web-mode)
         ("\\.shtml\\'" . web-mode)))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ yasnippet                                                     ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; snippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  (define-key yas-minor-mode-map (kbd "C-c e") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ YaTeX                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; LaTeX用モード
(use-package yatex
  :ensure t
  :commands (yatex yatex-mode)
  :mode (("\\.tex\\'" . yatex-mode)
         ("\\.ltx\\'" . yatex-mode)
         ("\\.cls\\'" . yatex-mode)
         ("\\.sty\\'" . yatex-mode)
         ("\\.clo\\'" . yatex-mode)
         ("\\.bbl\\'" . yatex-mode))
  :init
  (add-hook 'yatex-mode-hook '(lambda () (auto-fill-mode -1))) ;自動で改行しない
  :config
  (setq YaTeX-kanji-code nil)
  (setq tex-command "platex -interaction=nonstopmode")
  (cond ((string= (system-name) "nasa-ubuntu")
         (setq dvi2-command "xdvi"))
        (t (setq dvi2-command "pxdvi")))
  (setq bibtex-command "pbibtex")
  (setq dviprint-command-format "dvipdfmx"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ theme                                                         ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; テーマ格納ディレクトリのパス追加
(add-to-list 'custom-theme-load-path
             (file-name-as-directory (concat user-emacs-directory "theme"))
             )

;; テーマ選択
(load-theme 'gnupack-dark t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; init.el ends here
