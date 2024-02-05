;;; early-init.el --- .emacs.d/early-init.el early init file for emacs  -*- lexical-binding: t; -*-

;; Author: nasa9084

;;; Commentary:
;; early-init.el for configurations that should be configure
;; before GUI construction or loading package managers

;;; Code:

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ GUI configurations
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; Disable tool bar / menu bar
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

;; Enable menu-bar-mode not to send emacs background after mission control
;; menu-bar-lines are set to 0 in above line so menu-bar won't be shown
(menu-bar-mode)

;; workaround for the issue that the title bar becomes taller on macOS 14 (Sonoma)
;;
;; This commit will resolve the issue but has not yet released:
;; https://bitbucket.org/mituharu/emacs-mac/commits/5f6c306095c825eb01708e336f9d03c15271dfe9
(tool-bar-mode 1)
(tool-bar-mode 0)

;; Disable scroll bar
(push '(vertical-scroll-bars) default-frame-alist)

;; Configure default window size
(push '(width . 250) default-frame-alist)
(push '(height . 75) default-frame-alist)
(setq frame-inhibit-implied-resize t)

;; Not to show startup screen
(setq inhibit-startup-screen t)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; early-init.el ends here
