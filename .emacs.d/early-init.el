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

;; Disable scroll bar
(push '(vertical-scroll-bars) default-frame-alist)

;; Configure default window size
(push '(width . 250) default-frame-alist)
(push '(height . 75) default-frame-alist)

;; Not to show startup screen
(setq inhibit-startup-screen t)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; early-init.el ends here
