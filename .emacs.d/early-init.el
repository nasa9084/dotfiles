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

;; Disable scroll bar
(push '(vertical-scroll-bars) default-frame-alist)

;; Configure default window size
(push '(width . 180) default-frame-alist)
(push '(height . 75) default-frame-alist)

;; Not to show startup screen
(setq inhibit-startup-screen t)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; early-init.el ends here
