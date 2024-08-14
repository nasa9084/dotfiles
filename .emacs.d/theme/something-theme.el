;;; theme.el --- something theme

;;; Code:
(deftheme something "something color theme")
(custom-theme-set-faces
 'something

 ;; display-line-numbers-mode
 `(line-number-current-line ((t (:foreground ,"#CCCCCC"))))

 ;; flymake-popon-mode
 `(flymake-popon ((t (:background ,"#404040" :foreground ,"#CC9166" :distant-foreground ,"#CCCCCC" :weight bold))))

 ;; markdown-mode
 `(markdown-code-face ((t (:background ,"#333333" :extend t))))
 )
(provide-theme 'something)

;; ------------------------------------------------------------
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; something-theme.el ends here
