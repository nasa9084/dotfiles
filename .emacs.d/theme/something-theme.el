;;; theme.el --- something theme

;;; Code:
(deftheme something "something color theme")
(custom-theme-set-faces
 'something

 ;; display-line-numbers-mode
 `(line-number-current-line ((t (:foreground ,"#CCCCCC"))))

 ;; markdown-mode
 `(markdown-code-face ((t (:background ,"#333333" :extend t))))
 )
(provide-theme 'something)

;; ------------------------------------------------------------
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; ends here
