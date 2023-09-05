(deftheme my-override-light
  "Created 2022-09-15.")

(if (display-graphic-p)
    (custom-theme-set-faces
     'my-override-light
     '(region ((t (:background "#3399aa" :foreground "#ffffff")))))
  (custom-theme-set-faces
   'my-override-light
   '(region ((t (:background "dark turquoise" :foreground "#ffffff"))))))

(custom-theme-set-faces
 'my-override-light
 ;; '(default ((t ())))
 '(highlight ((t (:background "light gray" :foreground "black"))))
 '(isearch ((t (:background "deeppink" :foreground "black" :weight bold))))
 '(lazy-highlight ((t (:background "turquoise" :foreground "black" ))))
 '(helm-match ((t (:background "LightSkyBlue1" :foreground "black"))))
 ;; '(font-lock-comment-face ((t (:foreground  "dark green" :italic t))))
 '(font-lock-warning-face ((t (:background "#ffffff" :foreground "#ff6523" :inverse-video nil))))
 '(show-paren-match ((t (:weight bold :background nil :foreground "#000000"))))
 '(orderless-match-face-0 ((t (:foreground "#5555ff" :weight bold))))
 ;; '(magit-diff-context-highlight ((t ())))
 ;; '(mode-line ((t ())))
 ;; '(mode-line-buffer-id ((t ())))
 ;; '(mode-line-inactive ((t ())))
 )

(provide-theme 'my-override-light)
