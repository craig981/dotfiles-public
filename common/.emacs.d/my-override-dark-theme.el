(deftheme my-override-dark
  "Created 2022-09-14.")

(custom-theme-set-faces
 'my-override-dark
 '(highlight ((t (:background "#37464a" :foreground "#959595"))))
 '(isearch ((t (:background "#ffffff" :foreground "#050505"))))
 '(lazy-highlight ((t (:background "#909090" :foreground "#050505"))))
 '(helm-match ((t (:inherit region))))
 '(font-lock-warning-face ((t (:background "#243539" :foreground "#e81050"))))
 '(show-paren-match ((t (:weight bold :background nil :foreground "#dddddd")))))

(if (display-graphic-p)
    (progn
      (custom-theme-set-faces
       'my-override-dark
       ;; '(default ((t ())))
       '(default ((t (:background "#0d1f24"))))
       '(fringe ((t (:background nil))))
       '(mode-line-inactive ((t (:box (:line-width 1 :color "#112328")))))
       '(mode-line ((t (:box (:line-width 1 :color "#243539")))))
       '(mode-line-inactive ((t (:foreground "#878787" :box (:line-width 1 :color "#112328")))))
       '(orderless-match-face-0 ((t (:foreground "#a3d4e8" :weight bold))))
       '(font-lock-comment-face ((t (:foreground "#708080"))))
       )
      (set-face-attribute 'region nil :foreground "#ffffff" :background "#005f5f"))
  (custom-theme-set-faces
   'my-override-dark
   '(default ((t (:background "#070707"))))
   '(font-lock-comment-face ((t (:foreground "#757575"))))
   '(magit-diff-context-highlight ((t (:background "color-236" :foreground "#959595"))))
   '(mode-line ((t (:foreground "#808080" :background "#222222"))))
   '(mode-line-buffer-id ((t (:foreground "#C7B299"))))
   '(mode-line-inactive ((t (:foreground "#656565" :background "#222222")))))
  (set-face-attribute 'region nil :foreground "#ffffff" :background "#243539")
  ;; (my-unspecified-background)
  )

(provide-theme 'my-override-dark)
