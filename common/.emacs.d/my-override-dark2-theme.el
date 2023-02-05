(deftheme my-override-dark2
  "Created 2022-09-14.")

(custom-theme-set-faces
 'my-override-dark2
 ;; '(fringe ((t (:background nil))))
 ;; '(isearch ((t (:background "gold3" :foreground "black"))))
 ;; '(lazy-highlight ((t (:background "turquoise" :foreground "black" ))))
 '(helm-match ((t (:foreground "#ffffff" :background "#005f5f"))))
 ;; '(show-paren-match ((t (:weight bold :background nil :foreground "#ffffff"))))
)

(set-cursor-color "white")
(setq evil-normal-state-cursor '(box "white"))
(setq evil-insert-state-cursor '(box "orange"))

(provide-theme 'my-override-dark2)
