(deftheme my-override-dark3
  "Created 2022-09-14.")

(custom-theme-set-faces
 'my-override-dark3
 '(default ((t :foreground "#b8c6d5")))
 '(helm-match ((t (:foreground "#ffffff" :background "#005f5f"))))
)

(set-cursor-color "white")
(setq evil-normal-state-cursor '(box "white"))
(setq evil-insert-state-cursor '(box "orange"))

(provide-theme 'my-override-dark3)
