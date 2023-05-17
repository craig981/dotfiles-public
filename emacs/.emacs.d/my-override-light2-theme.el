(deftheme my-override-light2
  "Created 2023-03-18.")

(custom-theme-set-faces
 'my-override-light2
 '(default ((t (:background "#F0F0F6"))))
 )

(set-cursor-color "black")
(setq evil-normal-state-cursor '(box "black"))
(setq evil-insert-state-cursor '(box "orange"))

(provide-theme 'my-override-light2)
