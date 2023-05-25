(deftheme my-override-light2
  "Created 2023-03-18.")

(custom-theme-set-faces
 'my-override-light2
 ;; '(default ((t (:background "#F0F0F6"))))

 ;; swap removed and added faces from anti-zenburn
 '(diff-refine-removed ((t (:foreground "#502750" :background "#c0a0c0"))))
 '(diff-refine-added ((t (:foreground "#134c4c" :background "#83bcbc"))))
 '(diff-removed ((t (:foreground "#502750" :background "#c0a0c0"))))
 '(diff-added ((t (:foreground "#134c4c" :background "#83bcbc"))))
 '(org-checkbox ((t (:foreground "#000010" :background nil :box nil))))
 )

(set-cursor-color "black")
(setq evil-normal-state-cursor '(box "black"))
(setq evil-insert-state-cursor '(box "white"))

(provide-theme 'my-override-light2)
