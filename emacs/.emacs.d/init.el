
;;; Reduce garbage collections during startup
(defvar my-gc-cons-default gc-cons-threshold)
(setq gc-cons-threshold (* 50 1000 1000))

(defun my-after-init ()
  (message "Emacs startup %.2f sec, %d garbage collections"
           (float-time (time-subtract after-init-time before-init-time))
           gcs-done)
  (setq gc-cons-threshold my-gc-cons-default))

(add-hook 'emacs-startup-hook #'my-after-init)

;; ----------------------------------------------------------------------------
;;| Package
;; ----------------------------------------------------------------------------

(setq tls-checktrust t)
(setq gnutls-verify-error t)
(setq load-prefer-newer t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; ----------------------------------------------------------------------------
;;| Customs
;; ----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-date-style 'european)
 '(calendar-week-start-day 1)
 '(comint-prompt-read-only t)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(compile-command "make ")
 '(consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000  --smart-case --no-heading --with-filename --line-number --no-search-zip --hidden -g !{.git,.svn,.hg}/ -g !TAGS -g !build/ --no-ignore")
 '(custom-safe-themes
   '("fb7595c9571f2bd41635745d12551f35322296b70330056ddd0020ab2374671c" "d0dc7861b33d68caa92287d39cf8e8d9bc3764ec9c76bdb8072e87d90546c8a3" "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79" "b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9" "601a9b9bf21f5c72ddfb28c7e95b842a5b0130f55ad5e0b97d2ba1e0b91b0a2c" "7776ba149258df15039b1f0aba4b180d95069b2589bc7d6570a833f05fdf7b6d" "e17d91a99e14fc72f71f531f07d3dff44238c69f599998b50e95e67b589d8fa1" "a6e8bcffe4d8cac7463c5a7c67c0908316cc616da3816d3ce35c325d5e02fd97" "adfe1d522a4a100edade12797079ebbabf742a48cf098e7d10ea14012e156ee8" "7342266ffff707cc104313c9153342e44a47a9f22ed7157e4893aac74091ad27" "aa688776604bbddbaba9e0c0d77e8eb5f88d94308f223d1962b6e6b902add6a0" default))
 '(dabbrev-backward-only t)
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-fold-search t)
 '(default-input-method "swedish-postfix")
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-idle-delay 0.25)
 '(electric-indent-mode t)
 '(electric-pair-mode nil)
 '(evil-flash-delay 60)
 '(evil-motion-state-modes
   '(apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode dictionary-mode ert-results-mode help-mode Info-mode Man-mode speedbar-mode undo-tree-visualizer-mode view-mode woman-mode))
 '(find-name-arg "-iname")
 '(grep-find-ignored-directories '(".svn" ".git" ".hg"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.so" "*.a" "*.elc" "*.lib" "*.lo" "*.la" "*.pyc" "*.pyo" "TAGS"))
 '(helm-candidate-number-limit 10000)
 '(helm-follow-mode-persistent t)
 '(helm-move-to-line-cycle-in-source nil)
 '(helm-source-names-using-follow '("Imenu" "Helm occur"))
 '(hide-ifdef-initially t)
 '(hide-ifdef-shadow t)
 '(ibuffer-project-root-functions '((ibuffer-project-project-root . "Project")))
 '(image-dired-dir "/tmp/image-dired")
 '(image-dired-thumb-height 100)
 '(image-dired-thumb-width 100)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ispell-program-name "aspell")
 '(kill-ring-max 1000)
 '(ledger-report-native-highlighting-arguments '("--color"))
 '(magit-delete-by-moving-to-trash nil)
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(magit-log-auto-more t)
 '(magit-merge-arguments '("--no-ff"))
 '(magit-section-initial-visibility-alist '((stashes . show) (upstream . show)))
 '(magit-section-visibility-indicator '("" . t))
 '(magit-status-goto-file-position t)
 '(magit-status-headers-hook
   '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-repo-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header magit-insert-tags-header))
 '(magit-status-show-hashes-in-headers t)
 '(next-error-recenter '(4))
 '(olivetti-body-width 120)
 '(org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'")
 '(org-agenda-show-future-repeats nil)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span 3)
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item)))
 '(org-imenu-depth 3)
 '(org-modules '(ol-docview org-habit ol-info))
 '(org-refile-targets '((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3)))
 '(org-startup-indented t)
 '(org-use-fast-todo-selection 'expert)
 '(package-selected-packages
   '(cape cmake-mode consult ef-themes elfeed embark embark-consult emms evil evil-leader evil-collection evil-numbers fancy-dabbrev gnuplot helm hydra ibuffer-project ledger-mode magit marginalia markdown-mode nordic-night-theme olivetti orderless ox-pandoc paredit reykjavik-theme soft-morning-theme undo-tree vertico wgrep which-key yaml-mode))
 '(package-vc-selected-packages
   '((sandcastle-theme :vc-backend Git :url "https://github.com/habamax/sandcastle-theme")))
 '(project-vc-ignores '("./build/" "build/" ".#*" "*~" "*.elc" "*.pyc" "*.pyo"))
 '(read-quoted-char-radix 16)
 '(recentf-max-saved-items 1000)
 '(remote-file-name-inhibit-locks t)
 '(safe-local-variable-values
   '((my-input-method . swedish-postfix)
     (my-input-method . german-postfix)
     (buffer-auto-save-file-name)
     (tab-always-indent)
     (indent-tabs-mode nil)
     (evil-shift-width . 2)
     (evil-shift-width . 4)))
 '(shift-select-mode nil)
 '(tags-case-fold-search nil)
 '(tramp-histfile-override "/tmp/.tramp_history")
 '(tramp-ssh-controlmaster-options
   "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=60m" t)
 '(undo-tree-auto-save-history nil)
 '(use-short-answers t)
 '(vertico-count 15)
 '(vertico-group-format nil)
 '(warning-suppress-types '((comp)))
 '(world-clock-list
   '(("America/Los_Angeles" "Los Angeles")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Stockholm" "Stockholm")))
 '(world-clock-time-format "%R %Z"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-duplicate ((t (:background "Magenta" :foreground "white"))))
 '(flyspell-incorrect ((t (:background "red" :foreground "white"))))
 '(makefile-space ((t nil)))
 '(message-cited-text-1 ((t (:foreground "#878787"))))
 '(success ((t (:foreground "#00DD00" :weight bold)))))

;; ----------------------------------------------------------------------------
;;| Paths
;; ----------------------------------------------------------------------------

(when (and (eq system-type 'darwin)
	   (native-comp-available-p))
  (setenv "LIBRARY_PATH" (concat
			  (getenv "LIBRARY_PATH") ":"
			  (string-trim (shell-command-to-string "xcrun --sdk macosx --show-sdk-path"))
			  "/usr/lib")))

(if (eq system-type 'windows-nt)
    (let ((path (concat (getenv "HOME") "\\tools\\bin;"
			"C:\\Program Files\\CMake\\bin;"
			"C:\\cygwin64\\bin;"
			"C:\\Program Files\\Perforce;"
			"C:\\Program Files\\DJV2\\bin;"
			"C:\\windows\\system32")))
      (setenv "PATH" path)
      (setq exec-path (split-string path path-separator)))
  (when (display-graphic-p)
    (let ((path-from-shell (replace-regexp-in-string
			    "[[:space:]\n]*$" ""
			    (shell-command-to-string
			     (format "$SHELL %s -c 'printenv PATH'"
				     (if (string= "/bin/tcsh" (getenv "SHELL")) "" "-l"))))))
      (setenv "PATH" path-from-shell)
      (setq exec-path (split-string path-from-shell path-separator)))))

;; ----------------------------------------------------------------------------
;;| Evil mode
;; ----------------------------------------------------------------------------

(setq evil-want-integration t
      evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(require 'evil-leader)
(require 'evil-numbers)

(global-evil-leader-mode)

(evil-esc-mode 1)			; make C-[ escape
(evil-global-set-key 'insert   (kbd "C-c") 'evil-normal-state)
(evil-global-set-key 'replace  (kbd "C-c") 'evil-normal-state)
(evil-global-set-key 'operator (kbd "C-c") 'keyboard-quit)


(global-set-key (kbd "<f7>") 'evil-local-mode)
(add-hook 'evil-command-window-mode-hook 'evil-local-mode)

(setq-default evil-ex-search-case 'sensitive)
(setq-default evil-search-module 'evil-search)

(setq evil-emacs-state-tag  (propertize "<E>" 'face '((:foreground "#000000" :background "goldenrod"))))

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "w" 'evil-write)

(evil-declare-ignore-repeat 'evil-scroll-line-to-center)
(evil-declare-ignore-repeat 'hscroll-cursor-left)
(evil-declare-ignore-repeat 'hscroll-cursor-right)
(evil-declare-ignore-repeat 'recenter-top-bottom)
(evil-declare-ignore-repeat 'other-window)

;; (defun my-wrap-eol (func &rest args)
;;   "Temporarily disable evil-move-beyond-eol for evil commands,
;; leave it at 't' for Emacs commands"
;;   (if (and real-this-command
;; 	   (symbolp real-this-command)
;; 	   (string-match-p "^evil-" (symbol-name real-this-command)))
;;       (let ((evil-move-beyond-eol nil))
;; 	(apply func args))
;;     (apply func args)))

;; (dolist (func '(evil-normal-post-command evil-eolp))
;;   (advice-add func :around #'my-wrap-eol))

(defun my-forward-before-insert (&rest args)
  "Move the cursor forward before closing a tag or inserting a time stamp"
  (when (and (eq evil-state 'normal)
	     (not (eolp)))
    (forward-char)))

(defun my-delete-or-indent-left ()
  (interactive)
  (if (eolp)
      (evil-shift-left-line 1)
    (delete-char 1)))
(evil-global-set-key 'insert (kbd "C-d") 'my-delete-or-indent-left)

;; fall through to emacs keys, C/M-f/b and M-d M-m already works in insert mode
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)
(define-key evil-visual-state-map (kbd "C-w") 'kill-region)
(evil-global-set-key 'normal (kbd "C-t") nil)
(evil-global-set-key 'motion (kbd "C-a") nil)
(evil-global-set-key 'insert (kbd "C-a") nil)
(evil-global-set-key 'insert (kbd "C-e") nil)
(evil-global-set-key 'insert (kbd "C-k") nil)
(evil-global-set-key 'insert (kbd "C-y") nil)
(evil-global-set-key 'insert (kbd "C-n") nil)
(evil-global-set-key 'insert (kbd "C-p") nil)
(evil-global-set-key 'insert (kbd "C-o") nil)

;; (evil-global-set-key 'normal (kbd "C-r") nil)
;; (evil-global-set-key 'normal (kbd "R") 'evil-redo)

(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "C-k") 'kill-line)

(evil-global-set-key 'normal (kbd "C-a") 'evil-numbers/inc-at-pt)
(evil-global-set-key 'normal (kbd "C-p") 'evil-numbers/dec-at-pt)
(evil-global-set-key 'normal (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
(evil-global-set-key 'normal (kbd "g C-p") 'evil-numbers/dec-at-pt-incremental)

;;; don't want this in evil-motion-state-map because that affects operators, and
;;; we want 'dj' to delete two lines
(define-key evil-normal-state-map (kbd "j") #'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") #'evil-previous-visual-line)
(define-key evil-visual-state-map (kbd "j") #'evil-next-visual-line)
(define-key evil-visual-state-map (kbd "k") #'evil-previous-visual-line)

;; ----------------------------------------------------------------------------
;;| Undo
;; ----------------------------------------------------------------------------

(require 'undo-tree)
(global-undo-tree-mode)

(setq amalgamating-undo-limit 2)

(when (fboundp 'evil-set-undo-system)
  (evil-set-undo-system 'undo-tree))

(evil-declare-ignore-repeat 'evil-undo)

;; ----------------------------------------------------------------------------
;;| Syntax and indent
;; ----------------------------------------------------------------------------

(defun my-syntax-entry ()
  ;; - + are punctuation
  (modify-syntax-entry ?- ".")
  (modify-syntax-entry ?+ ".")
  ;; / is punctuation, so evil * works on path components
  (modify-syntax-entry ?/ ".")
  (modify-syntax-entry ?| ".")
  ;; _ is word constituent
  (modify-syntax-entry ?_ "w"))

(setq-default sentence-end-double-space nil)
(setq-default fill-column 78)      ; set tw=78
(setq-default truncate-lines t)    ; set nowrap
(setq-default tab-width 8)         ; set ts=8
(setq-default evil-shift-width 8)
(setq-default indent-tabs-mode t)  ; set noexpandtab
(setq-default align-to-tab-stop nil) ; tabs+spaces instead of all tabs
(setq-default tab-always-indent nil)

(electric-indent-mode 1)

;; ----------------------------------------------------------------------------
;;| Find file
;; ----------------------------------------------------------------------------

(setq my-input-method nil)

(defun my-find-file-hook ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil))

  (when (not (eq major-mode 'image-mode))
    (evil-local-mode 1)) ;; default to evil mode
  (when (and git-commit-mode (evil-normal-state-p) (looking-at "^$"))
    (evil-insert-state))

  (if my-input-method
      (set-input-method my-input-method)))

(add-hook 'find-file-hook 'my-find-file-hook)

;; ----------------------------------------------------------------------------
;;| Find file at point
;; ----------------------------------------------------------------------------

(defun my-find-file-at-point ()
  "If the file at the point exists, open it instead of prompting"
  (interactive)
  (let ((name (catch 'my-ffap-catch
		(ffap-file-at-point))))
    (if (ffap-file-exists-string name)
	(find-file name)
      (call-interactively 'find-file-at-point))
    (let ((file (buffer-file-name)))
      (when file
	(princ file)))))

(defun my-ffap-match-non-existent (name)
  "Added at the end of the ffap-alist to throw a non-existent file"
  (throw 'my-ffap-catch name))

(with-eval-after-load "ffap"
  (add-to-list 'ffap-alist '("." . my-ffap-match-non-existent) t))

(evil-global-set-key 'normal (kbd "gf") 'my-find-file-at-point)
(evil-global-set-key 'visual (kbd "gf") 'my-find-file-at-point)
(global-set-key (kbd "C-c f") #'my-find-file-at-point)

;; ----------------------------------------------------------------------------
;;| Convenience
;; ----------------------------------------------------------------------------

(when (version< emacs-version "29.1")
  (defalias 'yes-or-no-p 'y-or-n-p))

;; (column-number-mode t)

(show-paren-mode)
(setq-default
 show-paren-when-point-inside-paren t
 show-paren-when-point-in-periphery t)

(setq-default show-trailing-whitespace nil)

(defun my-insert-enter-hook ()
  ;; temporarily disable update of syntax highlighting while in insert mode,
  ;; as a workaround for typing becoming slow in some C++ buffers
  (when (derived-mode-p 'prog-mode)
    (jit-lock-mode nil))
  (show-paren-mode -1))
(defun my-insert-exit-hook ()
  (show-paren-mode 1)
  (jit-lock-mode t))
(add-hook 'evil-insert-state-entry-hook 'my-insert-enter-hook)
(add-hook 'evil-insert-state-exit-hook 'my-insert-exit-hook)

(setq next-error-highlight-no-select t) ;; leave highlight for occur
(setq ring-bell-function 'ignore) ;; stop binging noise on C-g
(setq register-preview-delay 0.5)

(setq-default vc-follow-symlinks t)
(setq-default backup-inhibited t)    ;; disable backup
(setq-default auto-save-default nil) ;; disable auto save

(recentf-mode 1)
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)
(setq history-length 1000)
(setq history-delete-duplicates t)
(savehist-mode 1)
(winner-mode 1)

(when (not (version< emacs-version "28.1"))
  (setq-default bookmark-set-fringe-mark nil))

(put 'narrow-to-region 'disabled nil)

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))

(require 'thingatpt)

(defun my-substitute (&optional range)
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
	 ;; want @ by default so dabbrev-expand works
	 (delim (if (string-match-p "@" sym) "#" "@")))
    (evil-ex (format "%ss%s\\_<%s\\_>%s" (or range "%") delim sym delim))))

(defun my-delete-whitespace (&optional prefix)
  (interactive "P")
  (cond
   ((region-active-p)
    (call-interactively 'delete-trailing-whitespace)
    (deactivate-mark)
    (message "Deleted trailing whitespace in region"))
   ((or (looking-at "[[:space:]\n]")
	(looking-back "[[:space:]\n]" (pos-bol)))
    (delete-horizontal-space prefix))
   (t
    (save-excursion
      (move-end-of-line 1)
      (delete-horizontal-space))
    (message "Deleted trailing whitespace on current line"))))

(defun my-copy-filename (&optional prefix)
  "Copy the filename of the current buffer. With the prefix arg,
copy the basename."
  (interactive "P")
  (let ((y (if-let ((x (buffer-file-name)))
	       (if prefix (file-name-nondirectory x) x)
	     (if prefix
		 (file-name-nondirectory (directory-file-name default-directory))
	       default-directory))))
    (kill-new y)
    (message "Yanked %s" y)))

(defun my-mirror-buffer ()
  "Mirror current buffer to other window"
  (interactive)
  (when (= 1 (count-windows))
    (split-window-right))
  (let ((buf (current-buffer))
	(line (line-number-at-pos)))
    (save-selected-window
      (other-window 1)
      (switch-to-buffer buf)
      (goto-line line)
      (recenter-top-bottom))))

(defun my-close-other-window ()
  (interactive)
  (when (> (count-windows) 1)
    (quit-window nil (next-window))))

(defun my-jump-buffer (name &optional other)
  (interactive)
  (let ((w (get-buffer-window name)))
    (cond
     (w
      (select-window w))
     (other
      (switch-to-buffer-other-window name))
     (t
      (switch-to-buffer name)))))

(defun my-join-line ()
  (interactive)
  (when (or (not evil-local-mode)
	    (evil-emacs-state-p))
    (join-line 1)))

(defun my-mark-until-whitespace ()
  "Select until the next whitespace char"
  (interactive)
  (when (looking-at "[^[:space:]\n]+")
    (push-mark (match-end 0) nil t)))

(defun my-open-line-above ()
  (interactive)
  (beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun my-open-line-below (&optional arg)
  (interactive "*p")
  (move-end-of-line nil)
  (newline-and-indent arg))

(defun my-delete-to-indent ()
  (interactive)
  (let ((p (point)))
    (back-to-indentation)
    (delete-region (point) p)))

(defun my-duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")
  (duplicate-line arg)
  (next-line arg))

(defun my-isearch-symbol-backward ()
  (interactive)
  (isearch-forward-symbol-at-point -1))

(defun my-init-file ()
  (if (eq system-type 'windows-nt)
      (concat (getenv "HOME") "\\dotfiles-public\\emacs\\.emacs.d\\init.el")
    user-init-file))

(defun my-find-init-file ()
  (interactive)
  (find-file (my-init-file)))

(require 'ispell)

(defun my-complete-word-ispell ()
  "Completes the symbol at point based on entries in the dictionary"
  (interactive)
  (let* ((word (thing-at-point 'symbol t))
         (bounds (bounds-of-thing-at-point 'symbol))
         (words (ispell-lookup-words (concat word "*"))))
    (when-let ((selection (completing-read "Words: " words)))
      (delete-region (car bounds) (cdr bounds))
      (insert selection))))

(evil-leader/set-key "s" #'my-substitute) ; substitute whole buffer
(evil-leader/set-key "S" ; substitute from current line to end of buffer
  (lambda ()
    (interactive)
    (my-substitute ".,$")))

(evil-leader/set-key "=" #'align-regexp)
(evil-leader/set-key "d" #'pwd)
(evil-leader/set-key "SPC" (kbd "=i{"))

(when (eq system-type 'gnu/linux)
  (evil-global-set-key 'motion (kbd "K") 'man))

(when (eq system-type 'darwin)
  ;; skip slow vertico minibuffer prompt for man pages
  (evil-global-set-key 'motion (kbd "K") (lambda ()
					   (interactive)
					   (man (thing-at-point 'word t))))
  ;; completion list for man pages is slow
  (defun my-advise-man-completion (&rest args) '())
  (advice-add #'Man-completion-table :override #'my-advise-man-completion))

(global-set-key (kbd "C-=") #'my-close-other-window)
(global-set-key (kbd "C-c d") #'pwd)
(global-set-key (kbd "C-c c") #'my-copy-filename)
(global-set-key (kbd "C-c n") #'toggle-truncate-lines)
(global-set-key (kbd "C-c w") 'evil-window-map)
(global-set-key (kbd "C-c w SPC") #'world-clock)
(global-set-key (kbd "C-c m") #'my-mirror-buffer)
(global-set-key (kbd "C-c z") #'my-find-init-file)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-=") 'winner-undo)
(global-set-key (kbd "M-+") 'winner-redo)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-'") #'delete-blank-lines)
(global-set-key (kbd "M-\\") #'my-delete-whitespace)
(global-set-key (kbd "M-#") #'my-mark-until-whitespace)
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-s ,") 'my-isearch-symbol-backward)
(global-set-key (kbd "M-s M-,") 'my-isearch-symbol-backward)

(global-set-key (kbd "M-p") #'evil-scroll-up)
(global-set-key (kbd "M-n") #'evil-scroll-down)
(global-set-key (kbd "M-i") #'evil-scroll-line-up)
(global-set-key (kbd "M-j") #'evil-scroll-line-down)
(global-set-key (kbd "M-]") #'evil-numbers/inc-at-pt)
(global-set-key (kbd "M-[") #'evil-numbers/dec-at-pt)
(global-set-key (kbd "M-SPC") evil-leader--default-map)
(evil-leader/set-key "t"   #'tab-to-tab-stop)
(evil-leader/set-key "M-t" #'tab-to-tab-stop)

(global-set-key (kbd "C-M-y") #'my-duplicate-line)
(global-set-key (kbd "C-M-o") #'my-open-line-above)
(global-set-key (kbd "C-o") #'my-open-line-below)
(global-set-key (kbd "C-;") #'goto-last-change)
(when (display-graphic-p)
 (global-set-key (kbd "C-<backspace>") #'my-delete-to-indent))

(push 'try-expand-line hippie-expand-try-functions-list)
(global-set-key (kbd "C-x C-l") 'hippie-expand) ;; line completion like vim

(global-set-key (kbd "C-h h") nil)
(global-set-key (kbd "C-h C-c") nil)
(global-set-key (kbd "C-h RET") 'man)
(global-set-key (kbd "C-x !") 'delete-other-windows-vertically)
(global-set-key (kbd "C-x g") 'subword-mode)
(global-set-key (kbd "C-x l") 'count-words-region)
(global-set-key (kbd "C-c M-f") #'flyspell-buffer)
(global-set-key (kbd "C-c M-s") #'ispell)
(global-set-key (kbd "C-c j") 'my-join-line)

(evil-global-set-key 'normal (kbd "]s") 'flyspell-goto-next-error)
(evil-global-set-key 'normal (kbd "[s")
		     (lambda ()
		       (interactive)
		       (flyspell-goto-next-error t)))

(let ((words "~/.cache/macDict/words"))
  (when (and (eq system-type 'gnu/linux)
	     (file-exists-p words))
    (setq-default ispell-alternate-dictionary (expand-file-name words))))

(defun my-advise-comment (&rest args)
  (when (evil-normal-state-p)
    (call-interactively 'evil-append)))

(advice-add 'comment-dwim :after 'my-advise-comment)

(when (not (display-graphic-p))
  (global-set-key (kbd "C-x ;") (kbd "C-x C-;"))
  (global-set-key (kbd "C-x C-'") (kbd "C-x '")))

(defun my-toggle-word-boundary (word-regex
				word-begin word-end
				non-word-regex non-word-begin)
  "Insert/remove word boundary around search term in the minibuffer"
  (let* (
	 (line (or (thing-at-point 'line)
		   (progn
		     (next-history-element 1)
		     (end-of-line)
		     (thing-at-point 'line)))))

    (if (string-match word-regex line)
	(let ((keep (match-string 1 line))
	      (remove (- (match-end 0)
			 (match-beginning 0))))
	  (beginning-of-line)
	  (delete-char remove)
	  (insert non-word-begin keep)
	  (end-of-line))

      (if (string-empty-p non-word-regex)
	  (progn
	    (beginning-of-line)
	    (insert word-begin)
	    (end-of-line)
	    (insert word-end))

	(if (string-match non-word-regex line)
	    (let ((remove (- (match-end 0)
			     (match-beginning 0))))
	      (beginning-of-line)
	      (delete-char remove)
	      (insert word-begin)
	      (end-of-line)
	      (insert word-end)))))))

(defun my-advise-paste (&rest args)
  "Set the mark so we can indent the pasted text with indent-region"
  (when evil-last-paste
    (let* ((beg (nth 3 evil-last-paste))
	   (end (nth 4 evil-last-paste)))
      (push-mark (if (>= (+ 1 (point)) end) beg end)))))

(advice-add 'evil-paste-before :after 'my-advise-paste)
(advice-add 'evil-paste-after :after 'my-advise-paste)
(advice-add 'evil-paste-after-cursor-after :after 'my-advise-paste)

(define-key indent-rigidly-map (kbd "<") 'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd ">") 'indent-rigidly-right-to-tab-stop)

(define-key minibuffer-local-map (kbd "<escape>") 'abort-minibuffers)

;; ----------------------------------------------------------------------------
;;| Olivetti
;; ----------------------------------------------------------------------------

(defvar my-olivetti-state t)

(defun my-olivetti-mode-hook ()
  (setq my-olivetti-state olivetti-mode))

(defun my-olivetti-mode-on-hook ()
  (toggle-truncate-lines -1)
  (visual-line-mode -1))

(add-hook 'olivetti-mode-hook 'my-olivetti-mode-hook)
(add-hook 'olivetti-mode-on-hook 'my-olivetti-mode-on-hook)

(global-set-key (kbd "C-c o") #'olivetti-mode)

;; ----------------------------------------------------------------------------
;;| Pulse
;; ----------------------------------------------------------------------------

(require 'pulse)

(defvar my-pulse-face)

(setq pulse-delay 0.06
      pulse-iterations 8)

(defun my-pulse-line (&rest r)
  (pulse-momentary-highlight-one-line (point) my-pulse-face))

(dolist (cmd '(bookmark-jump tab-new tab-close tab-next
	       other-window delete-window my-kill-buffer
	       quit-window org-agenda-quit magit-mode-bury-buffer
	       winner-undo winner-redo))
  (advice-add cmd :after 'my-pulse-line))

;; ----------------------------------------------------------------------------
;;| Help
;; ----------------------------------------------------------------------------

(require 'which-key)
(which-key-mode)

(with-eval-after-load "help"
  (define-key help-mode-map (kbd "C-w") 'evil-window-map)
  (define-key help-mode-map (kbd "SPC") evil-leader--default-map))

(with-eval-after-load "info"
  (define-key Info-mode-map (kbd "C-w") 'evil-window-map)
  (define-key Info-mode-map (kbd "SPC") evil-leader--default-map)
  (define-key Info-mode-map (kbd "M-n") nil))

(with-eval-after-load "man"
  (define-key Man-mode-map (kbd "C-w") 'evil-window-map)
  (define-key Man-mode-map (kbd "SPC") evil-leader--default-map)
  (define-key Man-mode-map (kbd "k") nil)
  (define-key Man-mode-map (kbd "M-p") nil)
  (define-key Man-mode-map (kbd "M-n") nil))

(push '("\\(\\*[Hh]elp\\*\\)\\|\\(\\*Man\\)"
	(display-buffer-reuse-mode-window
	 display-buffer-use-some-window
	 display-buffer-in-direction)
	(direction . right)
	(inhibit-same-window . t))
      display-buffer-alist)

;; ----------------------------------------------------------------------------
;;| Abbreviations
;; ----------------------------------------------------------------------------

(setq save-abbrevs nil)

(defun my-abbrev-expand ()
  "Don't expand in strings or comments"
  (if (not (nth 8 (syntax-ppss)))
      (abbrev--default-expand)))

(when (display-graphic-p)
  (global-set-key (kbd "C-x C-'") #'expand-abbrev))

;; ----------------------------------------------------------------------------
;;| Prog
;; ----------------------------------------------------------------------------

(defun my-prog-mode-hook ()
  (abbrev-mode -1)
  (evil-local-mode 1)
  (when my-olivetti-state
    (olivetti-mode 1))
  (auto-fill-mode 1)
  (setq-local comment-auto-fill-only-comments t)
  (setq-local show-trailing-whitespace t)
  (setq-local abbrev-expand-function #'my-abbrev-expand))

(add-hook 'prog-mode-hook #'my-prog-mode-hook)

;; ----------------------------------------------------------------------------
;;| Fancy dabbrev
;; ----------------------------------------------------------------------------

(require 'fancy-dabbrev)

(global-set-key (kbd "M-/") 'fancy-dabbrev-expand)
(global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
(setq-default fancy-dabbrev-menu-height 15)
(setq-default fancy-dabbrev-preview-context 'everywhere)
(setq-default fancy-dabbrev-preview-delay 0.15)
;; (push 'evil-input-method fancy-dabbrev-no-preview-for)

(define-key minibuffer-local-map (kbd "M-/") 'dabbrev-expand)

;; ----------------------------------------------------------------------------
;;| Lookup
;; ----------------------------------------------------------------------------

(require 'hydra)

(defun my-cpp-identifier-around-point ()
  (let* ((regex "\\(\\s-\\|[^[:alnum:]_:]\\)")
	 (beg (save-excursion
		(if (re-search-backward regex (pos-bol) t)
		    (when (looking-at regex)
		      (forward-char))
		  (move-beginning-of-line 1))
		(point)))
	 (end (save-excursion
		(if (re-search-forward regex (pos-eol) t)
		    (backward-char)
		  (move-end-of-line 1))
		(point)))
	 (sym (buffer-substring-no-properties beg end)))
    (if (string-empty-p sym) nil sym)))

(defun my-cpp-identifier-without-namespace (sym)
  (when sym
    (when-let ((beg (string-match-p "[^:]*$" sym)))
      (substring sym beg))))

(defun my-lookup-sym ()
  (cond
   (mark-active
    (format "\"%s\"" (buffer-substring-no-properties (region-beginning) (region-end))))
   ((or (eq major-mode 'c++-mode)
	(eq major-mode 'c-mode))
    (my-cpp-identifier-without-namespace (my-cpp-identifier-around-point)))
   (t (thing-at-point 'symbol t))))

(defhydra my-lookup-hydra (:exit t)
  "Lookup"
  ("g" (lambda () (interactive)
	 (browse-url (concat "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
			     (url-hexify-string (read-string "Google: " (my-lookup-sym))))))
   "Google")
  ("c" (lambda () (interactive)
	 (browse-url (concat "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
			     (url-hexify-string
			      (format "%s site:cppreference.com"
				      (read-string "cppreference: " (my-lookup-sym)))))))
   "C++ ref")
  ("q" (lambda () (interactive)
	 (browse-url (format "https://doc.qt.io/qt-5/%s.html"
			     (downcase (read-string "Qt: " (my-lookup-sym))))))
   "Qt")
  ("o" (lambda () (interactive)
	 (browse-url (concat "https://docs.gl/"
			     (read-string "OpenGL: " (concat "gl4/" (my-lookup-sym))))))
   "OpenGL")
  ("m" (lambda () (interactive)
	 (browse-url (format "https://help.autodesk.com/view/MAYAUL/2022/ENU/?query=%s&cg=Developer%%27s%%20Documentation"
			     (read-string "Maya API: " (my-lookup-sym)))))
   "Maya")
  ("h" (lambda () (interactive)
	 (browse-url (concat "https://www.google.com/search?ie=utf-8&oe=utf-8&q="
			     (url-hexify-string
			      (format "%s site:https://www.sidefx.com/docs/hdk/"
				      (read-string "Houdini HDK: " (my-lookup-sym)))))))
   "Houdini")
  ("t" (lambda () (interactive)
	 (let ((translate (let ((input-method (if (or (not (bound-and-true-p evil-local-mode))
						      (evil-emacs-state-p))
						  current-input-method
						evil-input-method)))
			    (cond
			     ((string= input-method "german-postfix") "https://translate.google.com/?sl=de&tl=en&op=translate&text=")
			     (t "https://translate.google.com/?sl=sv&tl=en&op=translate&text=")))))
	   (browse-url (concat translate (url-hexify-string (read-string "Translate: " (my-lookup-sym)))))))
   "Translate"))

(global-set-key (kbd "M-s M-w") #'my-lookup-hydra/body)

(defun my-dictionary-lookup ()
  (interactive)
  (start-process "macDict" nil "~/dev/macDict/macDict.sh" (or (thing-at-point 'word t) "")))

(global-set-key (kbd "M-s =") 'my-dictionary-lookup)

;; ----------------------------------------------------------------------------
;;| Keyboard
;; ----------------------------------------------------------------------------

(when (string= "goose" (system-name))
  ;; tilde in the same place as in US keyboard
  (keyboard-translate ?\§ ?\`)
  (keyboard-translate ?\± ?\~))

(when (eq system-type 'darwin)
  (setq-default mac-command-modifier 'meta)
  (setq-default mac-right-command-modifier 'control)
  (setq-default mac-option-modifier 'alt))

;; ----------------------------------------------------------------------------
;;| Clipboard
;; ----------------------------------------------------------------------------

(defun my-copy-to-xclipboard ()
  (interactive)
  (call-process-region (region-beginning) (region-end) "xsel" nil nil nil "-ib")
  (message "Yanked region"))

(when (eq system-type 'gnu/linux)
  (global-set-key (kbd "C-c y") 'my-copy-to-xclipboard)
  (evil-global-set-key 'visual (kbd "Y") 'my-copy-to-xclipboard))

;; ----------------------------------------------------------------------------
;;| Scrolling
;; ----------------------------------------------------------------------------

;; stop scrolling to centre when cursor is on first/last line and
;; moves up/down
(setq-default scroll-up-aggressively 0.0)
(setq-default scroll-down-aggressively 0.0)

(setq-default auto-hscroll-mode 't
	      hscroll-margin 5
	      hscroll-step 5)

(when (display-graphic-p)
  (pixel-scroll-precision-mode))

;; ----------------------------------------------------------------------------
;;| Text
;; ----------------------------------------------------------------------------

(defun my-after-evil-buffer-new (&rest args)
  (let ((buffer (window-buffer)))
    (when buffer
      (with-current-buffer buffer
	(text-mode)
	(my-syntax-entry)))))

;;; after :enew
(advice-add 'evil-buffer-new :after #'my-after-evil-buffer-new)

(defun my-text-mode-hook ()
  (setq-local show-trailing-whitespace t)
  (setq-local completion-at-point-functions '(my-complete-word-ispell))
  (define-key text-mode-map (kbd "C-M-i") #'complete-symbol)
  (turn-on-auto-fill)
  (my-syntax-entry)
  (evil-local-mode 1)
  (when my-olivetti-state
    (olivetti-mode 1))
  ;; (evil-emacs-state)
  (setq-local evil-move-beyond-eol t))

(add-hook 'text-mode-hook 'my-text-mode-hook)

(add-to-list 'auto-mode-alist '("/README\\'" . text-mode))

;; ----------------------------------------------------------------------------
;;| Message
;; ----------------------------------------------------------------------------

(defun my-scratch-message-buffer ()
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (setq buffer-offer-save t)
    (message-mode)))

(defun my-message-mode-hook ()
  (my-syntax-entry)
  (evil-local-mode 1)
  (when my-olivetti-state
    (olivetti-mode 1))
  ;; = is punctuation, so evil * works on key and val separately for key=val
  (modify-syntax-entry ?= ".")
  (setq-local completion-at-point-functions '(my-complete-word-ispell))
  (setq-local show-trailing-whitespace t)
  (setq-local evil-move-beyond-eol t)
  ;; stop paragraph lines after the first being extra indented by M-q
  (setq-local fill-paragraph-function nil))

(add-hook 'message-mode-hook 'my-message-mode-hook)

;;; make auto fill work in message mode, given that we don't have headers and
;;; are just using the message buffer as scratch space
(defun my-advise-message-point-in-header-p () nil)
(advice-add 'message-point-in-header-p :override 'my-advise-message-point-in-header-p)

(with-eval-after-load "message"
  (define-key message-mode-map (kbd "C-M-i") #'complete-symbol)
  (define-key message-mode-map (kbd "C-c C-j") nil)
  (define-key message-mode-map (kbd "C-c C-c") nil)
  (define-key message-mode-map (kbd "C-c C-s") nil))

(setq-default message-auto-save-directory nil)

(setq compose-mail-user-agent-warnings nil)

(global-set-key (kbd "C-x m") #'my-scratch-message-buffer)

(push 'message-mode evil-emacs-state-modes)

;; ----------------------------------------------------------------------------
;;| Org
;; ----------------------------------------------------------------------------

;;; override org version. 9.6 randomly fails to display tasks scheduled for the
;;; current day in the agenda.
;; (add-to-list 'load-path "~/dev/org-mode/lisp")

;;; move slowdown to startup instead of when opening an org file
(require 'org)

;;; attempt to workaround org 9.6 flakey agenda display
(setq org-element-use-cache nil)

;; (push 'org-mode evil-emacs-state-modes)

(defun my-optional-file (fn)
  (if (file-exists-p fn) fn nil))

(setq org-directory "~/org")
(setq org-agenda-files (list "~/" "~/org"))
(setq org-default-notes-file
      (or (my-optional-file "~/notes.org.gpg")
	  (my-optional-file "~/org/work.org")))
(setq org-log-done t)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-search-view-always-boolean t)
(setq org-html-validation-link nil)
(setq org-publish-use-timestamps-flag nil)
(setq org-catch-invisible-edits 'smart)
(setq org-cycle-separator-lines 0)
(setq org-src-fontify-natively t)
(setq org-startup-folded nil)
(setq org-confirm-babel-evaluate nil)
(setq org-todo-keywords '((sequence "TODO(t)" "PROGRESS(p)" "WAIT(w@/@)" "BLOCK(b@/@)" "|" "DONE(d!/!)" "CANCELLED(c@/@)")))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-capture-templates
      `(("m" "Bookmark" entry (file+headline org-default-notes-file "Bookmarks")
	 "* %?\n")
	("x" "Task" entry (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n")))

(when (string= "goose" (system-name))
  (push '("b" "Book" entry (file+headline org-default-notes-file "Books")
	  "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	org-capture-templates)
  (push '("s" "Show" entry (file+headline org-default-notes-file "Tasks")
	  "* TODO %? :show:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	org-capture-templates)
  (push '("r" "Read/watch" entry (file+headline org-default-notes-file "Tasks")
	  "* TODO %? :read:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	org-capture-templates)
  (push '("p" "Project" entry (file+headline org-default-notes-file "Tasks")
	  "* TODO %? :project:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	org-capture-templates)
  (push '("j" "Journal" entry (file+olp+datetree "journal.org.gpg")
	  "* %<%H:%M>\n\n%?" :jump-to-captured t :empty-lines 1)
	org-capture-templates)

  (defun my-revert-gcal-before-agenda ()
    "Revert buffer for gcal.org so the agenda displays the latest contents"
    (when-let ((gcal (get-file-buffer "~/org/gcal.org")))
      (with-current-buffer gcal
	(revert-buffer t t))))

  (add-hook 'org-agenda-mode-hook 'my-revert-gcal-before-agenda))

(setq my-org-agenda-common-review-settings
      '((org-agenda-show-all-dates t)
	(org-agenda-start-with-log-mode t)
	(org-agenda-start-with-clockreport-mode t)
	;; https://orgmode.org/manual/Special-Agenda-Views.html
	(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("TODO" "WAIT" "BLOCK")))
	(org-agenda-archives-mode t)
	(org-agenda-clockreport-parameter-plist '(:link t :hidefiles t :tags t :step day :maxlevel 2 :fileskip0 t :formula %))
	;; (org-agenda-hide-tags-regexp
	;;  (concat org-agenda-hide-tags-regexp "\\|ARCHIVE"))
	(org-agenda-start-on-weekday 1)))

(setq org-agenda-custom-commands
      `(("d" "Done stuff" todo "DONE" )
	("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))

	("." "Agenda"
	 ((agenda "")
	  ,@(if (string= "asusbox" (system-name)) '((tags "PIN")))

	  (tags-todo "-read-watch-project-show"
		     ((org-agenda-overriding-header "Unscheduled:")
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
		      (org-agenda-sorting-strategy '((tags category-down priority-down tag-down)))))

	  (tags "show" ((org-agenda-overriding-header "Shows:")
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED")))
			(org-agenda-sorting-strategy '(priority-down))))

	  ,@(if-let ((tag (if (string= "goose" (system-name)) "read|watch|project")))
		`((tags ,tag ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED")))
			      (org-agenda-sorting-strategy '((tags tag-up alpha-up))))))))

	 ((org-agenda-start-with-log-mode nil)
	  (org-tags-match-list-sublevels nil)))

	("w" "This week" agenda ""
	 ,(append my-org-agenda-common-review-settings
		  '((org-agenda-span 'week)
		    (org-agenda-overriding-header "Week in Review")))
	 ("/tmp/week.html"))

	("W" "Last week" agenda ""
	 ,(append my-org-agenda-common-review-settings
		  '((org-agenda-span 'week)
		    (org-agenda-start-day "-1w")
		    (org-agenda-overriding-header "Last week in Review")))
	 ("/tmp/lastweek.html"))))

(defun my-org-mode-hook ()

  (evil-local-mode 1)
  (when my-olivetti-state
    (olivetti-mode 1))

  ;; / is punctuation, so evil * works on path components
  (modify-syntax-entry ?/ ".")
  (auto-fill-mode 1)

  (setq-local completion-at-point-functions '(my-complete-word-ispell))
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 2)
  (setq-local tab-width 2)
  (setq-local evil-move-beyond-eol t)

  (cond
   ((not (display-graphic-p))
    ;; override the evil binding of C-i (jump forward), as C-i is the
    ;; same as tab in the terminal, which we want in org mode for
    ;; (un)collapsing headers
    (evil-local-set-key 'motion (kbd "C-i") 'org-cycle))
   (t
    (evil-local-set-key 'normal (kbd "<tab>") 'org-cycle)))

  (evil-local-set-key 'insert (kbd "<tab>") #'org-cycle)
  (evil-local-set-key 'insert (kbd "<backtab>") #'fancy-dabbrev-backward))

(defun my-org-capture-hook ()
  (interactive)
  (my-org-mode-hook)
  (when (evil-normal-state-p)
    (evil-insert-state)))

;;; https://www.youtube.com/watch?v=UpeKWYFe9fU
(defun my-org-attach-save-file-list-to-property (dir)
  "Save list of attachments to ORG_ATTACH_FILES property."
  (when-let* ((files (org-attach-file-list dir)))
    (org-set-property "ORG_ATTACH_FILES" (mapconcat #'identity files ", "))))

(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'org-capture-mode-hook 'my-org-capture-hook)
(add-hook 'org-attach-after-change-hook 'my-org-attach-save-file-list-to-property)

(defun my-org-clock-jump ()
  (interactive)
  (push-mark (point))
  (org-clock-jump-to-current-clock))

(defun my-www-get-page-title (url)
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "<title[^>]*>\\([^<]*\\)</title>" nil t 1)
    (let ((title (match-string 1)))
      (goto-char (point-min))
      (re-search-forward "charset=\\([-0-9a-zA-Z]*\\)" nil t 1)
      (let* ((c (match-string 1))
	     (coding (if (or (not c)
			     (string= c "UTF-8")
			     (string= c title))
			 "utf-8" c)))
	(decode-coding-string title (intern coding))))))

(defun my-wrap-org-link ()
  "With point at the start of a URL, turn it into [[url][title]]"
  (interactive)
  (let ((bounds (thing-at-point-bounds-of-url-at-point t)))
    (when (and bounds (< (car bounds) (cdr bounds)))
      (let* ((url (buffer-substring-no-properties (car bounds) (cdr bounds)))
	     (title (my-www-get-page-title url)))
	(save-excursion
	  (goto-char (cdr bounds))
	  (insert (format "][%s]]" title))
	  (goto-char (car bounds))
	  (insert "[["))))))

(defun my-insert-org-src-block  ()
  "Insert a src block with the same language as the previous block"
  (interactive)
  (let ((lang (save-excursion
		(org-babel-previous-src-block)
		(move-end-of-line 1)
		(thing-at-point 'word t))))
    (org-insert-structure-template (format "src %s" lang))))

(defun my-goto-random-line ()
  (interactive)
  (goto-line (+ 1 (random
		   (+ 1 (save-excursion
			  (goto-char (point-max))
			  (current-line))))))
  (recenter nil t))

(advice-add 'org-time-stamp-inactive :before #'my-forward-before-insert)
(advice-add 'org-insert-last-stored-link :before #'my-forward-before-insert)
(advice-add 'org-insert-link :before #'my-forward-before-insert)

(defun my-org-agenda-toggle-done ()
  (interactive)
  (setq org-agenda-skip-scheduled-if-done (not org-agenda-skip-scheduled-if-done))
  (org-agenda-redo-all))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "d") 'my-org-agenda-toggle-done)
  (define-key org-agenda-mode-map (kbd "C-w") 'evil-window-map))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-'") nil)
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "C-c C-j") nil)
  (define-key org-mode-map (kbd "C-c C-'") 'org-edit-special)
  (define-key org-mode-map (kbd "C-c [") 'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c ]") nil)
  (define-key org-mode-map (kbd "C-c M-e") 'org-decrypt-entry)

  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell . t)
				 (awk . t)
				 (python .t)
				 (emacs-lisp . t)
				 (gnuplot . t)))

  (defun my-update-inline-images ()
    (when org-inline-image-overlays
      (org-redisplay-inline-images)))

  (add-hook 'org-babel-after-execute-hook 'my-update-inline-images)

  (setq org-babel-python-command "python3")

  ;; org to pdf export
  (when (executable-find "pandoc")
    (require 'ox-pandoc)))


(defun my-set-evil-local-mode-in-agenda-buffers (state)
  "Enable/disable evil-local-mode in all org-agenda-files buffers"
  (let ((state-func (if (and state
			     (memq 'org-mode evil-emacs-state-modes))
			#'evil-emacs-state
		      #'evil-normal-state)))
    (dolist (f (org-agenda-files))
      (when-let ((b (get-file-buffer f)))
	(with-current-buffer b
	  (evil-local-mode state)
	  (when state
	    (funcall state-func)))))))

(defun my-advise-org-agenda-todo (func &rest args)
  "Switch off evil-local-mode in all org-agenda-files buffers before
org-agenda-todo runs, and enable it again afterwards. This is a
workaround for a bug where marking a habit task as DONE from the
agenda doesn't correctly keep it in a repeating TODO state when
the buffer the agenda was built from has evil-local-mode enabled."
  (my-set-evil-local-mode-in-agenda-buffers 0)
  (unwind-protect
      (apply func args)
    (my-set-evil-local-mode-in-agenda-buffers 1)))

(advice-add #'org-agenda-todo :around #'my-advise-org-agenda-todo)

(defun my-merge-tables-by-date (a b &optional empty)
  "'a' and 'b' are tables (lists where each element is a row). The first
column of each is a date of the form YYYY-MM-DD. Merge the tables
by date. Modifies the lists in 'a' and 'b'. 'empty' is used to
fill empty cells when the rows don't match, and defaults to the
empty string."
  (let* ((blank (or empty ""))
    	 (cols (if a (- (length (car a)) 1) 1))
    	 res)
    (while (and a b)
      (let ((ta (date-to-time (caar a)))
    	    (tb (date-to-time (caar b))))
    	(cond
    	 ((time-less-p ta tb)
    	  (push (nconc (car a) (list blank)) res)
    	  (setq a (cdr a)))
    	 ((time-less-p tb ta)
    	  (push (nconc (list (caar b)) (make-list cols blank) (cdar b)) res)
    	  (setq b (cdr b)))
    	 (t
    	  (push (nconc (car a) (cdar b)) res)
    	  (setq a (cdr a))
    	  (setq b (cdr b))))))
    (while a
      (push (nconc (car a) (list blank)) res)
      (setq a (cdr a)))
    (while b
      (push (nconc (list (caar b)) (make-list cols blank) (cdar b)) res)
      (setq b (cdr b)))
    (reverse res)))

(global-set-key (kbd "C-'") (lambda ()
			      (interactive)
			      (find-file org-default-notes-file)))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c x") 'org-capture)
(global-set-key (kbd "C-c M-t") #'my-wrap-org-link)
(global-set-key (kbd "C-c C-x C-j") 'my-org-clock-jump)

(defun my-agenda ()
  (interactive)
  (org-agenda nil ".")
  (when (string= "goose" (system-name))
    ;; hide work tasks
    (org-agenda-filter-by-tag '(4) ?w)))

(when (display-graphic-p)
  (evil-global-set-key 'normal (kbd "C-.") nil)
  (global-set-key (kbd "C-c q") 'my-agenda)
  (global-set-key (kbd "<XF86LaunchB>") 'my-agenda)
  (global-set-key (kbd "<LaunchB>") 'my-agenda))

(evil-leader/set-key-for-mode 'org-mode "c" 'my-insert-org-src-block)
(evil-leader/set-key-for-mode 'org-mode "SPC" 'my-goto-random-line)

;; ----------------------------------------------------------------------------
;;| Ledger
;; ----------------------------------------------------------------------------

(when (string= "goose" (system-name))
  (setq ledger-binary-path (expand-file-name "~/dotfiles-public/bin/ledger.bash")))

(with-eval-after-load "ledger-report"

  (defun my-ledger-report-hook ()
    (toggle-truncate-lines 0))
  (add-hook 'ledger-report-mode-hook 'my-ledger-report-hook)

  (add-to-list 'ledger-reports
	       '("expenses this month" "%(binary) -f %(ledger-file) bal -S \"-abs(total)\" ^Expenses: --period 'this month'"))
  (add-to-list 'ledger-reports
	       '("expenses last month" "%(binary) -f %(ledger-file) bal -S \"-abs(total)\" ^Expenses: --period 'last month'")))

;; ----------------------------------------------------------------------------
;;| Calendar
;; ----------------------------------------------------------------------------

(push '("\\*Calendar\\*"
        (display-buffer-reuse-window display-buffer-below-selected)
        (window-height . 10))
      display-buffer-alist)

(global-set-key (kbd "C-c M-r") 'calendar)

;; ----------------------------------------------------------------------------
;;| Calc
;; ----------------------------------------------------------------------------

(defun my-calc-copy ()
  "Copy the number at the top of the calc stack"
  (interactive)
  (let ((x (math-format-number (calc-top))))
    (kill-new x)
    (message "Yanked %s" x)))

(with-eval-after-load "calc-ext"
  (define-key calc-mode-map (kbd "C-c C-o") #'calc-reset)
  (define-key calc-mode-map (kbd "C-c M-o") #'calc-reset)
  (define-key calc-mode-map (kbd "C-M-w") #'my-calc-copy)
  (setq calc-multiplication-has-precedence nil)
  (setq calc-make-windows-dedicated t)
  (advice-add #'calc-user-define-formula :around #'my-disable-vertico))

(defun my-calc-hook ()
  (setq-local calc-angle-mode 'rad)
  (calc-trail-display 0))

(add-hook 'calc-mode-hook 'my-calc-hook)

(defun my-calc-yank ()
  "Yank number at point into calc stack"
  (interactive)
  (save-excursion
    (when (looking-at "[[:space:]]")
      (re-search-forward "[^[:space:]]"))
    (calc-yank-internal nil (format "%s" (number-at-point)))))

(global-set-key (kbd "C-x C-y") 'my-calc-yank)

;; ----------------------------------------------------------------------------
;;| Browser
;; ----------------------------------------------------------------------------

(when (eq system-type 'gnu/linux)
  ;; open org links in chrome
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "google-chrome"))

(setq org-file-apps '((auto-mode . emacs)
		      (directory . emacs)
		      ("\\.x?html?\\'" . "google-chrome %s")
		      ("\\.pdf\\'" . default)))

;; ----------------------------------------------------------------------------
;;| HTML
;; ----------------------------------------------------------------------------

(defun my-html-hook ()
  (define-key html-mode-map (kbd "M-o") nil)
  (evil-local-set-key 'normal (kbd "gb")
		      (lambda ()
			(interactive)
			(save-buffer)
			(browse-url-of-file))))

(add-hook 'html-mode-hook #'my-html-hook)

(advice-add 'sgml-close-tag :before #'my-forward-before-insert)

;; ----------------------------------------------------------------------------
;;| Vertico
;; ----------------------------------------------------------------------------

(require 'vertico)
;; (require 'vertico-directory)
(vertico-mode)

(define-key vertico-map (kbd "C-j") nil)
(define-key vertico-map (kbd "C-h f")
  (lambda ()
    (interactive)
    (describe-function (intern (vertico--candidate)))))

(defun my-disable-vertico (func &rest args)
  (let ((v vertico-mode))
    (vertico-mode 0)
    (unwind-protect
	(apply func args)
      (vertico-mode v))))

;; ----------------------------------------------------------------------------
;;| Marginalia
;; ----------------------------------------------------------------------------

(require 'marginalia)
(marginalia-mode)

(defun my-disable-marginalia (func &rest args)
  (let ((m marginalia-mode))
    (marginalia-mode 0)
    (unwind-protect
	(apply func args)
      (marginalia-mode m))))

;; ----------------------------------------------------------------------------
;;| Orderless
;; ----------------------------------------------------------------------------

(require 'orderless)
(setq completion-styles '(orderless flex))

;; ----------------------------------------------------------------------------
;;| Consult
;; ----------------------------------------------------------------------------

(require 'consult)

(global-set-key (kbd "C-c r") 'consult-recent-file)

(define-key minibuffer-local-map (kbd "M-r") 'consult-history)

(setq consult-preview-key "C-j")

(consult-customize consult-line :preview-key 'any)

(defun my-imenu ()
  (interactive)
  (let ((f (buffer-file-name)))
    (cond
     ((and f (file-equal-p f (my-init-file)))
      (let ((outline-regexp "^;;|"))
	(consult-outline)))
     ((eq major-mode 'org-mode) (consult-org-heading))
     (t (consult-imenu)))))

(defun my-ripgrep-project (&optional prefix)
  "Search project. Prefix takes C++ identifier instead of symbol if
in C/C++ mode."
  (interactive "P")
  (let* ((consult-preview-key 'any)
	 (consult--buffer-display #'switch-to-buffer-other-window)
	 (sym (cond
	       ((and prefix
		     (or (eq major-mode 'c++-mode)
			 (eq major-mode 'c-mode)))
		(my-cpp-identifier-around-point))
	       (t (thing-at-point 'symbol t))))
	 (initial (if sym (format "\\<%s\\>" sym) nil)))
    (consult-ripgrep nil initial)))

(define-key consult-async-map (kbd "M-w")
	    (lambda ()
	      (interactive)
	      (my-toggle-word-boundary "^#\\\\<\\(.*\\)\\\\>" "#\\<" "\\>"
					 "^#" "#")))

(evil-leader/set-key "r"   'my-ripgrep-project)
(evil-leader/set-key "M-r" 'my-ripgrep-project)
(evil-leader/set-key "i"   'my-imenu)
(evil-leader/set-key "M-i" 'my-imenu)

;; ----------------------------------------------------------------------------
;;| Grep
;; ----------------------------------------------------------------------------

(require 'wgrep)

(setq wgrep-enable-key "e")

(defun my-rgrep-project ()
  (interactive)
  (rgrep (read-string "Search for: " (format "\\<%s\\>" (thing-at-point 'symbol t)))
	 "*" (my-find-project-root)))

(evil-leader/set-key "M-f" 'my-rgrep-project)
(evil-leader/set-key "f"   'my-rgrep-project)
(evil-leader/set-key "M-g" 'rgrep)
(evil-leader/set-key "g"   'rgrep)

;;; disable vertico when rgrep asks for file type
(advice-add #'grep-read-files :around #'my-disable-vertico)

(define-key grep-mode-map (kbd "M-p") nil)
(define-key grep-mode-map (kbd "M-n") nil)

;; ----------------------------------------------------------------------------
;;| Embark
;; ----------------------------------------------------------------------------

(require 'embark)
(require 'embark-consult)

;;; export consult-ripgrep results to grep buffer
(define-key consult-async-map (kbd "C-c C-o") #'embark-export)

;; ----------------------------------------------------------------------------
;;| Completion
;; ----------------------------------------------------------------------------

(setq completion-ignore-case t)
(setq completion-show-help nil)
(setq completion-auto-select 'second-tab)

;; Don't want marginalia in *Completions* buffer when hitting TAB in
;; shell mode. Completion candidates are in a grid, and some are
;; pushed off-screen.
(advice-add #'completion--in-region :around #'my-disable-marginalia)
(advice-add #'minibuffer-complete :around #'my-disable-marginalia)

;;; popup the completion buffer at the bottom
(push '("\\*Completions\\*"
        (display-buffer-reuse-window display-buffer-at-bottom)
        (window-height . 10))
      display-buffer-alist)

;;; use vertico for completion-at-point, but not when completing
;;; file/directory names in shell/comint
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if (and vertico-mode (not (or ;;(derived-mode-p 'minibuffer-mode)
					      (derived-mode-p 'comint-mode)
					      (derived-mode-p 'eshell-mode))))
		   #'consult-completion-in-region
		 #'completion--in-region)
	       args)))

(defun my-icomplete-hook ()
  (let ((inhibit-message t))
    (toggle-truncate-lines 1)))
(add-hook 'icomplete-minibuffer-setup-hook 'my-icomplete-hook)

(setq icomplete-compute-delay 0.0)
(setq icomplete-matches-format nil)
(setq icomplete-show-matches-on-no-input t)
;; (setq completion-pcm-word-delimiters "-_./:|")
;; (setq icomplete-scroll t)
;; (icomplete-vertical-mode 1)

(with-eval-after-load 'icomplete
  (define-key icomplete-vertical-mode-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
  (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
  ;; TAB shows all completions in popup buffer
  ;; (define-key icomplete-minibuffer-map (kbd "TAB") 'icomplete-force-complete)
  ;; (define-key icomplete-minibuffer-map (kbd "C-j") 'ignore)
  (define-key icomplete-minibuffer-map (kbd "SPC") 'self-insert-command) ;; allow orderless to work
  (define-key icomplete-minibuffer-map (kbd "C-j") 'icomplete-force-complete-and-exit)
  (define-key icomplete-minibuffer-map (kbd "C-s") 'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "C-r") 'icomplete-backward-completions))

;; ----------------------------------------------------------------------------
;;| Buffers
;; ----------------------------------------------------------------------------

(setq read-buffer-completion-ignore-case t)

(defun my-invoke-with-completion (func)
  (let ((v vertico-mode)
	(m marginalia-mode)
	(ic icomplete-mode)
	(icv icomplete-vertical-mode))
    (vertico-mode 0)
    (marginalia-mode 0)
    (icomplete-vertical-mode -1)
    (icomplete-mode 1)
    (unwind-protect
	(call-interactively func)
      (icomplete-mode -1)
      (when v (vertico-mode 1))
      (when m (marginalia-mode 1))
      (when icv (icomplete-vertical-mode 1)))))

(defun my-kill-buffer ()
  (interactive)
  (if (or (get-buffer-process (current-buffer))
	  (eq major-mode 'org-agenda-mode)
	  (eq major-mode 'dired-mode)
	  (eq major-mode 'compilation-mode)
	  (not (buffer-modified-p))
	  (string= "*Async Shell Command*"
		   (buffer-name (current-buffer))))
      (kill-this-buffer)
    (my-invoke-with-completion #'kill-buffer)))

(defun my-switch-buffer ()
  (interactive)
  (my-invoke-with-completion #'consult-buffer))

(defun my-switch-buffer-other-window ()
  (interactive)
  (my-invoke-with-completion #'consult-buffer-other-window))

(evil-global-set-key 'motion (kbd "C-w d")   'my-kill-buffer)
(evil-global-set-key 'motion (kbd "C-w C-d") 'my-kill-buffer)
(global-set-key (kbd "C-x k") 'my-kill-buffer)

(global-set-key (kbd "C-j")     'my-switch-buffer)
(global-set-key (kbd "C-x b")   'my-switch-buffer)
(global-set-key (kbd "C-x 4 b") 'my-switch-buffer-other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "M-o") nil)
  (define-key ibuffer-mode-map (kbd "M-n") nil)
  (define-key ibuffer-mode-map (kbd "M-p") nil)

  (require 'ibuffer-project)

  (defun my-ibuffer-hook ()
    (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)))

  (add-hook 'ibuffer-mode-hook 'my-ibuffer-hook))

;; ----------------------------------------------------------------------------
;;| Complete filenames
;; ----------------------------------------------------------------------------

(require 'cape)

(defvar my-completing-filename nil)

(defun my-complete-filename ()
  "Expand the filename before the point"
  (interactive)
  (setq my-completing-filename t)
  (unwind-protect
      (while my-completing-filename
	(call-interactively 'cape-file)
	(setq my-completing-filename (eq my-completing-filename 'continue)))
    (setq my-completing-filename nil)))

(evil-global-set-key 'insert (kbd "C-x C-f") 'my-complete-filename)

;; keep completing into directory
(define-key (if vertico-mode
		vertico-map
	      icomplete-minibuffer-map)
  (kbd "C-x C-f")
  (lambda ()
    (interactive)
    (when my-completing-filename
      (setq my-completing-filename 'continue)
      (if vertico-mode
	  (vertico-exit)
	(icomplete-force-complete-and-exit)))))

;; ----------------------------------------------------------------------------
;;| Helm
;; ----------------------------------------------------------------------------

(require 'helm)
(require 'helm-occur)

;; display relative paths in grep results
(setq-default helm-grep-file-path-style 'relative)

(setq helm-split-window-default-side 'right)

(setq helm-highlight-only-all-matches t)
(setq helm-highlight-matches-around-point-max-lines '(25 . 25))
;;; no new frames
(setq helm-show-completion-display-function #'helm-show-completion-default-display-function)

(defun my-advise-propagate-input-method (func &rest args)
  "Allow func to use evil-input-method in minibuffer"
  (if (and (bound-and-true-p evil-local-mode)
	   evil-input-method)
      (evil-without-input-method-hooks
       (let ((input-method current-input-method))
	 (set-input-method evil-input-method)
	 (unwind-protect
	     (apply func args)
	   (set-input-method input-method))))
    (apply func args)))

(advice-add 'helm-occur :around #'my-advise-propagate-input-method)

(define-key helm-occur-map (kbd "M-w")
	    (lambda ()
	      (interactive)
	      (my-toggle-word-boundary "^\\\\_<\\(.*\\)\\\\_>" "\\_<" "\\_>" "" "")))

(define-key helm-map (kbd "C-c C-u") 'kill-whole-line)
(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(evil-leader/set-key "o" 'helm-occur) ;; M-n grabs symbol under point
(evil-leader/set-key "M-o" 'helm-occur)

;; ----------------------------------------------------------------------------
;;| Projects
;; ----------------------------------------------------------------------------

(defun my-find-project-root ()
  (let* ((dir (if (version< emacs-version "28.1")
		  (locate-dominating-file default-directory ".git")
		(let ((project (project-current nil)))
		  (and project (project-root project))))))
    (or (and dir (expand-file-name (file-name-as-directory dir)))
	default-directory)))

(defun my-find-file-in-project ()
  (interactive)
  (let ((read-file-name-completion-ignore-case t))
    (project-find-file)))

(defun my-find-file-in-project-other-window ()
  (interactive)
  (let* ((switch-to-buffer-obey-display-actions t)
	 (display-buffer-overriding-action '((display-buffer-pop-up-window)
					     (inhibit-same-window . t))))
    (my-find-file-in-project)))

(defun my-jump-project-dired ()
  (interactive)
  (dired (my-find-project-root)))


(defvar my-projects)
(cond
 ((file-exists-p "~/dev/git")
  (setq my-projects '(("~/dev/git" . 3))))
 ((file-exists-p "~/dev/work")
  (setq my-projects '(("~/dev/work" . 2)
		      ("~/dev" . 2))))
 (t
  (setq my-projects '(("~/dev" . 2)))))

(dolist (d `("~/dotfiles-public" "~/dotfiles" "~/notefiles" ,org-directory))
  (when (file-directory-p d)
    (push `(,d . 1) my-projects)))

(when (eq system-type 'windows-nt)
  (let ((d (concat (string-replace "\\" "/" (getenv "USERPROFILE"))
		   "/Documents/Unreal Projects")))
    (when (file-directory-p d)
      (push `(,d . 2) my-projects))))


(defun my-find-projects (dir depth)
  "Return a list of projects under 'dir' up to 'depth'"
  (if (string-suffix-p "/.git" dir)
      (list (string-remove-suffix "/.git" dir))
    (if (> depth 0)
	(seq-mapcat (lambda (x)
		      (my-find-projects
		       (concat dir "/" (string-remove-suffix "/" x))
		       (- depth 1)))
		    (seq-filter (lambda (x)
				  (and (string-suffix-p "/" x)
				       (not (or (string= "./" x)
						(string= "../" x)))))
				(file-name-all-completions "" dir))))))

(defun my-list-repos ()
  "Return an alist of repos, with the key the string to match
against, and the value the expanded full path to the repo"
  (let ((all '()))
    (dolist (proj my-projects)
      (let* ((dir (expand-file-name (car proj)))
	     (dir-slash (if (string-suffix-p "/" dir) dir (concat dir "/")))
	     (depth (cdr proj))
	     (repos (mapcar
		     (lambda (long)
		       (let ((short (if (string-prefix-p dir-slash long)
					(string-remove-prefix dir-slash long)
				      (file-name-nondirectory long))))
			 `(,short . ,long)))
		     (my-find-projects dir depth))))
	(setq all (nconc all repos))))
    all))

(defun my-choose-project (&optional action)
  "Choose a project then invoke action on it. If action is nil,
return the project path instead"
  (let* ((repos (my-list-repos))
	 (sel (assoc (completing-read "Repo: " repos) repos)))
    (if sel
	(let ((path (cdr sel)))
	  (if action
	      (funcall action path)
	    path)))))

(defun my-choose-project-and-invoke (func &rest args)
  "Set default-directory to the chosen project, then invoke func with args"
  (my-choose-project (lambda (path)
		       (let ((default-directory path))
			 (apply func args)
			 (pwd)))))

(defun my-choose-project-and-find-file ()
  (interactive)
  (my-choose-project-and-invoke #'my-find-file-in-project))

(defun my-choose-project-and-find-file-other-window ()
  (interactive)
  (my-choose-project-and-invoke #'my-find-file-in-project-other-window))

(defun my-choose-project-and-search (&optional prefix)
  (interactive "P")
  (my-choose-project-and-invoke 'my-ripgrep-project))

(defun my-choose-project-and-magit ()
  (interactive)
  (my-choose-project-and-invoke #'magit))

(defun my-choose-project-and-dired ()
  (interactive)
  (my-choose-project (lambda (path)
		       (dired path)
		       (pwd))))

(defun my-choose-project-and-shell ()
  (interactive)
  (my-choose-project-and-invoke #'project-shell))

(defun my-choose-project-and-term ()
  (interactive)
  (my-choose-project-and-invoke (lambda ()
				  (interactive)
				  (call-interactively #'ansi-term))))

(defun my-jump-notefiles ()
  (interactive)
  (let ((default-directory "~/notefiles"))
    (my-find-file-in-project)))

(global-set-key (kbd "C-c p e") #'my-choose-project-and-find-file)
(global-set-key (kbd "C-c p u") #'my-choose-project-and-find-file-other-window)
(global-set-key (kbd "C-c p r") #'my-choose-project-and-search)
(global-set-key (kbd "C-c p v") #'my-choose-project-and-magit)
(global-set-key (kbd "C-c p d") #'my-choose-project-and-dired)
(global-set-key (kbd "C-c p s") #'my-choose-project-and-shell)
(global-set-key (kbd "C-c p t") #'my-choose-project-and-term)
(global-set-key (kbd "C-c p n") #'my-jump-notefiles)

(global-set-key (kbd "C-c e") 'my-find-file-in-project)
(global-set-key (kbd "C-c u") 'my-find-file-in-project-other-window)
(global-set-key (kbd "C-x C-d") 'my-jump-project-dired)

(evil-leader/set-key "e" 'my-find-file-in-project)
(evil-leader/set-key "u" 'my-find-file-in-project-other-window)

;; ----------------------------------------------------------------------------
;;| Isearch
;; ----------------------------------------------------------------------------

;; space in search is a wildcard. 'M-s space' to toggle
(setq search-whitespace-regexp ".*" ; ".*?" for non-greedy
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil)
;; stop downcasing when symbol searching with M-s .
(setq search-upper-case t)
(setq isearch-lazy-count t)

(defun my-advise-isearch-lax-whitespace (func &rest args)
  (let ((isearch-lax-whitespace nil))
    (apply func args)))

(advice-add 'isearch-yank-char :around 'my-advise-isearch-lax-whitespace)
(advice-add 'isearch-yank-until-char :around 'my-advise-isearch-lax-whitespace)

(defun my-isearch-hook ()
  "Use evil-input-method in isearch minibuffer"
  (when (and (bound-and-true-p evil-local-mode)
	     evil-input-method)
    (evil-without-input-method-hooks
      (set-input-method evil-input-method))))

(add-hook 'isearch-mode-hook 'my-isearch-hook)

;; ----------------------------------------------------------------------------
;;| Dired
;; ----------------------------------------------------------------------------

(require 'dired)
(require 'dired-x)
(setq dired-dwim-target t)
(put 'dired-find-alternate-file 'disabled nil)

;;; workaround annoyance of having to select the editing line with vertico
(advice-add #'dired-do-rename :around #'my-disable-vertico)

(defun my-org-attach-dired-move ()
  (interactive)
  (let ((org-attach-method 'mv))
    (call-interactively #'org-attach-dired-to-subtree)))

(define-key dired-mode-map (kbd "SPC") evil-leader--default-map)
(define-key dired-mode-map (kbd "C-w") 'evil-window-map)
(define-key dired-mode-map (kbd ";") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c C-x C-a") #'org-attach-dired-to-subtree)
(define-key dired-mode-map (kbd "C-c C-x a") #'org-attach-dired-to-subtree)
(define-key dired-mode-map (kbd "C-c C-x m") #'my-org-attach-dired-move)

(defun my-dired-hook ()
  (auto-revert-mode 1))

(add-hook 'dired-mode-hook 'my-dired-hook)

(setq-default dired-listing-switches "-alh") ;; human-readable file sizes

(pcase system-type
  ('gnu/linux
   (setq dired-guess-shell-alist-user '(("\\.exr\\'" "djv")
					("\\.mp4\\'" "mpv")
					("\\.mkv\\'" "mpv")
					("\\.pdf\\'" "evince")
					("" "open"))))

  ('darwin
   (setq dired-guess-shell-alist-user '(("" "open"))))

  ('windows-nt
   (setq dired-guess-shell-alist-user '(("\\.mp4\\'" "djv")
					("\\.exr\\'" "djv")
					("\\.tif\\'" "djv")))))

(global-set-key (kbd "C-x C-j") 'dired-jump)

;; ----------------------------------------------------------------------------
;;| Magit
;; ----------------------------------------------------------------------------

(defun my-magit-hook ()
  (evil-local-mode 1)
  ;; want SPC to show/scroll commit at point
  (evil-leader-mode -1))

(defun my-magit-repolist-hook ()
  (evil-local-mode 1)
  (beginning-of-buffer))

(defun my-magit-list-repos ()
  (interactive)
  (other-window 1)
  (setq magit-repository-directories '())
  (dolist (proj (my-list-repos))
    (let ((dir (cdr proj)))
      (push `(,dir . 0) magit-repository-directories)))
  (magit-list-repositories))

(defun my-git-commit-mode-hook ()
  ;; want completion on elisp symbols
  (modify-syntax-entry ?- "_"))

(add-hook 'git-commit-mode-hook 'my-git-commit-mode-hook)

(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "x") #'magit-delete-thing)
  ;; (define-key magit-hunk-section-map (kbd "C-j") nil))

  (evil-collection-magit-setup)

  (add-hook 'magit-mode-hook #'my-magit-hook)

  (evil-define-key 'normal magit-mode-map (kbd "<escape>") nil) ;; stop escape burying buffer
  (evil-define-key 'normal magit-mode-map (kbd ";") 'magit-section-up)
  (evil-define-key 'normal magit-mode-map (kbd "SPC") evil-leader--default-map)
  (evil-define-key 'normal magit-mode-map (kbd "C-j") nil)  ; clashes with other-window
  (evil-define-key 'normal magit-mode-map (kbd "C-k") nil)
  (evil-define-key 'normal magit-mode-map (kbd "p") 'magit-section-backward)
  (evil-define-key 'normal magit-mode-map (kbd "n") 'magit-section-forward))

(with-eval-after-load 'magit-repos

  (add-hook 'magit-repolist-mode-hook #'my-magit-repolist-hook)

  (evil-define-key 'normal magit-repolist-mode-map (kbd "RET") 'magit-repolist-status)

  (setq magit-repolist-sort-key '("B>U" . t))
  (setq magit-repolist-column-flag-alist '((magit-unstaged-files . "U")
					   (magit-staged-files . "S"))
	magit-repolist-columns '(("Branch" 20 magit-repolist-column-branch nil)
				 ("Flag" 4 magit-repolist-column-flag)
				 ("Path" 50 magit-repolist-column-path nil)
				 ;; ("Name" 25 magit-repolist-column-ident nil)
				 ("B<U" 3 magit-repolist-column-unpulled-from-upstream
				  ((:right-align t)
				   (:help-echo "Upstream changes not in branch")))
				 ("B>U" 3 magit-repolist-column-unpushed-to-upstream
				  ((:right-align t)
				   (:help-echo "Local changes not in upstream"))))))

(require 'magit)

(evil-leader/set-key "v" 'magit-status)
(global-set-key (kbd "C-c v") 'magit-status)
(global-set-key (kbd "C-c V") 'my-magit-list-repos)

;; ----------------------------------------------------------------------------
;;| Ediff
;; ----------------------------------------------------------------------------

(setq-default ediff-custom-diff-options "-u")

(defun my-diff-mode-hook ()
  ;; stop overriding new window switch key
  (define-key diff-mode-map (kbd "M-o") nil))

(add-hook 'diff-mode-hook #'my-diff-mode-hook)

;; ----------------------------------------------------------------------------
;;| Compilation
;; ----------------------------------------------------------------------------

(defun my-compilation-mode-hook ()
  (modify-syntax-entry ?_ "w") ;; _ is word constituent, so * and # works
  (visual-line-mode)
  (evil-local-mode 1)
  (evil-emacs-state)
  (evil-local-set-key 'normal (kbd "q") 'quit-window)

  (when (and (eq system-type 'windows-nt)
	     (car (directory-files (my-find-project-root) nil ".*\\.uplugin\\'" t 1)))
    ;; unreal plugin sources are copied to a package/staging directory before
    ;; being compiled. make the filename in the error messages point at the
    ;; original instead of the copy.
    (setq-local compilation-transform-file-match-alist
		'(("\\`.*\\\\stage\\\\HostProject\\\\Plugins\\\\[^\\]+\\\\" "")
		  ("/bin/[a-z]*sh\\'" nil)))))

(defun my-grep-mode-hook ()
  (evil-local-mode -1))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)
(add-hook 'grep-mode-hook 'my-grep-mode-hook)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(global-set-key (kbd "C-c SPC") #'compile)
(global-set-key (kbd "C-c C-SPC") #'project-compile)
(global-set-key (kbd "C-c C-,") #'recompile)
(global-set-key (kbd "C-c ,") #'recompile)
(global-set-key (kbd "C-c g") (lambda () (interactive) (my-jump-buffer "*compilation*")))
(global-set-key (kbd "C-c h") (lambda () (interactive) (my-jump-buffer "*Help*" t)))
(define-key compilation-mode-map (kbd "SPC") evil-leader--default-map)
(define-key compilation-mode-map (kbd "C-w") 'evil-window-map)
(define-key compilation-mode-map (kbd "g") nil)
(define-key compilation-mode-map (kbd "M-p") nil)
(define-key compilation-mode-map (kbd "M-n") nil)

;; ----------------------------------------------------------------------------
;;| Makefile
;; ----------------------------------------------------------------------------

(defun my-makefile-hook ()
  (my-syntax-entry))
(add-hook 'makefile-mode-hook 'my-makefile-hook)

(defun my-makefile-no-warn-suspicious-lines ())
(advice-add 'makefile-warn-suspicious-lines :override #'my-makefile-no-warn-suspicious-lines)

(with-eval-after-load "cmake-mode"
  (defun my-cmake-hook ()
    (my-syntax-entry))
  (add-hook 'cmake-mode-hook 'my-cmake-hook))

;; ----------------------------------------------------------------------------
;;| Log and conf
;; ----------------------------------------------------------------------------

(defun my-log-settings ()
  (my-syntax-entry)
  (evil-local-mode 1)
  (toggle-truncate-lines 0))

(add-to-list 'auto-mode-alist '("\\.log\\'" . my-log-settings))

(defun my-conf-settings ()
  (my-syntax-entry)
  (evil-local-mode 1))

(add-hook 'conf-mode-hook 'my-conf-settings)

(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . my-conf-settings))

;; ----------------------------------------------------------------------------
;;| Markdown
;; ----------------------------------------------------------------------------

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-c") nil)
  (define-key markdown-mode-map (kbd "C-c C-v") markdown-mode-command-map)
  (define-key markdown-mode-map (kbd "M-p") nil)
  (define-key markdown-mode-map (kbd "M-n") nil))

(defun my-markdown-hook ()
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 2)
  (setq-local tab-width 2)
  (my-syntax-entry))

(add-hook 'markdown-mode-hook 'my-markdown-hook)

;; ----------------------------------------------------------------------------
;;| Yaml
;; ----------------------------------------------------------------------------

(defun my-yaml-hook ()
  (auto-fill-mode -1)
  (setq-local evil-shift-width 2)
  ;; = is punctuation
  (modify-syntax-entry ?= "."))

(add-hook 'yaml-mode-hook 'my-yaml-hook)

;; ----------------------------------------------------------------------------
;;| Comint
;; ----------------------------------------------------------------------------

(defun my-comint-ctrl-r ()
  (interactive)
  (if (comint-after-pmark-p)
      (if vertico-mode
	  (consult-history)
	(comint-dynamic-list-input-ring))
    (call-interactively 'isearch-backward)))

(defun my-comint-ret ()
  "When the cursor is in the middle of the shell output, stop the
return key from pasting the whole lot back and executing it."
  (interactive)
  (if (comint-after-pmark-p)
      (comint-send-input)
    (message "Point is before process mark, NOT sending")))

(defun my-comint-ctrl-d ()
  "The first C-d ends the process, second C-d deletes the buffer and
  closes the window."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (progn
	;; Fix "shell-apply-ansi-color: Text is read-only" error
	(advice-add 'comint-send-eof :override #'process-send-eof)
	(unwind-protect
	    (comint-delchar-or-maybe-eof 1)
	  (advice-remove 'comint-send-eof #'process-send-eof)))
    (kill-buffer)
    (if (> (count-windows) 1)
	(delete-window))))

(with-eval-after-load 'comint
  (define-key comint-mode-map (kbd "M-_") 'comint-insert-previous-argument)
  (define-key comint-mode-map (kbd "M-r") 'move-to-window-line-top-bottom)
  (define-key comint-mode-map (kbd "C-r") 'my-comint-ctrl-r)
  (define-key comint-mode-map (kbd "C-d") 'my-comint-ctrl-d)
  (define-key comint-mode-map (kbd "RET") 'my-comint-ret))

;; ----------------------------------------------------------------------------
;;| Shell
;; ----------------------------------------------------------------------------

(defun my-project-buffer-name (mode)
  (concat "*" (downcase mode)
          ":" (file-name-nondirectory
               (directory-file-name default-directory))
          "*"))

(advice-add 'project-prefixed-buffer-name :override 'my-project-buffer-name)

(defun my-project-shell ()
  (interactive)
  (let ((evil-split-window-below t))
    (evil-window-split))
  (if (project-current nil)
      (project-shell)
    (shell)))

(defun my-shell ()
  (interactive)
  (let ((evil-split-window-below t))
    (evil-window-split))
  (let* ((name (file-name-nondirectory
		(directory-file-name default-directory)))
	 (buf (generate-new-buffer (concat "*shell:" name "*"))))
    (shell buf)))

(defun my-match-shell-predicate (buffer-or-name &optional arg)
  "Predicate for match-buffers to find shells."
  (let ((name (if (bufferp buffer-or-name)
		  (buffer-name buffer-or-name)
		buffer-or-name)))
    (or (string-match-p "^\\*e?shell[^ ]+"	     name) ;; Not space to avoid *Shell Command Output* buffer
	(string-match-p "\\*ansi-term\\*"            name)
	(string-match-p "\\*Async Shell Command\\*"  name)
	(string-match-p "\\*compilation\\*<[0-9]+>"  name)
	(string-match-p "^\\*gud\\*$"		     name)
	(string-match-p "^\\*gud-.*\\*$"	     name))))

(defun my-jump-to-shell (&optional other)
  (interactive "P")
  (if-let ((target (car (match-buffers 'my-match-shell-predicate))))
      (my-jump-buffer target other)
    (message "No shell to jump to")))

(defun my-shell-hook ()
  (undo-tree-mode -1)			; don't shadow M-_
  (fancy-dabbrev-mode -1)
  (visual-line-mode 0)
  (toggle-truncate-lines 0)

  ;; fill out longest common part of filename first
  (setq-local completion-styles '(emacs21 flex))

  ;; don't ignore .git, etc
  (setq-local completion-ignored-extensions nil
	      completion-ignore-case nil))

(add-hook 'shell-mode-hook 'my-shell-hook)

(global-set-key (kbd "C-c t S") 'my-project-shell)
(global-set-key (kbd "C-c t s") 'my-shell)
(global-set-key (kbd "C-M-'") 'my-jump-to-shell)

(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "SPC") 'comint-magic-space))

(when (eq system-type 'windows-nt)
  (setq-default shell-file-name "bash.exe"))

(push '("\\*Async Shell Command\\*"
        (display-buffer-no-window))
      display-buffer-alist)

(defun my-sh-mode-hook ()
  (my-syntax-entry)
  ;; all these are symbols by default, want them as punctuation
  (dolist (c '(?! ?% ?* ?, ?. ?: ?^ ?~))
    (modify-syntax-entry c ".")))

(add-hook 'sh-mode-hook 'my-sh-mode-hook)

;; ----------------------------------------------------------------------------
;;| Term
;; ----------------------------------------------------------------------------

(defun my-expose-global-binding (map binding)
  (define-key map binding (lookup-key (current-global-map) binding)))

(with-eval-after-load 'term
  (my-expose-global-binding term-raw-map (kbd "M-o"))
  (my-expose-global-binding term-raw-map (kbd "C-j")))

(global-set-key (kbd "C-c t a") 'ansi-term)

;; ----------------------------------------------------------------------------
;;| Tags
;; ----------------------------------------------------------------------------

(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

(defun my-dir-predicate (subdir)
  (let* ((name (file-name-nondirectory
		(directory-file-name subdir))))
    (not (or (string= name "build")
	     (string= name "backup")
	     (string= name "registry")
	     (string= name "site-packages")
	     (string= name "Intermediate")
	     (string= name ".git")))))

(defun my-make-tags ()
  "Find a TAGS file above the default-directory, invoke
make TAGS in that directory."
  (interactive)
  (let* ((dir (locate-dominating-file default-directory "TAGS"))
	 (path (and dir (expand-file-name (file-name-as-directory dir)))))
    (if (not path)
	(message (format "No TAGS file found above %s" default-directory))
      (when (y-or-n-p (format "Run 'make TAGS' in %s" path))
	(message (format "Running 'make -C %s TAGS'" path))
	(call-process "make" nil nil nil "-C" path "TAGS")
	;; (visit-tags-table (concat path "TAGS"))
	))))

(defun my-run-ctags (&optional prefix)
  "Generate tags in the current project, and visit the tags file."
  (interactive)
  (let ((proj (my-find-project-root))
	(ctags (if (eq system-type 'darwin) "uctags" "ctags")))
    (when (y-or-n-p (format "Run '%s' in %s?" ctags proj))
      (message (format "Running '%s' in %s ..." ctags proj))
      (when (= 0 (shell-command
		  (format "cd \"%s\" && %s -R -e -f TAGS --exclude=.git --exclude=build . > /dev/null"
			  proj ctags)))
	(visit-tags-table (concat proj "TAGS"))))))

(defun my-find-tags-files ()
  "Find TAGS files and set tags-table-list"
  (let ((all '())
	(dirs (if (file-exists-p "~/dev/git")
		  '("~/dev/git")
		'("~/dev/"))))
    (message (format "Searching for TAGS files under %s ..." dirs))
    (dolist (dir dirs)
      (setq all (nconc all (directory-files-recursively
			    (expand-file-name dir) "^TAGS$" nil
			    'my-dir-predicate))))
    (setq-default tags-table-list all)))

(global-set-key (kbd "C-c t m") 'my-make-tags)
(global-set-key (kbd "C-c t r") 'my-run-ctags)

(setq xref-show-definitions-function #'xref-show-definitions-completing-read)

(defun my-advise-jump-to-definition (&rest r)
  (pwd))

(advice-add #'xref-find-definitions :after 'my-advise-jump-to-definition)
(advice-add #'xref-find-definitions-other-window :after 'my-advise-jump-to-definition)

;; ----------------------------------------------------------------------------
;;| Lisp
;; ----------------------------------------------------------------------------

(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(defun my-lisp-ctrl-j (&optional prefix)
  (interactive "P")
  (if prefix
      ;; still works if cursor is on a blank line below
      (pp-eval-last-sexp t)
    (call-interactively (lookup-key (current-global-map) (kbd "C-j")))))

(define-key paredit-mode-map (kbd "C-j") #'my-lisp-ctrl-j)
(define-key paredit-mode-map (kbd "M-s") nil)
(define-key paredit-mode-map (kbd "M-s s") 'paredit-splice-sexp)
(define-key paredit-mode-map (kbd "M-s r") 'paredit-raise-sexp)
(define-key paredit-mode-map (kbd "M-r") nil)

(defun my-lisp-common-hook ()
  (enable-paredit-mode)
  (setq-local evil-move-beyond-eol t)
  (setq-local evil-symbol-word-search t))

(add-hook 'emacs-lisp-mode-hook       'my-lisp-common-hook 'append)
(add-hook 'lisp-mode-hook             'my-lisp-common-hook 'append)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-common-hook 'append)
(add-hook 'scheme-mode-hook           'my-lisp-common-hook 'append)

(advice-add 'paredit-comment-dwim :after 'my-advise-comment)

(defun my-reindent-lisp-defun ()
  (interactive)
  (save-excursion
    (evil-previous-open-paren 20)
    (indent-sexp)))
(define-key lisp-mode-shared-map (kbd "C-c C-q") 'my-reindent-lisp-defun)

(defun my-advise-paredit-wrap (&rest args)
  (when (evil-normal-state-p)
    (evil-insert 1)))

(advice-add 'paredit-wrap-round :after 'my-advise-paredit-wrap)
(advice-add 'paredit-meta-doublequote :after 'my-advise-paredit-wrap)

;; ----------------------------------------------------------------------------
;;| Python
;; ----------------------------------------------------------------------------

(defvar my-python-interp "python3")
(setq-default python-shell-interpreter my-python-interp)

(defun my-python-shell-mode-hook ()
  (toggle-truncate-lines 0)
  (fancy-dabbrev-mode -1))

(defun my-python-mode-hook ()
  (my-syntax-entry)
  (setq-local tab-width 4)
  (setq-local evil-shift-width 4))

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'inferior-python-mode-hook 'my-python-shell-mode-hook)

;; (with-eval-after-load 'python
;;   (define-key python-mode-map (kbd "C-c C-a") 'pyvenv-activate))

(defun my-python-send-region ()
  (interactive)
  (if (use-region-p)
      (call-interactively 'python-shell-send-region)
    (message "No region active")))

(evil-leader/set-key-for-mode 'python-mode "." 'my-python-send-region)

(evil-set-initial-state 'inferior-python-mode 'emacs)

(add-to-list 'auto-mode-alist '("/SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("/SConscript\\'" . python-mode))

;; ----------------------------------------------------------------------------
;;| Cpp
;; ----------------------------------------------------------------------------

(defun my-wrap-if-endif (x &optional else)
  "Wrap the paragraph or region in #if x #endif,
    or #if x #else #endif if else is non-nil"
  (let (;; (is-blank-line (lambda ()
	;;               (not (string-match-p "." (thing-at-point 'line)))))
	(the-indent)
	(endpos)
	(prefix (lambda (c)
		  (newline)
		  (forward-line -1)
		  (indent-to c)
		  (insert (concat "#if " (number-to-string x)))
		  (when else
		    (insert "\n")
		    (indent-to c)
		    (setq endpos (point))
		    (insert "#else")))))

    (if (use-region-p)
	(let ((b (min (region-beginning) (region-end)))
	      (e (max (region-beginning) (region-end))))
	  (save-excursion
	    (goto-char b)
	    (setq the-indent (current-indentation)))
	  (save-excursion               ;; suffix
	    (goto-char e)
	    (newline)
	    (forward-line -1)
	    (indent-to the-indent)
	    (insert "#endif"))
	  (save-excursion               ;; prefix
	    (goto-char b)
	    (funcall prefix the-indent)))

      ;; no region, use paragraph
      (save-excursion                   ;; prefix
	(backward-paragraph)
	(forward-line)
	(setq the-indent (current-indentation))
	(funcall prefix the-indent))
      (save-excursion                   ;; suffix
	(forward-paragraph)
	(indent-to the-indent)
	(move-end-of-line 1)
	(insert "#endif\n")))

    (when endpos
      (goto-char endpos)
      (evil-open-above 1)
      (indent-to the-indent))))

(defun my-jump-to-header (&optional open-in-other-window)
  "Jump between C/C++ source file and corresponding header"
  (interactive "P")
  (let ((fn (buffer-file-name)))
    (if (not fn)
	(message "Buffer has no filename")
      (let* ((ext (file-name-extension fn))
	     (regex (concat "^" (file-name-base fn) "\\." (if (string= ext "h") "c\\(c\\|pp\\)?" "h") "$"))
	     (dir (my-find-project-root))
	     (files (directory-files-recursively dir regex nil 'my-dir-predicate))
	     (len (length files)))
	(if (= 0 len)
	    (message (format "No files found under \"%s\" matching \"%s\"" dir regex))
	  (let ((func (lambda ()
			(find-file (if (= 1 len)
				       (car files)
				     (completing-read "File: " files nil t))))))
	    (if open-in-other-window
		(let* ((switch-to-buffer-obey-display-actions t)
		       (display-buffer-overriding-action '((display-buffer-pop-up-window)
							   (inhibit-same-window . t))))
		  (funcall func))
	      (funcall func))
	    (message (buffer-file-name))))))))

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Customizing-C-and-C_002b_002b-indentation.html
(c-add-style "my-c-style"
	     '("linux"
	       (c-offsets-alist
		;; when a closing bracket is on it's own line don't
		;; indent more
		(arglist-close . 0)
		;; stop indenting function inside namespace
		(innamespace . 0)
		(topmost-intro . 0))))

(setq c-default-style "my-c-style")
(setq c-tab-always-indent nil)

(defvar my-cc-path)

;;; Allow my-find-file-at-point to find Unreal headers
(when (eq system-type 'windows-nt)
  (message "Finding Unreal C++ paths...")
  (setq ffap-file-name-with-spaces t)
  (setq-default my-cc-path
		(split-string
		 (string-trim
		  (shell-command-to-string
		   (concat "find \"C:/Program Files/Epic Games/UE_5.4/Engine\" "
			   "-type d \\( -name Plugins -o -name ThirdParty \\) -prune -false "
			   "-o \\( -name Private -o -name Public -o -name Classes \\)")))
		 "\n")))

(defun my-cc-settings (path)
  (modify-syntax-entry ?_ "w")
  (evil-local-set-key 'normal (kbd "[#") 'c-up-conditional)

  (hide-ifdef-mode 1)
  (add-hook 'after-save-hook 'hide-ifdefs 'append 'local)

  (make-local-variable path)
  (when (bound-and-true-p my-cc-path)
    (dolist (x my-cc-path)
      (add-to-list path x)))
  (vc-refresh-state)
  (add-to-list path (my-find-project-root))

  (when (and (not tags-table-list)
	     (let ((fn (buffer-file-name))) ; file under home dir?
	       (or (not fn)
		   (string-prefix-p (expand-file-name "~/")
				    (expand-file-name fn)))))
    (my-find-tags-files)))

(with-eval-after-load "hideif"
  ;; don't hide #ifdef FOO ... #endif, where FOO is undefined
  (setq hif-undefined-symbol t))

(require 'ffap)

(defun my-cpp-mode-hook ()
  (my-cc-settings 'ffap-c++-path))

(defun my-c-mode-hook ()
  (my-cc-settings 'ffap-c-path))

(add-hook 'c++-mode-hook 'my-cpp-mode-hook t)
(add-hook 'c-mode-hook 'my-c-mode-hook t)

(dolist (mode '(c++-mode c-mode))
  (evil-leader/set-key-for-mode mode
    "0" (lambda () (interactive) (my-wrap-if-endif 0))
    "1" (lambda () (interactive) (my-wrap-if-endif 1))
    "3" (lambda () (interactive) (my-wrap-if-endif 1))
    "2" (lambda () (interactive) (my-wrap-if-endif 1 t))))

(define-skeleton my-cpp-include "" nil
  "#include \"" - "\"")

(define-skeleton my-cpp-include-sys "" nil
  "#include <" - ">")

(define-skeleton my-cpp-for "" nil
  > "for ("
  (skeleton-read "Type: " "int") " "
  (setq v1 (skeleton-read "Variable: " "")) "=0; "
  v1 "<"
  (skeleton-read "End: " "") "; ++" v1 ") {\n"
  > - "\n"
  > -1 "}\n")

(define-skeleton my-cpp-for-iter "" nil
  > "for (\t"
  (skeleton-read "Container Type: " "std::vector<int>") "::"
  (skeleton-read "Iterator: " "const_iterator") "\n"
  > -1 (setq v1 (skeleton-read "Variable: " "it")) "="
  (setq v2 (skeleton-read "Container: " "")) ".begin(); " v1 "!=" v2 ".end(); ++" v1 "\n"
  > -2 ") {\n"
  > - "\n"
  > -1 "}\n")

(define-skeleton my-cpp-print-vec "" "Variable: "
  "<< " str "[0] << \" \" <<\n"
  > str "[1] << \" \" <<\n"
  > str "[2] << \" \" <<" -)

(define-skeleton my-cpp-include-guard "" nil
  "#ifndef INCLUDED_"
  (upcase (setq v1 (skeleton-read "Namespace: ")))
  "_"
  (setq v2 (upcase
	    (let ((name (buffer-file-name)))
	      (if name
		  (file-name-nondirectory
		   (file-name-sans-extension name))
		(skeleton-read "Name: "))))) "_H\n"
  "#define INCLUDED_" (upcase v1) "_" v2 "_H\n\n"
  "namespace " v1 " {\n\n"
  -
  "\n\n} // end namespace\n\n"
  "#endif\n")

(define-skeleton my-cpp-main "" nil
  "#include <iostream>\n\n"
  "using std::cerr;\n\n"
  "int main(int argc, const char *argv[]) {\n"
  > - "\n"
  > "return 0;\n}\n")

(defun my-advise-c-defun-name-and-limits (name-limits)
  "Remove the namespace from c-display-defun-name"
  (nconc (list (my-cpp-identifier-without-namespace (car name-limits)))
	 (cdr name-limits)))

(defun my-kill-c-function-name ()
  "Kill the name of the current C/C++ function, without namespace"
  (interactive)
  (advice-add 'c-defun-name-and-limits :filter-return 'my-advise-c-defun-name-and-limits)
  (unwind-protect
      (c-display-defun-name t)
    (advice-remove 'c-defun-name-and-limits 'my-advise-c-defun-name-and-limits)))

(with-eval-after-load "cc-mode"
  (define-key c-mode-base-map (kbd "C-c C-b") nil) ; don't want c-submit-bug-report
  (define-key c-mode-base-map (kbd "C-c C-i") #'my-jump-to-header)
  (define-key c-mode-base-map (kbd "C-c C-f") 'my-kill-c-function-name)

  (dolist (table (list c-mode-abbrev-table c++-mode-abbrev-table))
    (define-abbrev table "in"   "" 'my-cpp-include)
    (define-abbrev table "inc"  "" 'my-cpp-include-sys)
    (define-abbrev table "for"  "" 'my-cpp-for)
    (define-abbrev table "fori" "" 'my-cpp-for-iter)
    (define-abbrev table "pv"   "" 'my-cpp-print-vec)
    (define-abbrev table "main" "" 'my-cpp-main)))

(auto-insert-mode 1)
(define-auto-insert "\\.h\\'" 'my-cpp-include-guard)

;; ----------------------------------------------------------------------------
;;| Maya, Houdini, Arnold
;; ----------------------------------------------------------------------------

;; maya mel
(if (file-directory-p "~/dev/ermine/emacs")
  (progn
    (add-to-list 'load-path "~/dev/ermine/emacs")
    (require 'ermine))
  (add-to-list 'auto-mode-alist '("\\.mel\\'" . c-mode)))

;; houdini vex
(add-to-list 'auto-mode-alist '("\\.vex\\'" . c-mode))

;;; arnold
(defun my-arnold-settings ()
  (set (make-local-variable 'comment-start) "#"))
(add-to-list 'auto-mode-alist '("\\.ass\\'" . my-arnold-settings))

;;; USD
(when (load "~/.emacs.d/lisp/usda-syntax.el" t)
  (add-to-list 'auto-mode-alist '("\\.usd\\'" . usda-mode))
  (add-to-list 'auto-mode-alist '("\\.usda\\'" . usda-mode)))

;; ----------------------------------------------------------------------------
;;| Debug, gdb
;; ----------------------------------------------------------------------------

(defun my-gdb-mode-hook ()
  (define-key gud-mode-map (kbd "C-c C-r") 'comint-show-output)
  (define-key gud-mode-map (kbd "C-c C-p") 'comint-previous-prompt)
  (define-key gud-mode-map (kbd "C-c C-n") 'comint-next-prompt)
  (define-key gud-mode-map (kbd "C-r") 'comint-history-isearch-backward)
  (define-key gud-mode-map (kbd "C-c C-j") 'gud-down)
  (define-key gud-mode-map (kbd "C-c C-k") 'gud-up)
  (define-key gud-mode-map (kbd "C-c C-u") 'comint-kill-input))

(add-hook 'gdb-mode-hook 'my-gdb-mode-hook)

(setq gdb-many-windows nil)

;; TODO override gdb-setup-windows instead

;;; https://stackoverflow.com/a/41326527
(defun my-set-gdb-layout(&optional c-buffer)

  (if (not c-buffer)
      (setq c-buffer (window-buffer (selected-window)))) ;; save current buffer

  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)

  (let* ((w-source (selected-window))						     ;; left top
	 (w-gdb (split-window w-source nil 'right))				     ;; right bottom
	 (w-locals (split-window w-gdb nil 'above))				     ;; right middle bottom
	 (w-stack (split-window w-locals nil 'above))				     ;; right middle top
	 ;; (w-breakpoints (split-window w-stack nil 'above))			     ;; right top
	 (w-io (split-window w-source (floor (* 0.5 (window-body-height))) 'below))  ;; left bottom
	 )

    (set-window-buffer w-io (or (first (match-buffers "^\\*input/output of .*\\*$"))
				(get-buffer "*compilation*")
				(get-buffer "")
				(gdb-get-buffer-create 'gdb-inferior-io)))
    ;; (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-buffer w-gdb gud-comint-buffer)

    (set-window-dedicated-p w-io t)
    ;; (set-window-dedicated-p w-breakpoints t)
    (set-window-dedicated-p w-locals t)
    (set-window-dedicated-p w-stack t)
    (set-window-dedicated-p w-gdb t)
    (set-window-dedicated-p w-source nil)

    (select-window w-source)
    (set-window-buffer w-source c-buffer)))

(defadvice gdb (around args activate)
  (setq global-config-editing (current-window-configuration)) ;; to restore: (set-window-configuration c-editing)
  (let ((c-buffer (window-buffer (selected-window))) ;; save current buffer
        )
    ad-do-it
    (my-set-gdb-layout c-buffer)))
(defadvice gdb-reset (around args activate)
  ad-do-it
  (set-window-configuration global-config-editing))

;; ----------------------------------------------------------------------------
;;| Font
;; ----------------------------------------------------------------------------

(defun my-font-config ()
  (interactive)

  (when (display-graphic-p)

    (pcase system-type
      ('gnu/linux

       ;; install to ~/.fonts/  then fc-cache -v ~/.fonts

       (set-face-attribute 'default nil :font "Menlo:pixelsize=14:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")

       (pcase (system-name)
	 ("goose"
	  (if (string= "1920x1080" (string-trim
				    (shell-command-to-string
				     "xrandr | grep '\\*' | awk '{print $1}'")))
	      (set-face-attribute 'default nil :height 110) ; external monitor
	    (set-face-attribute 'default nil :height 130))) ; laptop screen

	 ("hedgehog"
	  (if (string-empty-p (string-trim
			       (shell-command-to-string
				"xrandr | awk '$2 == \"connected\" && $1 ~ /^(HDMI-|DP-)/ {print $1}'")))
	      (set-face-attribute 'default nil :height 120)   ; laptop screen
	    (set-face-attribute 'default nil :height 105))))) ; external monitor

      ('windows-nt
       ;; (set-frame-font "Fira Mono 11" nil t)
       (set-frame-font "JetBrains Mono 11" nil t))

      ('darwin
       (set-face-attribute 'default nil :family "Menlo" :height 160)))))

;; ----------------------------------------------------------------------------
;;| Colour theme
;; ----------------------------------------------------------------------------

(require 'font-lock)
(global-font-lock-mode t)

(blink-cursor-mode -1)
(setq-default cursor-type 'box)

(defun my-set-dark-mode (val)
  (when (display-graphic-p)
    (let ((inhibit-message t))
      (cond
       ((and (eq system-type 'gnu/linux)
	     (string= "ubuntu:GNOME" (getenv "XDG_CURRENT_DESKTOP")))
	(shell-command (format "gsettings set org.gnome.desktop.interface color-scheme 'prefer-%s'" (if val "dark" "light")))
	(shell-command (format "~/dotfiles-public/bin/term_theme.sh %s" (if val "dark" "light"))))
       ((eq system-type 'darwin)
	(shell-command (format "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to %s'" (if val "true" "false"))))))))

(defun my-theme-dark (x)
  (mapcar #'disable-theme custom-enabled-themes)
  (cond
   ((= x 0)
    (require 'reykjavik-theme)
    (load-theme 'reykjavik)
    (load-theme 'my-override-dark))
   ((= x 1)
    (require 'nordic-night-theme)
    (load-theme 'nordic-night)
    (load-theme 'my-override-dark3))
   ((= x 2)
    (load-theme 'ef-winter)
    (load-theme 'my-override-dark2))
   ((= x 3)
    (load-theme 'ef-autumn)
    (load-theme 'my-override-dark2)))

  (setq my-pulse-face 'next-error)
  (set-cursor-color "white")
  (setq evil-normal-state-cursor '(box "white"))
  (setq evil-insert-state-cursor '(box "orange"))

  (my-set-dark-mode t))

(defun my-theme-light (x)
  (mapcar #'disable-theme custom-enabled-themes)
  (cond
   ((= x 0)
    (require 'soft-morning-theme)
    (load-theme 'soft-morning)
    (load-theme 'my-override-light))
   ((= x 1)
    (require 'sandcastle-theme)
    (load-theme 'sandcastle))
   ((= x 2)
    (load-theme 'ef-cyprus)
    ;; (load-theme 'my-override-light)
    ))

  (setq my-pulse-face 'next-error)
  (set-cursor-color "black")
  (setq evil-normal-state-cursor '(box "black"))
  (setq evil-insert-state-cursor '(box "orange"))

  (my-set-dark-mode nil))

(defvar my-alpha 90)

(defun my-toggle-alpha-background ()
  (interactive)
  (set-frame-parameter nil 'alpha-background
		       (if (frame-parameter nil 'alpha-background)
			   nil my-alpha)))

(defhydra my-theme-hydra ()
  "Theme"
  ("y" (lambda () (interactive) (my-theme-dark 2)) "ef-winter")
  ("u" (lambda () (interactive) (my-theme-light 2)) "ef-cyprus")
  ("i" (lambda () (interactive)))
  ("o" (lambda () (interactive) (my-theme-dark 0)) "reykjavik")
  ("p" (lambda () (interactive) (my-theme-dark 3)) "ef-autumn")
  ("t" #'my-toggle-alpha-background "toggle alpha-background")
  ("0" (lambda () (interactive) (set-frame-parameter nil 'alpha-background 0)) "transparent")
  ("9" (lambda () (interactive) (set-frame-parameter nil 'alpha-background my-alpha)) "blend"))

(global-set-key (kbd "C-c i") 'my-theme-hydra/body)

;; ----------------------------------------------------------------------------
;;| Window setup
;; ----------------------------------------------------------------------------

(defun my-window-setup-hook ()

  (unless (display-graphic-p)
    ;; see terminal background colour/image
    (set-face-background 'default "unspecified-bg" (selected-frame)))

  (let* ((time (decode-time))
	 (month (decoded-time-month time))
	 (hour (decoded-time-hour time)))
    (cond
     ((and (> hour 7) (< hour 16))
      (my-theme-light 2))
     ((and (>= month 9) (<= month 11))
      (my-theme-dark 3))
     ((or (= month 12) (<= month 2))
      (my-theme-dark 2))
     (t (my-theme-dark 0))))

  (my-font-config)

  (when (and (eq system-type 'gnu/linux)
	     (string= (getenv "XDG_SESSION_TYPE") "wayland"))
    ;; hide window title bar. wayland only, breaks under X11 gnome.
    (set-frame-parameter nil 'undecorated t)
    ;; allow mouse resize at edges
    ;; (set-frame-parameter nil 'drag-internal-border 1)
    ;; (set-frame-parameter nil 'internal-border-width 5)
    )

  (when (string= "goose" (system-name))
    (my-toggle-alpha-background)))

(add-hook 'window-setup-hook 'my-window-setup-hook)

;; ----------------------------------------------------------------------------
;;| Music
;; ----------------------------------------------------------------------------

(when (and (eq system-type 'gnu/linux)
	   (file-exists-p "~/mp3"))

  (when (require 'emms nil t)

    (emms-all)
    (setq emms-player-list '(emms-player-mpv)
	  emms-info-functions '(emms-info-native))
    ;; Remove leading space so buffer is not hidden
    (setq emms-playlist-buffer-name "*EMMS Playlist*")

    (define-key emms-browser-mode-map (kbd "SPC") evil-leader--default-map)
    (define-key emms-browser-mode-map (kbd "<tab>") #'emms-browser-toggle-subitems-recursively)
    (define-key emms-browser-mode-map (kbd ";") #'emms-browser-move-up-level)
    (define-key emms-browser-mode-map (kbd "C-j") nil)
    (define-key emms-browser-mode-map (kbd "j") #'next-line)
    (define-key emms-browser-mode-map (kbd "k") #'previous-line)
    (define-key emms-playlist-mode-map (kbd "M-n") nil)
    (define-key emms-playlist-mode-map (kbd "M-p") nil)
    (define-key emms-playlist-mode-map (kbd "C-j") nil)
    (define-key emms-playlist-mode-map (kbd "SPC") evil-leader--default-map)
    (define-key emms-playlist-mode-map ";" #'emms-playlist-mode-center-current)
    (define-key emms-playlist-mode-map "c" #'emms-pause)
    (define-key emms-playlist-mode-map "j" #'next-line)
    (define-key emms-playlist-mode-map "k" #'previous-line)
    (define-key emms-playlist-mode-map "h" (lambda ()
					     (interactive)
					     (let ((emms-seek-seconds 60))
					       (emms-seek-backward))))
    (define-key emms-playlist-mode-map "l" (lambda ()
					     (interactive)
					     (let ((emms-seek-seconds 60))
					       (emms-seek-forward))))

    (global-set-key (kbd "<f8>") #'emms-playlist-mode-go)
    (global-set-key (kbd "<f9>") #'emms-add-playlist))

  (defun my-add-dired-to-playlist ()
    (interactive)
    (when (get-buffer emms-playlist-buffer-name)
	(emms-add-dired)))

  (define-key dired-mode-map (kbd "b") 'my-add-dired-to-playlist))

;; ----------------------------------------------------------------------------
;;| Non-public
;; ----------------------------------------------------------------------------

(load "~/dotfiles/init.el" t)
