
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
 '(auto-save-default nil)
 '(calendar-date-style 'european)
 '(calendar-week-start-day 1)
 '(comint-prompt-read-only t)
 '(compilation-ask-about-save nil)
 '(compilation-scroll-output t)
 '(compilation-skip-threshold 2)
 '(compile-command "make ")
 '(compose-mail-user-agent-warnings nil)
 '(consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000  --smart-case --no-heading --with-filename --line-number --no-search-zip --hidden -g !{.git,.svn,.hg}/ -g !TAGS -g !build/ --no-ignore")
 '(custom-safe-themes
   '("0ef72d410faac2f0bd2a76dfd3e7595d024daeeaccd3eb6c32951dcb0a209819" "699bcf84060181ce3fe52e84c083632ced5f3c0d6370ba5a755f94fe86ea7b41" "29a073e66535bad18e11e9bcaa17d7f2d17e4c79f01023e59e9841633915c232" "fb7595c9571f2bd41635745d12551f35322296b70330056ddd0020ab2374671c" "d0dc7861b33d68caa92287d39cf8e8d9bc3764ec9c76bdb8072e87d90546c8a3" "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79" "b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9" "601a9b9bf21f5c72ddfb28c7e95b842a5b0130f55ad5e0b97d2ba1e0b91b0a2c" "7776ba149258df15039b1f0aba4b180d95069b2589bc7d6570a833f05fdf7b6d" "e17d91a99e14fc72f71f531f07d3dff44238c69f599998b50e95e67b589d8fa1" "a6e8bcffe4d8cac7463c5a7c67c0908316cc616da3816d3ce35c325d5e02fd97" "adfe1d522a4a100edade12797079ebbabf742a48cf098e7d10ea14012e156ee8" "7342266ffff707cc104313c9153342e44a47a9f22ed7157e4893aac74091ad27" "aa688776604bbddbaba9e0c0d77e8eb5f88d94308f223d1962b6e6b902add6a0" default))
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
 '(gdb-debuginfod-enable-setting nil)
 '(gdb-many-windows t)
 '(gdb-restore-window-configuration-after-quit t)
 '(grep-find-ignored-directories '(".svn" ".git" ".hg"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.so" "*.a" "*.elc" "*.lib" "*.lo" "*.la" "*.pyc" "*.pyo" "TAGS"))
 '(helm-candidate-number-limit 10000)
 '(helm-follow-mode-persistent t)
 '(helm-move-to-line-cycle-in-source nil)
 '(helm-source-names-using-follow '("Imenu" "Helm occur"))
 '(hide-ifdef-initially t)
 '(hide-ifdef-shadow t)
 '(history-delete-duplicates t)
 '(history-length 1000)
 '(ibuffer-project-root-functions '((ibuffer-project-project-root . "Project")))
 '(image-dired-dir "/tmp/image-dired")
 '(image-dired-thumb-height 100)
 '(image-dired-thumb-width 100)
 '(inhibit-startup-screen t)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(initial-scratch-message nil)
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
 '(message-auto-save-directory nil)
 '(next-error-recenter '(4))
 '(olivetti-body-width 120)
 '(org-agenda-file-regexp "\\`[^.].*\\.org\\(\\.gpg\\)?\\'")
 '(org-agenda-files '("~/" "~/org"))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-search-view-always-boolean t)
 '(org-agenda-show-future-repeats nil)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-span 3)
 '(org-agenda-start-on-weekday nil)
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item . auto)))
 '(org-confirm-babel-evaluate nil)
 '(org-deadline-warning-days 7)
 '(org-directory "~/org")
 '(org-fold-catch-invisible-edits 'smart)
 '(org-html-validation-link nil)
 '(org-imenu-depth 3)
 '(org-insert-heading-respect-content t)
 '(org-log-done 'time)
 '(org-modules '(ol-docview org-habit ol-info))
 '(org-publish-use-timestamps-flag nil)
 '(org-refile-targets '((org-agenda-files :maxlevel . 3) (nil :maxlevel . 3)))
 '(org-src-fontify-natively t)
 '(org-startup-folded nil)
 '(org-startup-indented t)
 '(org-table-automatic-realign nil)
 '(org-todo-keywords
   '((sequence "TODO(t)" "PROGRESS(p)" "WAIT(w@/@)" "BLOCK(b@/@)" "|" "DONE(d!/!)" "CANCELLED(c@/@)")))
 '(org-use-fast-todo-selection 'expert)
 '(package-selected-packages
   '(ace-window calfw calfw-org cape cmake-mode consult ef-themes elfeed embark embark-consult emms evil evil-leader evil-collection evil-numbers fancy-dabbrev gnuplot helm hydra ibuffer-project ledger-mode magit marginalia markdown-mode nordic-night-theme olivetti orderless ox-pandoc paredit reykjavik-theme soft-morning-theme tempel undo-tree vertico wgrep which-key yaml-mode))
 '(package-vc-selected-packages
   '((sandcastle-theme :vc-backend Git :url "https://github.com/habamax/sandcastle-theme")))
 '(project-vc-ignores '("./build/" "build/" ".#*" "*~" "*.elc" "*.pyc" "*.pyo"))
 '(read-buffer-completion-ignore-case t)
 '(read-quoted-char-radix 16)
 '(recentf-max-saved-items 1000)
 '(register-preview-delay 0.5)
 '(remote-file-name-inhibit-locks t)
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((org-archive-location . "::")
     (org-archive-location . "notes.org_archive.gpg::")
     (my-input-method . swedish-postfix)
     (my-input-method . german-postfix)
     (buffer-auto-save-file-name)
     (tab-always-indent)
     (indent-tabs-mode nil)
     (evil-shift-width . 2)
     (evil-shift-width . 4)))
 '(save-place-forget-unreadable-files nil)
 '(shell-input-autoexpand nil)
 '(shift-select-mode nil)
 '(show-paren-when-point-in-periphery t)
 '(show-paren-when-point-inside-paren t)
 '(show-trailing-whitespace nil)
 '(tags-case-fold-search nil)
 '(tempel-path "~/.emacs.d/templates/*.eld")
 '(tramp-histfile-override "/tmp/.tramp_history")
 '(tramp-ssh-controlmaster-options
   "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=60m" t)
 '(undo-tree-auto-save-history nil)
 '(use-short-answers t)
 '(vc-follow-symlinks t)
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

(defvar my-evil-emacs-state nil)
;; (when (string= "goose" (system-name))
;;   (setq my-evil-emacs-state t))

(defun my-evil-default (&optional force-emacs)
  (evil-local-mode 1)
  (when (or (eq force-emacs 'emacs) my-evil-emacs-state)
    (evil-emacs-state)))


(setq evil-want-integration t
      evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(require 'evil-leader)
(require 'evil-numbers)

(global-evil-leader-mode)

(evil-esc-mode 1)			; make C-[ escape
(evil-global-set-key 'insert     (kbd "C-c <escape>") 'evil-normal-state)
(evil-global-set-key 'insert     (kbd "C-c SPC") 'ignore)
;; (when (display-graphic-p)
;;   (evil-global-set-key 'insert   (kbd "C-;") 'evil-normal-state)
;;   (evil-global-set-key 'replace  (kbd "C-;") 'evil-normal-state)
;;   (evil-global-set-key 'operator (kbd "C-;") 'evil-force-normal-state)
;;   (evil-global-set-key 'visual   (kbd "C-;") 'evil-exit-visual-state)
;;   (evil-global-set-key 'normal   (kbd "C-;") 'ignore))
;; (evil-global-set-key 'insert (kbd "TAB") 'evil-normal-state)
;; (evil-global-set-key 'insert (kbd "C-SPC") 'evil-normal-state)

(global-set-key (kbd "<f7>") 'evil-local-mode)
(add-hook 'evil-command-window-mode-hook 'evil-local-mode)

(setq-default evil-ex-search-case 'sensitive)
(setq-default evil-search-module 'evil-search)

(when (not my-evil-emacs-state)
  (setq evil-emacs-state-tag (propertize "<E>" 'face '((:foreground "#000000" :background "goldenrod")))))

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "w" 'save-buffer)

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
	     (save-excursion
	       (forward-char)
	       (eolp)))
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

;; (evil-global-set-key 'normal (kbd "C-r") nil)  ; fall through to isearch-backward
;; (evil-global-set-key 'normal (kbd "C-S-r") 'evil-redo)

;; ----------------------------------------------------------------------------
;;| Undo
;; ----------------------------------------------------------------------------

(require 'undo-tree)
(global-undo-tree-mode)

(setq amalgamating-undo-limit 4)

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

  (when (and (not (eq major-mode 'image-mode))
	     (not evil-local-mode))
    (my-evil-default))
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
      (call-interactively 'ffap))
    (when-let ((file (buffer-file-name)))
      (princ file))))

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

(defun my-insert-enter-hook ()
  ;; update: is this breaking esc sometimes?
  ;; temporarily disable update of syntax highlighting while in insert mode,
  ;; as a workaround for typing becoming slow in some C++ buffers
  ;; (when (derived-mode-p 'prog-mode)
  ;;   (unwind-protect
  ;; 	(jit-lock-mode nil)))
  (show-paren-mode -1))
(defun my-insert-exit-hook ()
  (show-paren-mode 1)
  ;; unwind-protect workaround, sometimes get locked in insert mode
  ;; (unwind-protect
  ;;     (jit-lock-mode t))
  )
(add-hook 'evil-insert-state-entry-hook 'my-insert-enter-hook)
(add-hook 'evil-insert-state-exit-hook 'my-insert-exit-hook)

(setq next-error-highlight-no-select t) ;; leave highlight for occur

(setq-default backup-inhibited t)    ;; disable backup

(recentf-mode 1)
(save-place-mode 1)
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
   ((string= "\n" (thing-at-point 'line t))
    (delete-blank-lines))
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

(defun my-sudo-find-file (filename)
  "Open a file as root."
  (interactive "FSudo Find file: ")
  (let ((fn (concat "/sudo:root@localhost:" (expand-file-name filename))))
    (find-file fn)))

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

(defun my-mark-until-whitespace ()
  "Select until the next whitespace char"
  (interactive)
  (when (looking-at "[^[:space:]\n]+")
    (push-mark (match-end 0) nil t)))

(defun my-open-line-above ()
  (interactive)
  (beginning-of-line nil)
  (newline) ;; (newline-and-indent)
  (forward-line -1)
  ;; (indent-according-to-mode)
  )

(defun my-open-line-below (&optional arg)
  (interactive "*p")
  (move-end-of-line nil)
  (newline-and-indent arg))

(defun my-open-line (&optional prefix)
  (interactive "P")
  (if prefix
      (my-open-line-above)
    (my-open-line-below)))

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

(defun my-zap-up-to-char (prefix char)
  "Zap up to char that works in minibuffer"
  (interactive "P\nc")
  (let ((count (cond ((null prefix) 1)
		     ((symbolp prefix) -1)
		     (t prefix))))
    (zap-up-to-char count char)))

(defun my-kill-in-quotes (&optional mark)
  (interactive "P")
  (push-mark)
  (let* ((r (evil-inner-double-quote))
	 (beg (car r))
	 (end (cadr r)))
    (goto-char beg)
    (if mark
	(push-mark end nil t)
      (kill-region beg end)))
  (when (evil-normal-state-p)
    (evil-insert-state)))

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

(defun my-clear-buffer ()
  "Delete all the text in the buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))))

(defun my-advise-emacs-kill (&rest args)
  "Enter insert mode after a kill command"
  (when (and evil-local-mode (evil-normal-state-p))
    (evil-insert-state)))

(dolist (cmd '(kill-word
	       kill-line
	       kill-sentence
	       kill-sexp
	       paredit-kill
	       org-meta-return
	       org-insert-todo-heading))
  (advice-add cmd :after #'my-advise-emacs-kill))

(defun my-join-lines ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'delete-indentation)
    (delete-indentation t)))

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

(global-set-key (kbd "C-c d") #'pwd)
(global-set-key (kbd "C-c c") #'my-copy-filename)
(global-set-key (kbd "C-c n") #'toggle-truncate-lines)
(global-set-key (kbd "C-c w") 'evil-window-map)
(global-set-key (kbd "C-c m") #'my-mirror-buffer)
(global-set-key (kbd "C-c q") #'my-kill-in-quotes)
(global-set-key (kbd "C-x C-z") nil)	; no suspend-frame

(define-key minibuffer-local-map (kbd "M-z") 'my-zap-up-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-=") 'winner-undo)
(global-set-key (kbd "M-+") 'winner-redo)
(global-set-key (kbd "M-\\") #'my-delete-whitespace)
(global-set-key (kbd "M-#") #'my-mark-until-whitespace)
(evil-global-set-key 'insert (kbd "M-#") (lambda () (interactive) (insert "£")))
(global-set-key (kbd "M-u") #'upcase-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-s ,") 'my-isearch-symbol-backward)
(global-set-key (kbd "M-s M-,") 'my-isearch-symbol-backward)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-p") #'evil-scroll-up)
(global-set-key (kbd "M-n") #'evil-scroll-down)
(evil-global-set-key 'insert (kbd "M-i") 'tab-to-tab-stop)
(global-set-key (kbd "M-i") #'evil-scroll-line-up)
(global-set-key (kbd "M-j") #'evil-scroll-line-down)
(global-set-key (kbd "M-[") (kbd "M-{"))
(global-set-key (kbd "M-]") (kbd "M-}"))
(global-set-key (kbd "M-SPC") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "C-M-;") #'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-M-'") #'evil-numbers/dec-at-pt)

(global-set-key (kbd "C-M-o") #'mode-line-other-buffer)
(global-set-key (kbd "C-M-y") #'my-duplicate-line)
(global-set-key (kbd "C-c C-j") #'my-join-lines)
;; (global-set-key (kbd "C-M-o") #'split-line)
(global-set-key (kbd "C-o") #'my-open-line-above)
(global-set-key (kbd "C-=") #'my-close-other-window)
(global-set-key (kbd "C-;") #'goto-last-change)

(when (display-graphic-p)
 (global-set-key (kbd "C-<backspace>") #'my-delete-to-indent))

(push 'try-expand-line hippie-expand-try-functions-list)
(global-set-key (kbd "C-x C-l") 'hippie-expand) ;; line completion like vim

(global-set-key (kbd "C-h h") nil)
(global-set-key (kbd "C-h C-c") nil)
(global-set-key (kbd "C-h RET") 'man)
(global-set-key (kbd "C-x !") 'delete-other-windows-vertically)
(global-set-key (kbd "C-x f") 'find-file)
(global-set-key (kbd "C-x g") 'subword-mode)
(global-set-key (kbd "C-x l") 'count-words-region)
(global-set-key (kbd "C-c M-f") #'flyspell-buffer)
(global-set-key (kbd "C-c M-s") #'ispell)

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

;;; stop accidentally moving out of the minibuffer
;; (define-key minibuffer-local-map (kbd "M-o") (lambda () (interactive)))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

 The generic `keyboard-quit' does not do the expected thing when
 the minibuffer is open.  Whereas we want it to close the
 minibuffer, even without explicitly focusing it.

 The DWIM behaviour of this command is as follows:

 - When the region is active, disable it.
 - When a minibuffer is open, but not focused, close the minibuffer.
 - When the Completions buffer is selected, close it.
 - In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; ----------------------------------------------------------------------------
;;| Olivetti
;; ----------------------------------------------------------------------------

(defvar my-olivetti-state nil)

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
      pulse-iterations 10)

(defun my-pulse-line (&rest r)
  (when (and (boundp 'my-pulse-face)
	     (not global-hl-line-mode))
   (pulse-momentary-highlight-one-line (point) my-pulse-face)))

(dolist (cmd '(bookmark-jump tab-new tab-close tab-next
	       other-window ace-window delete-window kill-this-buffer
	       quit-window org-agenda-quit magit-mode-bury-buffer
	       winner-undo winner-redo))
  (advice-add cmd :after 'my-pulse-line))

;; ----------------------------------------------------------------------------
;;| Help
;; ----------------------------------------------------------------------------

(require 'which-key)
(which-key-mode 1)

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
  (define-key Man-mode-map (kbd "M-n") nil)

  (defun my-man-mode-hook ()
    (evil-local-mode 1))

  (add-hook 'Man-mode-hook 'my-man-mode-hook))

(push '("\\(\\*[Hh]elp\\*\\)\\|\\(\\*Man\\)"
	(display-buffer-reuse-mode-window
	 display-buffer-use-some-window
	 display-buffer-in-direction)
	(direction . right)
	;; don't want this as jumping back/forwards opens a second help window
	;; (inhibit-same-window . t)
	)
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
;;| Tempel
;; ----------------------------------------------------------------------------

(require 'tempel)

(global-set-key (kbd "M-'") 'tempel-complete)

(define-key tempel-map (kbd "RET") 'tempel-next)

;; ----------------------------------------------------------------------------
;;| Prog
;; ----------------------------------------------------------------------------

(defun my-prog-mode-hook ()
  (abbrev-mode -1)
  (my-evil-default)
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
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (if (string-match-p "\\s-" text)	; wrap in quotes if there's whitespace
	  (format "\"%s\"" text)
	text)))
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

(defvar my-machine (or (string= "goose" (system-name))
		       (eq system-type 'darwin)))

(when my-machine
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
(setq scroll-up-aggressively 0.0
      scroll-down-aggressively 0.0
      scroll-step 1
      auto-hscroll-mode t
      hscroll-margin 5
      hscroll-step 0)

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
  (my-evil-default)
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
  (my-evil-default)
  (when my-olivetti-state
    (olivetti-mode 1))
  ;; = is punctuation, so evil * works on key and val separately for key=val
  (modify-syntax-entry ?= ".")
  (setq-local tab-width 2)
  (setq-local completion-at-point-functions '(my-complete-word-ispell))
  (setq-local show-trailing-whitespace t)
  (setq-local evil-move-beyond-eol t)
  ;; stop paragraph lines after the first being extra indented by M-q
  (setq-local fill-paragraph-function nil))

(add-hook 'message-mode-hook 'my-message-mode-hook)

;; (push 'message-mode evil-emacs-state-modes)

;;; make auto fill work in message mode, given that we don't have headers and
;;; are just using the message buffer as scratch space
(defun my-advise-message-point-in-header-p () nil)
(advice-add 'message-point-in-header-p :override 'my-advise-message-point-in-header-p)

(with-eval-after-load "message"
  (define-key message-mode-map (kbd "TAB") nil)
  (define-key message-mode-map (kbd "C-M-i") #'complete-symbol)
  (define-key message-mode-map (kbd "C-c C-j") nil)
  (define-key message-mode-map (kbd "C-c C-c") nil)
  (define-key message-mode-map (kbd "C-c C-s") nil))

(global-set-key (kbd "C-x m") #'my-scratch-message-buffer)

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

(defun my-optional-file (fn)
  (if (file-exists-p fn) fn nil))

(setq org-default-notes-file
      (or (my-optional-file "~/notes.org.gpg")
	  (my-optional-file "~/org/work.org")))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))

(setq org-capture-templates
      `(("m" "Bookmark" entry (file+headline org-default-notes-file "Bookmarks")
	 "* %?\n")
	("x" "Task" entry (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n")))

(when my-machine
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
      `(("p" "In progress" todo "PROGRESS")
	("w" "Waiting" todo "WAIT|BLOCK")
	("d" "Done" todo "DONE|CANCELLED")
	;; ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
	("l" "Learn"
	 ((tags "read|watch|project"
		((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED")))
		 (org-agenda-sorting-strategy '(priority-down tag-up alpha-up))))))

	("." "Agenda"
	 ((agenda "")
	  (tags-todo "-read-watch-project-show"
		     ((org-agenda-overriding-header "Unscheduled:")
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
		      (org-agenda-sorting-strategy '(todo-state-up priority-down category-down tag-down))))
	  (tags "show" ((org-agenda-overriding-header "Shows:")
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "CANCELLED")))
			(org-agenda-sorting-strategy '(priority-down)))))

	 ((org-agenda-start-with-log-mode nil)
	  (org-tags-match-list-sublevels nil)))

	("r" "This week" agenda ""
	 ,(append my-org-agenda-common-review-settings
		  '((org-agenda-span 'week)
		    (org-agenda-overriding-header "Week in Review")))
	 ("/tmp/week.html"))

	("R" "Last week" agenda ""
	 ,(append my-org-agenda-common-review-settings
		  '((org-agenda-span 'week)
		    (org-agenda-start-day "-1w")
		    (org-agenda-overriding-header "Last week in Review")))
	 ("/tmp/lastweek.html"))))

(when my-evil-emacs-state
  (push 'org-mode evil-emacs-state-modes))

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
  (when (and (eq major-mode 'org-mode)
	     (evil-normal-state-p))
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

(advice-add 'org-time-stamp-inactive :before #'my-forward-before-insert)
(advice-add 'org-insert-last-stored-link :before #'my-forward-before-insert)
(advice-add 'org-insert-link :before #'my-forward-before-insert)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "C") 'calfworg)
  (define-key org-agenda-mode-map (kbd "C-w") 'evil-window-map))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-,") nil)
  (define-key org-mode-map (kbd "C-'") nil)
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "C-c C-j") nil)
  (define-key org-mode-map (kbd "C-c [") 'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c ]") nil)
  (define-key org-mode-map (kbd "C-c M-e") 'org-decrypt-entry)
  (define-key org-mode-map (kbd "M-[") 'org-backward-paragraph)
  (define-key org-mode-map (kbd "M-]") 'org-forward-paragraph)

  (when (eq system-type 'darwin)
    (setq org-babel-awk-command "gawk"))

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

(defun my-org-capture-task ()
  (interactive)
  (org-capture nil "x"))

(defun my-org-agenda ()
  (interactive)
  (org-agenda nil ".")
  (when my-machine
    ;; hide work tasks
    (org-agenda-filter-by-tag '(4) ?w)))

(defun my-advise-org-fill-paragraph (func &rest args)
  "When the region is active, revert to fill-paragraph behaviour."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (when current-prefix-arg 'full) t)))
  (if (region-active-p)
      (fill-paragraph (car args) t)
    (apply func args)))

(advice-add 'org-fill-paragraph :around 'my-advise-org-fill-paragraph)

(defun my-advise-org-exec-src-block (func &rest args)
  "Make raw results containing a table align correctly, since we
defaulted the setting off."
  (let ((org-table-automatic-realign t))
    (apply func args)))

(advice-add 'org-babel-execute-src-block :around 'my-advise-org-exec-src-block)


(push '("\\*Org Select\\*"
        (display-buffer-below-selected))
      display-buffer-alist)

(pcase system-type
  ('gnu/linux
   (global-set-key (kbd "C-`") 'my-org-capture-task))
  ('darwin
   (global-set-key (kbd "C-§") 'my-org-capture-task)))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c x") 'org-capture)
(global-set-key (kbd "C-c M-t") 'my-wrap-org-link)
(global-set-key (kbd "C-c C-x C-j") 'my-org-clock-jump)

(when (display-graphic-p)
  (evil-global-set-key 'normal (kbd "C-.") nil)
  (global-set-key (kbd "<XF86LaunchB>")	'my-org-agenda)
  (global-set-key (kbd "<LaunchB>")	'my-org-agenda))

;; ----------------------------------------------------------------------------
;;| Ledger
;; ----------------------------------------------------------------------------

(setq ledger-binary-path (expand-file-name "~/dotfiles-public/bin/ledger.sh"))

(with-eval-after-load "ledger-report"

  (defun my-ledger-hook ()
    (auto-fill-mode -1))
  (add-hook 'ledger-mode-hook 'my-ledger-hook)

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

(when (and (require 'calfw nil t)
	   (require 'calfw-org nil t))
  (setq cfw:display-calendar-holidays nil)
  (defalias 'calfworg 'cfw:open-org-calendar))

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

(defun my-calc ()
  (interactive)
  (calc nil calc-full-mode t))

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
(vertico-mode)

(define-key vertico-map (kbd "C-j") 'vertico-exit-input)
(define-key vertico-map (kbd "C-h f") (lambda ()
					(interactive)
					(describe-function (intern (vertico--candidate)))))
(define-key vertico-map (kbd "C-h v") (lambda ()
					(interactive)
					(describe-variable (intern (vertico--candidate)))))

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
;;| Consult
;; ----------------------------------------------------------------------------

(require 'consult)

(defun my-imenu ()
  (interactive)
  (let ((f (buffer-file-name)))
    (cond
     ((and f (file-equal-p f (my-init-file)))
      (let ((outline-regexp "^;;|"))
	(consult-outline)))
     ;; unlike consult-imenu, consult-org-heading doesn't expand/reveal folded
     ;; org headings.
     ;; ((eq major-mode 'org-mode) (consult-org-heading))
     (t (consult-imenu)))))

(defun my-ripgrep (dir other-window)
  (let* ((consult-preview-key 'any)
	 (consult--buffer-display (if other-window #'switch-to-buffer-other-window #'switch-to-buffer))
	 (initial (if-let ((sym (thing-at-point 'symbol t)))
		      (format "\\<%s\\>" sym)
		    nil)))
    (consult-ripgrep dir initial)))

(defun my-ripgrep-project (&optional prefix)
  "Run ripgrep across the current project. Prefix opens in other window."
  (interactive "P")
  (my-ripgrep nil prefix))

(defun my-ripgrep-dir (&optional prefix)
  "Run ripgrep under a given directory. Prefix opens in other window."
  (interactive "P")
  (my-ripgrep (read-directory-name "Directory: ") prefix))

(setq consult-preview-key nil)
(consult-customize consult-line :preview-key 'any)
(consult-customize consult-theme :preview-key "C-l")
(consult-customize my-imenu :preview-key 'any)

(define-key consult-async-map (kbd "M-w")
	    (lambda ()
	      (interactive)
	      (my-toggle-word-boundary "^#\\\\<\\(.*\\)\\\\>" "#\\<" "\\>"
					 "^#" "#")))

(evil-leader/set-key "r"   'my-ripgrep-project)
(evil-leader/set-key "i"   'my-imenu)
(global-set-key (kbd "M-s M-r") 'my-ripgrep-project)
(global-set-key (kbd "M-s r") 'my-ripgrep-dir)
(global-set-key (kbd "M-s i") 'my-imenu)

(global-set-key (kbd "C-c r") 'consult-recent-file)
(define-key minibuffer-local-map (kbd "M-r") 'consult-history)

;; ----------------------------------------------------------------------------
;;| Grep
;; ----------------------------------------------------------------------------

(require 'wgrep)

(setq wgrep-enable-key "e")

(defun my-grep-project ()
  (interactive)
  (rgrep (read-string "Search for: " (format "\\<%s\\>" (thing-at-point 'symbol t)))
	 "*" (my-find-project-root)))

(global-set-key (kbd "M-s M-g") 'my-grep-project)
(global-set-key (kbd "M-s g") 'rgrep)

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

(require 'orderless)

(setq completion-styles '(orderless flex))
(setq completion-category-defaults nil)
(setq completion-category-overrides nil)
(setq completion-ignore-case t)
(setq completion-show-help nil)
;; (setq completion-auto-select 'second-tab)

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
        (apply (if (and vertico-mode (or (memq major-mode '(inferior-python-mode))
					 (not (or ;;(derived-mode-p 'minibuffer-mode)
					       (derived-mode-p 'comint-mode)
					       (derived-mode-p 'eshell-mode)))))
		   #'consult-completion-in-region
		 #'completion--in-region)
	       args)))

(setq icomplete-compute-delay 0.0)
(setq icomplete-matches-format nil)
(setq icomplete-show-matches-on-no-input t)
;; (setq completion-pcm-word-delimiters "-_./:|")
;; (setq icomplete-scroll t)
;; (icomplete-vertical-mode 1)

(with-eval-after-load 'icomplete
  ;; by default TAB shows all completions in popup buffer, C-M-i does
  ;; icomplete-force-complete
  (define-key icomplete-minibuffer-map (kbd "SPC") 'self-insert-command) ;; allow orderless to work
  (define-key icomplete-minibuffer-map (kbd "C-j") 'icomplete-ret)
  (define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-force-complete-and-exit)
  (define-key icomplete-minibuffer-map (kbd "C-s") 'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "C-r") 'icomplete-backward-completions))

;; ----------------------------------------------------------------------------
;;| Buffers
;; ----------------------------------------------------------------------------

(defun my-invoke-with-completion (func icomplete)
  (let ((v vertico-mode)
	(m marginalia-mode)
	(ic icomplete-mode)
	(icv icomplete-vertical-mode))
    (vertico-mode 0)
    (marginalia-mode 0)
    (icomplete-vertical-mode -1)
    (icomplete-mode icomplete)
    (unwind-protect
	(call-interactively func)
      (icomplete-mode -1)
      (when v (vertico-mode 1))
      (when m (marginalia-mode 1))
      (when icv (icomplete-vertical-mode 1)))))

(defun my-switch-buffer ()
  (interactive)
  (my-invoke-with-completion #'consult-buffer 1))

(defun my-switch-buffer-other-window ()
  (interactive)
  (my-invoke-with-completion #'consult-buffer-other-window 1))

(evil-global-set-key 'motion (kbd "C-w d")   'kill-this-buffer)
(evil-global-set-key 'motion (kbd "C-w C-d") 'kill-this-buffer)
(global-set-key (kbd "C-x k")		     'kill-this-buffer)

(global-set-key (kbd "C-j")     'my-switch-buffer)
(global-set-key (kbd "C-x C-b") 'my-switch-buffer)
(global-set-key (kbd "C-x b")   'my-switch-buffer)
(global-set-key (kbd "C-x 4 b") 'my-switch-buffer-other-window)
(global-set-key (kbd "C-x M-b") 'ibuffer)

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

(setq helm-grep-file-path-style 'relative)
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
(global-set-key (kbd "M-s M-o") 'helm-occur)

;; ----------------------------------------------------------------------------
;;| Ace window
;; ----------------------------------------------------------------------------

(when (require 'ace-window nil t)
  (global-set-key (kbd "C-'") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

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

(defun my-project-dired ()
  (interactive)
  (dired (my-find-project-root)))

(defvar my-projects)
(cond
 ((file-exists-p "~/dev/git")  (setq my-projects '(("~/dev/git" . 3))))
 ((file-exists-p "~/dev/work") (setq my-projects '(("~/dev/work" . 2)
						   ("~/dev" . 2))))
 (t                            (setq my-projects '(("~/dev" . 2)))))

(dolist (d `("~/dotfiles-public" "~/dotfiles" "~/notefiles" ,org-directory))
  (when (file-directory-p d)
    (push `(,d . 1) my-projects)))

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
      (let* ((dir ;; (expand-file-name (car proj))
	      (car proj))
	     (dir-slash (if (string-suffix-p "/" dir) dir (concat dir "/")))
	     (depth (cdr proj))
	     (repos (mapcar
		     (lambda (long)
		       (let (;; (short (if (string-prefix-p dir-slash long)
			     ;; 		(string-remove-prefix dir-slash long)
			     ;; 	      (file-name-nondirectory long)))
			     )
			 `(,long . ,long)))
		     (my-find-projects dir depth))))
	(setq all (nconc all repos))))
    all))

(defun my-choose-project (&optional action)
  "Choose a project then invoke action on it. If action is nil,
return the project path instead"
  (let* ((repos (my-list-repos))
	 (sel (assoc (completing-read "Project: " repos) repos)))
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
  (my-choose-project-and-invoke #'project-find-dir))

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

(evil-leader/set-key "e" 'my-find-file-in-project)
(evil-leader/set-key "u" 'my-find-file-in-project-other-window)

(global-set-key (kbd "C-x C-d") 'my-project-dired)

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
(setq dired-recursive-copies 'always)
(setq dired-dwim-target t)
(put 'dired-find-alternate-file 'disabled nil)

;;; workaround annoyance of having to select the editing line with vertico
(advice-add #'dired-do-rename :around #'my-disable-vertico)

(defun my-org-attach-dired-cp ()
  (interactive)
  (let ((org-attach-method 'cp))
    (call-interactively #'org-attach-dired-to-subtree)))

(defun my-org-attach-dired-mv ()
  (interactive)
  (let ((org-attach-method 'mv))
    (call-interactively #'org-attach-dired-to-subtree)))

(define-key dired-mode-map (kbd "SPC") evil-leader--default-map)
(define-key dired-mode-map (kbd "C-w") 'evil-window-map)
(define-key dired-mode-map (kbd ";") 'dired-up-directory)
(define-key dired-mode-map (kbd "C-c C-a c") #'my-org-attach-dired-cp)
(define-key dired-mode-map (kbd "C-c C-a m") #'my-org-attach-dired-mv)

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
					("\\.jp[e]?g\\'" "feh -Zx *")
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
  (my-evil-default)
  ;; want SPC to show/scroll commit at point
  (evil-leader-mode -1))

(defun my-magit-repolist-hook ()
  (my-evil-default 'emacs)
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
  (my-evil-default)
  (evil-local-set-key 'normal (kbd "q") 'quit-window))

(defun my-grep-mode-hook ()
  (evil-local-mode -1))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)
(add-hook 'grep-mode-hook 'my-grep-mode-hook)
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(defun my-compile (&optional prefix)
  (interactive "P")
  (if prefix
      (call-interactively 'compile)
    (call-interactively 'project-compile)))

(global-set-key (kbd "C-c C-SPC") 'my-compile)
(global-set-key (kbd "C-c SPC") 'my-compile)
(global-set-key (kbd "C-c C-.") #'recompile)
(global-set-key (kbd "C-c C-,") #'recompile)
(global-set-key (kbd "C-c ,") #'recompile)
(global-set-key (kbd "C-c g") (lambda () (interactive) (my-jump-buffer "*compilation*")))
(global-set-key (kbd "C-c h") (lambda () (interactive) (my-jump-buffer "*Help*" t)))
(define-key compilation-mode-map (kbd "SPC") evil-leader--default-map)
(define-key compilation-mode-map (kbd "C-w") 'evil-window-map)
;; (define-key compilation-mode-map (kbd "g") nil)
(define-key compilation-mode-map (kbd "M-p") nil)
(define-key compilation-mode-map (kbd "M-n") nil)
(define-key compilation-mode-map (kbd "C-c M-o") #'my-clear-buffer)

;; ----------------------------------------------------------------------------
;;| Makefile
;; ----------------------------------------------------------------------------

(defun my-makefile-hook ()
  (my-syntax-entry))
(add-hook 'makefile-mode-hook 'my-makefile-hook)

(with-eval-after-load "make-mode"
  (define-key makefile-gmake-mode-map (kbd "M-n") nil)
  (define-key makefile-gmake-mode-map (kbd "M-p") nil))

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
  (my-evil-default)
  (toggle-truncate-lines 0))

(add-to-list 'auto-mode-alist '("\\.log\\'" . my-log-settings))

(defun my-conf-settings ()
  (my-syntax-entry)
  (my-evil-default))

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

(defun my-insert-markdown-link ()
  (interactive)
  (let ((url (substring-no-properties (current-kill 0 t))))
    (cond
     ((string-match "^https://trello\\.com/[^/]+/[^/]+/\\([0-9]+\\)" url)
      (save-excursion
	(insert (format "[Trello %s](%s)" (match-string 1 url) url))))
     (t (call-interactively 'markdown-insert-link)))))

(evil-leader/set-key "l" 'my-insert-markdown-link)

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
  (if (project-current nil)
      (project-shell)
    (shell)))

(defun my-shell ()
  (interactive)
  (let* ((name (file-name-nondirectory
		(directory-file-name default-directory)))
	 (buf (generate-new-buffer (concat "*shell:" name "*"))))
    (shell buf)))

(push '("\\*shell[:*]"
	(display-buffer-below-selected)
	(window-height . 0.5))
      display-buffer-alist)

(push '("\\*Async Shell Command\\*"
        (display-buffer-no-window))
      display-buffer-alist)

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
      (if other
	  (switch-to-buffer-other-window target)
	(switch-to-buffer target))
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
	      completion-ignore-case t))

(add-hook 'shell-mode-hook 'my-shell-hook)

(when (eq system-type 'windows-nt)
  (setq-default shell-file-name "bash.exe"))

(defun my-sh-mode-hook ()
  (my-syntax-entry)
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 4)
  (setq-local tab-width 4)
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
  (define-key term-mode-map (kbd "M-p") nil)
  (define-key term-mode-map (kbd "M-n") nil)
  (my-expose-global-binding term-raw-map (kbd "M-o"))
  (my-expose-global-binding term-raw-map (kbd "C-j")))

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

(defun my-run-ctags (dir tagscmd)
  "Generate TAGS in a directory, and visit the tags file."
  (interactive (let* ((tagsbin (if (eq system-type 'darwin) "uctags" "ctags"))
		      (default-directory (read-directory-name
					  "Directory: "
					  (expand-file-name (file-name-as-directory default-directory)))))
		 (list default-directory
		       (read-shell-command
			"Command: "
			(format "%s -R -e -f TAGS --exclude=.git --exclude=build . > /dev/null" tagsbin)))))

  (let* ((expdir (expand-file-name (file-name-as-directory dir)))
	 (cmd (format "cd '%s' && %s" expdir tagscmd)))
    (when (y-or-n-p (format "Run `%s`?" cmd))
      (message (format "Running: %s ..." cmd))
      (when (= 0 (shell-command cmd))
	(visit-tags-table (concat expdir "TAGS"))))))

(defun my-find-tags (dir)
  "Find TAGS files and set tags-table-list"
  (interactive (list (read-directory-name
		      "Find tags under directory: "
		      (expand-file-name (file-name-as-directory (cond
								 ((if-let ((project (project-current nil)))
								      (project-root project)))
								 ((file-exists-p "~/dev/git") "~/dev/git")
								 (t "~/dev/")))))))
  (let ((all '())
	(dirs (list dir)))
    (message (format "Searching for TAGS files under %s ..." dirs))
    (dolist (d dirs)
      (setq all (nconc all (directory-files-recursively
			    (expand-file-name d) "^TAGS$" nil
			    'my-dir-predicate))))
    (setq-default tags-table-list all)
    (message "Found %d TAGS files" (length tags-table-list))))

(global-set-key (kbd "C-c t m") 'my-make-tags)
(global-set-key (kbd "C-c t r") 'my-run-ctags)
(global-set-key (kbd "C-c t l") 'my-find-tags)

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
(define-key paredit-mode-map (kbd "M-s u") 'paredit-raise-sexp)
(define-key paredit-mode-map (kbd "M-r") nil)
(when (not (version< emacs-version "30"))
  (define-key paredit-mode-map (kbd "M-q") nil))

(defun my-lisp-common-hook ()
  (enable-paredit-mode)
  (setq-local evil-move-beyond-eol t)
  (setq-local evil-symbol-word-search t))

(add-hook 'emacs-lisp-mode-hook       'my-lisp-common-hook 'append)
(add-hook 'lisp-mode-hook             'my-lisp-common-hook 'append)
(add-hook 'lisp-data-mode-hook        'my-lisp-common-hook 'append)
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
  (add-to-list path (my-find-project-root)))

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

(define-skeleton my-cpp-include-guard "" nil
  "#ifndef INCLUDED_"
  (upcase (setq v1 (skeleton-read "Namespace: ")))
  "_"
  (setq v2 (upcase
	    (let ((name (buffer-file-name))
		  (case-fold-search nil))
	      (if name
		  (replace-regexp-in-string
		   "^_" ""
		   (replace-regexp-in-string
		    "\\([A-Z]\\)" "_\\1"
		    (file-name-nondirectory
		     (file-name-sans-extension name)) t))
		(skeleton-read "Name: "))))) "_H\n"
  "#define INCLUDED_" (upcase v1) "_" v2 "_H\n\n"
  "namespace " v1 " {\n\n"
  -
  "\n\n} // end namespace\n\n"
  "#endif\n")

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
  (define-key c-mode-base-map (kbd "C-c C-f") 'my-kill-c-function-name))

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

(defun my-jump-to-gud-comint-buffer ()
  (interactive)
  (if-let ((w (get-buffer-window gud-comint-buffer)))
      (select-window w)))

(defun my-gdb-mode-hook ()
  (define-key gud-mode-map (kbd "C-c C-r") 'comint-show-output)
  (define-key gud-mode-map (kbd "C-c C-p") 'comint-previous-prompt)
  (define-key gud-mode-map (kbd "C-c C-n") 'comint-next-prompt)
  (define-key gud-mode-map (kbd "C-r") 'comint-history-isearch-backward)
  (define-key gud-mode-map (kbd "C-c C-u") 'comint-kill-input)
  (global-set-key (kbd "C-x C-a C-SPC") 'my-jump-to-gud-comint-buffer))

(add-hook 'gdb-mode-hook 'my-gdb-mode-hook)

;; TODO override gdb-setup-windows

;; ----------------------------------------------------------------------------
;;| Font
;; ----------------------------------------------------------------------------

(defun my-font-config ()
  (interactive)

  (pcase system-type

    ('gnu/linux
     ;; install to ~/.fonts/  then fc-cache -v ~/.fonts

     (set-face-attribute 'default nil :family "Menlo")

     (pcase (system-name)
       ("goose"
	(if (string= "1920x1080" (string-trim
				  (shell-command-to-string
				   "xrandr | grep '\\*' | awk '{print $1}'")))
	    (set-face-attribute 'default nil :height 110) ; external monitor
	  (set-face-attribute 'default nil :height 130))) ; laptop screen

       ("hedgehog"
	(cond
	 ;; remote display
	 ((getenv "SSH_CLIENT")
	  (set-face-attribute 'default nil :height 130))

	 ;; laptop screen
	 ((string-empty-p (string-trim (shell-command-to-string
					"xrandr | awk '$2 == \"connected\" && $1 ~ /^(HDMI-|DP-)/ {print $1}'")))
	  (set-face-attribute 'default nil :height 120))

	 ;; external monitor
	 (t (set-face-attribute 'default nil :height 105))))))

    ('windows-nt
     ;; (set-frame-font "Fira Mono 11" nil t)
     (set-frame-font "JetBrains Mono 11" nil t))

    ('darwin
     (set-face-attribute 'default nil :family "Menlo" :height 140))))

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

(defun my-theme-dark (theme &optional req)
  (mapcar #'disable-theme custom-enabled-themes)
  (require (or req 'ef-themes))
  (load-theme theme t)
  (cond
   ((eq theme 'reykjavik) (load-theme 'my-override-dark t))
   (t                     (load-theme 'my-override-dark2 t)))

  (set-cursor-color "white")
  (setq evil-emacs-state-cursor '(box "orange"))
  (setq evil-normal-state-cursor '(box "white"))
  (setq evil-insert-state-cursor '(box "goldenrod"))
  (setq my-pulse-face 'next-error)
  (my-set-dark-mode t))

(defun my-theme-light (theme &optional req)
  (mapcar #'disable-theme custom-enabled-themes)
  (require (or req 'ef-themes))
  (load-theme theme t)

  (set-cursor-color "black")
  (setq evil-emacs-state-cursor '(box "orange"))
  (setq evil-normal-state-cursor '(box "black"))
  (setq evil-insert-state-cursor '(box "goldenrod"))
  (setq my-pulse-face 'next-error)
  (my-set-dark-mode nil))

(defun my-preserve-compile-command (func &rest args)
  "For some reason, changing the theme reverts the compile-command
to its default value. Leave it alone!"
  (let ((old compile-command))
    (unwind-protect
	(apply func args)
      (setq compile-command old))))

(advice-add 'my-theme-dark :around 'my-preserve-compile-command)
(advice-add 'my-theme-light :around 'my-preserve-compile-command)

(defvar my-alpha 90)

(defun my-toggle-alpha-background ()
  (interactive)
  (set-frame-parameter nil 'alpha-background
		       (if (frame-parameter nil 'alpha-background)
			   nil my-alpha)))

(defhydra my-theme-hydra ()
  ("r" (lambda () (interactive) (my-theme-dark 'reykjavik 'reykjavik-theme)) "reykjavik")
  ("t" (lambda () (interactive) (my-theme-dark 'ef-night)) "ef-night")
  ("y" (lambda () (interactive) (my-theme-dark 'ef-winter)) "ef-winter")
  ("u" (lambda () (interactive) (my-theme-light 'ef-cyprus)) "ef-cyprus")
  ("i" (lambda () (interactive) (my-find-init-file)) "init.el" :exit t)
  ("o" (lambda () (interactive) (my-theme-dark 'ef-owl)) "ef-owl")
  ("p" (lambda () (interactive) (my-theme-dark 'ef-autumn)) "ef-autumn")
  ("a" #'my-toggle-alpha-background "toggle alpha-background")
  ("0" (lambda () (interactive) (set-frame-parameter nil 'alpha-background 0)) "transparent")
  ("9" (lambda () (interactive) (set-frame-parameter nil 'alpha-background my-alpha)) "blend"))

(global-set-key (kbd "C-c z") 'my-theme-hydra/body)

;; ----------------------------------------------------------------------------
;;| Jumps
;; ----------------------------------------------------------------------------

(defhydra my-jump-hydra (:hint nil)
  "
_e_: jump to shell     _s_: shell             _b_: bookmarks       _SPC_: agenda
_i_: init.el           _S_: project shell     _c_: calc            ^ ^
_n_: notes             _t_: term              _d_: calendar        ^ ^
_w_: world clock       ^ ^                    _r_: scratch         ^ ^
"
  ("r" #'scratch-buffer    :exit t)
  ("e" #'my-jump-to-shell)
  ("s" #'my-shell	   :exit t)
  ("S" #'my-project-shell  :exit t)
  ("t" #'ansi-term         :exit t)
  ("d" #'my-project-dired  :exit t)
  ("c" #'my-calc	   :exit t)
  ("d" #'calendar	   :exit t)
  ("b" #'bookmark-jump     :exit t)
  ("n" (lambda () (interactive) (find-file org-default-notes-file)) :exit t)
  ("i" #'my-find-init-file :exit t)
  ("SPC" #'my-org-agenda     :exit t)
  ("w" #'world-clock       :exit t))

(global-set-key (kbd "C-,") 'my-jump-hydra/body)

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
     ((and (> hour 7) (< hour 16))     (my-theme-light 'ef-cyprus))
     ((and (>= month 9) (<= month 11)) (my-theme-dark 'ef-autumn))
     ((or (= month 12) (<= month 2))   (my-theme-dark 'ef-winter))
     (t                                (my-theme-dark 'ef-winter))))

  (when (display-graphic-p)
    (my-font-config))

  (when (and (eq system-type 'gnu/linux)
	     (string= (getenv "XDG_SESSION_TYPE") "wayland"))
    ;; hide window title bar. wayland only, breaks under X11 gnome.
    (set-frame-parameter nil 'undecorated t))

  (when (string= "goose" (system-name))
    (my-toggle-alpha-background)))

(add-hook 'window-setup-hook 'my-window-setup-hook)

;;; disable trackpad zoom
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)
(global-set-key (kbd "<pinch>") 'ignore)

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
    (setq emms-seek-seconds 60)

    (define-key emms-browser-mode-map (kbd "SPC") evil-leader--default-map)
    (define-key emms-browser-mode-map (kbd "<tab>") #'emms-browser-toggle-subitems-recursively)
    (define-key emms-browser-mode-map (kbd ";") #'emms-browser-move-up-level)
    (define-key emms-browser-mode-map (kbd "C-j") nil)
    (define-key emms-playlist-mode-map (kbd "C-j") nil)
    (define-key emms-playlist-mode-map (kbd "M-n") nil)
    (define-key emms-playlist-mode-map (kbd "M-p") nil)
    (define-key emms-playlist-mode-map (kbd "SPC") evil-leader--default-map)
    (define-key emms-playlist-mode-map ";" #'emms-playlist-mode-center-current)
    (define-key emms-playlist-mode-map "k" #'emms-pause)
    (define-key emms-playlist-mode-map "j" (lambda ()
					     (interactive)
					     (let ((emms-seek-seconds 10))
					       (emms-seek-backward))))
    (define-key emms-playlist-mode-map "l" (lambda ()
					     (interactive)
					     (let ((emms-seek-seconds 10))
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
