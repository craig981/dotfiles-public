;;; -*- mode: emacs-lisp; -*-

(let ((trustfile
       (cond
	((eq system-type 'gnu/linux)
	 (if (string-match "\\.el7\\." operating-system-release)
	     "/etc/pki/tls/certs/ca-bundle.crt"	   ;; centos
	   "/etc/ssl/certs/ca-certificates.crt"))  ;; ubuntu
	(t "/usr/local/etc/openssl/cert.pem"))))   ;; darwin
  ;; https://curl.se/docs/caextract.html
  (setq tls-checktrust t)
  (setq tls-program
	(list
	 (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))
; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
; http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html

;; ----------------------------------------------------------------------------
;* Package
;; ----------------------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu"   . "https://elpa.gnu.org/packages/")))
;; (package-initialize)

;; ----------------------------------------------------------------------------
;* Paths
;; ----------------------------------------------------------------------------

(if (eq system-type 'windows-nt)
    (let ((path "C:/cygwin64/bin;C:/windows/system32;C:/Program Files/CMake/bin"))
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
;* Evil mode
;; ----------------------------------------------------------------------------

(setq evil-want-integration t
      evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(require 'evil-leader)
(require 'evil-numbers)

(global-evil-leader-mode)
(evil-esc-mode 1)			; make C-[ escape

(global-set-key (kbd "C-c C-M-e") #'evil-local-mode)

(defun my-find-file-hook ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil))
  (if (not (or (eq major-mode 'image-mode)
	       (derived-mode-p 'bongo-mode)))
      (evil-local-mode 1)))

(add-hook 'find-file-hook 'my-find-file-hook)
(add-hook 'evil-command-window-mode-hook 'evil-local-mode)

(when (fboundp 'evil-set-undo-system)
  (if (version< emacs-version "28.1")
      (progn
	(require 'undo-tree)
	(global-undo-tree-mode)
	(evil-set-undo-system 'undo-tree))
    (evil-set-undo-system 'undo-redo)))
(evil-declare-ignore-repeat 'evil-undo)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key "w" 'evil-write)

(evil-declare-ignore-repeat 'evil-scroll-line-to-center)
(evil-declare-ignore-repeat 'hscroll-cursor-left)
(evil-declare-ignore-repeat 'hscroll-cursor-right)
(evil-declare-ignore-repeat 'recenter-top-bottom)

;;; emacs end-of-line behaviour for M-e, C-M-f on S-expressions;
;;; vim end-of-line behaviour for $, l.
;;; :around advice on these functions doesn't work.
(defun my-disable-eol (&rest args)
  (setq evil-move-beyond-eol nil))
(defun my-enable-eol (&rest args)
  (setq evil-move-beyond-eol t))
(dolist (func '(evil-end-of-line
		evil-next-line
		evil-previous-line
		evil-forward-char))
	(advice-add func :before #'my-disable-eol))
(dolist (func '(forward-sentence
		forward-sexp
		forward-list
		kill-sentence
		paredit-forward
		paredit-forward-up))
	(advice-add func :before #'my-enable-eol))

(setq-default evil-ex-search-case 'sensitive)
;;(setq-default evil-search-module 'evil-search)
(define-key evil-normal-state-map (kbd "M-.") nil)
(define-key evil-normal-state-map (kbd "M-,") nil)

(defun my-delete-or-indent-left ()
  (interactive)
  (if (eolp)
      (evil-shift-left-line 1)
    (delete-char 1)))

;; fall through to emacs keys, C/M-f/b and M-d M-m already works in insert mode
(evil-global-set-key 'motion (kbd "C-a") nil)
(evil-global-set-key 'insert (kbd "C-a") nil)
(evil-global-set-key 'insert (kbd "C-e") nil)
(evil-global-set-key 'insert (kbd "C-k") nil)
(evil-global-set-key 'insert (kbd "C-y") nil)
(evil-global-set-key 'insert (kbd "C-n") nil)
(evil-global-set-key 'insert (kbd "C-p") nil)
(evil-global-set-key 'insert (kbd "C-d") 'my-delete-or-indent-left)

(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "C-k") 'kill-line)

(evil-global-set-key 'insert (kbd "C-c") 'evil-normal-state)
(evil-global-set-key 'visual (kbd "C-c") 'evil-normal-state)
(evil-global-set-key 'operator (kbd "C-c") 'evil-normal-state)
(evil-global-set-key 'replace (kbd "C-c") 'evil-normal-state)

(evil-global-set-key 'normal (kbd "C-a") 'evil-numbers/inc-at-pt)
(evil-global-set-key 'normal (kbd "C-p") 'evil-numbers/dec-at-pt)
(evil-global-set-key 'normal (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
(evil-global-set-key 'normal (kbd "g C-p") 'evil-numbers/dec-at-pt-incremental)

(defun my-advise-window-rotate ()
  (other-window 1))
(advice-add 'evil-window-rotate-downwards :after #'my-advise-window-rotate)

(defun my-advise-window-new (func &rest args)
  "Use evil-local-mode and the same major mode in the new buffer"
  (let ((mode (if (or (derived-mode-p 'prog-mode)
		      (eq major-mode 'org-mode))
		  major-mode
		'text-mode)))
    (apply func args)
    (evil-local-mode 1)
    (funcall mode)))
(advice-add 'evil-window-new :around #'my-advise-window-new)

;; ----------------------------------------------------------------------------
;* Syntax and indent
;; ----------------------------------------------------------------------------

;; treat symbol as word, so we include underscores in C++ identifiers
;; http://emacs.stackexchange.com/a/20717
(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))
;; the above doesn't work for evil-forward-search, so...
(add-hook 'c-mode-common-hook
	  (lambda () (modify-syntax-entry ?_ "w")))

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
(setq-default fill-column 70)      ; set tw=70
(setq-default truncate-lines t)    ; set nowrap
(setq-default tab-width 8)         ; set ts=8
(setq-default evil-shift-width 8)
(setq-default indent-tabs-mode t)  ; set noexpandtab
;; M-x make-variable-buffer-local tab-width
;; M-x set-variable tab-width 4
;; C-u M-x untabify to convert tabs to spaces

;; tabs+spaces instead of all tabs
(setq-default align-to-tab-stop nil)

(setq-default tab-always-indent t)

(electric-indent-mode 1)

;; ----------------------------------------------------------------------------
;* Convenience
;; ----------------------------------------------------------------------------

(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)

(show-paren-mode)
(setq-default
 show-paren-when-point-inside-paren t
 show-paren-when-point-in-periphery t)

(setq ring-bell-function 'ignore) ;; stop binging noise on C-g

(setq-default vc-follow-symlinks t)
(setq-default backup-inhibited t)    ;; disable backup
(setq-default auto-save-default nil) ;; disable auto save

(recentf-mode 1)
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)
(when (eq system-type 'gnu/linux)
  (setq history-length 1000)
  (setq history-delete-duplicates t)
  (savehist-mode 1))

(require 'thingatpt)

(defun my-substitute (&optional range)
  (interactive)
  (let* ((sym (thing-at-point 'symbol t))
	 ;; want @ by default so dabbrev-expand works
	 (delim (if (string-match-p "@" sym) "#" "@")))
    (evil-ex (format "%ss%s\\<%s\\>%s" (or range "%") delim sym delim))))

(defun my-delete-trailing-whitespace ()
  (interactive)
  (if (region-active-p)
      (progn
	(call-interactively 'delete-trailing-whitespace)
	(deactivate-mark)
	(message "Deleted trailing whitespace"))
    (message "No region active")))

(defun my-delete-space ()
  (interactive)
  (if (region-active-p)
      (my-delete-trailing-whitespace)
    (delete-horizontal-space)))

(defun my-align-regexp ()
  (interactive)
  (if (region-active-p)
      (progn
	(call-interactively 'align-regexp)
	(deactivate-mark))
    (message "No region active")))

(defun my-copy-filename ()
  (interactive)
  (let ((x (or (buffer-file-name) default-directory)))
    (kill-new x)
    (message "Yanked %s" x)))

(defun my-toggle-wrap ()
  (interactive)
  (toggle-word-wrap)
  (toggle-truncate-lines))

(defun my-mirror-buffer ()
  "Mirror current buffer to other window"
  (interactive)
  (let ((buf (current-buffer))
	(line (line-number-at-pos)))
    (save-selected-window
      (other-window 1)
      (switch-to-buffer buf)
      (goto-line line)
      (recenter-top-bottom))))

(defun my-kill-buffer ()
  "Kill unmodified buffers without asking"
  (interactive)
  (if (or buffer-read-only
	  (not (buffer-modified-p)))
      (kill-this-buffer)
    (call-interactively 'kill-buffer)))

(defun my-close-other-window ()
  (interactive)
  (quit-window nil (next-window)))

(defun my-man-page-hook ()
  (evil-local-mode))


(evil-leader/set-key "s" #'my-substitute) ; substitute whole buffer
(evil-leader/set-key "S" ; substitute from current line to end of buffer
  (lambda ()
    (interactive)
    (my-substitute ".,$")))

(evil-leader/set-key "=" #'my-align-regexp)
(evil-leader/set-key "\\" #'my-delete-trailing-whitespace)
(evil-leader/set-key "m" #'my-mirror-buffer)
(evil-leader/set-key "d" 'pwd)

(global-set-key (kbd "M-\\") #'my-delete-space)

(evil-global-set-key 'insert (kbd "C-x C-l") 'hippie-expand) ;; line completion like vim
(evil-global-set-key 'motion (kbd "K")
		     (if (eq system-type 'darwin)
			 ;; skip slow vertico minibuffer prompt for man pages
			 (lambda ()
			   (interactive)
			   (man (thing-at-point 'word t)))
		       'man))
(add-hook #'Man-mode-hook #'my-man-page-hook)

(global-set-key (kbd "C-c q") #'my-close-other-window)

(evil-global-set-key 'motion (kbd "C-w d") 'my-kill-buffer)
(evil-global-set-key 'motion (kbd "C-w C-d") 'my-kill-buffer)
(global-set-key (kbd "C-x k") 'my-kill-buffer)

(evil-leader/set-key "%" #'my-copy-filename)
(global-set-key (kbd "C-c %") #'my-copy-filename)
(global-set-key (kbd "C-c u") #'my-toggle-wrap)
(when (not (display-graphic-p))
  (global-set-key (kbd "C-x ;") (kbd "C-x C-;")))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "M-o") nil))

(global-set-key (kbd "M-p") (kbd "M-{"))
(global-set-key (kbd "M-n") (kbd "M-}"))
(global-set-key (kbd "M-o") (kbd "C-x o"))
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "M-=") 'count-words)
(global-set-key (kbd "M-'") #'delete-blank-lines)

(global-set-key (kbd "C-c w h") #'evil-window-move-far-left)
(global-set-key (kbd "C-c w l") #'evil-window-move-far-right)
(global-set-key (kbd "C-c w j") #'evil-window-move-very-bottom)
(global-set-key (kbd "C-c w k") #'evil-window-move-very-top)
(global-set-key (kbd "C-c w r") #'evil-window-rotate-downwards)
(global-set-key (kbd "C-c w =") #'balance-windows)

(global-unset-key (kbd "C-h h")) ;; stop accidentally opening hello file
(global-set-key (kbd "C-h C-c") nil) ;; disable describe-copying

(global-set-key (kbd "C-c j") 'jump-to-register)

(setq register-preview-delay 0.5)

(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

(put 'narrow-to-region 'disabled nil)

;; don't save context strings
(setq-default bookmark-make-record-function
      (lambda (&optional no-file no-context posn)
	(funcall 'bookmark-make-record-default no-file t posn)))

(when (not (version< emacs-version "28.1"))
  (setq-default bookmark-set-fringe-mark nil))

(defun my-advise-comment (&rest args)
  (when (evil-normal-state-p)
    (call-interactively 'evil-append)))

(advice-add 'comment-dwim :after 'my-advise-comment)

;; ----------------------------------------------------------------------------
;* Help
;; ----------------------------------------------------------------------------

(defun my-help-mode-hook ()
  (evil-local-mode)
  (evil-local-set-key 'motion (kbd "TAB") #'forward-button)
  (evil-local-set-key 'motion (kbd "q") 'quit-window))

(add-hook 'help-mode-hook #'my-help-mode-hook)

(defun my-messages-mode-hook ()
  (evil-local-mode)
  (evil-local-set-key 'normal (kbd "q") 'quit-window))

(add-hook 'messages-buffer-mode-hook 'my-messages-mode-hook)

(defun my-list-all-keymaps ()
  (let (maps)
    (mapatoms (lambda (sym)
		(and (boundp sym)
		     (keymapp (symbol-value sym))
		     (push sym maps))))
    (seq-sort (lambda (x y)
		(string< (symbol-name x)
			 (symbol-name y)))
	      maps)))

;; (my-lookup-key-in-all-keymaps (kbd "C-j"))
(defun my-lookup-key-in-all-keymaps (key)
  (dolist (m (my-list-all-keymaps))
    (let ((f (lookup-key (symbol-value m) key)))
      (when f
	(message (format "%-32S\t%S" m f))))))

;;; https://stackoverflow.com/a/36994486
(defun my-describe-keymap (keymap)
  "Describe a keymap"
  (interactive
   (list (completing-read
	  "Keymap: " (my-list-all-keymaps)
	  nil t)))
  (with-output-to-temp-buffer (format "*keymap: %s*" keymap)
    (princ (format "%s\n\n" keymap))
    (princ (substitute-command-keys (format "\\{%s}" keymap)))
    (with-current-buffer standard-output ;; temp buffer
      (setq help-xref-stack-item (list #'my-describe-keymap keymap)))))

(global-set-key (kbd "C-h M-v") 'my-describe-keymap)

;; ----------------------------------------------------------------------------
;* Abbreviations
;; ----------------------------------------------------------------------------

(add-hook 'text-mode-hook #'abbrev-mode)
(define-global-abbrev "retrun" "return")
(define-global-abbrev "cosnt" "const")
(define-global-abbrev "conat" "const")
(define-global-abbrev "trinagle" "triangle")

;;; want C-x C-l to expand line first
(push 'try-expand-line hippie-expand-try-functions-list)

;; ----------------------------------------------------------------------------
;* Fancy dabbrev
;; ----------------------------------------------------------------------------

(require 'fancy-dabbrev)

(global-fancy-dabbrev-mode)

(defun my-insert-mode-tab (&optional arg)
  (interactive "P")
  (if (not evil-input-method)
      (fancy-dabbrev-expand-or-indent)
    (insert-tab arg)))

(evil-global-set-key 'insert (kbd "TAB") 'my-insert-mode-tab)
(global-set-key (kbd "M-/") 'fancy-dabbrev-expand)
(global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
(setq-default fancy-dabbrev-menu-height 15)
(setq-default fancy-dabbrev-preview-context 'everywhere)
(setq-default fancy-dabbrev-preview-delay 0.25)
(push 'evil-input-method fancy-dabbrev-no-preview-for)

;; ----------------------------------------------------------------------------
;* Lang
;; ----------------------------------------------------------------------------

(defun my-lookup ()
  (interactive)
  (let* ((google "https://www.google.com/search?ie=utf-8&oe=utf-8&q=")
	 (input-method (if (or (not (bound-and-true-p evil-local-mode))
			       (evil-emacs-state-p))
			   current-input-method
			 evil-input-method))
	 (translate (cond
		     ((string= input-method "swedish-postfix") "https://translate.google.com/?sl=sv&tl=en&op=translate&text=")
		     ((string= input-method "german-postfix") "https://translate.google.com/?sl=de&tl=en&op=translate&text=")
		     (t nil))))
    (browse-url
     (if mark-active
	 (let ((text (buffer-substring (region-beginning) (region-end))))
	   (cond
	    (translate (concat translate (url-hexify-string text)))
	    (t (concat google (url-hexify-string (format "\"%s\"" text))))))

       (let ((sym (thing-at-point 'symbol t))
	     (cc (or (eq major-mode 'c++-mode)
		     (eq major-mode 'c-mode))))
	 (cond
	  ((and cc sym (string-match-p "^gl[A-Z][^\s-]+$" sym))
	   (concat "https://docs.gl/" (read-string "OpenGL: " (concat "gl4/" sym))))
	  ((and cc sym (string-match-p "^M[A-Z][^\s-]+$" sym))
	   (format "https://help.autodesk.com/view/MAYAUL/2020/ENU/?query=%s&cg=Developer%%27s%%20Documentation"
		   (read-string "Maya API: " sym)))
	  (translate (concat translate (url-hexify-string (read-string "Translate: " sym))))
	  (t (concat google (url-hexify-string (read-string "Search Google: " sym))))))))))

(global-set-key (kbd "M-s M-w") #'my-lookup)
(evil-leader/set-key "SPC" #'my-lookup)

;; ----------------------------------------------------------------------------
;* Keyboard
;; ----------------------------------------------------------------------------

(when (eq system-type 'darwin)
  ;; tilde in the same place as in US keyboard
  (keyboard-translate ?\§ ?\`)
  (keyboard-translate ?\± ?\~)

  (setq-default mac-command-modifier 'meta)
  (setq-default mac-right-command-modifier 'control)
  (setq-default mac-option-modifier 'alt))

;; ----------------------------------------------------------------------------
;* Clipboard
;; ----------------------------------------------------------------------------

(defun my-copy-to-xclipboard ()
  (interactive)
  (if (region-active-p)
      (progn
	(call-process-region (region-beginning) (region-end) "xsel" nil nil nil "-ib")
	(message "Yanked region")
	(deactivate-mark))
    (message "No region active, can't yank")))

(when (eq system-type 'gnu/linux)
  (global-set-key (kbd "C-c y") 'my-copy-to-xclipboard)
  (evil-global-set-key 'visual (kbd "Y") 'my-copy-to-xclipboard))

;; ----------------------------------------------------------------------------
;* Scrolling
;; ----------------------------------------------------------------------------

;; http://emacs.stackexchange.com/questions/8126/zs-and-ze-from-vim
(defun hscroll-cursor-left ()
  (interactive "@")
  (set-window-hscroll (selected-window) (current-column)))
(defun hscroll-cursor-right ()
  (interactive "@")
  (set-window-hscroll (selected-window) (- (current-column) (window-width) -1)))
(evil-global-set-key 'motion (kbd "zs") 'hscroll-cursor-left)
(evil-global-set-key 'motion (kbd "ze") 'hscroll-cursor-right)

;; stop scrolling to centre when cursor is on first/last line and
;; moves up/down
(setq-default scroll-up-aggressively 0.0)
(setq-default scroll-down-aggressively 0.0)

(setq-default auto-hscroll-mode 't
	      hscroll-margin 5
	      hscroll-step 5)

;; ----------------------------------------------------------------------------
;* Text
;; ----------------------------------------------------------------------------

(defun my-after-evil-buffer-new (&rest args)
  (let ((buffer (window-buffer)))
    (when buffer
      (with-current-buffer buffer
	(text-mode)))))

;;; make :enew edit in text-mode
(advice-add 'evil-buffer-new :after #'my-after-evil-buffer-new)

(defun my-text-mode-hook ()
  (turn-on-auto-fill)
  (my-syntax-entry)
  (evil-local-mode))

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; ----------------------------------------------------------------------------
;* Calendar
;; ----------------------------------------------------------------------------

(setq-default calendar-week-start-day 1) ;; start on monday

(push '("\\*Calendar\\*"
        (display-buffer-reuse-window display-buffer-below-selected)
        (window-height . 10))
      display-buffer-alist)

(global-set-key (kbd "C-c C-M-c") 'calendar)

;; ----------------------------------------------------------------------------
;* Org
;; ----------------------------------------------------------------------------

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(evil-leader/set-key "g" (lambda ()
			   (interactive)
			   (org-agenda nil "g")))
(evil-leader/set-key "n" (lambda ()
			   (interactive)
			   (find-file org-default-notes-file)))
(evil-leader/set-key-for-mode 'org-mode "," 'org-insert-structure-template)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "j" (kbd "n"))
  (define-key org-agenda-mode-map "k" (kbd "p"))
  (define-key org-agenda-mode-map (kbd "C-w") 'evil-window-map)
  (define-key org-agenda-mode-map (kbd "h") (lambda () (interactive)))
  ;; (setq org-agenda-sorting-strategy
  ;;     (cons '(agenda tag-up habit-down time-up priority-down category-keep)
  ;; 	    org-agenda-sorting-strategy))
  )

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "M-n") 'forward-paragraph)
  (define-key org-mode-map (kbd "M-p") 'backward-paragraph)
  (define-key org-mode-map (kbd "C-c M-[") 'org-toggle-link-display)
  (define-key org-mode-map (kbd "C-c M-]") 'org-toggle-link-display)

  (org-babel-do-load-languages 'org-babel-load-languages
			       '((shell . t)
				 (python .t)
				 (emacs-lisp . t)
				 (gnuplot . t)))

  (define-key org-mode-map (kbd "C-j") nil)

  (when (eq system-type 'darwin)

    (setq org-babel-python-command "python3")

    ;; for pdf export
    (require 'ox-pandoc)))

(require 'org)

(when (eq system-type 'gnu/linux)
  (setq org-agenda-files (list "~/notes.org"))
  (setq org-default-notes-file "~/notes.org"))
(when (eq system-type 'darwin)
  (setq org-agenda-files (list "~/notes.org.gpg"))
  (setq org-default-notes-file "~/notes.org.gpg"))
(setq org-directory "~/org")
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
;; (when (eq system-type 'darwin)
;;   (setq org-todo-keywords '((sequence "TODO" "WAITING" "DONE"))))

(setq org-capture-templates
      '(("b" "Bookmark" entry (file+headline org-default-notes-file "Bookmarks")
	 "* %?\n")
	("t" "Task" entry (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n")))
(when (eq system-type 'darwin)
    (push '("B" "Book" entry (file+headline org-default-notes-file "Books")
	    "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	  org-capture-templates)
    (push '("r" "Read/watch" entry (file+headline org-default-notes-file "Tasks")
	    "* %? :READ:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	  org-capture-templates)
    (push '("p" "Project" entry (file+headline org-default-notes-file "Tasks")
	    "* %? :PROJECT:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	  org-capture-templates))
(when (eq system-type 'gnu/linux)
    (push '("n" "Next" entry (file+headline org-default-notes-file "Tasks")
	    "* %? :NEXT:\n:PROPERTIES:\n:CREATED: %U\n:END:\n")
	  org-capture-templates))

(setq my-org-agenda-common-review-settings
      '((org-agenda-show-all-dates t)
	(org-agenda-start-with-log-mode t)
	(org-agenda-start-with-clockreport-mode t)
	;; https://orgmode.org/manual/Special-Agenda-Views.html
	(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("TODO" "WAITING")))
	(org-agenda-archives-mode t)
	(org-agenda-clockreport-parameter-plist '(:link t :hidefiles t :tags t :step day :maxlevel 2 :fileskip0 t :formula %))
	;; (org-agenda-hide-tags-regexp
	;;  (concat org-agenda-hide-tags-regexp "\\|ARCHIVE"))
	(org-agenda-start-on-weekday 1)))

(let ((tag (if (eq system-type 'darwin) "READ|PROJECT" "NEXT")))
  (setq org-agenda-custom-commands
	`(("d" "Done stuff" todo "DONE" )
	  ("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))
	  ("g" ,(format "Agenda and %s" tag)
	   ((agenda "" )
	    ,@(if (eq system-type 'gnu/linux) '((tags "PIN")))
	    (tags ,tag ((org-agenda-skip-function
			 '(org-agenda-skip-entry-if 'todo '("DONE")))
			(org-agenda-sorting-strategy
			 '((tags tag-up alpha-up))))))
	   ((org-agenda-start-with-log-mode nil)
	    (org-tags-match-list-sublevels nil)))
	  ("w" "This week"
	   agenda ""
	   ,(append my-org-agenda-common-review-settings
		    '((org-agenda-span 'week)
		      (org-agenda-overriding-header "Week in Review")))
	   ("/tmp/week.html"))
	  ("W" "Last week"
	   agenda ""
	   ,(append my-org-agenda-common-review-settings
		    '((org-agenda-span 'week)
		      (org-agenda-start-day "-1w")
		      (org-agenda-overriding-header "Last week in Review")))
	   ("/tmp/lastweek.html")))))

(defun my-org-shift (left)
  (if (string-match-p "^[\s-]*[*-] " (thing-at-point 'line))
      (if left
	  (if (eolp)
	      (org-metaleft)
	    (delete-char 1))
	(org-metaright))
    (if left
	(my-delete-or-indent-left)
      (evil-shift-right-line 1))))

(defun my-org-mode-hook ()
  ;; override the evil binding of C-i (jump forward), as C-i is the
  ;; same as tab in the terminal, which we want in org mode for
  ;; (un)collapsing headers
  (when (not (display-graphic-p))
    (evil-local-set-key 'motion (kbd "C-i") 'org-cycle))
  (when (and (display-graphic-p)
	     (not (version< emacs-version "28.1")))
    (evil-local-set-key 'normal (kbd "TAB") 'org-cycle))

  (evil-local-set-key 'insert (kbd "<backtab>") #'fancy-dabbrev-backward)
  (evil-local-set-key 'insert (kbd "C-t") (lambda ()
					    (interactive)
					    (my-org-shift nil)))
  (evil-local-set-key 'insert (kbd "C-d") (lambda ()
					    (interactive)
					    (my-org-shift t)))

  ;; / is punctuation, so evil * works on path components
  (modify-syntax-entry ?/ ".")
  (auto-fill-mode 1)
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 2)
  (setq-local tab-width 2))

(defun my-org-capture ()
  (interactive)
  (my-org-mode-hook)
  (evil-insert-state))

(defun my-org-src-hook ()
  (evil-local-mode 1))

(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'org-capture-mode-hook 'my-org-capture)
(add-hook 'org-src-mode-hook 'my-org-src-hook)

(defun my-org-clock-jump ()
  (interactive)
  (push-mark (point))
  (org-clock-jump-to-current-clock))
(global-set-key (kbd "C-c C-x C-j") 'my-org-clock-jump)


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
  (let* ((url (thing-at-point 'url t))
	 (title (my-www-get-page-title url)))
    (save-excursion
      (insert "[[")
      (evil-forward-WORD-end)
      (forward-char)
      (insert (format "][%s]]" title)))))

(global-set-key (kbd "C-c M-t") #'my-wrap-org-link)

(defun my-insert-org-src-block  ()
  "Insert a src block with the same language as the previous block"
  (interactive)
  (let ((lang (save-excursion
		(org-babel-previous-src-block)
		(move-end-of-line 1)
		(thing-at-point 'word t))))
    (org-insert-structure-template (format "src %s" lang))))

(evil-leader/set-key-for-mode 'org-mode "l" 'my-insert-org-src-block)

(defun my-forward-before-insert (&rest args)
  "Move the cursor forward before closing a tag or inserting a time stamp"
  (when (and (eq evil-state 'normal)
	     (not (eolp)))
    (forward-char)))

(advice-add 'org-time-stamp-inactive :before #'my-forward-before-insert)

;; ----------------------------------------------------------------------------
;* Browser
;; ----------------------------------------------------------------------------

(when (eq system-type 'gnu/linux)
  ;; open org links in chrome
  (setq browse-url-browser-function 'browse-url-generic)
  (setq browse-url-generic-program "google-chrome"))

(setq org-file-apps '((auto-mode . emacs)
		      ("\\.mm\\'" . default)
		      ("\\.x?html?\\'" . "google-chrome %s")
		      ("\\.pdf\\'" . default)))

;; ----------------------------------------------------------------------------
;* HTML
;; ----------------------------------------------------------------------------

(defun my-html-hook ()
  (define-key html-mode-map (kbd "M-o") nil)
  (evil-local-set-key 'normal (kbd "g SPC")
		      (lambda ()
			(interactive)
			(save-buffer)
			(browse-url-of-file))))

(add-hook 'html-mode-hook #'my-html-hook)

(advice-add 'sgml-close-tag :before #'my-forward-before-insert)

;; ----------------------------------------------------------------------------
;* Which key
;; ----------------------------------------------------------------------------

(require 'which-key)
(which-key-mode)

;; ----------------------------------------------------------------------------
;* Winner
;; ----------------------------------------------------------------------------

(winner-mode 1)
(global-set-key (kbd "C-c h") 'winner-undo)
(global-set-key (kbd "C-c H") 'winner-redo)

;; ----------------------------------------------------------------------------
;* Vertico, orderless, marginalia
;; ----------------------------------------------------------------------------

;; popup the completion buffer at the bottom
(push '("\\*Completions\\*"
        (display-buffer-reuse-window display-buffer-at-bottom)
        (window-height . 10))
      display-buffer-alist)

(require 'vertico)
(require 'orderless)
(require 'marginalia)

(vertico-mode)
(marginalia-mode)
(setq completion-styles '(orderless flex))
(setq completion-ignore-case t)

(define-key vertico-map (kbd "C-j") nil)
(define-key vertico-map (kbd "C-h f")
  (lambda ()
    (interactive)
    (describe-function (intern (vertico--candidate)))))

(defun my-disable-vertico (func &rest args)
  (vertico-mode -1)
  (unwind-protect
      (apply func args)
    (vertico-mode)))

(defun my-disable-marginalia (func &rest args)
  (marginalia-mode -1)
  (unwind-protect
      (apply func args)
    (marginalia-mode)))
;; Don't want marginalia in *Completions* buffer when hitting TAB in
;; shell mode. Completion candidates are in a grid, and some are
;; pushed off-screen.
(advice-add #'completion-at-point :around #'my-disable-marginalia)

;; ----------------------------------------------------------------------------
;* Consult
;; ----------------------------------------------------------------------------

(require 'consult)

(global-set-key (kbd "C-j") 'consult-buffer)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)

(define-key consult-narrow-map (kbd "C-j") 'vertico-exit)

(setq consult-preview-key nil)	; stop preview when any key is pressed
(consult-customize
 consult-buffer consult-buffer-other-window consult-theme
 :preview-key (kbd "C-j"))

(require 'consult-dir)

(global-set-key (kbd "C-x C-d") #'consult-dir)
(define-key vertico-map (kbd "C-x C-d") #'consult-dir)

;;; insert directory name into buffer
(setq consult-dir-default-command
      (lambda ()
	(interactive)
	(insert default-directory)))

;; ----------------------------------------------------------------------------
;* Embark
;; ----------------------------------------------------------------------------

(require 'embark)

(define-key minibuffer-local-map (kbd "C-o") 'embark-act)

;; ----------------------------------------------------------------------------
;* Helm
;; ----------------------------------------------------------------------------

;; (let ((dir "~/dev/helm"))
;;   (when (file-directory-p dir)
;;     (add-to-list 'load-path dir)))

(require 'helm)
(require 'helm-config)

(helm-minibuffer-history-mode)

(define-key helm-map (kbd "C-c C-u") 'kill-whole-line)
(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(setq helm-highlight-only-all-matches t)
(setq helm-highlight-matches-around-point-max-lines '(25 . 25))

;;; no new frames
(setq helm-show-completion-display-function #'helm-show-completion-default-display-function)

;; ----------------------------------------------------------------------------
;* Calc
;; ----------------------------------------------------------------------------

(with-eval-after-load "calc-ext"
  (define-key calc-mode-map (kbd "C-c r") #'calc-reset)
  (define-key calc-mode-map (kbd "C-c C-r") #'calc-reset)
  (setq calc-multiplication-has-precedence nil)
  (advice-add #'calc-user-define-formula :around #'my-disable-vertico))

(global-set-key (kbd "C-c M-c") (kbd "C-x * c"))

;; ----------------------------------------------------------------------------
;* Tramp
;; ----------------------------------------------------------------------------

(with-eval-after-load "tramp"
  (setq-default tramp-histfile-override "/tmp/.tramp_history")
  (setq remote-file-name-inhibit-locks t))

;; ----------------------------------------------------------------------------
;* Find file at point
;; ----------------------------------------------------------------------------

(require 'ffap)

(defun my-find-file-at-point ()
  "Like find-file-at-point but without prompt"
  (interactive)
  (let ((f (ffap-file-at-point)))
    (if (and f (file-exists-p f))
	(find-file f)
      (find-file-at-point f))
    (princ (buffer-file-name))))

(global-set-key (kbd "C-c f") 'my-find-file-at-point)
(evil-global-set-key 'normal (kbd "gf") 'my-find-file-at-point)

(evil-global-set-key 'motion (kbd "C-w f")
		     (lambda ()
		       (interactive)
		       (evil-window-split)
		       (my-find-file-at-point)))

;; ----------------------------------------------------------------------------
;* Complete filenames
;; ----------------------------------------------------------------------------

;; match a filename before the point. from company-files.
(defvar my-file-regexps
  (let* ((root (if (eq system-type 'windows-nt)
                   "[a-zA-Z]:/"
                 "/"))
         (begin (concat "\\(?:\\.\\{1,2\\}/\\|~/\\|" root "\\)")))
    (list (concat "\"\\(" begin "[^\"\n]*\\)")
	  (concat "\"\\(" "[^\"\n]*\\)")
          (concat "\'\\(" begin "[^\'\n]*\\)")
          (concat "\'\\(" "[^\'\n]*\\)")
          (concat "\\(?:[ \t=\[]\\|^\\)\\(" begin "[^ \t\n]*\\)")
	  (concat "\\(?:[ \t=\[]\\|^\\)\\(" "[^ \t\n]*\\)"))))

(defvar my-completing-filename nil)

(defun my-complete-filename ()
  "Expand the filename before the point"
  (interactive)
  (setq my-completing-filename t)
  (unwind-protect
      (while my-completing-filename
	(let ((path (catch 'foo
		      (dolist (regexp my-file-regexps)
			(when (looking-back regexp (point-at-bol))
			  (throw 'foo (or (match-string-no-properties 1) ""))))
		      "")))
	  (let* ((file (or (file-name-nondirectory path) ""))
		 (dir  (or (file-name-directory path) "./"))
		 (candlist (file-name-all-completions file dir))
		 (selection (completing-read
			     (format "Complete in %s/: "
				     (directory-file-name dir))
			     candlist nil t file)))

	    (delete-region (- (point) (length file)) (point))
	    (insert selection)

	    (setq my-completing-filename (eq my-completing-filename 'continue)))))

    (setq my-completing-filename nil)))

(evil-global-set-key 'insert (kbd "C-x C-f") 'my-complete-filename)

(define-key vertico-map (kbd "C-x C-f") ;; keep completing into directory
  (lambda ()
    (interactive)
    (when my-completing-filename
      (setq my-completing-filename 'continue)
      (vertico-exit))))

;; ----------------------------------------------------------------------------
;* Helm Ag and Occur
;; ----------------------------------------------------------------------------

(require 'helm-ag)
(require 'helm-occur)

;; display relative paths in grep results
(setq-default helm-grep-file-path-style 'relative)
(setq-default helm-ag-use-grep-ignore-list t)
(setq-default helm-ag-ignore-patterns '("build/"))
(setq-default helm-ag-base-command "ag --nocolor --nogroup --ignore-case --hidden")

(defun my-ag ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'helm-ag)))

(defun my-search-project ()
  (interactive)
  (if (file-remote-p default-directory)
      (helm-ag-project-root)
    (helm-do-ag-project-root)))

(global-set-key (kbd "C-c s") 'my-ag)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c r") 'my-search-project)
(evil-leader/set-key "r" 'my-search-project)
;; M-n grabs symbol under point. swiper slow to start on 3K line buffer
(evil-leader/set-key "o" 'helm-occur)

;;; C-c C-e in helm-ag to enter editable mode
(defun my-after-helm-ag-edit (&rest args)
  (my-syntax-entry)
  (evil-local-mode))
(advice-add #'helm-ag--edit :after #'my-after-helm-ag-edit)

(defun my-toggle-symbol-boundary (start end start-regex)
  "insert/remove start/end around search term for word/symbol boundary"
  (let (remove)
    (when (not (thing-at-point 'line))	; empty
      (next-history-element 1)
      (end-of-line))
    (save-excursion
      (beginning-of-line)
      (setq remove (looking-at start-regex)))

    (save-excursion
      (beginning-of-line)
      (if remove
	  (delete-char (length start))
	(insert start)))
    (save-excursion
      (end-of-line)
      (if remove
	  (delete-char (- (length end)))
	(insert end)))))

(define-key helm-ag-map (kbd "M-w")
  (lambda ()
    (interactive)
    (my-toggle-symbol-boundary "\\b" "\\b" "\\\\b")))

(define-key helm-ag-map (kbd "C-c C-o") (kbd "C-c o"))

;;; leave the highlight for occur
(setq next-error-highlight-no-select t)

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

;; ----------------------------------------------------------------------------
;* Imenu
;; ----------------------------------------------------------------------------

(defun my-imenu ()
  (interactive)
  (let ((f (buffer-file-name)))
    (if (and f (file-equal-p f user-init-file))
	(helm-multi-occur-1 (list (current-buffer)) "^;\\* ")
      (helm-imenu))))

(global-set-key (kbd "C-c i") 'my-imenu)
(evil-leader/set-key "i" 'my-imenu)

;; ----------------------------------------------------------------------------
;* Projects
;; ----------------------------------------------------------------------------

(defun my-find-project-root ()
  (let* ((dir (if (version< emacs-version "28.1")
		  (locate-dominating-file default-directory ".git")
		(project-root (project-current t)))))
    (or (and dir (expand-file-name (file-name-as-directory dir)))
	default-directory)))

(defvar my-override-initial-input)

(defun my-complete-with-initial-input (func &rest args)
  "Override the initial-input argument to completing-read"
  (pcase-let ((`(,prompt ,collection ,predicate ,require-match) args))
    (funcall func prompt collection predicate require-match
	   my-override-initial-input)))

;; C-u opens in other window
(defun my-find-file-in-project (&optional open-in-other-window initial-input)
  (interactive "P")
  (let ((read-file-name-completion-ignore-case t))
    (if open-in-other-window
	(let* ((switch-to-buffer-obey-display-actions t)
	       (display-buffer-overriding-action '((display-buffer-pop-up-window)
						   (inhibit-same-window . t))))
	  (if initial-input
	      ;; override initial-input arg to completing-read
	      (let ((my-override-initial-input initial-input))
		(advice-add 'completing-read :around 'my-complete-with-initial-input)
		(unwind-protect
		    (call-interactively 'project-find-file)
		  (advice-remove 'completing-read 'my-complete-with-initial-input)))

	    (call-interactively 'project-find-file)))

      (call-interactively 'project-find-file))))

(defun my-find-file-in-project-other-window ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'my-find-file-in-project)))

(defun my-jump-project-dired ()
  (interactive)
  (dired (my-find-project-root)))


(defvar my-projects)
(if (eq system-type 'darwin)
    (setq my-projects '(("~/dev" . 2)))
  (setq my-projects '(("~/dev/git" . 3))))
(dolist (d '("~/dotfiles-public" "~/dotfiles" "~/notefiles" "~/.emacs.d/elpa"))
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

(defun my-choose-project-and-search ()
  (interactive)
  (my-choose-project-and-invoke #'my-search-project))

(defun my-choose-project-and-magit ()
  (interactive)
  (my-choose-project-and-invoke #'magit))

(defun my-choose-project-and-dired ()
  (interactive)
  (my-choose-project (lambda (path)
		       (dired path)
		       (pwd))))

(global-set-key (kbd "C-c p e") #'my-choose-project-and-find-file)
(global-set-key (kbd "C-c p u") #'my-choose-project-and-find-file-other-window)
(global-set-key (kbd "C-c p r") #'my-choose-project-and-search)
(global-set-key (kbd "C-c p s") #'my-choose-project-and-search)
(global-set-key (kbd "C-c p m") #'my-choose-project-and-magit)
(global-set-key (kbd "C-c p d") #'my-choose-project-and-dired)

(global-set-key (kbd "C-c e") 'my-find-file-in-project)
(global-set-key (kbd "C-c d") 'my-jump-project-dired)

(global-set-key (kbd "C-c X") (lambda ()
				(interactive)
				(let ((default-directory "~/dotfiles-public"))
				  (my-find-file-in-project))))
(global-set-key (kbd "C-c x") (lambda ()
				(interactive)
				(find-file user-init-file)))
(global-set-key (kbd "C-c z") (lambda ()
				(interactive)
				(magit "~/dotfiles-public")))
(global-set-key (kbd "C-c n") (lambda ()
				(interactive)
				(let ((default-directory "~/notefiles"))
				  (my-find-file-in-project))))

(evil-leader/set-key "e" 'my-find-file-in-project)
(evil-leader/set-key "u" 'my-find-file-in-project-other-window)

;; ----------------------------------------------------------------------------
;* Isearch
;; ----------------------------------------------------------------------------

(defun my-isearch-yank-region ()
  "Use region as search pattern"
  (let ((search (buffer-substring-no-properties
		 (region-beginning)
		 (region-end)))
	(isearch-case-fold-search nil))
    (setq deactivate-mark t)
    (isearch-yank-string search)))

;; C-w yanks region if active, otherwise default behaviour (word)
(define-key isearch-mode-map (kbd "C-w")
  (lambda ()
    (interactive)
    (if (use-region-p)
	(my-isearch-yank-region)
      (if (version< emacs-version "27.1")
	  (isearch-yank-word-or-char)
	(isearch-yank-word-or-char 1)))))

(defun my-isearch-remove-failed-part ()
  "Remove failed part of search string, or last char if successful."
  (interactive)
  (if isearch-regexp
      (isearch-delete-char)
    (if (equal isearch-string "")
	(isearch-update)
      (if isearch-success
	  (isearch-delete-char)
	(while (isearch-fail-pos) (isearch-pop-state)))
      (isearch-update))))
(define-key isearch-mode-map (kbd "DEL") 'my-isearch-remove-failed-part)

;; space in search is a wildcard. 'M-s space' to toggle
(setq search-whitespace-regexp ".*"
      isearch-lax-whitespace t
      isearch-regexp-lax-whitespace nil)
;; stop downcasing when symbol searching with M-s .
(setq search-upper-case t)

;; ----------------------------------------------------------------------------
;* Yasnippet
;; ----------------------------------------------------------------------------

(require 'yasnippet)
(setq-default yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-reload-all)
(evil-global-set-key 'insert (kbd "C-]") #'yas-expand)

;; ----------------------------------------------------------------------------
;* Dired
;; ----------------------------------------------------------------------------

(require 'dired)
(require 'dired-x)
(setq dired-dwim-target t)

;; kill dired buffer instead of burying it
(define-key dired-mode-map (kbd "q")
  (lambda ()
    (interactive)
    (quit-window t)))

(define-key dired-mode-map (kbd "j") 'dired-next-line)
(define-key dired-mode-map (kbd "k") 'dired-previous-line)
(define-key dired-mode-map (kbd "J") 'dired-goto-file)
(define-key dired-mode-map (kbd "K") 'dired-do-kill-lines)
(define-key dired-mode-map (kbd "SPC") evil-leader--default-map)
(define-key dired-mode-map (kbd "C-w") 'evil-window-map)

;; make q work when viewing a file with v
(defun my-view-mode-hook ()
  (evil-emacs-state))

(add-hook 'view-mode-hook 'my-view-mode-hook)

(setq-default dired-listing-switches "-alh") ;; human-readable file sizes
(when (eq system-type 'darwin)
 (setq dired-guess-shell-alist-user '(("" "open"))))

(setq-default image-dired-dir "/tmp/image-dired") ; where to store thumbnails
(setq-default image-dired-thumb-width 200)
(setq-default image-dired-thumb-height 200)

(with-eval-after-load "image-dired"
  ;; show full size
  (define-key image-dired-thumbnail-mode-map (kbd "C-<return>")
    (lambda ()
      (interactive)
      (let ((current-prefix-arg 4)) ;; emulate C-u
	(call-interactively 'image-dired-display-thumbnail-original-image)))))

(global-set-key (kbd "C-x C-j") 'dired-jump)

;;; stop opening multiple image buffers
(push '((lambda (buf actions)
	  (eq 'image-mode (with-current-buffer buf major-mode)))
        (display-buffer-reuse-window display-buffer-use-some-window))
      display-buffer-alist)

;; put the thumbnail buffer at the bottom
(push '((lambda (buf actions)
	  (eq 'image-dired-thumbnail-mode (with-current-buffer buf major-mode)))
        (display-buffer-reuse-window display-buffer-at-bottom)
        (window-height . 10))
      display-buffer-alist)

;; ----------------------------------------------------------------------------
;* Magit
;; ----------------------------------------------------------------------------

(defun my-magit-hook ()
  (evil-local-mode 1))

(defun my-magit-repolist-hook ()
  (evil-local-mode 1)
  (evil-local-set-key 'normal (kbd "q") 'quit-window)
  (beginning-of-buffer))

(defun my-magit-list-repos ()
  (interactive)
  (other-window 1)
  (setq magit-repository-directories '())
  (dolist (proj (my-list-repos))
    (let ((dir (cdr proj)))
      (push `(,dir . 0) magit-repository-directories)))
  (magit-list-repositories))

(with-eval-after-load 'magit

  (evil-collection-magit-setup)

  (add-hook 'magit-mode-hook #'my-magit-hook)

  (advice-add 'git-commit-mode :after 'evil-insert-state)

  (setq magit-delete-by-moving-to-trash nil)

  (evil-define-key 'normal magit-mode-map (kbd "<escape>") nil) ;; stop escape burying buffer
  (evil-define-key 'normal magit-mode-map (kbd "C-j") nil)
  (evil-define-key 'normal magit-mode-map (kbd "C-k") nil)
  (evil-define-key 'normal magit-mode-map (kbd "C-p") (kbd "p"))
  (evil-define-key 'normal magit-mode-map (kbd "C-n") (kbd "n"))
  (evil-define-key 'normal magit-mode-map (kbd "p") 'magit-section-backward)
  (evil-define-key 'normal magit-mode-map (kbd "n") 'magit-section-forward))

(require 'magit)

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

(evil-leader/set-key "v" 'magit-status)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c M") #'my-magit-list-repos)

;; ----------------------------------------------------------------------------
;* Ediff
;; ----------------------------------------------------------------------------

(setq-default ediff-custom-diff-options "-u")

(defun my-diff-mode-hook ()
  ;; stop overriding new window switch key
  (define-key diff-mode-map (kbd "M-o") nil)
  (define-key diff-mode-map (kbd "M-SPC") nil))

(add-hook 'diff-mode-hook #'my-diff-mode-hook)

;; ----------------------------------------------------------------------------
;* Compilation
;; ----------------------------------------------------------------------------

(setq-default compilation-ask-about-save nil)
(setq-default compilation-scroll-output t)
(setq-default compilation-skip-threshold 2) ;; skip warnings

(defun my-compilation-mode-hook ()
  (visual-line-mode)
  (evil-local-mode)
  (setq-local split-width-threshold 1000)
  (evil-local-set-key 'normal (kbd "q") 'quit-window))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun my-jump-compilation ()
  (interactive)
  (let ((w (get-buffer-window "*compilation*")))
      (if w
	  (select-window w)
	(switch-to-buffer "*compilation*"))))

(defun my-compile-project ()
  (interactive)
  (let ((d (my-find-project-root)))
    (if d
	(let ((default-directory d))
	  (call-interactively 'compile))
      (message "Not in a git repo"))))

(defvar my-compile-key "C-c C-SPC")
(global-set-key (kbd my-compile-key) #'my-compile-project)
(global-set-key (kbd "C-c C-,") #'recompile)
(global-set-key (kbd "C-c ,") #'recompile)
(global-set-key (kbd "C-c g") #'my-jump-compilation)
(define-key compilation-mode-map (kbd "SPC") evil-leader--default-map)
(define-key compilation-mode-map (kbd "C-w") 'evil-window-map)
(define-key compilation-mode-map (kbd "?") nil)
(define-key compilation-mode-map (kbd "h") nil)
(define-key compilation-mode-map (kbd "g") nil)
(define-key compilation-mode-map (kbd "0") 'evil-beginning-of-line)

;; ----------------------------------------------------------------------------
;* Makefile
;; ----------------------------------------------------------------------------

(defun my-makefile-hook ()
  (my-syntax-entry)
  (local-set-key (kbd my-compile-key) #'my-compile-project))
(add-hook 'makefile-mode-hook 'my-makefile-hook)

(defun my-makefile-no-warn-suspicious-lines ())
(advice-add 'makefile-warn-suspicious-lines :override #'my-makefile-no-warn-suspicious-lines)

(defun my-cmake-hook ()
  (my-syntax-entry))
(with-eval-after-load "cmake-mode"
  (add-hook 'cmake-mode-hook 'my-cmake-hook))

(defun my-log-settings ()
  (my-syntax-entry)
  (toggle-truncate-lines 0))
(add-to-list 'auto-mode-alist '("\\.log\\'" . my-log-settings))

;; ----------------------------------------------------------------------------
;* Yaml
;; ----------------------------------------------------------------------------

(defun my-yaml-hook ()
  (auto-fill-mode -1)
  (setq-local evil-shift-width 2)
  ;; = is punctuation
  (modify-syntax-entry ?= "."))

(add-hook 'yaml-mode-hook 'my-yaml-hook)

;; ----------------------------------------------------------------------------
;* Shell
;; ----------------------------------------------------------------------------

(defun my-spawn-shell ()
  (interactive)
  (let* ((dir        (directory-file-name (expand-file-name default-directory)))
	 (name       (file-name-nondirectory dir))
	 (currentbuf (get-buffer-window (current-buffer)))
	 (newbuf     (generate-new-buffer-name
		      (if (or (string-empty-p name)
			      (string= dir (expand-file-name "~")))
			  "*shell*"
			(format "*shell:%s*" name)))))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

(defun my-split-shell ()
  (interactive)
  (let ((evil-split-window-below t))
    (evil-window-split))
  (my-spawn-shell))

(setq comint-prompt-read-only t)
(define-key shell-mode-map (kbd "M-_") 'comint-insert-previous-argument)
(define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
(define-key shell-mode-map (kbd "SPC") 'comint-magic-space)
(define-key shell-mode-map (kbd "C-c C-l") 'comint-clear-buffer)
(define-key comint-mode-map (kbd "C-r") 'comint-history-isearch-backward)
(define-key comint-mode-map (kbd "M-r") 'move-to-window-line-top-bottom)

;; When the cursor is in the middle of the shell output, stop the
;; return key from pasting the whole lot back and executing it
(define-key comint-mode-map
  (kbd (if (display-graphic-p) "<return>" "RET"))
  (lambda ()
    (interactive)
    (if (comint-after-pmark-p)
	(comint-send-input)
      (message "Point is before process mark, NOT sending"))))

(defun my-shell-ctrl-d ()
  "first C-d ends the shell, second C-d deletes the buffer and
  closes the window"
  (interactive)
  (if (get-buffer-process (current-buffer))
      (progn
	;; Fix "shell-apply-ansi-color: Text is read-only" error
	(advice-add 'comint-send-eof :override #'process-send-eof)
	(unwind-protect
	    (comint-delchar-or-maybe-eof 1)
	  (advice-remove 'comint-send-eof #'process-send-eof)))
    (kill-buffer)
    (if (> (count-windows) 2)
	(delete-window))
    (other-window 1)))

(defun my-shell-hook ()
  (fancy-dabbrev-mode -1)
  (visual-line-mode 0)
  (toggle-truncate-lines 0)
  (define-key shell-mode-map (kbd "C-d") #'my-shell-ctrl-d)

  ;; fill out longest common part of filename first
  (setq-local completion-styles '(emacs21 flex))

  ;; don't ignore .git, etc
  (setq-local completion-ignored-extensions nil
	      completion-ignore-case nil))

(add-hook 'shell-mode-hook 'my-shell-hook)
(evil-set-initial-state 'shell-mode 'emacs)

(add-hook 'sh-mode-hook 'my-syntax-entry)

(global-set-key (kbd "C-c v") 'shell)
(global-set-key (kbd "C-c V") 'my-split-shell)

;; ----------------------------------------------------------------------------
;* Term
;; ----------------------------------------------------------------------------

(if (and (display-graphic-p)
	 (require 'vterm "vterm" t))
    (global-set-key (kbd "C-c t") 'vterm)
  (global-set-key (kbd "C-c t") 'ansi-term))

(defun expose-global-binding-in-term (binding)
  (define-key term-raw-map binding
    (lookup-key (current-global-map) binding)))
(with-eval-after-load 'term
  (expose-global-binding-in-term (kbd "M-o")))

;; ----------------------------------------------------------------------------
;* Eshell
;; ----------------------------------------------------------------------------

(defun my-eshell-last-arg ()
  "Insert last argument of previous command"
  (interactive)
  (insert (car (last (split-string-shell-command (eshell-previous-input-string 0))))))

(defun my-eshell-hook ()
  (fancy-dabbrev-mode -1)
  (visual-line-mode 0)
  (toggle-truncate-lines 1)
  (define-key eshell-hist-mode-map (kbd "M-r") #'move-to-window-line-top-bottom)
  (define-key eshell-hist-mode-map (kbd "C-r") #'helm-eshell-history)
  (define-key eshell-hist-mode-map (kbd "C-c C-l") #'eshell/clear)
  (define-key eshell-mode-map (kbd "M-m") 'eshell-bol)
  (local-set-key (kbd "M-_") 'my-eshell-last-arg))

(add-hook 'eshell-mode-hook 'my-eshell-hook)

(global-set-key (kbd "C-c b") 'eshell)

;; ----------------------------------------------------------------------------
;* Tags
;; ----------------------------------------------------------------------------

(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

(evil-global-set-key 'normal (kbd "C-w .") (kbd "C-x 4 ."))
(evil-global-set-key 'normal (kbd "C-w C-.") (kbd "C-x 4 ."))

(defun my-rebuild-and-load-tags (&optional one-project)
  "Find a TAGS file above the default-directory, invoke make TAGS
in that directory. If run with a prefix arg, generate tags in the
current project instead. Visit the tags file."
  (interactive "P")
  (if one-project
      (let ((proj (my-find-project-root))
	    (ctags (if (eq system-type 'darwin) "uctags" "ctags")))
	(when (y-or-n-p (format "Run %s in %s?" ctags proj))
	  (message (format "Running %s in %s" ctags proj))
	  (when (= 0 (shell-command
		      (format "cd \"%s\" && %s -R -e -f TAGS --exclude=.git --exclude=build . > /dev/null"
			      proj ctags)))
	    (visit-tags-table (concat proj "TAGS")))))

    (let* ((dir (locate-dominating-file default-directory "TAGS"))
	   (path (and dir (expand-file-name (file-name-as-directory dir)))))
      (if (not path)
	  (message (format "No existing TAGS file found above %s" default-directory))
	(when (y-or-n-p (format "Run make TAGS in %s" path))
	  (message (format "Running make -C %s TAGS" path))
	  (call-process "make" nil nil nil "-C" path "TAGS")
	  (visit-tags-table (concat path "TAGS")))))))

(global-set-key (kbd "C-c C-]") #'my-rebuild-and-load-tags)

;; ----------------------------------------------------------------------------
;* Lisp
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

(defun my-lisp-common-hook ()
  (enable-paredit-mode)
  (evil-local-mode 1))

(add-hook 'emacs-lisp-mode-hook       'my-lisp-common-hook 'append)
(add-hook 'lisp-mode-hook             'my-lisp-common-hook 'append)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-common-hook 'append)
(add-hook 'scheme-mode-hook           'my-lisp-common-hook 'append)

(advice-add 'paredit-comment-dwim :after 'my-advise-comment)

;; ----------------------------------------------------------------------------
;* Python
;; ----------------------------------------------------------------------------

(defvar my-python-interp "python3")
(setq-default python-shell-interpreter my-python-interp)

(defun my-python-shell-mode-hook ()
  (toggle-truncate-lines 0)
  (fancy-dabbrev-mode -1))

(defun my-python-mode-hook ()
  (my-syntax-entry)
  ;; (anaconda-mode)
  ;; (anaconda-eldoc-mode)
  (setq-local tab-width 4)
  (setq-local evil-shift-width 4))

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'inferior-python-mode-hook 'my-python-shell-mode-hook)

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-a") 'pyvenv-activate))

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
;* Eglot
;; ----------------------------------------------------------------------------

(require 'eglot)

(add-to-list 'eglot-server-programs '(python-mode . ("pylsp" "-v")))

(setq eglot-autoshutdown t)

;; ----------------------------------------------------------------------------
;* C++
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

(evil-leader/set-key
  "0" (lambda () (interactive) (my-wrap-if-endif 0))
  "1" (lambda () (interactive) (my-wrap-if-endif 1))
  "3" (lambda () (interactive) (my-wrap-if-endif 1))
  "2" (lambda () (interactive) (my-wrap-if-endif 1 t)))

(defun my-jump-to-header ()
  (interactive)
  (let ((fn (buffer-file-name)))
    (if (not fn)
	(message "Buffer has no filename")
      (let ((ext (file-name-extension fn)))
	(my-find-file-in-project t (concat (file-name-base fn) "."
					   (if (string= ext "h") "c" "h")))
	(message (buffer-file-name))))))

(evil-leader/set-key "h" #'my-jump-to-header)

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

(setq-default c-default-style "my-c-style")
(setq-default c-tab-always-indent t)


;;grey out between #if 0 #endif
;;https://stackoverflow.com/questions/7189742/c-c-mode-in-emacs-change-color-of-code-within-if-0-endif-block
(defun cpp-highlight-if-0/1 ()
  "Modify the face of text in between #if 0 ... #endif."
  (interactive)
  (setq cpp-known-face '(foreground-color . "#686858"))
  (setq cpp-unknown-face 'default)
  (setq cpp-face-type 'dark)
  (setq cpp-known-writable 't)
  (setq cpp-unknown-writable 't)
  (setq cpp-edit-list
	'((#("1" 0 1
	     (fontified nil))
	   nil
	   (foreground-color . "#686858")
	   both nil)
	  (#("0" 0 1
	     (fontified nil))
	   (foreground-color . "#686868")
	   nil
	   both nil)))
  (cpp-highlight-buffer t))

(defun my-c-cpp-settings()
  (local-set-key (kbd my-compile-key) #'my-compile-project)
  (evil-local-set-key 'normal (kbd "[#") 'c-up-conditional)
  (local-set-key (kbd "TAB") #'fancy-dabbrev-expand-or-indent)
  ;; don't want c-submit-bug-report
  (local-set-key (kbd "C-c C-b") nil)
  (auto-fill-mode -1)
  (setq-local fill-column 80)
  (cpp-highlight-if-0/1)
  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)
  (yas-minor-mode))

(defvar my-cc-path)

(defun my-cpp-mode-hook ()
  (my-c-cpp-settings)
  (make-local-variable 'ffap-c++-path)
  (when (bound-and-true-p my-cc-path)
    (dolist (x my-cc-path)
      (add-to-list 'ffap-c++-path x)))
  (vc-refresh-state)
  (add-to-list 'ffap-c++-path (my-find-project-root)))

(defun my-c-mode-hook ()
  (my-c-cpp-settings)
  (make-local-variable 'ffap-c-path)
  (when (bound-and-true-p my-cc-path)
    (dolist (x my-cc-path)
      (add-to-list 'ffap-c-path x)))
  (vc-refresh-state)
  (add-to-list 'ffap-c-path (my-find-project-root)))

(add-hook 'c++-mode-hook 'my-cpp-mode-hook t)
(add-hook 'c-mode-hook 'my-c-mode-hook t)

;; 4 character tabs or spaces for some projects
(dir-locals-set-class-variables 'fourchartabs
				'((c-mode . ((tab-width . 4)
					     (evil-shift-width . 4)))
				  (c++-mode . ((tab-width . 4)
					       (evil-shift-width . 4)))))
(dir-locals-set-class-variables 'fourspaces
				'((c-mode . ((tab-width . 4)
					     (evil-shift-width . 4)
					     (indent-tabs-mode . nil)))
				  (c++-mode . ((tab-width . 4)
					       (evil-shift-width . 4)
					       (indent-tabs-mode . nil)))))

;; ----------------------------------------------------------------------------
;* Rust
;; ----------------------------------------------------------------------------

(defun my-rust-mode-hook ()
  (auto-fill-mode -1)
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 4))

(add-hook 'rust-mode-hook 'my-rust-mode-hook)

;; ----------------------------------------------------------------------------
;* Maya, Houdini, Arnold
;; ----------------------------------------------------------------------------

;; maya mel
(when (file-directory-p "~/dev/ermine/emacs")
  (add-to-list 'load-path "~/dev/ermine/emacs")
  (require 'ermine))

;; houdini vex
(add-to-list 'auto-mode-alist '("\\.vex\\'" . c-mode))

;;; arnold
(defun my-arnold-settings ()
  (set (make-local-variable 'comment-start) "#"))
(add-to-list 'auto-mode-alist '("\\.ass\\'" . my-arnold-settings))

;; ----------------------------------------------------------------------------
;* Debug, gdb
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

  (set-window-dedicated-p (selected-window) nil) ;; unset dedicate state if needed
  (switch-to-buffer gud-comint-buffer)
  (delete-other-windows)

  (let* ((w-source (selected-window))						     ;; left top
	 (w-gdb (split-window w-source nil 'right))				     ;; right bottom
	 (w-locals (split-window w-gdb nil 'above))				     ;; right middle bottom
	 (w-stack (split-window w-locals nil 'above))				     ;; right middle top
	 ;; (w-breakpoints (split-window w-stack nil 'above))			     ;; right top
	 (w-io (split-window w-source (floor (* 0.66 (window-body-height))) 'below)) ;; left bottom
	 )

    (set-window-buffer w-io (or (get-buffer "*compilation*")
				(get-buffer "*shell*")
				(gdb-get-buffer-create 'gdb-inferior-io)))
    ;; (set-window-buffer w-breakpoints (gdb-get-buffer-create 'gdb-breakpoints-buffer))
    (set-window-buffer w-locals (gdb-get-buffer-create 'gdb-locals-buffer))
    (set-window-buffer w-stack (gdb-get-buffer-create 'gdb-stack-buffer))
    (set-window-buffer w-gdb gud-comint-buffer)

    (set-window-dedicated-p w-io t)
    ;; (set-window-dedicated-p w-breakpoints t)
    (set-window-dedicated-p w-locals t)
    (set-window-dedicated-p w-stack t)

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
;* Colours and splash screen
;; ----------------------------------------------------------------------------

(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(blink-cursor-mode 0)
(setq-default cursor-type 'box)

;; (defun my-highlight-yanked-region (orig-fn beg end &rest args)
;;   "Highlight yanked region. https://blog.meain.io/2020/emacs-highlight-yanked/"
;;   (let ((pulse-delay 0.2)
;; 	(pulse-iterations 1))
;;     (pulse-momentary-highlight-region beg end 'highlight))
;;   (apply orig-fn beg end args))
;; (advice-add 'evil-yank :around #'my-highlight-yanked-region)

(defun my-window-setup-hook ()
  (when (and (display-graphic-p)
	     (file-exists-p "~/Pictures/splash"))
    (require 'seq)
    (setq fancy-splash-image
	  (let ((choices (seq-filter
			  (lambda (fn)
			    (let ((ext (file-name-extension fn)))
			      (or (string= ext "jpg")
				  (string= ext "jpeg")
				  (string= ext "png"))))
			  (directory-files "~/Pictures/splash"
					   t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))))
	    (elt choices (random (length choices))))))
  (unless (display-graphic-p)
    ;; see terminal background colour/image
    (set-face-background 'default "unspecified-bg" (selected-frame))))

(add-hook 'window-setup-hook 'my-window-setup-hook)

(with-eval-after-load "evil-leader"
  (define-key splash-screen-keymap (kbd "SPC") evil-leader--default-map))

(defun my-theme-dark (x)
  (cond
   (x
    (require 'reykjavik-theme)
    (load-theme 'reykjavik)
    (load-theme 'my-override-dark))
   (t
    (load-theme 'ef-night)
    (load-theme 'my-override-dark2)))

  (set-cursor-color "white")
  (setq evil-normal-state-cursor '(box "white"))
  (setq evil-insert-state-cursor '(box "orange")))

(defun my-theme-light (x)
  (cond
   (x
    (require 'soft-morning-theme)
    (load-theme 'soft-morning))
   (t
    (load-theme 'ef-day)))

  (load-theme 'my-override-light)
  (setq evil-normal-state-cursor '(box "black"))
  (setq evil-insert-state-cursor '(box "orange")))

(defun my-color-theme-toggle (dark)
  (interactive)
  (let ((x (or (memq 'soft-morning custom-enabled-themes)
	       (memq 'reykjavik custom-enabled-themes))))
    (mapcar #'disable-theme custom-enabled-themes)
    (if dark
	(my-theme-dark (not x))
      (my-theme-light (not x)))))

(global-set-key (kbd "<f5>") (lambda () (interactive) (my-color-theme-toggle t)))
(global-set-key (kbd "<f6>") (lambda () (interactive) (my-color-theme-toggle nil)))

;; ----------------------------------------------------------------------------
;* Font
;; ----------------------------------------------------------------------------

;; linux: install to ~/.fonts/  then fc-cache -v ~/.fonts
(when (and (eq system-type 'gnu/linux)
	   (display-graphic-p))

  (defun my-font-config ()
    (interactive)

    (set-face-attribute 'default nil :font "Menlo:pixelsize=14:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")

    (if (string= (string-trim (shell-command-to-string
			       "xrandr | awk '/^HDMI-1/{print $2}'"))
		 "connected")
	;; external monitor
	(set-face-attribute 'default nil :height 105)
      ;; laptop screen
      (set-face-attribute 'default nil :height 110)))

  (my-font-config)
  (global-set-key (kbd "C-c F") #'my-font-config))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 150))

;; ----------------------------------------------------------------------------
;* Menu toolbar
;; ----------------------------------------------------------------------------

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))

;; ----------------------------------------------------------------------------
;* Email
;; ----------------------------------------------------------------------------

(defun my-scratch-mail-buffer ()
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (setq buffer-offer-save t)
    (message-mode)))

(defun my-message-mode-hook ()
  (my-syntax-entry)
  ;; = is punctuation, so evil * works on key and val separately for key=val
  (modify-syntax-entry ?= ".")
  (setq-local fill-column 72)
  ;; stop paragraph lines after the first being extra indented by M-q
  (setq-local fill-paragraph-function nil)
  (define-key message-mode-map (kbd "C-c C-c") nil)
  (define-key message-mode-map (kbd "C-c C-s") nil))

(add-hook 'message-mode-hook 'my-message-mode-hook)

(setq-default message-auto-save-directory nil)

(setq compose-mail-user-agent-warnings nil)

(global-set-key (kbd "C-x m") #'my-scratch-mail-buffer)

;; ----------------------------------------------------------------------------
;* Music
;; ----------------------------------------------------------------------------

(when (or (eq system-type 'darwin)
	  (and (eq system-type 'gnu/linux)
	       (display-graphic-p)
	       (string= (string-trim (shell-command-to-string "lsb_release -i -s")) "Ubuntu")))

  (require 'bongo) ;; need this before opening a playlist

  (evil-leader/set-key "b" #'bongo)

  (setq bongo-logo nil)
  (setq bongo-display-track-icons nil)
  ;; seek doesn't work reliably with vlc backend
  ;; need at least mplayer 1.5 on macOS 11 to fix echoing
  ;;	brew install mplayer
  ;; mplayer volume resets to max after seeking
  (setq bongo-enabled-backends '(vlc))
  (setq bongo-vlc-program-name (if (eq system-type 'darwin)
				   "/Applications/VLC.app/Contents/MacOS/VLC"
				 "/usr/bin/vlc"))

  (defun my-bongo-mode-hook ()
    (define-key bongo-mode-map (kbd "SPC") evil-leader--default-map)
    (define-key bongo-mode-map (kbd "z") (kbd "C-c C-p"))
    (define-key bongo-mode-map (kbd "x") 'bongo-start/stop)
    (define-key bongo-mode-map (kbd "c") 'bongo-pause/resume)
    (global-set-key (kbd "<f7>") 'bongo-play-previous)
    (global-set-key (kbd "<f8>") 'bongo-pause/resume)
    (global-set-key (kbd "<f9>") 'bongo-play-next)
    (define-key bongo-mode-map (kbd "b") (kbd "C-c C-n"))
    (define-key bongo-mode-map (kbd "f") nil)
    (define-key bongo-mode-map (kbd "j") (kbd "n"))
    (define-key bongo-mode-map (kbd "k") (kbd "p"))
    (define-key bongo-mode-map (kbd "o") 'bongo-switch-buffers)
    (define-key bongo-mode-map (kbd "O") 'bongo-list-buffers)
    (define-key bongo-mode-map (kbd "V") 'bongo-seek-backward-60)
    (define-key bongo-mode-map (kbd "h") 'bongo-seek-backward-10)
    (define-key bongo-mode-map (kbd "H") 'bongo-seek-backward-3)
    (define-key bongo-mode-map (kbd "L") 'bongo-seek-forward-3)
    (define-key bongo-mode-map (kbd "l") 'bongo-seek-forward-10)
    (define-key bongo-mode-map (kbd "v") 'bongo-seek-forward-60)
    (define-key bongo-mode-map (kbd ";") 'bongo-recenter)

    (bongo-mode-line-indicator-mode -1))

  (defun my-bongo-set-volume ()
    (let ((process (get-process "bongo-mplayer")))
      (when process
	(process-send-string process "volume 5 1\n")))
    (let ((process (get-process "bongo-vlc")))
      (when process
	(process-send-string process "volume 175\n"))))

  (defun my-bongo-start-hook ()
    (when (get-process "bongo-vlc")
      (sit-for 0.2))
    (my-bongo-set-volume))

  (add-hook 'bongo-player-started-hook 'my-bongo-start-hook)
  (add-hook 'bongo-mode-hook 'my-bongo-mode-hook))

;; ----------------------------------------------------------------------------
;* Customs
;; ----------------------------------------------------------------------------

(load "~/dotfiles/init.el" t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("9ae0e81ced7c8d587cb1db9fcb528856315d352822082518884e8726fe681d1d" "601a9b9bf21f5c72ddfb28c7e95b842a5b0130f55ad5e0b97d2ba1e0b91b0a2c" "5062213da61cc87f29f4a117ecb4cfa85c9fe55a3281d6ba40246ca01fd08e1f" "b330e75c3717166db3a8a41eb180c705b46b97c584dffdb917b310f86025c811" "661d0eb8fa7cbfcc93b68047e7186caed97d2d48982ae5f1a938d31a55cd7df9" "062535e77813505521514d50abffc7ebc584e79004041be91c579a81fc7a3d35" "2fbb0cbdfff7c8a150b66f7537f9a23ad98107d98f2826c6c1dfaa2a3085c199" "a4853578a26cf51c2f70907c40597edf32a1914c58be345537ac3471544fc55d" "c730acdf0dfa1a919d7fe8cab427589f24150d5fa3715e205cb7bd94e0865dda" "6e995e092ec297b0919bc94b1bbf2936a668b5bbe7230dc3306bde69a6400aba" "ae1967910e7ce79efb0559b1f92e6249d3d0bb719d0335b24daf7ec861dbd0b1" "fb6ed924288a0bc476f21bab8f83dbc30e343833d8ad94a84e095a4b18ead1d5" "40798acf74d422d815f9c1d478440e01bc90d4971807d243767415bea128e71a" "0aebb395ae1889592594d1f5519f459dfb778304a8f2b7c4344f0c2e52b11c13" "44602e56a462a8c56597d87d57abe38948b3255ab1940ab2afd885309fad0436" "4ca16a4e1d915f78a5cd90b744b22dbc0507f62e8211724e9c7f86d672ac2df3" "6b0174fa9963275fd7f6181b0f706dc9546ede1df7361d410d9890652a148159" "9ce5b769ac6cf63c24e570c0092466db0ee473a7edfde1be518bf957322f738b" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "8efa3d21b3fa1ac084798fae4e89848ec26ae5c724b9417caf4922f4b2e31c2a" "fd1dd4d022ece05400c7bd1efc2ae5cca5cd64a53f3670da49d0c8f0ef41f4e3" "f0c94bf6a29c232300e46af50f46ce337e721eacca6d618e8654a263db5ecdbe" "621595cbf6c622556432e881945dda779528e48bb57107b65d428e61a8bb7955" "e6ccd0cc810aa6458391e95e4874942875252cd0342efd5a193de92bfbb6416b" "45f7fec480eb3bdf364cbfcbc8d11ed0228bcf586ce7370fc30a6ce5770f181a" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" default))
 '(dabbrev-backward-only t)
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-fold-search nil)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(eldoc-echo-area-use-multiline-p nil)
 '(eldoc-idle-delay 0.25)
 '(electric-indent-mode t)
 '(electric-pair-mode nil)
 '(evil-flash-delay 60)
 '(evil-motion-state-modes
   '(apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode dictionary-mode ert-results-mode help-mode Info-mode Man-mode speedbar-mode undo-tree-visualizer-mode view-mode woman-mode))
 '(helm-ag-insert-at-point 'symbol)
 '(helm-follow-mode-persistent t)
 '(helm-source-names-using-follow '("Imenu" "AG" "Helm occur"))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ispell-program-name "aspell")
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(magit-merge-arguments '("--no-ff"))
 '(magit-section-initial-visibility-alist '((stashes . show) (upstream . show)))
 '(magit-section-visibility-indicator '("" . t))
 '(magit-status-headers-hook
   '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-repo-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header magit-insert-tags-header))
 '(org-blank-before-new-entry '((heading . auto) (plain-list-item)))
 '(org-imenu-depth 3)
 '(org-refile-targets '((org-agenda-files :maxlevel . 2)))
 '(package-selected-packages
   '(anaconda-mode
     bongo
     cmake-mode
     consult
     consult-dir
     eglot
     embark
     evil
     evil-leader
     evil-collection
     evil-numbers
     fancy-dabbrev
     gandalf-theme
     gnuplot
     gnuplot-mode
     helm
     helm-ag
     magit
     marginalia
     markdown-mode
     orderless
     ox-pandoc
     paredit
     pyvenv
     reykjavik-theme
     rust-mode
     soft-morning-theme
     undo-tree
     vertico
     which-key
     yaml-mode
     yasnippet))
 '(read-quoted-char-radix 16)
 '(safe-local-variable-values
   '((tab-always-indent)
     (evil-input-method . swedish-postfix)
     (evil-input-method . german-postfix)
     (indent-tabs-mode nil)
     (evil-shift-width . 2)
     (evil-shift-width . 4)))
 '(tramp-ssh-controlmaster-options
   "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=60m" t)
 '(undo-tree-auto-save-history nil)
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-duplicate ((t (:background "Magenta" :foreground "white"))))
 '(flyspell-incorrect ((t (:background "red" :foreground "white"))))
 '(message-cited-text-1 ((t (:foreground "#878787"))))
 '(success ((t (:foreground "#00DD00" :weight bold)))))

(if (< (decoded-time-hour (decode-time)) 13)
    (my-theme-light t)
  (my-theme-dark t))
