;;; -*- mode: emacs-lisp; -*-

(let ((trustfile
       (cond
	((eq system-type 'gnu/linux)
	 (if (string-match "\.el[7-9]\." operating-system-release)
	     "/etc/pki/tls/certs/ca-bundle.crt"	   ;; centos
	   "/etc/ssl/certs/ca-certificates.crt"))  ;; ubuntu
	(t "/usr/local/etc/openssl/cert.pem"))))   ;; darwin
  (setq tls-checktrust t)
  (setq tls-program
	(list
	 (format "gnutls-cli --x509cafile %s -p %%p %%h" trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))
; https://glyph.twistedmatrix.com/2015/11/editor-malware.html
; http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html

(when (display-graphic-p)
  (add-hook 'after-init-hook
	    (lambda ()
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
		      (elt choices (random (length choices))))))))

;; ----------------------------------------------------------------------------
;* Package
;; ----------------------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; ----------------------------------------------------------------------------
;* Paths
;; ----------------------------------------------------------------------------

(when (display-graphic-p)
  (let ((path-from-shell (replace-regexp-in-string
			  "[[:space:]\n]*$" ""
			  (shell-command-to-string "$SHELL -l -c 'printenv PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

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

(defun my-find-file-hook ()
  (if (not (eq major-mode 'image-mode))
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

(defun my-advise-window-rotate ()
  (other-window 1))
(advice-add 'evil-window-rotate-downwards :after #'my-advise-window-rotate)

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

(defvar my-lang "en")

(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode t)

(show-paren-mode)
(setq-default
 show-paren-when-point-inside-paren t
 show-paren-when-point-in-periphery t)

(setq ring-bell-function 'ignore) ;; stop binging noise on C-g

(setq-default calendar-week-start-day 1) ;; start on monday
(setq-default vc-follow-symlinks t)
(setq-default backup-inhibited t)    ;; disable backup
(setq-default auto-save-default nil) ;; disable auto save

(recentf-mode 1)
(save-place-mode 1)
(setq save-place-forget-unreadable-files nil)

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
  (if (buffer-modified-p)
      (call-interactively 'kill-buffer)
    (kill-this-buffer)))

(defun my-close-other-window ()
  (interactive)
  (quit-window nil (next-window)))

(defun my-man-page-hook ()
  (evil-local-mode))

;; justinmk
(defun my-google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    (if (string= my-lang "se")
	"https://translate.google.com/?sl=sv&tl=en&op=translate&text="
	"https://www.google.com/search?ie=utf-8&oe=utf-8&q=")
    (url-hexify-string (if mark-active
                           (format (if (string= my-lang "se") "%s" "\"%s\"")
				   (buffer-substring (region-beginning) (region-end)))
                         (read-string "Search Google: "))))))

(evil-leader/set-key "s" #'my-substitute) ; substitute whole buffer
(evil-leader/set-key "S" ; substitute from current line to end of buffer
  (lambda ()
    (interactive)
    (my-substitute ".,$")))

(evil-leader/set-key "=" #'my-align-regexp)
(evil-leader/set-key "k" #'my-delete-trailing-whitespace)
(evil-leader/set-key "m" #'my-mirror-buffer)
(evil-leader/set-key "d" 'pwd)

(evil-global-set-key 'insert (kbd "C-x C-l") 'hippie-expand) ;; line completion like vim
(evil-global-set-key 'motion (kbd "K")
		     (if (eq system-type 'darwin)
			 ;; skip slow vertico minibuffer prompt for man pages
			 (lambda ()
			   (interactive)
			   (man (thing-at-point 'word t)))
		       'man))
(add-hook #'Man-mode-hook #'my-man-page-hook)

(evil-global-set-key 'motion (kbd "Q") #'my-close-other-window)
(global-set-key (kbd "C-c q") #'my-close-other-window)

(evil-global-set-key 'motion (kbd "C-w d") 'my-kill-buffer)
(evil-global-set-key 'motion (kbd "C-w C-d") 'my-kill-buffer)
(global-set-key (kbd "C-x k") 'my-kill-buffer)

(global-set-key (kbd "C-c %") #'my-copy-filename)
(global-set-key (kbd "C-c t") #'my-toggle-wrap)
(global-set-key (kbd "C-c b") 'bookmark-jump)
(evil-leader/set-key ";" (kbd "C-x C-;"))

(global-set-key (kbd "C-x C-h") (kbd "C-x h"))
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "M-o") (kbd "C-x o"))
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "M-=") 'count-words)
(global-set-key (kbd "M-'") #'delete-blank-lines)
(global-set-key (kbd "M-SPC") #'cycle-spacing)

(global-set-key (kbd "C-c w h") #'evil-window-move-far-left)
(global-set-key (kbd "C-c w l") #'evil-window-move-far-right)
(global-set-key (kbd "C-c w j") #'evil-window-move-very-bottom)
(global-set-key (kbd "C-c w k") #'evil-window-move-very-top)
(global-set-key (kbd "C-c w r") #'evil-window-rotate-downwards)

(global-unset-key (kbd "C-h h")) ;; stop accidentally opening hello file
(global-set-key (kbd "C-h C-c") nil) ;; disable describe-copying

(global-set-key (kbd "C-x g") #'my-google)

(define-key minibuffer-local-map (kbd "<escape>") 'abort-recursive-edit)

(put 'narrow-to-region 'disabled nil)

(defun my-help-mode-hook ()
  (evil-local-mode)
  (evil-local-set-key 'motion (kbd "TAB") #'forward-button))

(add-hook #'help-mode-hook #'my-help-mode-hook)

;; don't save context strings
(setq-default bookmark-make-record-function
      (lambda (&optional no-file no-context posn)
	(funcall 'bookmark-make-record-default no-file t posn)))

;; ----------------------------------------------------------------------------
;* Abbreviations
;; ----------------------------------------------------------------------------

(add-hook 'text-mode-hook #'abbrev-mode)
(define-global-abbrev "retrun" "return")
(define-global-abbrev "cosnt" "const")
(define-global-abbrev "conat" "const")
(define-global-abbrev "trinagle" "triangle")

;; ----------------------------------------------------------------------------
;* Fancy dabbrev
;; ----------------------------------------------------------------------------

(when (not (string-match "\.el[7-9]\." operating-system-release)) ;; centos

  (require 'fancy-dabbrev)

  (global-fancy-dabbrev-mode)

  (evil-global-set-key 'insert (kbd "TAB") 'fancy-dabbrev-expand-or-indent)
  (global-set-key (kbd "M-/") 'fancy-dabbrev-expand-or-indent)
  (global-set-key (kbd "<backtab>") 'fancy-dabbrev-backward)
  (setq-default fancy-dabbrev-menu-height 15)
  (setq-default fancy-dabbrev-preview-context 'everywhere)
  (setq-default fancy-dabbrev-preview-delay 0.1))

;; ----------------------------------------------------------------------------
;* Keyboard
;; ----------------------------------------------------------------------------

(defun my-char (en se)
  (interactive)
  (cond
   ((string= my-lang "se") (insert se))
   (t (insert en))))

(defun my-toggle-lang ()
  (interactive)
  (cond
   ((string= my-lang "se") (setq-local my-lang "en"))
   ((string= my-lang "en") (setq-local my-lang "se")))
  (message (format "Lang: %s" my-lang)))

(when (eq system-type 'darwin)
  ;; tilde in the same place as in US keyboard
  (keyboard-translate ?\§ ?\`)
  (keyboard-translate ?\± ?\~)
  ;; make ISO backtick escape
  ;; (keyboard-translate ?\` ?\)

  (setq-default mac-command-modifier 'meta)
  (setq-default mac-right-command-modifier 'control)
  (setq-default mac-option-modifier 'alt)

  (dolist (remap '(("[" . "å")
		   (";" . "ö")
		   ("]" . "ä")))
    (let ((en (car remap))
	  (se (cdr remap)))
      (evil-global-set-key 'insert (kbd en)
			   `(lambda () (interactive) (my-char ,en ,se)))
      (define-key minibuffer-local-map (kbd en)
	`(lambda () (interactive) (my-char ,en ,se)))))

  ;; shift+alt+space
  (global-set-key (kbd "A-SPC") 'my-toggle-lang))


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
;* Org
;; ----------------------------------------------------------------------------

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c k") 'org-capture)

(evil-leader/set-key "g" 'org-agenda-list)

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map "j" (kbd "n"))
  (define-key org-agenda-mode-map "k" (kbd "p"))
  (define-key org-agenda-mode-map (kbd "C-w") 'evil-window-map))

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

(org-babel-do-load-languages 'org-babel-load-languages '((shell . t)))

(setq org-agenda-custom-commands
      '(("d" "Done stuff" todo "DONE" )
	("n" "Agenda and all TODOs" ((agenda "") (alltodo "")))))
(setq org-capture-templates
      '(("t" "Task" entry (file+headline org-default-notes-file "Tasks")
	 "* TODO %?\n  SCHEDULED: %t\n  %U\n")
	("b" "Bookmark" entry (file+headline org-default-notes-file "Bookmarks")
	 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n")))

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

(add-to-list 'org-agenda-custom-commands
	     `("w" "Week"
	       agenda ""
	       ,(append my-org-agenda-common-review-settings
			'((org-agenda-span 'week)
			  (org-agenda-overriding-header "Week in Review")))
	       ("/tmp/week.html")))

(add-to-list 'org-agenda-custom-commands
	     `("W" "Last week"
	       agenda ""
	       ,(append my-org-agenda-common-review-settings
			'((org-agenda-span 'week)
			  (org-agenda-start-day "-1w")
			  (org-agenda-overriding-header "Last week in Review")))
	       ("/tmp/lastweek.html")))

(defun my-org-mode-hook ()
  ;; override the evil binding of C-i (jump forward), as C-i is the
  ;; same as tab in the terminal, which we want in org mode for
  ;; (un)collapsing headers
  (when (not (display-graphic-p))
    (evil-local-set-key 'motion (kbd "C-i") 'org-cycle))
  ;; / is punctuation, so evil * works on path components
  (modify-syntax-entry ?/ ".")
  (auto-fill-mode 1)
  (setq-local indent-tabs-mode nil)
  (setq-local evil-shift-width 2)
  (setq-local tab-width 2)
  (local-set-key (kbd "M-n") 'forward-paragraph)
  (local-set-key (kbd "M-p") 'backward-paragraph)
  (local-set-key (kbd "C-c L") 'org-toggle-link-display)
  (local-set-key (kbd "C-c I") 'org-toggle-inline-images))

(defun my-org-capture ()
  (interactive)
  (my-org-mode-hook)
  (evil-insert-state))

(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'org-capture-mode-hook 'my-org-capture)

(defun my-org-clock-jump ()
  (interactive)
  (push-mark (point))
  (org-clock-jump-to-current-clock))
(global-set-key (kbd "C-c C-x C-j") 'my-org-clock-jump)

(when (eq system-type 'darwin)
  ;; for pdf export
  (require 'ox-pandoc))

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
;* Which key
;; ----------------------------------------------------------------------------

(require 'which-key)
(which-key-mode)

;; ----------------------------------------------------------------------------
;* Winner
;; ----------------------------------------------------------------------------

(winner-mode 1)
(global-set-key (kbd "C-c C-h") 'winner-undo)
(global-set-key (kbd "C-c H") 'winner-redo)

;; ----------------------------------------------------------------------------
;* Calc
;; ----------------------------------------------------------------------------

(with-eval-after-load "calc-ext"
  (define-key calc-mode-map (kbd "C-c r") #'calc-reset)
  (define-key calc-mode-map (kbd "C-c C-r") #'calc-reset)
  (setq calc-multiplication-has-precedence nil))

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
(setq completion-styles '(orderless))

(define-key vertico-map (kbd "C-j") nil)

;; ----------------------------------------------------------------------------
;* Consult
;; ----------------------------------------------------------------------------

(require 'consult)

;; for virtual buffers
(evil-leader/set-key "j" 'consult-buffer)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x 4 b") 'consult-buffer-other-window)

(consult-customize
 consult-buffer consult-buffer-other-window consult-theme
 :preview-key (kbd "C-j"))

;; ----------------------------------------------------------------------------
;* Helm
;; ----------------------------------------------------------------------------

(require 'helm)
(require 'helm-config)

(define-key helm-map (kbd "C-c C-u") 'kill-whole-line)
(define-key helm-map (kbd "<escape>") nil)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; ----------------------------------------------------------------------------
;* Tramp
;; ----------------------------------------------------------------------------

(with-eval-after-load "tramp"
  (setq-default tramp-histfile-override "/tmp/.tramp_history"))

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
  (let* ((dir (locate-dominating-file default-directory ".git")))
    (or (and dir (expand-file-name (file-name-as-directory dir)))
	default-directory)))

(defvar my-ignore-dir-list '("build" "plugged" ".git"))

;; C-u opens in other window
(defun my-find-file-in-project (&optional open-in-other-window initial-input)
  (interactive "P")
  (let* ((dir (my-find-project-root))
	 (default-directory dir)	;; so we can open files with relative names
	 (shell-file-name "/bin/bash")	;; tcsh slow at work
	 (ignore-args (string-join (mapcar (lambda (d) (format "-name '%s'" d))
					   my-ignore-dir-list)
				   " -o "))
	 (cmd (format "find \"%s\" -type d \\( %s \\) -prune -false -o \\( -type f -o -type l \\)"
		      dir ignore-args))
	 (candlist (mapcar
		    (lambda (file)
		      (string-remove-prefix dir (expand-file-name file)))
		    (split-string (shell-command-to-string cmd)
				  "[\r\n]+" t))))

    (if (null candlist)
	(message "No files")

      (let ((file (completing-read
		   (format "Find in %s/: "
			   (file-name-nondirectory (directory-file-name dir)))
		   candlist nil t initial-input)))
	(when file
	  (if open-in-other-window
	      (find-file-other-window file)
	    (find-file file)))))))

(defun my-find-file-in-project-other-window ()
  (interactive)
  (let ((current-prefix-arg 4)) ;; emulate C-u
    (call-interactively 'my-find-file-in-project)))

(evil-leader/set-key "e" 'my-find-file-in-project)
(evil-leader/set-key "u" 'my-find-file-in-project-other-window)
(global-set-key (kbd "C-c e") 'my-find-file-in-project)

(defun my-jump-project-dired ()
  (interactive)
  (dired (or (vc-root-dir) (my-find-project-root))))

(global-set-key (kbd "C-c d") #'my-jump-project-dired)

(defvar my-projects)
(when (eq system-type 'darwin)
  (setq my-projects '(("~/dev" . 2))))
(when (eq system-type 'gnu/linux)
  (setq my-projects '(("~/dev/git" . 3))))
(dolist (d '("~/dotfiles-public" "~/dotfiles" "~/notefiles"))
  (when (file-directory-p d)
    (push `(,d . 1) my-projects)))

(defun my-list-repos ()
  "Return an alist of repos, with the key the string to match
against, and the value the expanded full path to the repo"
  (let ((all '())
	(shell-file-name "/bin/bash")) ; tcsh slow at work
    (dolist (proj my-projects)
      (let* ((dir (expand-file-name (car proj)))
	     (depth (cdr proj))
	     (cmd (format "find \"%s\" -maxdepth %d -type d -name .git" dir depth))
	     (repos (mapcar
		     (lambda (x)
		       (let* ((long (string-remove-suffix "/.git" x))
			      (short (file-name-nondirectory long)))
			 `(,short . ,long)))
		     (split-string (shell-command-to-string cmd)
				   "\n" t))))
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

(defun my-choose-project-and-search ()
  (interactive)
  (my-choose-project-and-invoke #'my-search-project))

(defun my-choose-project-and-magit ()
  (interactive)
  (my-choose-project-and-invoke #'magit))

(global-set-key (kbd "C-c p e") #'my-choose-project-and-find-file)
(global-set-key (kbd "C-c p s") #'my-choose-project-and-search)
(global-set-key (kbd "C-c p m") #'my-choose-project-and-magit)

(global-set-key (kbd "C-c x") (lambda ()
				(interactive)
				(let ((default-directory "~/dotfiles-public"))
				  (my-find-file-in-project))))
(global-set-key (kbd "C-c n") (lambda ()
				(interactive)
				(let ((default-directory "~/notefiles"))
				  (my-find-file-in-project))))

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
(define-key dired-mode-map (kbd "SPC") evil-leader--default-map)

;; ----------------------------------------------------------------------------
;* Magit
;; ----------------------------------------------------------------------------

(require 'magit)

(evil-collection-magit-setup)

;; this is on by default and breaks C-c exiting evil insert mode
(when (fboundp 'global-magit-file-mode)
  (global-magit-file-mode -1))

(defun my-magit-hook ()
  (evil-local-mode 1)
  (company-mode -1))

(defun my-magit-repolist-hook ()
  (tabulated-list-sort 0)
  (beginning-of-buffer))

(add-hook 'magit-mode-hook #'my-magit-hook)
(add-hook 'magit-repolist-mode-hook #'my-magit-repolist-hook)
(advice-add 'git-commit-mode :after 'evil-insert-state)

(setq magit-delete-by-moving-to-trash nil)

(setq-default magit-repolist-column-flag-alist '((magit-unstaged-files . "U")
						 (magit-staged-files . "S"))
	      magit-repolist-columns '(("Branch" 8 magit-repolist-column-branch nil)
				       ("Flag" 4 magit-repolist-column-flag)
				       ("Path" 50 magit-repolist-column-path nil)
				       ;; ("Name" 25 magit-repolist-column-ident nil)
				       ("B<U" 3 magit-repolist-column-unpulled-from-upstream
					((:right-align t)
					 (:help-echo "Upstream changes not in branch")))
				       ("B>U" 3 magit-repolist-column-unpushed-to-upstream
					((:right-align t)
					 (:help-echo "Local changes not in upstream")))
				       ))

(define-key magit-repolist-mode-map (kbd "j") (kbd "n"))
(define-key magit-repolist-mode-map (kbd "k") (kbd "p"))

;; stop escape burying magit buffers
(evil-define-key 'normal magit-mode-map (kbd "<escape>") nil)

(evil-define-key 'normal magit-mode-map (kbd "C-n") (kbd "C-j"))
(evil-define-key 'normal magit-mode-map (kbd "C-p") (kbd "C-k"))
(evil-define-key 'normal magit-mode-map (kbd "p") (kbd "C-p"))

(evil-leader/set-key "v" 'magit-status)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c M")
		(lambda ()
		  (interactive)
		  (other-window 1)
		  (setq magit-repository-directories '())
		  (dolist (proj (my-list-repos))
		    (let ((dir (cdr proj)))
		      (push `(,dir . 0) magit-repository-directories)))
		  (magit-list-repositories)))

;; ----------------------------------------------------------------------------
;* Ediff
;; ----------------------------------------------------------------------------

(setq-default ediff-custom-diff-options "-u")

;; stop overriding new window switch key
(define-key diff-mode-map (kbd "M-o") nil)
(define-key diff-mode-map (kbd "M-SPC") nil)

;; ----------------------------------------------------------------------------
;* Compilation
;; ----------------------------------------------------------------------------

(setq-default compilation-ask-about-save nil)
(setq-default compilation-scroll-output t)
(setq-default compilation-skip-threshold 2) ;; skip warnings

(defun my-compilation-mode-hook ()
  (visual-line-mode)
  (evil-local-mode)
  (setq-local split-width-threshold 1000))

(add-hook 'compilation-mode-hook 'my-compilation-mode-hook)

(defun my-jump-compilation ()
  (interactive)
  (let ((w (get-buffer-window "*compilation*")))
      (if w
	  (select-window w)
	(switch-to-buffer "*compilation*"))))

(defun my-compile-project ()
  (interactive)
  (let ((d (or (vc-root-dir)
	       (my-find-project-root))))
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

;; ----------------------------------------------------------------------------
;* Yaml
;; ----------------------------------------------------------------------------

(defun my-yaml-hook ()
  (setq-local evil-shift-width 2))

(add-hook 'yaml-mode-hook 'my-yaml-hook)

;; ----------------------------------------------------------------------------
;* Shell
;; ----------------------------------------------------------------------------

(defun my-spawn-shell ()
  (let ((currentbuf (get-buffer-window (current-buffer)))
	(newbuf     (generate-new-buffer-name "*shell*")))
    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

(defun my-split-shell ()
  (interactive)
  (let ((evil-split-window-below t))
    (evil-window-split))
  (my-spawn-shell))

(global-set-key (kbd "C-c v") 'shell)
(global-set-key (kbd "C-c V") 'my-split-shell)
(evil-leader/set-key "t" 'shell)
(evil-leader/set-key "T" 'my-split-shell)

(setq comint-prompt-read-only t)
(define-key shell-mode-map (kbd "M-_") 'comint-insert-previous-argument)
(define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
(define-key shell-mode-map (kbd "SPC") 'comint-magic-space)
(define-key shell-mode-map (kbd "C-c C-l") 'comint-clear-buffer)

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
  (company-mode -1)
  (fancy-dabbrev-mode -1)
  (visual-line-mode 0)
  (toggle-truncate-lines 0)
  (define-key shell-mode-map (kbd "C-d") #'my-shell-ctrl-d)
  ;; don't ignore .git, etc
  (setq-local completion-ignored-extensions nil))

(add-hook 'shell-mode-hook 'my-shell-hook)
(evil-set-initial-state 'shell-mode 'emacs)

(add-hook 'sh-mode-hook 'my-syntax-entry)

;; ----------------------------------------------------------------------------
;* Tags
;; ----------------------------------------------------------------------------

(require 'etags-select "~/.emacs.d/lisp/etags-select")

;; make return to select tag work in terminal, and C-m in the GUI
(if (display-graphic-p)
    (define-key etags-select-mode-map (kbd "C-m") (kbd "<return>"))
  (define-key etags-select-mode-map (kbd "RET") (kbd "<return>")))

(defun my-jump-to-tag-in-split-window ()
  (interactive)
  (evil-window-split)
  ;;(evil-jump-to-tag)
  (etags-select-find-tag-at-point))

(evil-global-set-key 'normal (kbd "C-w C-]") #'my-jump-to-tag-in-split-window)
(evil-global-set-key 'motion (kbd "C-]") #'etags-select-find-tag-at-point)

(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

(defun my-rebuild-and-load-tags ()
  "Find a TAGS file above the default-directory, invoke make TAGS
in that directory, then visit-tags-table on the file"
  (interactive)
  (let* ((dir (locate-dominating-file default-directory "TAGS"))
	 (path (and dir (expand-file-name (file-name-as-directory dir)))))
    (if (not path)
	(message (format "No existing TAGS file found above %s" default-directory))
      (when (y-or-n-p (format "Rebuild tags in %s?" path))
	(message (format "Rebuilding tags, make -C %s TAGS" path))
	(call-process "make" nil nil nil "-C" path "TAGS")
	(visit-tags-table (concat path "TAGS"))))))

(global-set-key (kbd "C-c C-]") #'my-rebuild-and-load-tags)

;; ----------------------------------------------------------------------------
;* Company mode
;; ----------------------------------------------------------------------------

(require 'company)
(require 'company-statistics)

(global-company-mode 1)
(company-statistics-mode 1)
(setq company-statistics-auto-save nil)

(defun my-company-newline ()
  (interactive)
  (company-abort)
  (newline-and-indent))
;; return inserts a newline, instead of selecting the first suggestion
(define-key company-active-map (kbd (if (display-graphic-p) "<return>" "RET"))
  'my-company-newline)
(when (display-graphic-p)
  (define-key company-active-map (kbd "C-m") 'my-company-newline))

(defun my-company-abort ()
  (interactive)
  (company-abort)
  (cond
   ((evil-insert-state-p) (evil-normal-state))
   ((and ;;(bound-and-true-p git-commit-mode)
	 (evil-emacs-state-p))
    ;; Send another C-c event, so in a git commit message we only have
    ;; to hit C-c twice, rather than three times, to abort the
    ;; completion and finish the commit.
    (setq unread-command-events (listify-key-sequence "\C-c")))))

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

(dolist (complete-key `(,(kbd "TAB") [tab]))
  (define-key company-active-map complete-key 'company-complete-selection)
  (define-key company-search-map complete-key 'company-complete-selection))
(dolist (abort-key `(,(kbd "C-c") ,(kbd "<escape>")))
  (define-key company-active-map abort-key 'my-company-abort)
  (define-key company-search-map abort-key 'my-company-abort))

;; once the completion popup is already open, pressing C-x C-f again
;; descends into the selected directory and continues, like vim
(define-key company-active-map (kbd "C-x C-f")
  (lambda ()
    (interactive)
    (company-complete-selection)
    (insert "/")
    (call-interactively 'company-files)))

(setq-default company-dabbrev-downcase nil)	 ;; stop downcasing completion results
(setq-default company-dabbrev-ignore-case t)	 ;; case insensitive when gathering results
(setq-default company-dabbrev-code-everywhere t) ;; complete in comments and strings
(setq-default company-minimum-prefix-length 3)	 ;; num chars before idle completion starts
;; (setq-default company-idle-delay 0)		 ;; immediate completion
(setq-default company-idle-delay nil)		 ;; no idle completion
(setq-default company-tooltip-limit 15)		 ;; number of items in popup menu
(setq-default company-selection-wrap-around t)
(setq-default company-backends
	      '(company-dabbrev company-capf company-files
				(company-keywords company-dabbrev-code company-etags)))
(setq-default company-frontends
	      '(company-pseudo-tooltip-unless-just-one-frontend
		company-preview-if-just-one-frontend
		company-echo-metadata-frontend))
(setq-default company-format-margin-function nil) ;; no icons

;; (defun my-company-dabbrev--prefix ()
;;   "Modified version of company-dabbrev--prefix that doesn't
;; return nil if the point is in the middle of a word"
;;   (company-grab-line (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)"
;;                              company-dabbrev-char-regexp)
;;                      1))
;; ;;; Make completion work when point is at the beginning or in the
;; ;;; middle of a word
;; (advice-add 'company-dabbrev--prefix :override #'my-company-dabbrev--prefix)

;; want company mode to work with evil-repeat
;; (searching within the candidates then evil-repeat still doesn't work however.)
(evil-declare-change-repeat 'company-complete)

;; after C-n, use C-s C-r to search and C-o to narrow list to results
(evil-global-set-key 'insert (kbd "C-p") 'company-complete)
(evil-global-set-key 'insert (kbd "C-n") 'company-complete)
(evil-global-set-key 'insert (kbd "C-x C-f") 'company-files)

;; ----------------------------------------------------------------------------
;* Lisp
;; ----------------------------------------------------------------------------

(require 'paredit)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

;; still works if cursor is on a blank line below
(define-key paredit-mode-map (kbd "C-j") (lambda ()
					   (interactive)
					   (pp-eval-last-sexp t)))

(defun my-lisp-common-hook ()
  (enable-paredit-mode))

(add-hook 'emacs-lisp-mode-hook       'my-lisp-common-hook 'append)
(add-hook 'lisp-mode-hook             'my-lisp-common-hook 'append)
(add-hook 'lisp-interaction-mode-hook 'my-lisp-common-hook 'append)
(add-hook 'scheme-mode-hook           'my-lisp-common-hook 'append)

;; ----------------------------------------------------------------------------
;* Python
;; ----------------------------------------------------------------------------

(when (eq system-type 'gnu/linux)
  (setq-default python-shell-interpreter
		(if (string-match "\.el[7-9]\." operating-system-release)
		    "millpython2.7" ;; centos
		  "python3")))      ;; ubuntu

(defun my-python-shell-mode-hook ()
  (toggle-truncate-lines 0))

(defun my-python-mode-hook ()
  (my-syntax-entry)
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (setq-local tab-width 4)
  (setq-local evil-shift-width 4))

(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'inferior-python-mode-hook 'my-python-shell-mode-hook)

(evil-leader/set-key-for-mode 'python-mode "f"
  (lambda ()
    (interactive)
    (if (use-region-p)
	(call-interactively 'python-shell-send-region)
      (message "No region active"))))

(evil-set-initial-state 'inferior-python-mode 'emacs)

(add-to-list 'auto-mode-alist '("/SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("/SConscript\\'" . python-mode))

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
  "3" (lambda () (interactive) (my-wrap-if-endif 1))
  "1" (lambda () (interactive) (my-wrap-if-endif 1 t)))

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
  (local-set-key (kbd "C-c C-b") (kbd "C-c b"))
  (auto-fill-mode -1)
  (setq-local fill-column 80)
  (cpp-highlight-if-0/1)
  (add-hook 'after-save-hook 'cpp-highlight-if-0/1 'append 'local)
  (yas-minor-mode))

(defvar my-cc-path)

(defun my-cpp-mode-hook ()
  (my-c-cpp-settings)
  (make-local-variable 'ffap-c++-path)
  (when my-cc-path
    (dolist (x my-cc-path)
      (add-to-list 'ffap-c++-path x)))
  (vc-refresh-state)
  (add-to-list 'ffap-c++-path (or (vc-root-dir) (my-find-project-root))))

(defun my-c-mode-hook ()
  (my-c-cpp-settings)
  (make-local-variable 'ffap-c-path)
  (when my-cc-path
    (dolist (x my-cc-path)
      (add-to-list 'ffap-c-path x)))
  (vc-refresh-state)
  (add-to-list 'ffap-c-path (or (vc-root-dir) (my-find-project-root))))

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
  (company-mode -1)
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

    (set-window-buffer w-io (or (get-buffer "*shell*")
				(get-buffer "*compilation*")
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
;* Colours
;; ----------------------------------------------------------------------------

(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

(blink-cursor-mode 0)
(setq-default cursor-type 'box)

;; see terminal background colour/image
(defun my-unspecified-background ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'my-unspecified-background)

(defun my-theme-dark ()
  (require 'reykjavik-theme)
  (load-theme 'reykjavik)
  (set-cursor-color "white")
  (setq evil-normal-state-cursor '(box "white"))
  (setq evil-insert-state-cursor '(box "orange"))
  (custom-set-faces
   '(isearch ((t (:background "gold2" :foreground "black" :weight bold))))
   '(lazy-highlight ((t (:background "turquoise" :foreground "black" ))))
   '(show-paren-match ((t (:weight bold :background nil :foreground "#dddddd")))))
  (if (display-graphic-p)
      (progn
  	(custom-set-faces
  	 ;; '(default ((t ())))
	 '(default ((t (:background "#0d1f24"))))
	 '(fringe ((t (:background nil))))
	 '(mode-line-inactive ((t (:box (:line-width 1 :color "#112328")))))
	 '(mode-line ((t (:box (:line-width 1 :color "#243539")))))
	 '(mode-line-inactive ((t (:foreground "#878787" :box (:line-width 1 :color "#112328")))))
	 '(orderless-match-face-0 ((t (:foreground "#a3d4e8" :weight bold))))
  	 '(font-lock-comment-face ((t (:foreground "#708080")))))
  	(set-face-attribute 'region nil :foreground "#ffffff" :background "#005f5f"))
    (custom-set-faces
     '(default ((t (:background "#070707"))))
     '(font-lock-comment-face ((t (:foreground "#757575"))))
     '(magit-diff-context-highlight ((t (:background "color-236" :foreground "#959595"))))
     '(mode-line ((t (:foreground "#808080" :background "#222222"))))
     '(mode-line-buffer-id ((t (:foreground "#C7B299"))))
     '(mode-line-inactive ((t (:foreground "#656565" :background "#222222")))))
    (set-face-attribute 'region nil :foreground "#ffffff" :background "#243539")
    (my-unspecified-background)))

(defun my-theme-light ()
  ;; (require 'gandalf-theme)
  ;; (load-theme 'gandalf)
  (require 'soft-morning-theme)
  (load-theme 'soft-morning)

  (set-cursor-color "ForestGreen")
  (setq evil-normal-state-cursor '(box "black"))
  (setq evil-insert-state-cursor '(box "orange"))
  (custom-set-faces
   '(default ((t ())))
   '(isearch ((t (:background "deeppink" :foreground "black" :weight bold))))
   '(lazy-highlight ((t (:background "turquoise" :foreground "black" ))))
   '(font-lock-comment-face ((t (:foreground  "dark green" :italic t))))
   '(show-paren-match ((t (:weight bold :background nil :foreground "#000000"))))
   '(region ((t (:background "#3399aa" :foreground "#ffffff"))))
   '(orderless-match-face-0 ((t (:foreground "#0000ff" :weight bold))))
   '(magit-diff-context-highlight ((t ())))
   '(mode-line ((t ())))
   '(mode-line-buffer-id ((t ())))
   '(mode-line-inactive ((t ())))))

(defun my-color-theme-toggle ()
  (interactive)
  (let ((cur (format "%s" (car custom-enabled-themes))))
    (mapcar #'disable-theme custom-enabled-themes)
    (if (string-equal "reykjavik" cur)
	(my-theme-light)
      (my-theme-dark))))

(defun my-load-theme ()
  (interactive)
  (ido-vertical-mode 1)
  (unwind-protect
      (let ((x (ido-completing-read
		"Theme: "
		(mapcar 'symbol-name (custom-available-themes)))))
	(when x
	  (mapc #'disable-theme custom-enabled-themes)
	  (load-theme (intern x) t)))
    (ido-vertical-mode -1)))

(global-set-key (kbd "<f6>") 'my-color-theme-toggle)

;; ----------------------------------------------------------------------------
;* Font
;; ----------------------------------------------------------------------------

;; linux: install to ~/.fonts/  then fc-cache -v ~/.fonts
(when (and (eq system-type 'gnu/linux)
	   (string= (getenv "XDG_CURRENT_DESKTOP") "i3"))

  (set-face-attribute 'default nil :font "Ubuntu Mono:pixelsize=14:foundry=DAMA:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")

  (if (string= (string-trim
		(shell-command-to-string "xrandr | awk '/^HDMI-1/{print $2}'"))
	       "connected")
      ;; external monitor
      (set-face-attribute 'default nil :height 105)
    ;; laptop screen
    (set-face-attribute 'default nil :height 135)))

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 150))

;; ----------------------------------------------------------------------------
;* Menu toolbar
;; ----------------------------------------------------------------------------

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(menu-bar-mode -1)

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
  (setq-local fill-paragraph-function nil))
(add-hook 'message-mode-hook 'my-message-mode-hook)

(define-key message-mode-map (kbd "C-c C-c") nil)
(define-key message-mode-map (kbd "C-c C-s") nil)

(setq-default message-auto-save-directory nil)

(setq compose-mail-user-agent-warnings nil)

(global-set-key (kbd "C-x m") #'my-scratch-mail-buffer)

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
   '("8efa3d21b3fa1ac084798fae4e89848ec26ae5c724b9417caf4922f4b2e31c2a" "fd1dd4d022ece05400c7bd1efc2ae5cca5cd64a53f3670da49d0c8f0ef41f4e3" "f0c94bf6a29c232300e46af50f46ce337e721eacca6d618e8654a263db5ecdbe" "621595cbf6c622556432e881945dda779528e48bb57107b65d428e61a8bb7955" "e6ccd0cc810aa6458391e95e4874942875252cd0342efd5a193de92bfbb6416b" "45f7fec480eb3bdf364cbfcbc8d11ed0228bcf586ce7370fc30a6ce5770f181a" "01ce486c3a7c8b37cf13f8c95ca4bb3c11413228b35676025fdf239e77019ea1" "4af6fad34321a1ce23d8ab3486c662de122e8c6c1de97baed3aa4c10fe55e060" "83db918b06f0b1df1153f21c0d47250556c7ffb5b5e6906d21749f41737babb7" default))
 '(dabbrev-backward-only t)
 '(dabbrev-case-distinction nil)
 '(dabbrev-case-fold-search nil)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(electric-indent-mode t)
 '(electric-pair-mode nil)
 '(evil-flash-delay 60)
 '(evil-motion-state-modes
   '(apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode dictionary-mode ert-results-mode help-mode Info-mode Man-mode speedbar-mode undo-tree-visualizer-mode view-mode woman-mode))
 '(helm-ag-insert-at-point 'symbol)
 '(ido-vertical-indicator ">")
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ispell-program-name "aspell")
 '(magit-log-arguments '("--graph" "--color" "--decorate" "-n256"))
 '(magit-merge-arguments '("--no-ff"))
 '(magit-section-initial-visibility-alist '((stashes . show) (upstream . show)))
 '(magit-section-visibility-indicator '("" . t))
 '(magit-status-headers-hook
   '(magit-insert-error-header magit-insert-diff-filter-header magit-insert-repo-header magit-insert-head-branch-header magit-insert-upstream-branch-header magit-insert-push-branch-header magit-insert-tags-header))
 '(package-selected-packages
   '(anaconda-mode
     cmake-mode
     company
     company-statistics
     consult
     evil
     evil-leader
     evil-collection
     evil-numbers
     fancy-dabbrev
     gandalf-theme
     helm
     helm-ag
     ido-vertical-mode
     magit
     marginalia
     orderless
     ox-pandoc
     paredit
     reykjavik-theme
     rust-mode
     soft-morning-theme
     undo-tree
     vertico
     which-key
     yaml-mode
     yasnippet))
 '(safe-local-variable-values
   '((tab-always-indent)
     (my-lang . "se")
     (indent-tabs-mode nil)
     (evil-shift-width . 2)
     (evil-shift-width . 4)))
 '(tramp-ssh-controlmaster-options
   "-o ControlMaster=auto -o ControlPath=tramp.%%C -o ControlPersist=60m" t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil)))
 '(company-preview ((t (:foreground "darkgray"))))
 '(company-preview-common ((t (:foreground "darkgray"))))
 '(company-scrollbar-bg ((t (:background "gray"))))
 '(company-scrollbar-fg ((t (:background "black"))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black" :weight normal))))
 '(company-tooltip-common ((t (:foreground "black"))))
 '(company-tooltip-common-selection ((t (:foreground "white"))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white"))))
 '(font-lock-comment-face ((t (:foreground "#708080"))))
 '(lazy-highlight ((t (:background "#555566" :foreground "#aaaabb" :weight extra-bold))))
 '(magit-diff-context-highlight ((t nil)))
 '(message-cited-text-1 ((t (:foreground "#878787"))))
 '(mode-line ((t nil)))
 '(mode-line-buffer-id ((t nil)))
 '(mode-line-inactive ((t nil)))
 '(show-paren-match ((t (:weight bold :background nil :foreground "#FFDD00"))))
 '(success ((t (:foreground "#00DD00" :weight bold)))))

(if (< (decoded-time-hour (decode-time)) 13)
    (my-theme-light)
  (my-theme-dark))
