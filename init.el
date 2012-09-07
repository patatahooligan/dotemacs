(require 'cl)

;; EL-GET
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'exec-path "~/bin/")


(if (file-readable-p "~/.emacs.d/personal.el")
    (load-file       "~/.emacs.d/personal.el")
  (load-file   "~/.emacs.d/dummy-personal.el"))


(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (let (el-get-master-branch)
       (goto-char (point-max))
       (eval-print-last-sexp)))))

(setq my:el-get-packages
      '(
	;; Python
	ipython
	python-mode
	python-pep8
	pylookup

	;; Auto Complete packages
	auto-complete
	auto-complete+
	auto-complete-etags
	auto-complete-yasnippet
	auto-complete-emacs-lisp
	auto-complete-extension
	ac-dabbrev
	ac-python

	;; themes
	tomorrow-night-paradise-theme
	tomorrow-theme
	naquadah-theme
	monokai-theme
	zenburn-theme

	;; Misc
	etags-table
	zencoding-mode
	git-emacs
	visual-basic-mode
	yasnippet
	ido-ubiquitous
	ido-hacks
	smex
	bm
	goto-last-change
	xml-rpc lisppaste
	undo-tree))

(setq
 el-get-sources
 '(el-get
   auto-complete
   zencoding-mode
   python-pep8
   git-emacs
   visual-basic-mode
   yasnippet

   (:name ido-hacks
	  :description "Ido hacks"
	  :type github
	  :pkgname "scottjad/ido-hacks")
   (:name bm
	  :description "Simple bookmark manager"
	  :type github
	  :pkgname "joodland/bm")

   (:name smex				; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))
   (:name goto-last-change		; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-/") 'goto-last-change)))))

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
(when (el-get-executable-find "cvs")
  (add-to-list 'el-get-sources 'emacs-goodies-el)) ; the debian addons for emacs

(when (el-get-executable-find "svn")
  (loop for p in '(psvn    		; M-x svn-status
		   yasnippet		; powerful snippet mode
		   )
	do (add-to-list 'el-get-sources p)))

(el-get 'sync my:el-get-packages)


;; on to the visual settings
(load-theme 'tomorrow-night-paradise t)
(setq inhibit-splash-screen t)	; no splash screen, thanks
(line-number-mode 1)	; have line numbers and
(column-number-mode 1)	; column numbers in the mode line

(tool-bar-mode -1)	; no tool bar with icons
(scroll-bar-mode -1)	; no scroll bars

(global-linum-mode 1)	; add line numbers on the left

;; CLIPBOARD
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
(setq x-select-enable-clipboard t)

;; (global-set-key "\C-w" 'clipboard-kill-region)
;; (global-set-key "\M-w" 'clipboard-kill-ring-save)
;; (global-set-key "\C-y" 'clipboard-yank)

;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("22ee8a5d6de86d5181f40e411c354ddde1f84ff246ad8a3cc8fa050282738d80" "fca8ce385e5424064320d2790297f735ecfde494674193b061b9ac371526d059" "a2187840d0077aad2a626aea943edcf1c8733b0d68c77e4ad7130cb425a25af9" "159bb8f86836ea30261ece64ac695dc490e871d57107016c09f286146f0dae64" "4aafea32abe07a9658d20aadcae066e9c7a53f8e3dfbd18d8fa0b26c24f9082c" "8281168b824a806489ca7d22e60bb15020bf6eecd64c25088c85b3fd806fc341" "d6a00ef5e53adf9b6fe417d2b4404895f26210c52bb8716971be106550cea257" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ido-ubiquitous-mode t)
 '(safe-local-variable-values (quote ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook" (add-hook (quote write-contents-functions) (lambda nil (delete-trailing-whitespace) nil)) (require (quote whitespace)) "Sometimes the mode needs to be toggled off and on." (whitespace-mode 0) (whitespace-mode 1)) (whitespace-line-column . 80) (whitespace-style face trailing lines-tail) (require-final-newline . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Ido mode
(require 'ido)
(ido-mode t)
(require 'ido-hacks)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-auto-merge-work-directories-length -1)
(ido-everywhere t)
(require 'ido-ubiquitous)

;; Basic stuff
(require 'uniquify)
(custom-set-variables
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b"
                                                          ("%b - Dir:  " default-directory)))))))
(server-start)
(delete-selection-mode t)
(show-paren-mode)
(electric-pair-mode t)
(global-hl-line-mode t)
(set-face-attribute 'default nil :height 90)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline 'query)
(set-input-method 'greek)
(toggle-input-method)
(setq scroll-step 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(highlight-parentheses-mode)
(global-set-key "\C-Z" 'revert-buffer)


(add-to-list 'auto-mode-alist '("[.]zcml" . nxml-mode))
(add-to-list 'auto-mode-alist '("[.]pt" . html-mode))

;; Indent buffer
(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))
(global-set-key "\C-x\\" 'indent-buffer)

;; YASnippet
(require 'yasnippet)
(setq yas-snippet-dirs "~/.emacs.d/el-get/yasnippet/snippets/")
(yas--initialize)

;; Auto Complete
(require 'auto-complete+)
(require 'auto-complete-extension)
(require 'auto-complete-yasnippet)
(require 'auto-complete-etags)
(require 'auto-complete-config)
(require 'ac-python)
(require 'auto-complete-emacs-lisp)
(ac-config-default)

;; ORG mode
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map "\M-j" 'org-meta-return)))
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(tab)] nil)))
(require 'org)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files my-orgmode-agenda-files
      org-hierarchical-todo-statistics nil
      org-support-shift-select 'always)

;; Python
(setq py-mode-map python-mode-map)
(setq ipython-command "/usr/bin/ipython2")
(setq ipython-completion-command-string "print(';'.join(__IP.Completer.all_completions('%s')))\n")
(require 'python-mode)
(require 'ipython)
(define-key python-mode-map "\C-cp" '(lambda () (interactive) (insert "import ipdb; ipdb.set_trace()")))
(define-key python-mode-map "\C-ch" 'pylookup-lookup)


;; desktop
(require 'desktop)
(desktop-save-mode t)
(setq desktop-buffers-not-to-save
      (concat "\\("
	      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq make-backup-files nil)


;; UNDO TREE
(require 'undo-tree)
(global-undo-tree-mode)

;; RECENT FILES
(require 'recentf)
(recentf-mode)
(setq recentf-max-saved-items 100)
(defun steve-ido-choose-from-recentf ()
  "Use ido to select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file (ido-completing-read "Open file: " recentf-list nil t)))
(global-set-key [(meta f11)] 'steve-ido-choose-from-recentf)

;; ERC
;; check channels
(require 'erc)

(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"

				"324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;; joining && autojoing

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs")))

(defun fakedrake-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

      (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick my-freenode-nick :full-name my-freenode-fullname :password my-freenode-password)
      )))

(defun my-destroy-erc ()
  "Kill all erc buffers!!"
  (interactive)
  (save-excursion
    (dolist (i (buffer-list))
      (with-current-buffer i
	(cond
	 ((eq major-mode 'erc-mode) (kill-buffer (current-buffer))))))))
;; switch to ERC with Ctrl+c e
(global-set-key (kbd "C-c e s") 'fakedrake-erc-start-or-switch) ;; ERC
(global-set-key (kbd "C-c e k") 'my-destroy-erc)


(require 'lisppaste)


;; BOOKMARKS
(require 'bm)
(setq bm-highlight-style nil)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; ETAGS
(require 'etags-table)
(setq etags-table-search-up-depth 10)
(add-to-list 'ido-ubiquitous-command-exceptions 'find-tag)
