;; Author: Andrew Hilborne
;;
;; I've been doing this for nearly 20 years, but in 2018 I have stolen
;; some ideas from Kyle Purdon @
;; https://realpython.com/emacs-the-best-python-editor/
;;

;; TODO - improvements for the future
;; -------------------------------------

; 1. use use-package + :custom for customisations

; 2. This could be a good idea. Just look in it to see what's been changed
; (setq custom-file (make-temp-file "emacs-custom"))

; 3. Put (m)elpa in vc, or changed package versions in the future will bite me

; 4. Do I need use-package :ensure?

;; XXX Only needed on a new host
;; (setq package-check-signature nil)

;; START - especially load-path
;; --------------------------------------

; These stanzas enable us to run from an init file which isn't
; ~/.emacs.d/init.el. Like this: emacs -q -l "this file"
(setq user-init-file (or load-file-name (buffer-file-name))
      user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp")))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Fonts - try again
;; --------------------------------------
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :slant 'normal
                    :weight 'normal
                    :width 'normal)


;; INITIAL PACKAGES
;; --------------------------------------

; I think this will cause package.el to load tha autoload files from
; all the packages in package-selected-packages, which is surely going
; to remove some of the advantages of using use-package below. What this
; _does_ mean is that {init.el, custom.el} are sufficient to load
; emacs as we want it on a brand new machine.
(require 'package)

;; ;; Quelpa for git support inside use-package
;; (use-package quelpa-use-package
;;   :demand
;;   :init (setq quelpa-update-melpa-p nil))

;; FIXME XXX Should use https where possible (!Windows)
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

; Version 27+ deprecates the cl library
(setq byte-compile-warnings '(cl-functions))

; Current magit doesn't support emacs 24 (Will be *much* slower, according to the author)
(if (version< emacs-version "25")
    (progn
      (add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/")
             t)
      (add-to-list 'package-pinned-packages '(magit . "melpa-stable"))
      (add-to-list 'package-pinned-packages '(ghub . "melpa-stable"))
      (add-to-list 'package-pinned-packages '(git-commit . "melpa-stable"))
      (add-to-list 'package-pinned-packages '(magit-popup . "melpa-stable"))
      (add-to-list 'package-pinned-packages '(with-editor . "melpa-stable"))))

(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages '(
    ; better-defaults
    use-package
    gnu-elpa-keyring-update             ; keep signatures up-to-date
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; USE-PACKAGE
;; --------------------------------------
(require 'use-package)
(setq use-package-always-ensure t)

;; load material theme
;; NB (load-theme 'material-light t) gives the light version, or use
;;    M-x load-theme
(use-package material-theme
  :demand
  :config
  (load-theme 'material t))

(use-package elpy
  ;; Since we don't (for example) hook into python-mode to enable
  ;; elpy, it may never be started without :demand.
  :demand
  :bind ([remap elpy-shell-kill-all] . elpy-shell-kill)
  :config
  (setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules))
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"
;;	python-shell-interpreter-args "-i" ; XXX for build server
        gud-pdb-command-name "python -m pdb"
        enable-remote-dir-locals t
        comint-scroll-show-maximum-output nil) ; see var docs
  (add-hook 'python-mode-hook
            (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace))
            (lambda()(pyvenv-mode)(pyvenv-tracking-mode))
            (lambda()(toggle-truncate-lines)))
  (add-hook 'elpy-mode-hook
            (lambda ()(elpy-shell-set-local-shell (elpy-project-root))))
  (elpy-enable)
)

;; enable autopep8 formatting on save with elpy
;; (use-package py-autopep8
;;   :after elpy
;;   :config
;;   (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))
  
;; use flycheck not flymake with elpy
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode)

(use-package csv-mode)

;; (use-package ibuffer-vc                 ; VC column for ibuffer
;;   :demand)

(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/pv")
        projectile-switch-project-action #'projectile-dired
        projectile-enable-caching t))

(use-package magit
;; XXX Broken  :commands global-magit-file-mode
  :init
  ;; Minor mode with a few key bindings
;; XXX Broken  (global-magit-file-mode 1)
  :config
  ;; See docs for magit-log-margin FIXME: not working yet?
  (setq magit-log-margin '(t age-abbreviated magit-log-margin-width nil 18))

  ;; submodules
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpulled-from-upstream)
  (setq magit-module-sections-nested nil)
  
  (defun my-git-dired (dir)
    (interactive
     "DDirectory inside a git repository: \n")
    (condition-case nil
        (dired (cons "*git-dired*" (my-git-ls-files dir)))
      (error (message "Execution of git-ls-files failed"))))

  ; Only used by my-git-dired
  (defun my-git-ls-files (dir)
    (save-excursion
      (cd dir)
      (split-string
       ;; The following is shell-command-to-string with error handling added.
       (with-output-to-string
         (with-current-buffer
             standard-output
           (unless (= 0 (call-process shell-file-name nil t nil
                                      shell-command-switch "git ls-files"))
             (error "Not a git repo")))))))

  :bind ("C-x g" . magit-status)
        ("C-x M-g" . magit-dispatch))

(use-package undo-tree                  ; better (visual) undo handling
  :config
  (global-undo-tree-mode))

(use-package rainbow-mode)              ; hex colours

;; Smileys for heaven's sake!
;; (use-package mplayer-mode
;;   :ensure nil
;;   :quelpa (mplayer-mode
;;            :fetcher github
;;            :repo "markhepburn/mplayer-mode"))

(use-package try)

(use-package yaml-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.yml$"   . yaml-mode))
  (add-hook 'yaml-mode-hook
     '(lambda ()
       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))  

(use-package filladapt
  :config
  (setq-default filladapt-mode t))

(use-package markdown-mode
  :config
  (setq markdown-command "pandoc --from=markdown --to=html --standalone --mathjax --highlight-style=pygments")
  (add-to-list 'auto-mode-alist '("\\.md$"   . markdown-mode)))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package realgud)

;; Org mode
(use-package org-bullets :demand)

(use-package org
  :after org-bullets
  :config
  (setq org-archive-location "~/org/archive.org::"
        org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-default-notes-file (concat org-directory "/todo.org")
        org-agenda-files
        (remove (expand-file-name "~/org/archive.org")
                (remove (expand-file-name "~/org/jobs_done.org")
                        (directory-files "~/org" t ".*\.org")))
        org-agenda-show-inherited-tags nil

        org-export-backends '(ascii html iacalendar latex md)

        org-use-fast-todo-selection (quote expert)
        org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
          (sequence "EACHDAY(e)" "|" "CHECKED(c)")
          (sequence "WAITING(w)" "RESPONDED(r)" "|" "FINISHED(f)")
          (sequence "URGENT(u)" "INPROGRESS(i)" "ACTION(a)" "|" "OVER(o)"))

        org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("EACHDAY" . (:foreground "yellow" :weight bold))
          ("CHECKED" . (:foreground "green" :weight bold))
          ("WAITING" . (:foreground "orange" :weight bold))
          ("RESPONDED" . (:foreground "yellow" :weight bold))
          ("CANCELED" . (:foreground "blue" :weight bold)))

        org-agenda-custom-commands
        (quote
         (("n" "Agenda and all TODOs"
           ((agenda "" nil)
            (alltodo "" nil))
           nil)

          ;; ("w" "Weekly meetings"
          ;;  ((tags-todo "thisW" nil)
          ;;   (agenda "" nil))
          ;;  ((org-agenda-tag-filter-preset (quote ("+thisW")))
          ;;   (org-agenda-use-tag-inheritance nil)))

          ;; ("f" "Rev17 in factory quick hack"
          ;;  ((tags-todo "factory" nil)
          ;;   (agenda "" nil))
          ;;  ((org-agenda-tag-filter-preset (quote ("+factory")))
          ;;   (org-agenda-use-tag-inheritance nil)))

          ("v" "Veea"
           ((tags-todo "todo" nil)
            (tags-todo "veea+today" nil)
            (agenda "" ((org-agenda-span 'day)) ))
           nil)

          ("V" "Veea FULL!"
           ((tags-todo "todo" nil)
            (tags-todo "home" nil)
            (tags-todo "veea+today" nil)
            (tags-todo "veea+later" nil)
            (tags-todo "veea+background" nil)
            (tags-todo "veea+personal" nil)
            (agenda "" ((org-agenda-span 'day)) ))
           nil)

;;
;; ((org-agenda-tag-filter-preset (quote ("+newjob")))
;;            (org-agenda-prefix-format "  %s %?t ")


          )
         )
        )

        (defun my-org-files-list ()
          (remove (expand-file-name "~/org/archive.org")
                  (directory-files "~/org" t ".*\.org")))

        (setq org-tags-column -50
              org-refile-allow-creating-parent-nodes t
              org-refile-use-outline-path t
              org-outline-path-complete-in-steps t
              org-blank-before-new-entry '((heading . nil)
                                           (plain-list-item . auto))

              ;; org-refile-targets '((nil :maxlevel . 2)
              ;;                      (my-org-files-list :maxlevel . 2)
              ;;                      ("~/pv/wip/oWIP.org" :maxlevel . 1)
              ;;                      ("staff.org" :maxlevel . 2)
              ;;                      ("todo.org" :maxlevel . 2)))
              )

        (setq org-capture-templates
              '(("r" "Recruiter"
                 entry (file+headline "~/org/notes.org" "Recruiter calls")
                 "*** URGENT %?\n  %i")
                ))

        (require 'ox-latex)
        (unless (boundp 'org-latex-classes)
          (setq org-latex-classes nil))
        (add-to-list 'org-latex-classes
                     '("article"
                       "\\documentclass{article}"
                       ("\\section{%s}" . "\\section*{%s}")))

        (defun my/org-mode-buffer-setup ()
          "Per-buffer setup for org mode"

          (visual-line-mode)
          (org-indent-mode)
          (org-bullets-mode)
          (real-auto-save-mode)
          (setq real-auto-save-interval 20))

        (add-hook 'org-mode-hook 'my/org-mode-buffer-setup))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(require 'real-auto-save)

(use-package go-mode
  :defer t
  :ensure t
  :init
  (defun my/go-mode-buffer-setup ()
    "-amh- customisations for Go mode"
    (progn
      (if (not (string-match "go" compile-command))
          (set (make-local-variable 'compile-command)
               "go build -v && go test -v && go vet"))
      (auto-complete-mode 1)
      (hs-minor-mode 1)
      (setq-local comment-auto-fill-only-comments t)
      (setq
       ;; gofmt-command 'goimports
       indent-tabs-mode t
       tab-width 4)))
  (defun my/compile ()
    "-amh- Save then compile"
    (progn
      (save-buffer)
      (compile)))

  :mode ("\\.go\\'" . go-mode)  
  :bind (:map go-mode-map
              ("C-c C-c" . 'compile)
              ("M-."     . 'godef-jump))
  :config
  (add-hook 'go-mode-hook 'my/go-mode-buffer-setup))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp/goflymake")))
(require 'go-flymake)

;; Still go-mode
(add-hook 'before-save-hook 'gofmt-before-save)

;; Wahay! - Locks a buffer to a window, we hope ;)
(define-minor-mode sticky-buffer-mode "Make the current window always display
    this buffer."  nil " sticky" nil (set-window-dedicated-p (selected-window)
    sticky-buffer-mode))

;; General cruft from down the years, but tidied up a little
;; --------------------------------------

;; Default face
;; (set-face-attribute 'default nil :family "Courier" :foundry "Adobe" :height 87)
;; TODO install Source Code Pro first
;;; xxx (set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :height 87)
(setq kill-buffer-query-functions nil)

;; Global values for variables. Shouldn't be used for vars which can be
;; buffer-local
(setq
 auto-compression-mode t                ; for .gz files, etc
 change-log-default-name "~/NOTES"
 column-number-mode t
 compilation-scroll-output t
 completion-ignored-extensions
   (quote ("CM/" "CVS/" ".so" ".o" ".obj" ".elc" "~" ".bin" ".lbin" ".dvi" ".class"))
 delete-key-deletes-forward t
 desktop-dirname (getenv "HOME")
 ;; Original excludes remote files (buffers)
 ;; desktop-files-not-to-save "\\(^/[^/:]*:\\|(ftp)$\\)"
 desktop-files-not-to-save "^$"
 dired-auto-revert-buffer t
 dired-dwim-target t
 dired-listing-switches "-al"
 ;; dired-omit-files (concat "^\\.?#\\|^\\.$\\|^\\.\\.$\\|_flymake\\.py$\\|"
 ;;                          "^\\.git\\|^\\.dir-locals\\|^\\.pytest_cache")
 ;; Don't omit parent directory (why?!! would anyone do this?)
 dired-omit-files (concat "^\\.?#\\|^\\.$\\\|_flymake\\.py$\\|^\\.editorconfig\\|"
                          "^\\.git\\|^\\.dir-locals\\|^\\.pytest_cache^\\|^__pycache__")
 display-buffer-reuse-frames t          ; multiple monitors
 inhibit-startup-screen t
 line-number-mode t
 make-backup-files nil
 mouse-yank-at-point t
 scroll-step 1
 set-scroll-bar-mode 'right
 split-width-threshold 140              ; for narrower-than laptop display
 vc-revert-show-diff nil
 visible-bell t
 )

;; Affect all buffers, until var is set explicitly (buffer-local)
;;
(setq-default indent-tabs-mode nil)

;; Random. Mainly like this for hide-show-minor-mode
(progn
  ;; Why can't I do this with setq?
  (menu-bar-mode -1)
  ;; or this?
  (put 'narrow-to-region 'disabled nil)
  ;; or this?
  ;; XXX Brand new in 2018!
  (ignore-errors
    (window-divider-mode t))
  (show-paren-mode t)
  (tool-bar-mode 0)
  (blink-cursor-mode 0))

;; Filling
;;; XXX (require 'filladapt)
;;; XXX (setq-default filladapt-mode t)
(auto-fill-mode 1)
(setq fill-column 79)

;; TRAMP

;; Move non-TRAMP auto-saves to a single directory
;; FIXME Use a var for the dir name
(setq
 auto-save-file-name-transforms
 '(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "/tmp/\\2" t)
   (".*" "~/.emacs.d/auto-saves/" t)))  ; trailing / is crucial - see var docs
(unless (file-exists-p "~/.emacs.d/auto-saves")
  (make-directory "~/.emacs.d/auto-saves"))
(setq auto-save-default t
      auto-save-timeout 3
      auto-save-interval 200)

;; Password saving for TRAMP
(setq auth-source-debug t
      password-cache-expiry 3600
      auth-source-debug t
      auth-sources '((:source "~/.authinfo.gpg")))

;; Could change this for different hosts e.g. Powervault/elsewhere
(customize-set-variable 'tramp-default-user "amh")

;; Fix for ediff problem
(set-variable 'ediff-coding-system-for-write 'raw-text)

(require 'dired-x)  ; Get back some of those nice features from xemacs!
(add-hook 'dired-mode-hook
          '(lambda ()
             ;; (dired-hide-details-mode)
             ;; periodically revert, but see also dired-auto-revert-buffer
             (auto-revert-mode)
             (set-variable 'dired-omit-verbose nil)
             (dired-omit-mode)
             ;; revert silently
             (setq auto-revert-verbose nil)))

;; IBUFFER
;; -------

(use-package ibuffer-project
  :demand
  :init
  (setq ibuffer-project-use-cache t))

;; Keys for electric-buffer/ibuffer
(require 'ibuffer)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key (kbd "C-c C-b") 'electric-buffer-list)

;; Make ibuffer more like electric-buffer-list-mode
;; FIXME This only really works for me, 'cos I always use SPC here.
(define-key ibuffer-mode-map (kbd "SPC")
  '(lambda ()
  (interactive)
  ;; this next means we can 'swap' between two buffers,
  ;; without *Ibuffer* getting in the way
  (bury-buffer "*Ibuffer*")             
  (ibuffer-visit-buffer)))

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
;; --        (ibuffer-switch-to-saved-filter-groups "std")
             (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups)) ; By .git P
             )
          )

;; Even more like e-buf-list - this puts cursor on last-changed buffer.
;; One of several solutions from https://www.emacswiki.org/emacs/IbufferMode.
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
    "Open ibuffer with cursor pointed to most recent buffer name"
    (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 20 20 :left :elide) ; change: 20s were originally 18s
              " "
              (mode 16 16 :left :elide)
              " "
              " " project-file-relative)
              ;;              " " filename)
        (mark " "
              (name 16 -1)
              " " filename)))

;; Re-enable SPACE as completion character in find-file, etc. See etc/NEWS 22.1.
(define-key minibuffer-local-filename-completion-map
  " " 'minibuffer-complete-word)
;; XXX Not on ubu (define-key minibuffer-local-must-match-filename-map
;;   " " 'minibuffer-complete-word)  

;; Setup for modes

;; C and related modes
(add-hook 'c-mode-common-hook (lambda ()
  "-amh- customisations for all of c-mode and related modes"
  (progn
    (setq-local comment-auto-fill-only-comments t)
    (setq
     tab-width 4
     indent-tabs-mode nil
     c-basic-offset 4))))

;; text-mode
;; Moved auto-fill-mode here to try and improve when it is actually turned on
(add-hook 'text-mode-hook '(lambda()
                             (auto-fill-mode 1)
                             (setq fill-column 80)))

;; sh-mode
(add-hook 'sh-mode-hook (lambda ()
                          "-amh- sh-mode customisations"
                          (progn
                            (setq sh-indent-for-case-label 0
                                  sh-indent-for-case-alt '+))))

;; follow-mouse
(require 'follow-mouse)
(setq follow-mouse-deselect-active-minibuffer nil)
(turn-on-follow-mouse)

;; man-mode
(require 'man)
(progn
  (setq
   Man-switches "--nj")
  (set-face-attribute 'Man-overstrike nil :inherit font-lock-type-face :bold t)
  (set-face-attribute 'Man-underline nil :inherit font-lock-keyword-face :underline t))

;; hs-minor-mode
(add-hook 'c-common-mode-hook (lambda () (hs-minor-mode 1)))
(add-hook 'python-mode-hook (lambda () (hs-minor-mode 1)))
(add-hook 'sh-mode-hook (lambda () (hs-minor-mode 1)))
(add-hook 'makefile-mode-hook (lambda () (hs-minor-mode 1)))

;; occur mode
(add-hook 'occur-mode-hook
          (lambda () (setq list-matching-lines-default-context-lines 4)))

(defun my-hs-minor-mode-map-fix () 
  "Change the hs-minor-mode-map to use Control-C h as the prefix key.
- Run Once
Note well that this function _removes_ itself from the hs-minor-mode hook when it is run."
  (let ((map hs-minor-mode-map))
    (define-key map "\C-c@\C-h"	         nil)
    (define-key map "\C-c@\C-s"	         nil)
    (define-key map "\C-c@\C-\M-h"       nil)
    (define-key map "\C-c@\C-\M-s"       nil)
    (define-key map "\C-c@\C-l"	         nil)
    (define-key map "\C-c@\C-c"	         nil)

    (define-key map "\C-c\h\C-h"	'hs-hide-block)
    (define-key map "\C-c\h\C-s"	'hs-show-block)
    (define-key map "\C-c\h\C-\M-h"     'hs-hide-all)
    (define-key map "\C-c\h\C-\M-s"     'hs-show-all)
    (define-key map "\C-c\h\C-l"	'hs-hide-level)
    (define-key map "\C-c\h\C-c"	'hs-toggle-hiding)

    (remove-hook 'hs-minor-mode-hook 'my-hs-minor-mode-map-fix)))

(add-hook 'hs-minor-mode-hook 'my-hs-minor-mode-map-fix)

;; Change default action for \r in dired to make browsing quicker. Can
;; still open fle at point using 'o'.
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "\r") #'dired-view-file))

;; CPerl mode
(setq cperl-brace-offset -4
      cperl-indent-level 4)

;; Associate extensions with modes
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))
(add-to-list 'auto-mode-alist '("\\.org$"  . org-mode))
(add-to-list 'auto-mode-alist '("\\.ovpn$" . conf-space-mode))
(add-to-list 'auto-mode-alist '("\\.conf$" . conf-space-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\(append\\|class\\)?$" . conf-space-mode))

;; One-handed (telephone) notebook
(defun jump-to-scratch()
  (interactive)
  (find-file "~/org/notes.org"))

                                        ;  (text-mode))
(global-set-key (kbd "M-s s") 'jump-to-scratch)

;; Create a quick note
(defun make-quick-note()
  (interactive)
  (org-capture nil "Q"))
(global-set-key (kbd "C-c q") 'make-quick-note)
(global-set-key [f6] 'make-quick-note)

;; Jump to WIP.org
(defun jump-to-wip()
  (interactive)
  (bookmark-jump "wip"))
(global-set-key (kbd "C-c w") 'jump-to-wip)

;; Per-frame zoom
(load "zoom-frm")
(global-set-key (kbd "C-x z") 'zoom-in/out)

;; Add final message so using C-h l I can see if init failed
(message "init.el loaded successfully.")

;;; Local Variables: ***
;;; End: ***
(put 'downcase-region 'disabled nil)
