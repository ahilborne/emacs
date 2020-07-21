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

(use-package ibuffer-vc)                ; VC column for ibuffer

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
  :commands global-magit-file-mode
  :init
  ;; Minor mode with a few key bindings
  (global-magit-file-mode 1)
  :config
  ;; See docs for magit-log-margin FIXME: not working yet?
  (setq magit-log-margin '(t age-abbreviated magit-log-margin-width nil 18))

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

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md$"   . markdown-mode)))

(use-package undo-tree                  ; better (and visual) undo handling
  :config
  (global-undo-tree-mode))

(use-package rainbow-mode)              ; hex colours

;; Smileys for heaven's sake!
;; (use-package mplayer-mode
;;   :ensure nil
;;   :quelpa (mplayer-mode
;;            :fetcher github
;;            :repo "markhepburn/mplayer-mode"))

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
  (setq markdown-command "pandoc --from=markdown --to=html --standalone --mathjax --highlight-style=pygments"))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  ;; -amh- We don't need 'p' for 'previous line', so steal it for an easier-to-type 'peek'
  :bind ([remap treemacs-previous-line] . treemacs-peek)
  (:map global-map
        ("M-0"       . treemacs)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs-select-window)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; (use-package treemacs-icons-dired
;;   :after treemacs dired
;;   :ensure t
;;   :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package realgud)

;; Wahay! - Locks a buffer to a window, we hope ;)
(define-minor-mode sticky-buffer-mode "Make the current window always display
    this buffer."  nil " sticky" nil (set-window-dedicated-p (selected-window)
    sticky-buffer-mode))

;; General cruft from down the years, but tidied up a little
;; --------------------------------------

;; Default face
;; (set-face-attribute 'default nil :family "Courier" :foundry "Adobe" :height 87)
;; TODO install Source Code Pro first
(set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :height 87)
(setq delete-selection-mode 1
      kill-buffer-query-functions nil)

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
 delete-selection-mode 1
 desktop-dirname (getenv "HOME")
 dired-auto-revert-buffer t
 dired-dwim-target t
 dired-listing-switches "-al"
 ;; dired-omit-files (concat "^\\.?#\\|^\\.$\\|^\\.\\.$\\|_flymake\\.py$\\|"
 ;;                          "^\\.git\\|^\\.dir-locals\\|^\\.pytest_cache")
 ;; Don't omit parent directory (why?!! would anyone do this?)
 dired-omit-files (concat "^\\.?#\\|^\\.$\\\|_flymake\\.py$\\|"
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
             (ibuffer-switch-to-saved-filter-groups "std")))
;;           (ibuffer-vc-set-filter-groups-by-vc-root)))   ; Group by .git project

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
              (vc-status 14 14 :left)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

;; Re-enable SPACE as completion character in find-file, etc. See etc/NEWS 22.1.
(define-key minibuffer-local-filename-completion-map
  " " 'minibuffer-complete-word)
(define-key minibuffer-local-must-match-filename-map
  " " 'minibuffer-complete-word)  

;; Setup for modes

;; C and related modes
(add-hook 'c-mode-common-hook (lambda ()
  "-amh- customisations for all of c-mode and related modes"
  (progn
    (setq-local comment-auto-fill-only-comments t)
    (setq
     tab-width 4
     indent-tabs-node nil
     c-basic-offset 4))))

;; Org mode
(use-package org-bullets :demand)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package org
  :after org-bullets
  :config
  (setq org-archive-location "~/org/archive.org::"
        org-src-fontify-natively t
        org-hide-emphasis-markers t
        org-default-notes-file (concat org-directory "/todo.org")
        org-agenda-files '("~/pv/wip/WIP.org" "~/org")

        org-export-backends '(ascii html iacalendar latex md odt))

  (defun my-org-files-list ()
    (remove (expand-file-name "~/org/archive.org")
            (directory-files "~/org" t ".*\.org")))

  (setq org-refile-allow-creating-parent-nodes t
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps t
        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto))

        org-refile-targets '((nil :maxlevel . 2)
                             (my-org-files-list :maxlevel . 2)
                             ("~/pv/wip/oWIP.org" :maxlevel . 1)
                             ("staff.org" :maxlevel . 2)
                             ("todo.org" :maxlevel . 2)))

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
    (auto-save-visited-mode)

;   (set (make-local-variable 'auto-save-visited-file-name) t)
    (setq auto-save-interval 20))

  (add-hook 'org-mode-hook 'my/org-mode-buffer-setup))


(add-hook 'org-mode-hook 'my-org-mode-autosave-settings)
(defun my-org-mode-autosave-settings ()
  ;; (auto-save-mode 1)   ; this is unnecessary as it is on by default
  (set (make-local-variable 'auto-save-visited-file-name) t)
  (setq auto-save-interval 20))

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

;; hs-minor-mode
(add-hook 'c-common-mode-hook (lambda () (hs-minor-mode 1)))
(add-hook 'python-mode-hook (lambda () (hs-minor-mode 1)))
(add-hook 'sh-mode-hook (lambda () (hs-minor-mode 1)))

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
