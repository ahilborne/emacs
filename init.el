;; Author: Andrew Hilborne
;;
;; I've been doing this for nearly 20 years, but in 2018 I have stolen
;; some ideas from Kyle Purdon @
;; https://realpython.com/emacs-the-best-python-editor/
;;

;; START - especially load-path
;; --------------------------------------

; These stanzas enable us to run from an init file which isn't
; ~/.emacs.d/init.el. Like this: emacs -q -l "this file"
(setq user-init-file (or load-file-name (buffer-file-name))
      user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp")))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
  
;; INITIAL PACKAGES
;; --------------------------------------

; I think this will cause package.el to load tha autoload files from
; all the packages in package-selected-packages, which is surely going
; to remove some of the advantages of using use-package below. What this
; _does_ mean is that {init.el, custom.el} are sufficient to load
; emacs as we want it on a brand new machine.
(require 'package)

(add-to-list 'package-archives
       '("melpa" . "https://melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages '(
    ; better-defaults
    use-package
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
;; NB (local-theme 'material-light t) gives the light version, or use
;;    M-x load-theme
(use-package material-theme
	     :config
	     (load-theme 'material t))

(use-package elpy
  :config
  (setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules))
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt"
        comint-scroll-show-maximum-output nil) ; see var docs
  (add-hook 'python-mode-hook
            (lambda()(pyvenv-mode)(pyvenv-tracking-mode)))
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

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package filladapt
  :config
  (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
  (add-hook 'text-mode-hook '(lambda() (setq fill-column 80))))

;; General cruft from down the years, but tidied up a little
;; --------------------------------------

;; Default face
;; (set-face-attribute 'default nil :family "Courier" :foundry "Adobe" :height 87)
;; TODO install Source Code Pro first
(set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :height 87)

;; Global values for variables. Shouldn't be used for vars which can be
;; buffer-local
(setq
 auto-compression-mode t                ; for .gz files, etc
 change-log-default-name "~/NOTES"
 column-number-mode t
 completion-ignored-extensions
   (quote ("CM/" "CVS/" ".so" ".o" ".obj" ".elc" "~" ".bin" ".lbin" ".dvi" ".class"))
 delete-key-deletes-forward t
 delete-selection-mode 1
 inhibit-startup-screen t
 line-number-mode t
 make-backup-files nil
 mouse-yank-at-point t
 scroll-step 1
 set-scroll-bar-mode 'right
 split-width-threshold 150              ; for narrower-than laptop display
 vc-revert-show-diff nil
 visible-bell t
 )

;; Affect all buffers, until var is set explicitly (buffer-local)
;;
(setq-default indent-tabs-mode nil)

(show-paren-mode t)
(tool-bar-mode 0)
(blink-cursor-mode 0)
;; Why can't I do this with setq?
(menu-bar-mode -1)
;; or this?
(put 'narrow-to-region 'disabled nil)
;; or this?
;; XXX Brand new in 2018!
(window-divider-mode t)

;; Fix for ediff problem
(set-variable 'ediff-coding-system-for-write 'raw-text)

(require 'dired-x)  ; Get back some of those nice features from xemacs!

;; Keys for electric-buffer/ibuffer
(require 'ibuffer)
(global-set-key "\C-c\C-b" 'ibuffer)
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

;; Make ibuffer more like electric-buffer-list-mode
(define-key ibuffer-mode-map (kbd "SPC") 'ibuffer-visit-buffer)
(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)))
;;	     (ibuffer-switch-to-saved-filter-groups "home")))
(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 30 30 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
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
    (setq
     tab-width 4
     indent-tabs-node nil
     c-basic-offset 4)
    (turn-off-filladapt-mode))))

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

;; CPerl mode
(setq cperl-brace-offset -4
      cperl-indent-level 4)

;; Associate extensions with modes
(add-to-list 'auto-mode-alist '("\\.h$"    . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ovpn$" . conf-space-mode))
(add-to-list 'auto-mode-alist '("\\.conf$" . conf-space-mode))

;; Add final message so using C-h l I can see if init failed
(message "init.el loaded successfully.")

;;; Local Variables: ***
;;; End: ***
