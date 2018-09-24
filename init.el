;; Author: Andrew Hilborne
;;
;; I've been doing this for nearly 20 years, but in 2018 I have stolen
;; some ideas from Kyle Purdon @
;; https://realpython.com/emacs-the-best-python-editor/
;;

;; START - especially load-path
;; --------------------------------------

; These stanzas enable us to run from an init file which isn't ~/.emacs.d/init.el. Like this:
;       emacs -q -l "this file"
(setq user-init-file (or load-file-name (buffer-file-name))
      user-emacs-directory (file-name-directory user-init-file))

(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "elisp")))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)
  
;; INSTALL PACKAGES
;; --------------------------------------

; I think this will cause package.el to load tha autload files from
; all the packages in package-selected-packages, which is surely going
; to remove some of the advantages of using use-cusom below. What this
; _does_ mean is that {init.el, custom.el} are sufficient to load
; emacs as we want it on a brand new machine.
(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

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

;; BASIC CUSTOMIZATION
;; --------------------------------------
(require 'use-package)
(setq use-package-always-ensure t)

; load material theme
(use-package material-theme
	     :config
	     (load-theme 'material t))

;; (set-face-attribute 'default nil :family "Courier" :foundry "Adobe" :height 87)
(set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :height 87)

(use-package elpy
  :config
  (setq elpy-modules (delete 'elpy-module-highlight-indentation elpy-modules))
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt")
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
(use-package pipenv)
