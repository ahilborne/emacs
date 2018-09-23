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
(setq use-package-always-ensure t)

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t)        ;; hide the startup message
(use-package material-theme
	     :config
(load-theme 'material t))                ;; load material theme

;; (set-face-attribute 'default nil :family "Courier" :foundry "Adobe" :height 87)
(set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :height 87)

(use-package elpy
  :config
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt")
  (elpy-enable))
(use-package magit
  :bind ("C-x g" . magit-status))
(use-package pipenv)
