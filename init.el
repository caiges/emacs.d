
;; Fix SSL issue where we can't hit elpa!
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Don't need buttons in emacs! By running this early we avoid a flash
;; of buttons before they are removed.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)

;; Put temp files in the system's temp directory.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Line numbers!
(global-display-line-numbers-mode)

;; BORING: Ensure everything is UTF-8 all the time
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(if (string-equal system-type "windows-nt")
    (set-clipboard-coding-system 'utf-16le-dos)
  (set-clipboard-coding-system 'utf-8))

;; Tab versus spaces
(setq-default indent-tabs-mode nil)
(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if        
  ;; neither, we use the current indent-tabs-mode                               
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))
(infer-indentation-style)

;; indent by 2
(setq tab-stop-list (number-sequence 2 120 2))

;; don't fuck with the previous line when RET is pressed to go to the next line


;; Add the fantastic marmalade package repository to your lists to access hundreds of packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(dolist (source package-archives)
  (add-to-list 'package-archives source t))

(package-initialize)

(package-refresh-contents)

;; Automatically install a bunch of useful packages. You should look read up about these.
(setq my-packages
      '(
	use-package
        ))

(dolist (package my-packages) 
  (unless (package-installed-p package)
    (package-install package)))

;; Fire up the minor modes the theme we want going all the time everywhere
(require 'use-package)

(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (global-set-key [f8] 'neotree-toggle)
)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-city-lights t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ace-window
  :bind (
         ("M-o" . ace-window)
        ) 
  :ensure t)

(use-package ido
  :ensure t)

(use-package json
  :ensure t)

(use-package magit
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package web-mode
  :ensure t)

(use-package undo-tree
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package enh-ruby-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package ansible
  :ensure t)

(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
)

(use-package groovy-mode
  :ensure t)

(global-undo-tree-mode)
(ido-mode t)

;; Settings for ido. The most important one is fuzzy matching, like Sublime Text.
(setq
 ido-case-fold t
 ido-enable-prefix nil
 ido-enable-flex-matching t
 ido-create-new-buffer 'always
 ido-use-filename-at-point nil
 ido-max-prospects 10
 
 yas/prompt-functions '(yas/ido-prompt)
 )

;; Some keybindings for the extra packages and improved built-ins
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; This one is kind of big, it reindents when hitting return. Something you usually need to do manually.
;;(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key "\M-s" 'other-window)

;; Ruby specific settings
(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

;; YAML settings
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Ansible settings
(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))

;; Shell settings
(add-to-list 'display-buffer-alist
             `(,(rx bos "*shell*")
               display-buffer-same-window
               (reusable-frames . visible)))

;; Golang settings
(add-hook 'before-save-hook #'gofmt-before-save)


;; put custom-set-variables in another file instead of this one
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
