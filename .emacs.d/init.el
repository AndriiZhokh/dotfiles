(scroll-bar-mode -1)                                                ; hide scroll bar
(tool-bar-mode -1)                                                  ; hide tool bar
(tooltip-mode -1)                                                   ; disable tooltips
(electric-pair-mode 1)                                              ; auto close brackets
(menu-bar-mode -1)                                                  ; hide menu bar
(global-hl-line-mode 1)                                             ; highlight current line
(global-display-line-numbers-mode 1)                                ; display line numbers
(set-fringe-mode 10)                                                ; vertical margins

(setq display-line-numbers-type 'relative)                          ; display relative line numbers
(setq inhibit-startup-message t)                                    ; do not show default startup screen
(setq visible-bell t)                                               ; set up visible bell
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))          ; set back up directory to backup files

(set-face-attribute 'default nil :font "Cascadia Code" :height 110) ; set up font

(load-theme 'modus-operandi)                                        ; set default theme

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; I don't know why I can't install this package
(use-package command-log-mode)
;; global-command-log-mode need to be enabled first
;; clm/toggle-command-log-buffer C-c o to display buffer that will show your current pressed keys and commands

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initialinputs-alist nil))

(use-package evil
  :config
  (evil-mode 1))

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

					; Commands
;; C-h f --- describe function
;; C-h v --- describe variable
;; C-x C-f - find file
;; C-c o --- toggle command log buffer

					; TODOs
;; TODO: ...

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(command-log-mode doom-modeline use-package markdown-mode evil counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
