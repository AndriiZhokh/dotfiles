(scroll-bar-mode -1)                                                ; hide scroll bar
(tool-bar-mode -1)                                                  ; hide tool bar
(tooltip-mode -1)                                                   ; disable tooltips
(electric-pair-mode 1)                                              ; auto close brackets
(menu-bar-mode -1)                                                  ; hide menu bar
(global-hl-line-mode 1)                                             ; highlight current line
(column-number-mode)                                                ; display column number in modeline
(global-display-line-numbers-mode 1)                                ; display line numbers
(set-fringe-mode 10)                                                ; vertical margins

(setq display-line-numbers-type 'relative)                          ; display relative line numbers
(setq inhibit-startup-message t)                                    ; do not show default startup screen
(setq visible-bell t)                                               ; set up visible bell
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))          ; set back up directory to backup files

(set-face-attribute 'default nil :font "Cascadia Code" :height 110) ; set up font

;; (load-theme 'modus-operandi)                                     ; set default theme

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; global-command-log-mode need to be enabled first
;; clm/toggle-command-log-buffer C-c o to display buffer that will show your current pressed keys and commands
(use-package command-log-mode)

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
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; After first install run the
;; M-x all-the-icons-install-fonts
;; and select directory where do you want to install the font icons
(use-package all-the-icons
  :if (display-graphic-p))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init
  (load-theme 'doom-tokyo-night t)
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap-describe-key] . helpful-key))

(use-package general
  :config
  (general-create-definer batmacs/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (batmacs/leader-key
    "t"  '(:ignore t          :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "f"  '(:ignore t :which-key "file")
    "ff" '(find-file :which-key "find file")
    "b"  '(:ignore t             :which-key "buffer")
    "be" '(eval-buffer           :which-key "eval buffer")
    "bs" '(counsel-switch-buffer :which-key "switch to buffer")
    "bk" '(kill-this-buffer      :which-key "kill current buffer")))

					; Keybindings
;; Global
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; escape to quit prompts
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)   ; switch to buffer

;; Map specific mode
(define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme) ; load custom theme

					; Commands
;; C-h f --- describe function
;; C-h v --- describe variable
;; C-x C-f - find file
;; C-c o --- toggle command log buffer
;; when M-x is started type M-o on some function and you can see additional options that you can choose

					; TODOs
;; TODO: Start Emacs in home directory
;; TODO: font ligatures support
;; TODO: do not open new dired buffer every time when navigating through folders
;; TODO: syntax check for English and Ukrainian languages
;; TODO: Do not wrap lines
;; TODO: transperancy
;; TODO: switch between separated windows and close that separated windows

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(general all-the-icons doom-themes helpful ivy-rich which-key rainbow-delimiters command-log-mode doom-modeline use-package markdown-mode evil counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
