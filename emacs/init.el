(scroll-bar-mode -1)                                           ; hide scroll bar
(tool-bar-mode -1)                                             ; hide tool bar
(tooltip-mode -1)                                              ; disable tooltips
(electric-pair-mode 1)                                         ; auto close brackets
(menu-bar-mode -1)                                             ; hide menu bar
(global-hl-line-mode 1)                                        ; highlight current line
(column-number-mode)                                           ; display column number in modeline
(global-display-line-numbers-mode 1)                           ; display line numbers
(set-fringe-mode 10)                                           ; vertical margins
(setq display-line-numbers-type 'relative)                     ; display relative line numbers
(setq inhibit-startup-message t)                               ; do not show default startup screen
(setq visible-bell t)                                          ; set up visible bell

;; hide border around the emacs frame
;; (set-frame-parameter nil 'undecorated t)
;; (add-to-list 'default-frame-alist '(undecorated . t))

(put 'dired-find-alternate-file 'disabled nil)                 ; navigate in dired with 'a' wihtout opening new buffers

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))     ; set back up directory to backup files

;; disable line numbers for specific modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Iosevka NFM" :height 240)                  ; set up font

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes") ; add custom theme directory

(load-theme 'compline t)                                       ; load the theme

;; START setup elpaca package manager
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq package-enable-at-startup nil)                       ; disable default package manager package.el

(elpaca elpaca-use-package
  (elpaca-use-package-mode))                               ; install and enable use-package for elpaca
;; END setup elpaca package manager

(use-package evil
  :ensure t
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
	 ("C-x b" . consult-buffer)
	 ("C-x C-r" . consult-recent-file)))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-attributes
	'(hl-line git-msg file-size collapse subtree-state))
  (setq dirvish-mode-line-format
	'(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-cache-dir (expand-file-name "dirvish/" user-emacs-directory)))
