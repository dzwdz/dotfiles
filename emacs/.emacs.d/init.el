(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'evil)
  (package-install 'evil))
(unless (package-installed-p 'general)
  (package-install 'general))
(unless (package-installed-p 'selectrum)
  (package-install 'selectrum))
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))

(require 'general)
(require 'evil)
(evil-mode 1)
(general-evil-setup)
(general-imap "j"
  (general-key-dispatch 'self-insert-command
    :timeout 0.1
    "k" 'evil-normal-state))

(require 'selectrum)
(selectrum-mode +1)

(setq inhibit-startup-message t)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(load-theme 'gruvbox-light-soft t)

(global-set-key (kbd "C-s") 'save-buffer)

; modeline
(set-face-attribute 'default nil :font "Jetbrains Mono" :height 120)
(let ((bg "#d5c4a1"))
  (set-face-attribute 'mode-line nil
		      :box `(:line-width 8 :color ,bg)
		      :background bg))

(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

(setq org-startup-with-inline-images t)

(find-file "~/Notes/index.org")
(setq org-roam-directory "~/Notes")
(add-hook 'after-init-hook 'org-roam-mode)
(global-set-key (kbd "C-c n") 'org-roam-find-file)
(global-set-key (kbd "C-c i") 'org-roam-insert)
(setq org-return-follows-link t)
(setq org-hide-leading-stars t)

(setq backup-directory-alist '(("." . "~/tmp/emacs")))

(setq completion-ignore-case t)

(setq org-agenda-files '("~/Notes"))
