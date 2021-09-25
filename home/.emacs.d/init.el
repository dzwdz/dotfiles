(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; buffer switching
; https://emacsredux.com/blog/2013/04/28/switch-to-previous-buffer/
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(when (not package-archive-contents)
  (package-refresh-contents))

;; EVIL
(unless (package-installed-p 'evil)
  (package-install 'evil))
(unless (package-installed-p 'evil-collection)
  (package-install 'evil-collection))
(unless (package-installed-p 'general)
  (package-install 'general))

(setq evil-want-keybinding nil)
(require 'evil)
(require 'evil-collection)
(require 'general)
(evil-mode 1)
(evil-collection-init)
(general-evil-setup)
(general-imap "j"
  (general-key-dispatch 'self-insert-command
    :timeout 0.1
    "k" 'evil-normal-state))
(general-nmap "SPC"
  (general-key-dispatch 'next-char
    "SPC" 'er-switch-to-previous-buffer
    "i"   'org-roam-node-insert))

(unless (package-installed-p 'evil-org)
  (package-install 'evil-org))
(require 'evil-org)
(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; incremental search
(unless (package-installed-p 'selectrum)
  (package-install 'selectrum))
(require 'selectrum)
(selectrum-mode +1)
(setq completion-ignore-case t)

(unless (package-installed-p 'selectrum-prescient)
  (package-install 'selectrum-prescient))
(require 'selectrum-prescient)
(selectrum-prescient-mode +1)

;; org mode setup
(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))
(require 'org)

(setq org-roam-directory (file-truename "~/Notes"))
(setq org-agenda-files '("~/Notes"))
(setq org-roam-v2-ack t)
(org-roam-db-autosync-mode)

(setq org-startup-with-inline-images t)
(setq org-startup-with-latex-preview t)
; makes the LaTeX preview bigger
(setq org-format-latex-options '(:foreground default      :background default       :scale 2.0
			    :html-foreground "Black" :html-background "Transparent" :html-scale 1.0
			    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
; disable splitting
(setq org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))

(global-set-key (kbd "s-a") 'org-roam-node-find)
(setq org-return-follows-link t)

; graphical notes
(setq org-file-apps
      (append '(("\\.png\\'" . "krita --nosplash --canvasonly \"%s\"")) org-file-apps))
(defun org-artist-get-path ()
  (format "./img/%s %s.png" (truncate (time-to-seconds)) (car (org-roam--extract-titles))))
(defun org-artist-insert ()
  (interactive)
  (let ((img-path (org-artist-get-path)))
    (call-process "convert" nil nil nil "-size" "512x512" "xc:transparent" img-path)
    (org-insert-link nil img-path))
  (org-redisplay-inline-images))
(global-set-key (kbd "C-l") 'org-redisplay-inline-images)

;; making emacs pretty
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(load-theme 'gruvbox-light-soft t)


(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq org-hide-leading-stars nil)
(set-face-attribute 'org-document-title nil :font "Luxi Serif" :height 3.0 :foreground "#32302f")


;; goyo
(unless (package-installed-p 'visual-fill-column)
  (package-install 'visual-fill-column))
(require 'visual-fill-column)
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(setq-default visual-fill-column-width 100
	      visual-fill-column-center-text t)

;; show the index on load
(setq inhibit-startup-message t)
(find-file "~/Notes/index.org")

;; tabs
(setq whitespace-style '(tab-mark))
(global-whitespace-mode)
(setq indent-tabs-mode t)
(setq tab-width 4)

;; misc
(global-set-key (kbd "C-s") 'save-buffer)
(setq backup-directory-alist '(("." . "~/.tmp/emacs")))
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)

(global-set-key (kbd "C-x C-b") 'bs-show) ; tiny buffer mgmt
