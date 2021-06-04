(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;; EVIL
(unless (package-installed-p 'evil)
  (package-install 'evil))
(unless (package-installed-p 'general) ; used for the jk binding
  (package-install 'general))

(require 'evil)
(require 'general)
(evil-mode 1)
(general-evil-setup)
(general-imap "j"
  (general-key-dispatch 'self-insert-command
    :timeout 0.1
    "k" 'evil-normal-state))

(with-eval-after-load 'evil-maps ; don't fuck up org mode
  (define-key evil-motion-state-map (kbd "SPC") nil)
  (define-key evil-motion-state-map (kbd "RET") nil)
  (define-key evil-motion-state-map (kbd "TAB") nil))

					;
;; incremental search
(unless (package-installed-p 'selectrum)
  (package-install 'selectrum))
(require 'selectrum)
(selectrum-mode +1)
(setq completion-ignore-case t)

;; org mode setup
(unless (package-installed-p 'org-roam)
  (package-install 'org-roam))
(require 'org)

(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-directory "~/Notes")
(setq org-agenda-files '("~/Notes"))

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

(global-set-key (kbd "C-c n") 'org-roam-find-file)
(global-set-key (kbd "C-c i") 'org-roam-insert)
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


;; making emacs pretty
(unless (package-installed-p 'gruvbox-theme)
  (package-install 'gruvbox-theme))
(load-theme 'gruvbox-light-soft t)


(set-face-attribute 'default nil :font "JetBrains Mono" :height 120)
(let ((bg "#d5c4a1"))
  (set-face-attribute 'mode-line nil
		      :box `(:line-width 8 :color ,bg)
		      :background bg))
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq org-hide-leading-stars nil)
(set-face-attribute 'org-document-title nil :font "Liberation Serif" :height 3.0 :foreground "#32302f")


;; show the index on load
(setq inhibit-startup-message t)
(find-file "~/Notes/index.org")


;; misc
(global-set-key (kbd "C-s") 'save-buffer)
(setq backup-directory-alist '(("." . "~/tmp/emacs")))






