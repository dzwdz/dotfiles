;;; This is a tiny Emacs config, just for Org mode, that tries not to
;;; deviate much from stock Emacs.  In particular, it uses no packages
;;; that aren't included in the stock install (on Debian).

;;; To run code under the cursor, run C-x C-e. I think that requires
;;; you to be on the ending paren.


;; Fix dumb hang when closing Emacs.
(setq x-select-enable-clipboard-manager nil)

;; Don't put ~ backup files everywhere
(setq-default make-backup-files nil)
(setq-default cursor-type 'bar)

;; Looks.
(load-theme 'modus-operandi)
(blink-cursor-mode 0) ; too distracting
(set-frame-font "Iosevka Fixed 14")

(setq inhibit-startup-screen t)

(tool-bar-mode -1)
(global-tab-line-mode 1)

;; Lesser evil: if you're going to be horrible about indenting with
;; tabs, please just don't use them, even if they're superior.
(setq-default indent-tabs-mode nil)

;; Display whitespace if programming.
(add-hook 'prog-mode-hook #'whitespace-mode)
(setq whitespace-style '(face trailing tabs tab-mark))

;; Tell me where I am.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(global-display-fill-column-indicator-mode)
(setq-default column-number-mode 1)

;; Potentially controversial territory.
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Lisp parens
(show-paren-mode 1)

;; Scroll a line at a time instead of moving so point is at the middle
;; of the frame, basically the vi scrolling behavior.
(setq scroll-step 1)

;; C-c, C-x, C-v
(cua-mode 1)

;;; Org mode stuff
(require 'org)
(setq calendar-week-start-day 1) ; week starts on Monday
;;; Those keybindings are standard, directly from Org's docs.
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-agenda-files (quote ("~/org")))

(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-span 14)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(org-agenda-structure ((t (:height 1.0)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
