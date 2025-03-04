(require 'org)

(setq org-id-link-to-org-use-id 'create-if-interactive)

;(setq org-startup-indented t)

;; Don't insert the annoying blank lines when doing M-RET.
(setq org-blank-before-new-entry '((heading . nil)
                                   (plain-list-item . nil)))
;; Also, don't split lines.
(setq org-M-RET-may-split-line '((default . nil)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)" "DEAD(x)")))

(setq org-use-fast-todo-selection 'expert)

(setq calendar-week-start-day 1)        ; Weeks start on Monday.
(setq org-agenda-start-on-weekday nil)  ; Don't show past days.

(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-span 7)

(setq org-agenda-files (quote ("~/org")))

(add-to-list 'org-modules 'org-habit t)
(setq org-extend-today-until 4
      org-use-effective-time t)

;; Lesser evil: if you're going to be horrible about indenting with
;; tabs, please just don't use them, even if they're superior.
(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook #'whitespace-mode)
(setq whitespace-style '(face trailing tabs tab-mark))

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq-default display-line-numbers-width 4)

(show-paren-mode 1)

;; C-c, C-x, C-v
(cua-mode 1)
;; I have strong muscle memory for C-w from Vim, so I keep erasing
;; stuff by accident.
(global-unset-key (kbd "C-w"))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(global-tab-line-mode 1)
(setq tab-line-separator "") ; Pack the tabs tightly together.

(setq tab-line-close-tab-function
      (lambda (tab)
        (let* ((buffer ; as in tab-line.el, no clue how this works
                (if (bufferp tab) tab (cdr (assq 'buffer tab)))))
          (cond
           ;; If this is the last tab, close the window.
           ((<= (length (funcall tab-line-tabs-function)) 1)
            (delete-window))
           ;; Otherwise, proceed as in the default implementation
           ;; for bury-buffer.
           ((eq buffer (current-buffer))
            (bury-buffer))
           (t
            (set-window-prev-buffers
             nil (assq-delete-all buffer (window-prev-buffers)))
            (set-window-next-buffers
             nil (delq buffer (window-next-buffers))))))))

(load-theme 'modus-operandi)            ; Best looking default theme.
(set-face-attribute 'default nil :font "Iosevka Light 14")
(setq-default cursor-type 'bar)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(defun set-margins (faces line-width)
  (dolist (face faces)
    (set-face-attribute
     face nil
     :box
     (if line-width
         `(:line-width ,line-width :color ,(face-attribute face :background))
       nil))))

(setq modus-themes-after-load-theme-hook
      (lambda ()
        (set-margins '(tab-line-tab tab-line-tab-inactive) '(7 . 4))
        (set-margins '(mode-line mode-line-inactive) '(7 . 4))))
;; This hook usually only runs after modus-theme-toggle, so let's
;; trigger it manually.
(run-hooks 'modus-themes-after-load-theme-hook)

(setq fill-column 80)
(global-display-fill-column-indicator-mode)
(setq-default column-number-mode 1)

(setq confirm-kill-emacs #'yes-or-no-p)

(icomplete-mode 1)                      ; Probably redundant.
(icomplete-vertical-mode 1)

(define-key icomplete-minibuffer-map [?\t] 'icomplete-force-complete)

(setq scroll-step 1)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

(setq-default make-backup-files nil)

(setq x-select-enable-clipboard-manager nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(org-agenda-structure ((t (:height 1.0)))))
