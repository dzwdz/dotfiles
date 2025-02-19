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
(set-frame-font "Iosevka Fixed Light 14")
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)


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

;; Set a fixed size for the line number column, so it doesn't jump around.
(setq-default display-line-numbers-width 4)

;; Lisp parens
(show-paren-mode 1)

;; Scroll a line at a time instead of moving so point is at the middle
;; of the frame, basically the vi scrolling behavior.
(setq scroll-step 1)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(3 ((shift) . hscroll)))

;; C-c, C-x, C-v
(cua-mode 1)

;;; Org mode stuff
(require 'org)
(setq calendar-week-start-day 1) ; week starts on Monday
;; Those keybindings are standard, directly from Org's docs.
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-agenda-files (quote ("~/org")))

(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
(setq org-agenda-span 14)

(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "|" "DONE" "DEAD")))

(setq org-startup-indented t)

;; Don't insert the annoying blank lines when doing M-RET.
(setq org-blank-before-new-entry
      '((heading . nil)
        (plain-list-item . nil)))
;; Also, don't split lines.
(setq org-M-RET-may-split-line '((default . nil)))

;; Nicer completion.
(icomplete-mode 1)
(icomplete-vertical-mode 1)

;; The default TAB behaviour in icomplete is stupid - force it to
;; complete to the first option.
(define-key icomplete-minibuffer-map [?\t] 'icomplete-force-complete)

;; Tabs.
(setq tab-line-separator "")
(global-tab-line-mode 1)

;; Close the window if the last tab is closed.
;; This is similar to the default bury-buffer behaviour.
;; The function call is wrapped in with-selected window,
;; so it is operating on the window that contains this tab.
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


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t nil)))
 '(org-agenda-structure ((t (:height 1.0)))))
