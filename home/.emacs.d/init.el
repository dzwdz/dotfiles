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
(setq org-agenda-todo-ignore-scheduled 'future)

(setq org-agenda-files (quote ("~/org")))

(setq org-agenda-time-grid '((today require-timed remove-match)
                             (800 945 1130 1315 1500)
                             ".." "┈┈┈┈┈┈┈┈┈┈┈┈┈")
      org-agenda-current-time-string "┈┈┈┈┈┈┈┈┈┈┈┈┈ now")

(setq org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-prefix-format '((agenda . "%i %?-12t% s") ; hide file info
                                 (todo . "%c ❱ %b")        ; breadcrumbs
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))
(setq org-agenda-format-date
      (lambda (date)
        (let ((fmt (concat "┈ "
                           (org-agenda-format-date-aligned date)
                           " ")))
          (concat "\n" fmt
                  (make-string (- fill-column (length fmt)) ?┈)))))

(defun dzwdz/apply-org-styles ()
  ;; Get rid of the annoying background.
  (set-face-attribute 'org-agenda-date-today nil :background nil)
  ;; Make TODOs stand out (applies everywhere, but matters especially
  ;; in the agenda).
  (set-face-attribute 'org-todo nil :weight 'black))
(add-hook 'modus-themes-after-load-theme-hook 'dzwdz/apply-org-styles)

(setq org-agenda-deadline-leaders '("Deadline:  "
                                    "   in %2dd: "
                                    "%2dd. late: "))
(setq org-agenda-scheduled-leaders '("Scheduled: "
                                     " %2dd. ago: "))

;; Hide the ugly block separator (defaults to ======)
(setq org-agenda-block-separator ?\s)

(setq org-agenda-custom-commands
      '(("n" "Agenda and unassigned to-dos"
         ((agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'scheduled 'deadline))))))))

(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up timestamp-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

(setq org-capture-templates
      '(
        ;; https://karl-voit.at/2014/08/10/bookmarks-with-orgmode/
        ("b" "Bookmark"
         entry (file+headline "~/org/tabdump.org" "Uncategorized")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")

        ("c" "Generic"
         entry (file "~/org/misc.org")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")

        ;; https://www.howardism.org/Technical/Emacs/journaling-org.html
        ("j" "Journal Entry"
         entry (file+datetree "~/org/journal.org")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n")
        ))

(add-to-list 'org-modules 'org-habit t)
(setq org-extend-today-until 4
      org-use-effective-time t)

(setq org-habit-graph-column (- 80  ; fill column
				1   ; otherwise the fill column indicator breaks
				21  ; org-habit-preceding-days (not available yet)
				7)) ; org-habit-following-days (not available yet)

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

(setq modus-themes-headings '((t . (background overline))))

(load-theme 'modus-operandi)
;; This hook usually only runs after modus-theme-toggle, so I'll
;; trigger it manually.
(run-hooks 'modus-themes-after-load-theme-hook)

(set-face-attribute 'default nil :font "Iosevka 14" :weight 'normal)
(setq-default cursor-type 'bar)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(setq-default fill-column 80)
(global-display-fill-column-indicator-mode)
(setq-default column-number-mode 1)

(setq confirm-kill-emacs #'yes-or-no-p)

(icomplete-mode 1)                      ; Probably redundant.
(icomplete-vertical-mode 1)

(define-key icomplete-minibuffer-map [?\t] 'icomplete-force-complete)

(defun dzwdz/build-file-cache ()
  (interactive)                         ; why not
  (file-cache-add-directory-using-find "~/org")
  (file-cache-add-file "~/src/dotfiles/home/.emacs.d/init.org")
  (file-cache-add-file "~/sleep"))
(eval-after-load "filecache" #'dzwdz/build-file-cache)

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
