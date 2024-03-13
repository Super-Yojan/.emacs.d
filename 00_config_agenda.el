;;;; General Agenda Settings
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
(setq org-startup-folded t)

(setq org-use-fast-todo-selection t)
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "|" "DONE(d)")
	(sequence "TASK(T)")
	(sequence "AMOTIVATOR(MA)" "TMOTIVATOR(MT)" "CMOTIVATOR(MC)" "|")
	(sequence "WAITING(w@/!)" "INACTIVE(i)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)")))
;; Custom colors for the keywords
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
	("TASK" :foreground "#5C888B" :weight bold)
	("NEXT" :foreground "blue" :weight bold)
	("PROJ" :foreground "magenta" :weight bold)
	("AMOTIVATOR" :foreground "#F06292" :weight bold)
	("TMOTIVATOR" :foreground "#AB47BC" :weight bold)
	("CMOTIVATOR" :foreground "#5E35B1" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("INACTIVE" :foreground "magenta" :weight bold)
	("SOMEDAY" :foreground "cyan" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)))
;; Auto-update tags whenever the state is changed
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
	("WAITING" ("SOMEDAY") ("INACTIVE") ("WAITING" . t))
	("INACTIVE" ("WAITING") ("SOMEDAY") ("INACTIVE" . t))
	("SOMEDAY" ("WAITING") ("INACTIVE") ("SOMEDAY" . t))
	(done ("WAITING") ("INACTIVE") ("SOMEDAY"))
	("TODO" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("TASK" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("NEXT" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("PROJ" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("AMOTIVATOR" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("TMOTIVATOR" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("CMOTIVATOR" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))
	("DONE" ("WAITING") ("CANCELLED") ("INACTIVE") ("SOMEDAY"))))


;; == Refile ==
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;;  Be sure to use the full path for refile setup
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; == Archive ==
(setq org-archive-location "archive/%s_archive::")
(defvar org-archive-file-header-format "#+FILETAGS: ARCHIVE\nArchived entries from file %s\n")


(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))


(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))



(require 'org-agenda)

(setq org-agenda-files (quote ("~/org")))
(setq org-default-notes-file "~/org/refile.org")
(setq org-agenda-tags-column org-tags-column)
(setq org-agenda-sticky t)
(setq org-agenda-inhibit-startup nil)
(setq org-agenda-dim-blocked-tasks nil)

;; Set the times to display in the time grid
(setq org-agenda-time-grid
      (quote
       ((daily today remove-match)
        (800 1200 1600 2000)
        "......" "----------------")))

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
        (sequence "TASK(f)" "|" "DONE(d)")
	(sequence "NEXT(n)" "|" "DOING(d)")
        (sequence "MAYBE(m)" "|" "CANCELLED(c)")))


(setq org-capture-templates
       '(("t" "todo" entry (file org-default-notes-file)
	  "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	 ("m" "Meeting" entry (file org-default-notes-file)
	  "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	 ("d" "Diary" entry (file+datetree "~/org/diary.org")
	  "* %?\n%U\n" :clock-in t :clock-resume t)
	 ("i" "Idea" entry (file org-default-notes-file)
	  "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	 ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	  "** NEXT %? \nDEADLINE: %t") ))

(defun org-toggle-todo-and-fold ()
  (interactive)
  (if (equal major-mode 'org-mode)
  (save-excursion
    (org-back-to-heading t) ;; Make sure command works even if point is
                            ;; below target heading
    (cond ((looking-at "\*+ TODO")
           (org-todo "NEXT")
           (hide-subtree))
          ((looking-at "\*+ DONE")
           (org-todo "TODO")
           (hide-subtree))
          ((looking-at "\*+ NEXT")
           (org-todo "DONE")
           (hide-subtree))
          (t (message "Can only toggle between TODO and DONE."))))))


(defun gs/org-agenda-add-location-string ()
  "Gets the value of the LOCATION property"
  (let ((loc (org-entry-get (point) "LOCATION")))
    (if (> (length loc) 0)
	(concat "{" loc "} ")
      "")))

(defun gs/org-agenda-prefix-string ()
  "Format"
  (let ((path (org-format-outline-path (org-get-outline-path))) ; "breadcrumb" path
	(stuck (gs/org-agenda-project-warning))) ; warning for stuck projects
       (if (> (length path) 0)
	   (concat stuck ; add stuck warning
		   " [" path "]") ; add "breadcrumb"
	 stuck)))


(defvar org-agenda-display-settings
  '((org-agenda-start-with-log-mode t)
    (org-agenda-log-mode-items '(clock))
    (org-agenda-prefix-format '((agenda . "  %-12:c%?-12t %(gs/org-agenda-add-location-string)% s")
				(timeline . "  % s")
				(todo . "  %-12:c %(gs/org-agenda-prefix-string) ")
				(tags . "  %-12:c %(gs/org-agenda-prefix-string) ")
				(search . "  %i %-12:c")))
    (org-agenda-todo-ignore-deadlines 'near)
    (org-agenda-todo-ignore-scheduled t))
  "Display settings for my agenda views.")

(setq org-agenda-custom-commands
 `(("h" "Habits" agenda "STYLE=\"habit\""
	 ((org-agenda-overriding-header "Habits")
	  (org-agenda-sorting-strategy
	   '(todo-state-down effort-up category-keep))))
      ("a"
       ,org-agenda-display-settings)))

