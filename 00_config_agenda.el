;;;; General Agenda Settings


(setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")


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


(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
        (sequence "TASK(f)" "|" "DONE(d)")
        (sequence "MAYBE(m)" "|" "CANCELLED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "DarkOrange1" :weight bold))
        ("MAYBE" . (:foreground "sea green"))
        ("DONE" . (:foreground "light sea green"))
        ("CANCELLED" . (:foreground "forest green"))
        ("TASK" . (:foreground "blue"))))

(setq org-tags-exclude-from-inheritance '("prj")
      org-stuck-projects '("+prj/-MAYBE-DONE"
                           ("TODO" "TASK") ()))

(setq org-agenda-custom-commands
      '(("h" "Work todos" tags-todo
         "-personal-doat={.+}-dowith={.+}/!-TASK"
         ((org-agenda-todo-ignore-scheduled t)))
        ("H" "All work todos" tags-todo "-personal/!-TASK-MAYBE"
         ((org-agenda-todo-ignore-scheduled nil)))
        ("A" "Work todos with doat or dowith" tags-todo
         "-personal+doat={.+}|dowith={.+}/!-TASK"
         ((org-agenda-todo-ignore-scheduled nil)))
        ("j" "TODO dowith and TASK with"
         ((org-sec-with-view "TODO dowith")
          (org-sec-where-view "TODO doat")
          (org-sec-assigned-with-view "TASK with")
          (org-sec-stuck-with-view "STUCK with")))
        ("J" "Interactive TODO dowith and TASK with"
         ((org-sec-who-view "TODO dowith")))))

(defvar org-sec-with "nobody"
  "Value of the :with: property when doing an
   org-sec-tag-entry. Change it with org-sec-set-with,
   set to C-c w")

(defvar org-sec-where ""
  "Value of the :at: property when doing an
   org-sec-tag-entry. Change it with org-sec-set-with,
   set to C-c W")

(defvar org-sec-with-history '()
  "History list of :with: properties")

(defvar org-sec-where-history '()
  "History list of :where: properties")

(defun org-sec-set-with ()
  "Changes the value of the org-sec-with variable for use
   in the next call of org-sec-tag-entry."
  (interactive)
  (setq org-sec-with (read-string "With: " nil
                                  'org-sec-with-history "")))
;;(global-set-key "w" 'org-sec-set-with)

(defun org-sec-set-where ()
  "Changes the value of the org-sec-where variable for use
   in the next call of org-sec-tag-entry."
  (interactive)
  (setq org-sec-where
        (read-string "Where: " nil
                     'org-sec-where-history "")))
;;(global-set-key "W" 'org-sec-set-where)


(defun org-sec-set-dowith ()
  "Sets the value of the dowith property."
  (interactive)
  (let ((do-with
         (read-string "Do with: "
                      nil 'org-sec-dowith-history "")))
    (unless (string= do-with "")
      (org-entry-put nil "dowith" do-with))))
(global-set-key "\C-cd" 'org-sec-set-dowith)

(defun org-sec-set-doat ()
  "Sets the value of the doat property."
  (interactive)
  (let ((do-at (read-string "Do at: "
                            nil 'org-sec-doat-history "")))
    (unless (string= do-at "")
      (org-entry-put nil "doat" do-at))))
(global-set-key "\C-cD" 'org-sec-set-doat)

(defun org-sec-tag-entry ()
  "Adds a :with: property with the value of org-sec-with if
   defined, an :at: property with the value of org-sec-where
   if defined, and an :on: property with the current time."
  (interactive)
  (save-excursion
    (org-entry-put nil "on" (format-time-string
                             (org-time-stamp-format 'long)
                             (current-time)))
    (unless (string= org-sec-where "")
      (org-entry-put nil "at" org-sec-where))
    (unless (string= org-sec-with "nobody")
      (org-entry-put nil "with" org-sec-with))))
(global-set-key "\C-cj" 'org-sec-tag-entry)


(defun join (lst sep &optional pre post)
  (mapconcat (function (lambda (x)
                         (concat pre x post)))
             lst sep))

(defun org-sec-with-view (par &optional who)
  "Select tasks marked as dowith=who, where who
   defaults to the value of org-sec-with."
  (org-tags-view '(4) (join (split-string (if who
                                              who
                                            org-sec-with))
                            "|" "dowith=\"" "\"")))

(defun org-sec-where-view (par)
  "Select tasks marked as doat=org-sec-where."
  (org-tags-view '(4) (concat "doat={" org-sec-where "}")))

(defun org-sec-assigned-with-view (par &optional who)
  "Select tasks assigned to who, by default org-sec-with."
  (org-tags-view '(4)
                 (concat (join (split-string (if who
                                                 who
                                               org-sec-with))
                               "|")
                         "/TASK")))

(defun org-sec-stuck-with-view (par &optional who)
  "Select stuck projects assigned to who, by default
   org-sec-with."
  (let ((org-stuck-projects
         `(,(concat "+prj+"
                    (join (split-string (if who
                                            who
                                          org-sec-with)) "|")
                    "/-MAYBE-DONE")
           ("TODO" "TASK") ())))
    (org-agenda-list-stuck-projects)))

(defun org-sec-who-view (par)
  "Builds agenda for a given user.  Queried. "
  (let ((who (read-string "Build todo for user/tag: "
                          "" "" "")))
    (org-sec-with-view "TODO dowith" who)
    (org-sec-assigned-with-view "TASK with" who)
    (org-sec-stuck-with-view "STUCK with" who)))

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
