;;; "Compiled" snippets and support files for `prog-mode'  -*- lexical-binding:t -*-
;;; contents of the .yas-setup.el support file:
;;;
;;; -*- lexical-binding: t -*-
(require 'yasnippet)

;; whitespace removing functions from Magnar Sveen ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun yas-s-trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun yas-s-trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun yas-s-trim (s)
  "Remove whitespace at the beginning and end of S."
  (yas-s-trim-left (yas-s-trim-right s)))


(defun yas-string-reverse (str)
  "Reverse a string STR manually to be compatible with emacs versions < 25."
  (apply #'string
         (reverse
          (string-to-list str))))

(defun yas-trimmed-comment-start ()
  "This function returns `comment-start' trimmed by whitespaces."
  (yas-s-trim comment-start))

(defun yas-trimmed-add-comment ()
  "This function returns `comment-start' trimmed by whitespaces. It uses
`comment-add' to determine how many comment symbols to insert."
  (yas-s-trim (apply #'concat (mapcar (lambda (x)
                                        comment-start)
                                      (number-sequence 0 comment-add)))))

(defun yas-trimmed-comment-end ()
  "This function returns `comment-end' trimmed by whitespaces if
`comment-end' is not empty. Otherwise the reversed output of
function `yas-trimmed-comment-start' is returned."
  (if (eq (length comment-end) 0)
      (yas-string-reverse (yas-trimmed-comment-start))
    (yas-s-trim comment-end)))
;;; Snippet definitions:
;;;
(yas-define-snippets 'prog-mode
                     '(("x" "`comment-start`XXX: $0`comment-end`" "xxx" nil nil nil "/Users/yojan/.emacs.d/snippets/prog-mode/xxx" nil nil)
                       ("t" "`comment-start`TODO: $0`comment-end`" "todo" nil nil nil "/Users/yojan/.emacs.d/snippets/prog-mode/todo" nil nil)
                       ("spl" "`(yas-trimmed-add-comment)` SPDX-License-Identifier: $0`comment-end`" "spdxlicense" nil nil nil "/Users/yojan/.emacs.d/snippets/prog-mode/spdxlicense" nil nil)
                       ("spc" "`(yas-trimmed-add-comment)` SPDX-FileCopyrightText: $0`comment-end`" "spdxcopyright" nil nil nil "/Users/yojan/.emacs.d/snippets/prog-mode/spdxcopyright" nil nil)
                       ("fi" "`comment-start`FIXME: $0`comment-end`" "fixme"
                        (not
                         (member major-mode
                                 '(sh-mode bash-ts-mode)))
                        nil nil "/Users/yojan/.emacs.d/snippets/prog-mode/fixme" nil nil)
                       ("col" "`(yas-trimmed-comment-start)` ${1:comment} ${1:$(let* ((start (yas-trimmed-comment-start))\n                                                       (lastcom (aref start (1- (length start))))\n                                                       (end (yas-trimmed-comment-end))\n                                                       (endpadlen (- 79 (+ (current-column) (length end)))))\n                                              (concat (make-string (max endpadlen 0) lastcom)\n                                                      end))}$0" "commentline" nil nil nil "/Users/yojan/.emacs.d/snippets/prog-mode/commentline" nil nil)
                       ("cob" "${1:$(let* ((col (current-column))\n           (str \"\")\n           (lastcom (substring (yas-trimmed-comment-start) -1))\n           (start (yas-trimmed-comment-start))\n           (end (yas-trimmed-comment-end))\n           (over (- (+ (string-width yas-text) (length start) (length end) col) 77)))\n         (while (< (length str) (+ (- 79 (length start) (length end) col) (if (> over 0) over 0)))\n                (setq str (concat str lastcom)))\n       (concat start str end))}\n${1:$(let* ((col (current-column))\n           (str \"\")\n           (start (yas-trimmed-comment-start))\n           (start (if (string= start \";\") \";;\" start))\n           (end (yas-trimmed-comment-end)))\n         (while (< (length str) (ffloor (/ (- 78.0 (+ col (length start) (string-width yas-text) (length end))) 2.0)))\n                (setq str (concat str \" \")))\n        (concat start str))} ${1:comment} ${1:$(\n        let* ((col (current-column))\n              (str \"\")\n              (start (yas-trimmed-comment-start))\n              (start (if (string= start \";\") \";;\" start))\n              (end (yas-trimmed-comment-end)))\n              (while (< (length str)\n                        (- 79 (length end)\n                           (if (eq (mod (+ (string-width yas-text) (length start) (length end)) 2) 1) (- col 1) col)))\n              (setq str (concat str \" \")))\n                                                 (concat str end))}\n${1:$(let* ((col (current-column))\n           (str \"\")\n           (lastcom (substring (yas-trimmed-comment-start) -1))\n           (start (yas-trimmed-comment-start))\n           (end (yas-trimmed-comment-end))\n           (over (- (+ (string-width yas-text) (length start) (length end) col) 77)))\n         (while (< (length str) (+ (- 79 (length start) (length end) col) (if (> over 0) over 0)))\n                (setq str (concat str lastcom)))\n       (concat start str end))}$0" "commentblock" nil nil nil "/Users/yojan/.emacs.d/snippets/prog-mode/commentblock" nil nil)
                       ("co" "`(yas-trimmed-comment-start)` ${1:comment}`(unless (eq (length comment-end) 0) (concat \" \" (yas-trimmed-comment-end)))`$0" "comment" nil nil nil "/Users/yojan/.emacs.d/snippets/prog-mode/comment" nil nil)))


;;; Do not edit! File generated at Thu Nov 20 13:20:18 2025
