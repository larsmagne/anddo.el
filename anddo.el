;;; anddo.el --- simple todo list management -*- lexical-binding: t -*-
;; Copyright (C) 2024 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: efficiency

;; This file is not part of GNU Emacs.

;; anddo.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;;; Code:

(require 'sqorm)
(require 'cl-lib)

(defvar anddo-listing-mode 'new
  "Default listing mode for anddo.
Possible values are `new', `all' and `most'.")

(defconst anddo--tables
  '((item
     (id integer :primary)
     (status text)
     (subject text)
     (body text)
     (entry-time text)
     (modification-time text))))

(defun anddo--create-tables ()
  (sqorm-open (expand-file-name "anddo.sqlite3" user-emacs-directory))
  (sqorm-create-tables anddo--tables))

(defun anddo-import-text-file (file)
  "Import a text file into anddo.
The text file is on the format:

New:
* Foo bar
  Something here
* Zot"
  (interactive "fFile to import: ")
  (with-temp-buffer
    (insert-file-contents file)
    (let ((status nil)
	  (subject "")
	  (body ""))
      (while (not (eobp))
	(cond
	 ((looking-at "\\([^: ]+\\):")
	  (setq status (match-string 1)))
	 ((or (looking-at "[*] \\(.*\\)")
	      (looking-at "\n"))
	  (let ((new-subject (match-string 1)))
	    (when (and status subject)
	      (sqorm-insert
	       (sqorm-make
		'item
		(list nil (downcase status)
		      subject
		      body
		      (format-time-string "%F %T")
		      nil)
		anddo--tables))
	      (setq body ""))
	    (setq subject new-subject)))
	 ((looking-at "  \\(.*\\)")
	  (setq body (concat body (match-string 1)))))
	(forward-line 1)))))

(defun anddo ()
  "Display the todo list."
  (interactive)
  (pop-to-buffer "*anddo*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (anddo-mode)
    (make-vtable
     :columns '("More" "Status" "Item")
     :objects (sqorm-select-where
	       (format
		"select * from item %s order by id"
		(cond
		 ((eq anddo-listing-mode 'new)
		  "where status = 'new'")
		 ((eq anddo-listing-mode 'all)
		  "")
		 (t
		  "where status in ('new', 'possibly', 'in-progress')"))))
     :getter
     (lambda (item column vtable)
       (pcase (vtable-column vtable column)
	 ("More" (if (length= (plist-get item :body) 0)
		     ""
		   "⬇️"))
	 ("Status" (if (equal (plist-get item :status) "new")
		       ""
		     (plist-get item :status)))
	 ("Item" (plist-get item :subject))))
     :keymap anddo-mode-map)))

(defvar anddo-statuses '("new" "done" "in-progress"
			 "possibly" "not-doing"))

(defvar-keymap anddo-mode-map
  "n" #'anddo-new-item
  "l" #'anddo-toggle-listing-mode
  "<RET>" #'anddo-show-body
  "<DEL>" #'anddo-delete-item)

(define-derived-mode anddo-mode special-mode "anddo"
  "Major mode for listing todo lists."
  (setq truncate-lines t))

(defun anddo-toggle-listing-mode ()
  "Cycle through three listing modes: New-only, non-closed, all."
  (interactive)
  (let ((id (plist-get (vtable-current-object) :id)))
    (setq anddo-listing-mode
	  (cl-case anddo-listing-mode
	    ('new 'more)
	    ('more 'all)
	    ('all 'new)))
    (anddo)
    (when id
      (when-let ((match (text-property-search-forward
			 'vtable-object id (lambda (v item)
					     (= v (plist-get item :id))))))
	(goto-char (prop-match-beginning match))))))

(defun anddo-show-body ()
  "Display the body of an item, if any."
  (interactive)
  (let ((body (plist-get (vtable-current-object) :body)))
    (if (zerop (length body))
	(user-error "No body for the current item")
      (message "%s" body))))

(provide 'anddo)

;;; anddo.el ends here
