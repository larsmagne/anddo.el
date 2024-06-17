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
  (unless sqorm-db
    (sqorm-open (expand-file-name "anddo.sqlite3" user-emacs-directory))
    (sqorm-create-tables anddo--tables)))

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

(defun anddo--rank (item)
  (pcase (plist-get item :status)
    ("new" 1)
    ("in-progress" 2)
    ("possibly" 3)
    ("not-doing" 4)
    ("done" 5)))

(defun anddo ()
  "Display the todo list."
  (interactive)
  (pop-to-buffer "*anddo*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (anddo--create-tables)
    (anddo-mode)
    (make-vtable
     :columns '((:name "More" :width 4)
		(:name "Status" :width 4)
		(:name "Item" :width 500))
     :objects (sort
	       (sqorm-select-where
		(format
		 "select * from item %s order by id desc"
		 (cond
		  ((eq anddo-listing-mode 'new)
		   "where status = 'new'")
		  ((eq anddo-listing-mode 'all)
		   "")
		  (t
		   "where status in ('new', 'possibly', 'in-progress')"))))
	       (lambda (i1 i2)
		 (< (anddo--rank i1) (anddo--rank i2))))
     :getter
     (lambda (item column vtable)
       (pcase (vtable-column vtable column)
	 ("More" (if (length= (plist-get item :body) 0)
		     ""
		   "⬇️"))
	 ("Status" (pcase (plist-get item :status)
		     ("new" "⚡")
		     ("in-progress" "🛠️")
		     ("possibly" "❓")
		     ("not-doing" "⛔")
		     ("done" "☑️")))
	 ("Item" (plist-get item :subject))))
     :keymap anddo-mode-map)))

(defvar anddo-statuses '("new" "done" "in-progress"
			 "possibly" "not-doing"))

(defvar-keymap anddo-mode-map
  "n" #'anddo-new-item
  "e" #'anddo-edit-item
  "s" #'anddo-change-status
  "l" #'anddo-toggle-listing-mode
  "<RET>" #'anddo-show-body
  "<DEL>" #'anddo-delete-item
  :menu
  '("Anddo"
    ["Add new todo item" anddo-new-item]
    ["Edit todo item" anddo-edit-item (vtable-current-object)]
    ["Change status" anddo-change-status (vtable-current-object)]
    ["Toggle listing mode" anddo-toggle-listing-mode]
    ["Show the body of the item" anddo-show-body (vtable-current-object)]
    ["Delete todo item" anddo-delete-item (vtable-current-object)]))

(define-derived-mode anddo-mode special-mode "anddo"
  "Major mode for listing todo lists."
  (setq truncate-lines t)
  (make-local-variable 'sqorm-db))

(defun anddo-toggle-listing-mode ()
  "Cycle through three listing modes: New-only, non-closed, all."
  (interactive nil anddo-mode)
  (setq anddo-listing-mode
	(cl-case anddo-listing-mode
	  (new 'more)
	  (more 'all)
	  (all 'new)))
  (anddo--regenerate))

(defun anddo--regenerate ()
  (let ((id (plist-get (vtable-current-object) :id)))
    (anddo)
    (when id
      (when-let ((match (text-property-search-forward
			 'vtable-object id (lambda (v item)
					     (= v (plist-get item :id))))))
	(goto-char (prop-match-beginning match))))))

(defun anddo-show-body ()
  "Display the body of an item, if any."
  (interactive nil anddo-mode)
  (let ((body (plist-get (vtable-current-object) :body)))
    (if (zerop (length body))
	(user-error "No body for the current item")
      (message "%s" body))))

(defun anddo-new-item ()
  "Add a new todo item."
  (interactive nil anddo-mode)
  (let ((lines (string-lines
		(string-trim
		 (read-string-from-buffer "Enter a todo item" "")))))
    (unless (equal lines '(""))
      (sqorm-insert (list :_type 'item
			  :status "new"
			  :subject (pop lines)
			  :body (string-join lines "\n")
			  :entry-time (format-time-string "%F %T")))
      (anddo--regenerate))))

(defun anddo-edit-item ()
  "Edit the item under point."
  (interactive nil anddo-mode)
  (let ((item (vtable-current-object)))
    (unless item
      (user-error "No item under point"))
    (let ((lines (string-lines (read-string-from-buffer
				"Edit the todo item"
				(string-join
				 (list (plist-get item :subject)
				       (plist-get item :body))
				 "\n"))))
	  subject body)
      (sqorm-exec
       "update item set subject = ?, body = ? where id = ?"
       (list (setq subject (pop lines))
	     (setq body (string-join lines "\n"))
	     (plist-get item :id)))
      (plist-put item :subject subject)
      (plist-put item :body body)
      (vtable-update-object (vtable-current-table) item item))))

(defun anddo-delete-item ()
  "Delete the item under point."
  (interactive nil anddo-mode)
  (let ((item (vtable-current-object)))
    (unless item
      (user-error "No item under point"))
    (when (y-or-n-p "Really delete?")
      (sqorm-exec (format "delete from item where id = %d" (plist-get item :id))
		  nil)
      (vtable-remove-object (vtable-current-table) item))))

(defun anddo-change-status ()
  "Change the status of the item under point."
  (interactive nil anddo-mode)
  (let ((item (vtable-current-object)))
    (unless item
      (user-error "No item under point"))
    (let ((new-status (completing-read "New status: " anddo-statuses nil t)))
      (sqorm-exec
       "update item set status = ?, modification_time = ? where id = ?"
       (list new-status
	     (format-time-string "%F %T")
	     (plist-get item :id)))
      (plist-put item :status new-status)
      (vtable-update-object (vtable-current-table) item item))))

(provide 'anddo)

;;; anddo.el ends here
