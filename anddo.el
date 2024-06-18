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

(require 'cl-lib)
(require 'vtable)

(defvar anddo-listing-mode 'new
  "Default listing mode for anddo.
Possible values are `new', `all' and `most'.")

(defvar anddo-statuses
  '(("new" "⚡" t)
    ("in-progress" "🛠️" t)
    ("possibly" "❓" nil)
    ("not-doing" "⛔" nil)
    ("done" "☑️" t))
  "Alist of possible statuses and how they should be displayed.
The third element says whether to display a count of the status
in the mode line.

The order is significant -- the todo items will be listed in the
same order.")

(defvar anddo--db nil)

(defun anddo ()
  "Display the todo list."
  (interactive)
  (pop-to-buffer "*anddo*")
  (when anddo--db
    (sqlite-close anddo--db)
    (setq-local anddo--db nil))
  (anddo-mode)
  (anddo--create-tables)
  (anddo--generate))

(defun anddo--create-tables ()
  (unless anddo--db
    (setq-local anddo--db
		(sqlite-open
		 (expand-file-name "anddo.sqlite" user-emacs-directory)))
    (sqlite-execute anddo--db "create table if not exists item (id integer primary key, status text, subject text, body text, entry_time text, modification_time text)")))

(defun anddo--insert (status subject body)
  (sqlite-execute
   anddo--db
   "insert into item(status, subject, body, entry_time) values(?, ?, ?, ?)"
   (list status subject body (format-time-string "%F %T"))))

(defun anddo-import-text-file (file)
  "Import a text file into anddo.
The text file is on the format:

New:
* Foo bar
  Something here
* Zot"
  (interactive "fFile to import: " nil anddo-mode)
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
	      (anddo--insert (downcase status) subject body)
	      (setq body ""))
	    (setq subject new-subject)))
	 ((looking-at "  \\(.*\\)")
	  (setq body (concat body (match-string 1)))))
	(forward-line 1)))))

(defun anddo--rank (item)
  (seq-position anddo-statuses
		(assoc (plist-get item :status) anddo-statuses)))

(defun anddo--transform-result (result)
  "Make sqlite results into plists."
  (cl-loop with columns = (pop result)
	   for row in result
	   collect
	   (cl-loop for column in columns
		    for value in row
		    append (list (intern (format ":%s" (replace-regexp-in-string
							"_" "-" column)))
				 value))))

(defun anddo--generate ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (make-vtable
     :columns '((:name "More" :width 4)
		(:name "Status" :width 4)
		(:name "Item" :width 80))
     :objects (sort
	       (anddo--transform-result
		(sqlite-select
		 anddo--db
		 (format
		  "select * from item %s order by id desc"
		  (cond
		   ((eq anddo-listing-mode 'new)
		    "where status = 'new'")
		   ((eq anddo-listing-mode 'all)
		    "")
		   (t
		    "where status in ('new', 'possibly', 'in-progress')")))
		 nil 'full))
	       (lambda (i1 i2)
		 (< (anddo--rank i1) (anddo--rank i2))))
     :getter
     (lambda (item column vtable)
       (pcase (vtable-column vtable column)
	 ("More" (if (length= (plist-get item :body) 0)
		     ""
		   "📁"))
	 ("Status" (cadr (assoc (plist-get item :status) anddo-statuses)))
	 ("Item" (plist-get item :subject))))))
  (setq-local global-mode-string (anddo--mode-line)))

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
  (setq-local global-mode-string (anddo--mode-line)))

(defun anddo--mode-line ()
  (if (not anddo--db)
      ""
    (string-join
     (cl-loop for (status name show) in anddo-statuses
	      when show
	      collect (format
		       "%s:%s" name
		       (caar
			(sqlite-select
			 anddo--db "select count(*) from item where status = ?"
			 (list status)))))
     " ")))

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
    (anddo--generate)
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
      (anddo--insert "new" (pop lines) (string-join lines "\n"))
      (let ((item (car
		   (anddo--transform-result
		    (sqlite-select
		     anddo--db
		     "select * from item where id in (select max(id) from item)"
		     nil 'full)))))
	(goto-char (point-min))
	(vtable-insert-object (vtable-current-table) item
			      (vtable-current-object))
	(setq-local global-mode-string (anddo--mode-line))))))

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
      (sqlite-execute
       anddo--db
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
      (sqlite-execute
       anddo--db "delete from item where id = ?" (list (plist-get item :id)))
      (vtable-remove-object (vtable-current-table) item)
      (setq-local global-mode-string (anddo--mode-line)))))

(defun anddo-change-status ()
  "Change the status of the item under point."
  (interactive nil anddo-mode)
  (let ((item (vtable-current-object)))
    (unless item
      (user-error "No item under point"))
    (let ((new-status (completing-read "New status: " anddo-statuses nil t)))
      (sqlite-execute
       anddo--db
       "update item set status = ?, modification_time = ? where id = ?"
       (list new-status (format-time-string "%F %T") (plist-get item :id)))
      (plist-put item :status new-status)
      (vtable-update-object (vtable-current-table) item item)
      (setq-local global-mode-string (anddo--mode-line)))))

(provide 'anddo)

;;; anddo.el ends here
