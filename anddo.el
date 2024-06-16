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
  "Import a text file into anddo."
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

(provide 'anddo)

;;; anddo.el ends here
