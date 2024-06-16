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
     (id integer)
     (status text)
     (subject text)
     (body text)
     (entry-time text)
     (modification-time text))))

(defun anddo--create-tables ()
  (sqorm-create-tables anddo--tables))
  
    
     

(provide 'anddo)

;;; anddo.el ends here
