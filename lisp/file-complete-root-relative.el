;;; file-complete-root-relative.el --- Completion style for files  -*- lexical-binding:t -*-
;;
;; Copyright (C) 2019 Free Software Foundation, Inc.
;;
;; Author: Stephen Leake <stephen_leake@stephe-leake.org>
;; Maintainer: Stephen Leake <stephen_leake@stephe-leake.org>
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary

;; A file completion style in which the root directory is left out of
;; the completion string displayed to the user.
;;
;; We accomplish this by preprocessing the list of absolute file names
;; to be in that style, in an alist with the original absolute file
;; names, and do completion on that alist.

(require 'cl-lib)

(defun fc-root-rel-to-alist (root files)
  "Return a file-root-rel alist with file names from FILES.
Result is a list (REL-NAME . ABS-NAME), where REL-NAME is ABS-NAME with ROOT deleted.
An error is signaled if any name in FILES does not begin with ROOT."
  (let ((root-len (length root))
	result)
    (mapc
     (lambda (abs-name)
       (unless (string-equal root (substring abs-name 0 root-len))
	 (error "%s does not begin with %s" abs-name root))
       (push (cons (substring abs-name root-len) abs-name) result))
     files)
    result))

(defun fc-root-rel-completion-table (files string pred action)
  "Implement a completion table for file names in FILES,
FILES is a list of (REL-NAME . ABS-NAME).

STRING, PRED, ACTION are completion table arguments."
  (cond
   ((eq action 'alist)
    (cdr (assoc string files)))

   ((eq (car-safe action) 'boundaries)
    ;; We don't use boundaries; return the default definition.
    (cons 'boundaries
	  (cons 0 (length (cdr action)))))

   ((eq action 'metadata)
    (cons 'metadata
	  (list
	   '(alist . t)
	   '(category . project-file))))

   ((null action)
    (try-completion string files pred))

   ((eq 'lambda action)
    (test-completion string files pred))

   ((eq t action)
    (all-completions string files pred))

   ))

(provide 'file-complete-root-relative)
;;; file-complete-root-relative.el ends here
