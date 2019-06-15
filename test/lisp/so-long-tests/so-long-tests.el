;;; so-long-tests.el --- Test suite for so-long.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; Author: Phil Sainty <psainty@orcon.net.nz>
;; Keywords: convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Most of these tests use the shebang #!emacs to get `normal-mode' to
;; select `emacs-lisp-mode', as using a file-local mode variable might
;; trigger `so-long-file-local-mode-function'.

;; Running the tests with "make lisp/so-long-tests" is like:
;;
;; HOME=/nonexistent EMACSLOADPATH= LC_ALL=C \
;; EMACS_TEST_DIRECTORY=/home/phil/emacs/trunk/repository/test \
;; "../src/emacs" --no-init-file --no-site-file --no-site-lisp \
;; -L ":." -l ert -l lisp/so-long-tests.el --batch --eval \
;; '(ert-run-tests-batch-and-exit (quote (not (tag :unstable))))'
;;
;; See also `ert-run-tests-batch-and-exit'.

;;; Code:

(require 'ert)
(require 'so-long)
(load (expand-file-name "so-long-tests-helpers"
                        (file-name-directory (or load-file-name
                                                 default-directory))))

(declare-function so-long-tests-remember "so-long-tests-helpers")
(declare-function so-long-tests-assert-active "so-long-tests-helpers")
(declare-function so-long-tests-assert-reverted "so-long-tests-helpers")

;; Enable the automated behaviour for all tests.
(global-so-long-mode 1)

(ert-deftest so-long-tests-threshold-under ()
  "Under threshold."
  (with-temp-buffer
    (insert "#!emacs\n")
    (insert (make-string (1- so-long-threshold) ?x))
    (normal-mode)
    (should (eq major-mode 'emacs-lisp-mode))))

(ert-deftest so-long-tests-threshold-at ()
  "At length threshold."
  (with-temp-buffer
    (insert "#!emacs\n")
    (insert (make-string (1- so-long-threshold) ?x))
    (normal-mode)
    (should (eq major-mode 'emacs-lisp-mode))))

(ert-deftest so-long-tests-threshold-over ()
  "Over length threshold."
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (insert (make-string (1+ so-long-threshold) ?x))
    (normal-mode)
    (so-long-tests-assert-active 'so-long-mode)
    (so-long-revert)
    (so-long-tests-assert-reverted 'so-long-mode)))

(ert-deftest so-long-tests-skip-comments ()
  "Skip leading shebang, whitespace, and comments."
  ;; Long comment, no newline.
  (with-temp-buffer
    (insert "#!emacs\n")
    (insert (make-string (1+ so-long-threshold) ?\;))
    (normal-mode)
    (should (eq major-mode 'emacs-lisp-mode)))
  ;; Long comment, with newline.
  (with-temp-buffer
    (insert "#!emacs\n")
    (insert (make-string (1+ so-long-threshold) ?\;))
    (insert "\n")
    (normal-mode)
    (should (eq major-mode 'emacs-lisp-mode)))
  ;; Long comment, with short text following.
  (with-temp-buffer
    (insert "#!emacs\n")
    (insert (make-string (1+ so-long-threshold) ?\;))
    (insert "\n")
    (insert (make-string so-long-threshold ?x))
    (normal-mode)
    (should (eq major-mode 'emacs-lisp-mode)))
  ;; Long comment, with long text following.
  (with-temp-buffer
    (insert "#!emacs\n")
    (insert (make-string (1+ so-long-threshold) ?\;))
    (insert "\n")
    (insert (make-string (1+ so-long-threshold) ?x))
    (normal-mode)
    (should (eq major-mode 'so-long-mode))))

(ert-deftest so-long-tests-max-lines ()
  "Give up after `so-long-max-lines'."
  (with-temp-buffer
    (insert "#!emacs\n")
    ;; Insert exactly `so-long-max-lines' non-comment lines, followed
    ;; by a long line.
    (dotimes (_ so-long-max-lines)
      (insert "x\n"))
    (insert (make-string (1+ so-long-threshold) ?x))
    (normal-mode)
    (should (eq major-mode 'emacs-lisp-mode))
    ;; If `so-long-max-lines' is nil, don't give up the search.
    (let ((so-long-max-lines nil))
      (normal-mode)
      (should (eq major-mode 'so-long-mode)))
    ;; If `so-long-skip-leading-comments' is nil, all lines are
    ;; counted, and so the shebang line counts, which makes the
    ;; long line one line further away.
    (let ((so-long-skip-leading-comments nil)
          (so-long-max-lines (1+ so-long-max-lines)))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode))
      (let ((so-long-max-lines (1+ so-long-max-lines)))
        (normal-mode)
        (should (eq major-mode 'so-long-mode))))))

(ert-deftest so-long-tests-actions ()
  "Test each of the standard actions."
  (dolist (action (mapcar #'car so-long-action-alist))
    (with-temp-buffer
      (insert "#!emacs\n")
      (normal-mode)
      (so-long-tests-remember)
      (insert (make-string (1+ so-long-threshold) ?x))
      (let ((so-long-action action))
        (normal-mode)
        (so-long-tests-assert-active action)
        (so-long-revert)
        (so-long-tests-assert-reverted action)))))

(ert-deftest so-long-tests-command-so-long ()
  "Test using the `so-long' command."
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (insert (make-string (1+ so-long-threshold) ?x))
    (so-long)
    (so-long-tests-assert-active 'so-long-mode)
    (so-long-revert)
    (so-long-tests-assert-reverted 'so-long-mode))
  (dolist (action (mapcar #'car so-long-action-alist))
    (with-temp-buffer
      (insert "#!emacs\n")
      (normal-mode)
      (so-long-tests-remember)
      (insert (make-string (1+ so-long-threshold) ?x))
      (so-long action)
      (so-long-tests-assert-active action t)
      (so-long-revert)
      (so-long-tests-assert-reverted action t))))

(ert-deftest so-long-tests-major-mode ()
  "Test calling `so-long-mode' directly."
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (so-long-mode)
    (so-long-tests-assert-active 'so-long-mode)
    (so-long-revert)
    (so-long-tests-assert-reverted 'so-long-mode)))

(ert-deftest so-long-tests-minor-mode ()
  "Test calling `so-long-minor-mode' directly."
  (with-temp-buffer
    (insert "#!emacs\n")
    (normal-mode)
    (so-long-tests-remember)
    (so-long-minor-mode 1)
    (so-long-tests-assert-active 'so-long-minor-mode t)
    (so-long-minor-mode 0)
    (so-long-tests-assert-reverted 'so-long-minor-mode t)))

(ert-deftest so-long-tests-target-modes ()
  "Targeted major modes."
  (with-temp-buffer
    (insert "#!emacs\n")
    (insert (make-string (1+ so-long-threshold) ?x))
    ;; Nil target modes.
    (let ((so-long-target-modes nil))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode)))
    ;; Non-matching target modes.
    (let ((so-long-target-modes '(text-mode)))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode)))
    ;; Matching mode (direct).
    (let ((so-long-target-modes '(emacs-lisp-mode)))
      (normal-mode)
      (should (eq major-mode 'so-long-mode)))
    ;; Matching mode (indirect).
    (let ((so-long-target-modes '(prog-mode)))
      (normal-mode)
      (should (eq major-mode 'so-long-mode)))))

(ert-deftest so-long-tests-predicate ()
  "Custom predicate function."
  (with-temp-buffer
    (insert "#!emacs\n")
    ;; Always false.
    (let ((so-long-predicate #'ignore))
      (normal-mode)
      (should (eq major-mode 'emacs-lisp-mode)))
    ;; Always true.
    (let ((so-long-predicate (lambda () t)))
      (normal-mode)
      (should (eq major-mode 'so-long-mode)))))

(ert-deftest so-long-tests-file-local-action ()
  "File-local action."
  (with-temp-buffer
    (setq buffer-file-name (concat (make-temp-name "so-long-tests-") ".el"))
    (normal-mode)
    (so-long-tests-remember)
    (insert ";; -*- so-long-action:so-long-minor-mode; -*-\n")
    (put 'so-long-action 'safe-local-variable #'symbolp)
    (insert (make-string (1+ so-long-threshold) ?x))
    (normal-mode)
    (so-long-tests-assert-active 'so-long-minor-mode)
    (so-long-revert)
    (so-long-tests-assert-reverted 'so-long-minor-mode)))

(ert-deftest so-long-tests-file-local-action-eval-so-long ()
  "File-local action and eval:(so-long)."
  (with-temp-buffer
    (setq buffer-file-name (concat (make-temp-name "so-long-tests-") ".el"))
    (normal-mode)
    (so-long-tests-remember)
    (insert ";; -*- so-long-action:so-long-minor-mode; eval:(so-long) -*-\n")
    (put 'so-long-action 'safe-local-variable #'symbolp)
    (push '(eval . (so-long)) safe-local-variable-values)
    (normal-mode)
    (so-long-tests-assert-active 'so-long-minor-mode)
    (so-long-revert)
    (so-long-tests-assert-reverted 'so-long-minor-mode)))

(defmacro so-long-tests-deftest-file-local-emacs-lisp-mode
    (sym docstring file-local-spec)
  "Generate tests for using `emacs-lisp-mode' as a file-local mode."
  `(ert-deftest ,sym ()
     ,docstring
     (let ((orig so-long-file-local-mode-function))
       ;; Do nothing at all.
       (setq-default so-long-file-local-mode-function 'so-long-inhibit)
       (with-temp-buffer
         (insert ,file-local-spec)
         (insert (make-string (1+ so-long-threshold) ?x))
         (normal-mode)
         ;; Remember the `emacs-lisp-mode' state.  The other cases
         ;; will validate the 'reverted' state against this.
         (so-long-tests-remember)
         (should (eq major-mode 'emacs-lisp-mode)))
       ;; Downgrade the action from major mode to minor mode.
       (setq-default so-long-file-local-mode-function 'so-long-mode-downgrade)
       (with-temp-buffer
         (insert ,file-local-spec)
         (insert (make-string (1+ so-long-threshold) ?x))
         (normal-mode)
         (so-long-tests-assert-active 'so-long-minor-mode t)
         (so-long-revert)
         (so-long-tests-assert-reverted 'so-long-minor-mode t))
       ;; Do not treat the file-local mode specially.
       (setq-default so-long-file-local-mode-function nil)
       (with-temp-buffer
         (insert ,file-local-spec)
         (insert (make-string (1+ so-long-threshold) ?x))
         (normal-mode)
         (so-long-tests-assert-active 'so-long-mode)
         (so-long-revert)
         (so-long-tests-assert-reverted 'so-long-mode))
       ;; end
       (setq-default so-long-file-local-mode-function orig))))

(so-long-tests-deftest-file-local-emacs-lisp-mode
  so-long-tests-file-local-emacs-lisp-mode-short-form
  "File-local mode (short form). -*- emacs-lisp -*-"
  ";; -*- emacs-lisp -*-\n")

(so-long-tests-deftest-file-local-emacs-lisp-mode
  so-long-tests-file-local-emacs-lisp-mode-long-form
  "File-local mode (long form). -*- emacs-lisp -*-"
  ";; -*- mode: emacs-lisp -*-\n")

(defmacro so-long-tests-deftest-file-local-so-long-mode
    (sym docstring file-local-spec)
  "Generate tests for using `so-long-mode' as a file-local mode."
  `(ert-deftest ,sym ()
     ,docstring
     (let ((orig so-long-file-local-mode-function))
       ;; Do nothing at all.
       (setq-default so-long-file-local-mode-function 'so-long-inhibit)
       (with-temp-buffer
         ;; Remember the new-buffer state.  The other cases will
         ;; validate the 'reverted' state against this.
         (so-long-tests-remember)
         (insert ,file-local-spec)
         (insert (make-string (1+ so-long-threshold) ?x))
         (normal-mode)
         (should (eq major-mode 'so-long-mode)))
       ;; Downgrade from major mode to minor mode.
       (setq-default so-long-file-local-mode-function 'so-long-mode-downgrade)
       (with-temp-buffer
         (insert ,file-local-spec)
         (insert (make-string (1+ so-long-threshold) ?x))
         (normal-mode)
         (so-long-tests-assert-active 'so-long-mode)
         (so-long-revert)
         (so-long-tests-assert-reverted 'so-long-mode))
       ;; Do not treat the file-local mode specially.
       (setq-default so-long-file-local-mode-function nil)
       (with-temp-buffer
         (insert ,file-local-spec)
         (insert (make-string (1+ so-long-threshold) ?x))
         (normal-mode)
         (so-long-tests-assert-active 'so-long-mode)
         (so-long-revert)
         (so-long-tests-assert-reverted 'so-long-mode))
       ;; end
       (setq-default so-long-file-local-mode-function orig))))

(so-long-tests-deftest-file-local-so-long-mode
  so-long-tests-file-local-so-long-mode-short-form
  "File-local mode (short form). -*- so-long -*-"
  ";; -*- so-long -*-\n")

(so-long-tests-deftest-file-local-so-long-mode
  so-long-tests-file-local-so-long-mode-long-form
  "File-local mode (long form). -*- mode: so-long -*-"
  ";; -*- mode: so-long -*-\n")

;;; so-long-tests.el ends here
