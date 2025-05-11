;;; nerd-icons-grep-tests.el --- Tests for nerd-icons-grep -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Created: May 11, 2025
;; Modified: May 11, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/hron/nerd-icons-grep-tests
;; Package-Requires: ((emacs "30.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tests for nerd-icons-grep
;;
;;; Code:

(ert-deftest nerd-icons-grep-tests--show-icons ()
  (with-current-buffer-window "*nerd-icons-grep-test*" nil nil
    ;; Setup a buffer and run grep--heading-filter inside it
    (grep-mode)
    (let ((inhibit-read-only t))
      (dlet ((compilation-filter-start (point)))
        (insert-file-contents "test.txt")
        (goto-char (point-max))
        (grep--heading-filter)
        (goto-char (point-min))))
    ;; Checks
    (save-excursion
      (goto-char (point-min))
      (should (eq (search-forward " nerd-icons-grep.el" nil t) 22))
      (should (eq (search-forward " ipsum.c" nil t) 99)))))

(provide 'nerd-icons-grep-tests)
;;; nerd-icons-grep-tests.el ends here
