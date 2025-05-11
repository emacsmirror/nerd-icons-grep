;;; nerd-icons-grep.el --- Add nerd-icons to grep-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Aleksei Gusev
;;
;; Author: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Maintainer: Aleksei Gusev <aleksei.gusev@gmail.com>
;; Created: May 07, 2025
;; Modified: May 07, 2025
;; Version: 0.0.1
;; Keywords: tools, grep, icons
;; Homepage: https://github.com/hron/nerd-icons-grep
;; Package-Requires: ((emacs "30.1") (nerd-icons "0.0.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Add nerd-icons to grep-mode buffers when `grep-use-headings' is `t'
;;
;;; Code:

(require 'nerd-icons)
(require 'grep)

(defvar-local nerd-icons-grep--state nil
  "Variable to keep track of the `nerd-icons-grep--heading-filter' state.")

(defun nerd-icons-grep--heading-filter ()
  "Filter function to add nerd icons to headings of output of a grep process."
  (unless nerd-icons-grep--state
    (setq nerd-icons-grep--state (cons (point-min-marker) nil)))
  (save-excursion
    (let ((limit (car nerd-icons-grep--state))
          prop)
      ;; Move point to the old limit and update limit marker.
      (move-marker limit (prog1 (pos-bol) (goto-char limit)))
      (while (setq prop (text-property-search-forward 'compilation-annotation))
        (let* ((start (prop-match-beginning prop))
               (end (prop-match-end prop))
               (heading (string-trim (buffer-substring-no-properties start end))))
          (save-excursion
            (goto-char start)
            (remove-text-properties (pos-bol) (pos-eol) '(outline-level))
            (insert-before-markers "\n" (nerd-icons-icon-for-file heading) " ")
            (add-text-properties (pos-bol) (pos-eol) '(outline-level 1)))
          (setf (cdr nerd-icons-grep--state) (prop-match-end prop)))))))

(advice-add 'grep--heading-filter :after #'nerd-icons-grep--heading-filter)

(ert-deftest nerd-icons-grep--show-icons ()
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

(provide 'nerd-icons-grep)
;;; nerd-icons-grep.el ends here
