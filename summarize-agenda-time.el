;;; summarize-agenda-time.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Cash Prokop-Weaver
;;
;; Author: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Maintainer: Cash Prokop-Weaver <cashbweaver@gmail.com>
;; Created: November 22, 2023
;; Modified: November 22, 2023
;; Version: 0.0.1
;; Keywords: calendar
;; Homepage: https://github.com/cashweaver/summarize-agenda-time
;; Package-Requires: ((emacs "26.1") (dash "2.19.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Utility to summarize efforts and scheduled durations in org-agenda.
;;
;;  Based on: https://emacs.stackexchange.com/a/52376
;;
;;; Code:

(require 'cl-lib)
(require 'dash)

(defgroup summarize-agenda-time nil
  "Options related to the clocktable-by-tag dblock."
  :tag "Org-agenda summarize agenda time"
  :group 'org)

(defcustom summarize-agenda-time--enabled t
  "Enable agenda time summarizing when non-nil."
  :group 'summarize-agenda-time
  :type 'boolean)

(defcustom summarize-agenda-time--show-max-duration t
  "Show maximum duration in agenda headline if non-nil."
  :group 'summarize-agenda-time
  :type 'boolean)

(defcustom summarize-agenda-time--max-duration-minutes (* 8 60)
  "Maximum duration in minutes.

Displayed in agenda headline when `org-agenda-summarize-duration--show-max-duration' is non-nil."
  :group 'summarize-agenda-time
  :type 'number)

(defcustom summarize-agenda-time--ignore-entry-fns '()
  "A list of functions which are run during summarization.

Example:
(lambda (marker)
  (member
   (org-with-point-at marker
    (org-entry-get nil \"ITEM\"))
   '(\"Foo\", \"Bar\")))

Ignore any headline for which at least one of these functions returns non-nil."
  :group 'summarize-agenda-time
  :type '(repeat symbol))

(defun summarize-agenda-time-enable ()
  "Enable agenda time summarization."
  (setq summarize-agenda-time--enabled t))

(defun summarize-agenda-time-disable ()
  "Disable agenda time summarization."
  (setq summarize-agenda-time--enabled nil))

(defun summarize-agenda-time--get-total-minutes (start end)
  "Return sum of agenda item duration minutes between START and END points."
  (let (duration-minutes)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (when (member (org-get-at-bol 'type) '("scheduled" "past-scheduled"))
          (let ((marker (org-get-at-bol 'org-hd-marker)))
            (unless (--any
                     (apply it `(,marker))
                     summarize-agenda-time--ignore-entry-fns)
              (push (or (get-text-property (point) 'duration)
                        (get-text-property (point) 'effort-minutes))
                    duration-minutes))))
        (forward-line)))
    (cl-reduce #'+ (cl-remove-if-not 'identity duration-minutes))))

(defun summarize-agenda-time--has-agenda-date-header-p ()
  "Return non-nil when the current buffer contains text with the
`org-agenda-date-header' property."
  (save-excursion
    (goto-char (point-min))
    (text-property-any (point) (point-max) 'org-agenda-date-header t)))

(defun summarize-agenda-time--get-day-ranges ()
  "Return list of agenda day start and end positions in current buffer in
ascending order.

Ascending order is useful because we can insert text without corrupting our character pointers."
  (save-excursion
    (goto-char (point-min))
    (let ((searching t)
          (start-pos
           (text-property-any (point) (point-max) 'org-agenda-date-header t))
          end-pos
          heading-positions)
      (while searching
        (goto-char
         (text-property-not-all
          start-pos (point-max) 'org-agenda-date-header t))
        (let ((next-date-header-pos
               (text-property-any
                (point) (point-max) 'org-agenda-date-header t)))
          (if next-date-header-pos
              (setq end-pos next-date-header-pos)
            (progn
              (setq
               end-pos
               (or (text-property-any
                    (point) (point-max) 'face 'org-agenda-structure)
                   (point-max))
               searching nil))))
        ;; `push' ensures positions are in ascending order.
        (push `(:start-pos ,start-pos :end-pos ,end-pos) heading-positions)
        (setq start-pos end-pos))
      heading-positions)))

(defun summarize-agenda-time--insert-summary (insert-pos start-pos end-pos)
  "Insert agenda time summary at INSERT-POS for headings between START-POS and END-POS."
  (when-let* ((total-minutes
               (summarize-agenda-time--get-total-minutes start-pos end-pos))
              (total-duration (org-duration-from-minutes total-minutes))
              (summary
               (if summarize-agenda-time--show-max-duration
                   (let ((max-duration
                          (org-duration-from-minutes
                           summarize-agenda-time--max-duration-minutes))
                         (percentage
                          (round
                           (*
                            100
                            (/ total-minutes
                               summarize-agenda-time--max-duration-minutes)))))
                     (format " (%s/%s; %s%%)"
                             total-duration
                             max-duration
                             percentage))
                 (format " (%s)" total-duration))))
    (save-excursion
      (goto-char insert-pos)
      (insert-and-inherit summary))))

(defun summarize-agenda-time--summarize ()
  "Insert the durations for each day inside the agenda buffer."
  (when (and summarize-agenda-time--enabled
             (summarize-agenda-time--has-agenda-date-header-p))
    (save-excursion
      (cl-dolist (day-range (summarize-agenda-time--get-day-ranges))
        (summarize-agenda-time--insert-summary
         (save-excursion
           (goto-char (plist-get day-range :start-pos))
           (end-of-line)
           (point))
         (plist-get day-range :start-pos) (plist-get day-range :end-pos))))))

(add-hook 'org-agenda-finalize-hook 'summarize-agenda-time--summarize)

(provide 'summarize-agenda-time)
;;; summarize-agenda-time.el ends here
