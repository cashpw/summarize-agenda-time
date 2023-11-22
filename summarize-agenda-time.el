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
;; Package-Requires: ((emacs "26.1"))
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

(defgroup summarize-agenda-time nil
  "Options related to the clocktable-by-tag dblock."
  :tag "Org-agenda Summarize Efforts"
  :group 'org)

(defcustom summarize-agenda-time--show-max-effort
  t
  "Show maximum effort in agenda headline if non-nil."
  :group 'summarize-agenda-time
  :type 'boolean)

(defcustom summarize-agenda-time--max-effort-minutes
  (* 8 60)
  "Maximum effort in minutes.

Displayed in agenda headline when `org-agenda-summarize-effort--show-max-effort' is non-nil."
  :group 'summarize-agenda-time
  :type 'number)

(defun summarize-agenda-time--get-scheduled-duration-in-minutes (pos)
  "Return the scheduled duration, in minutes, of the heading at POS, or nil."
  (let ((timestamp (org-entry-get pos "SCHEDULED")))
    (when timestamp
      (let ((timestamp-with-duration-regexp "\\(<.*\\)\\([0-9][0-9]:[0-9][0-9]\\)-\\([0-9][0-9]:[0-9][0-9]\\)\\(.*>\\)"))
        (when (string-match timestamp-with-duration-regexp
                            timestamp)
          (let* ((start-org-time (org-time-string-to-time
                                  (replace-regexp-in-string timestamp-with-duration-regexp
                                                            "\\1\\2\\4"
                                                            timestamp)))
                 (end-org-time (org-time-string-to-time
                                (replace-regexp-in-string timestamp-with-duration-regexp
                                                          "\\1\\3\\4"
                                                          timestamp))))
            (/ (org-time-subtract end-org-time
                                  start-org-time)
               60)))))))

(defun summarize-agenda-time--summarize-efforts (limit)
  "Return sum of minutes of effort or scheduled time between point and LIMIT."
  (let (total)
    (save-excursion
      (while (< (point) limit)
        (let* ((pos (org-get-at-bol 'org-hd-marker))
               (effort (org-entry-get pos
                                      "Effort"))
               (headline-text (org-entry-get pos
                                             "ITEM"))
               (scheduled-duration-in-minutes (summarize-agenda-time--get-scheduled-duration-in-minutes pos)))
          (message "%s %s %s" headline-text effort scheduled-duration-in-minutes)
          (push (if scheduled-duration-in-minutes
                    scheduled-duration-in-minutes
                  effort)
                total))
        (forward-line)))
    (org-duration-from-minutes
     (cl-reduce #'+
                (mapcar #'org-duration-to-minutes
                        (cl-remove-if-not 'identity
                                          total))))))

(defun summarize-agenda-time--get-next-date-header-pos (pos)
  "Return position of the next `org-agenda-date-header' after POS, else nil."
  (let ((value t))
    (text-property-any pos
                       (point-max)
                       'org-agenda-date-header
                       value)))

(defun summarize-agenda-time--insert ()
  "Insert the efforts for each day inside the agenda buffer."
  (save-excursion
    (let (pos)
      (while (setq pos (summarize-agenda-time--get-next-date-header-pos (point)))
        (goto-char pos)
        (end-of-line)
        (when-let* ((next-day-pos (summarize-agenda-time--get-next-date-header-pos (point)))
                    (effort (summarize-agenda-time--summarize-efforts next-day-pos))
                    (effort-text (if summarize-agenda-time--show-max-effort
                                     (format " (%s/%s)"
                                             effort
                                             (org-duration-from-minutes summarize-agenda-time--max-effort-minutes))
                                   (format " %s"
                                           effort))))
          (insert-and-inherit effort-text))
        (forward-line)))))

(add-hook 'org-agenda-finalize-hook 'summarize-agenda-time--insert)

(provide 'summarize-agenda-time)
;;; summarize-agenda-time.el ends here
