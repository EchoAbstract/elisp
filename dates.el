;;; dates.el --- Collection of date helpers -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2019 Brian Wilson <brian@polytopes.me>
;;
;; Author: Brian Wilson <brian@polytopes.me>
;; URL: TODO
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; A little package of miscalanious time and date routines

;;; Code:
(require 'calendar)
(require 'cl-extra)

(defun baw/gregorian-date-p (date)
  "Return T if DATE is Gregorian, else nil."
  (and (listp date)
       (= (length date) 3)
       (cl-every (lambda (component) (integerp component)) date)))

(defun baw/add-days (date days)
  "Add DAYS to DATE."
  (if (not (baw/gregorian-date-p date))
      (+ date days)
    (calendar-gregorian-from-absolute (+ (calendar-absolute-from-gregorian date) days))))

(defun baw/subtract-days (date days)
  "Subtract DAYS from DATE."
  (baw/add-days date (- days)))

(defun baw/get-start-of-week (&optional date)
  "Return the starting day of DATE's week or current week."
  (let* ((current-date (if date date (calendar-current-date)))
	 (offset-from-start (calendar-day-of-week current-date)))
    (baw/subtract-days current-date offset-from-start)))

(defun baw/formatted-date (&optional date)
  "Return DATE (or CURRENT-DATE) as a formatted string."
  (let ((date-string (calendar-date-string (if date
					       date
					     (calendar-current-date)))))
    (format "%s" date-string)))

(defun baw/formatted-time (&optional time)
  "Return TIME (or CURRENT-TIME) as a formatted string."
  (let ((decoded-time (decode-time (if time
				       time
				     (current-time)))))
    (let ((hour (nth 2 decoded-time))
	  (minute (nth 1 decoded-time))
	  (second (nth 3 decoded-time)))
      (format "%02d:%02d:%02d" hour minute second))))

(defun baw/formatted-date-time-string (&optional date time)
  "Get a human readable (and verbose) version of the DATE and TIME."
  (let ((ds (if date
		(baw/formatted-date date)
	      (baw/formatted-date)))
	(ts (if time
		(baw/formatted-time time)
	      (baw/formatted-time))))
    (format "%s %s" ds ts)))

(defun baw/insert-beginning-of-week ()
  "Insert the date string of the first day of the week at point."
  (interactive)
  (insert (baw/formatted-date (baw/get-start-of-week))))

;; TESTS
(require 'ert)
(ert-deftest dates-test-gregorian-date-p ()
  "Tests that gregorian date detection function."
  (should (equal (baw/gregorian-date-p (calendar-current-date)) t))
  (should (null (baw/gregorian-date-p (calendar-absolute-from-gregorian (calendar-current-date))))))

(provide 'dates)
;;; dates.el ends here


