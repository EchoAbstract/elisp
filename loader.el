;;; loader.el --- package to load all of my files    -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020  Brian Wilson

;; Author: Brian Wilson
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(defvar *baw/ready-libraries*
  '(dates)
  "Libraries that are ready for general usage.")

(defvar *baw/experimental-libraries*
  '(compile)
  "Libraries that are not ready for general usage.")

(defconst baw/elisp-load-directory
  (file-name-directory load-file-name)
  "The directory this file is located.")

(defun baw/load-all (&optional experimental)
  "Load all package ready for usage.  Load not ready ones with EXPERIMENTAL."

  ;; We need to be in the load path
  (add-to-list 'load-path baw/elisp-load-directory t)

  (dolist (lib *baw/ready-libraries*)
    (message "Loading baw library: %s" lib)
    (require lib))

  (when experimental
    (dolist (lib *baw/experimental-libraries*)
      (message "Loading experimental baw library: %s" lib)
      (require lib))))

(provide 'loader)
;;; loader.el ends here
