;;; color.el --- Various Color utilities   -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019  Brian Wilson

;; Author: Brian Wilson <bwilson@oblong.com>
;; Keywords: c, extensions, lisp, tools

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

;; Woot

;;; Code:


(require 'ansi-color)

(defun baw/ansi-colorify-region (&optional beginning end)
  "Color ANSI control codes for region (or from BEGINNING to END."
  (interactive)
  (let ((b (if (null beginning)
               (region-beginning)
             beginning))
        (e (if (null end)
               (region-end)
             end)))
    (ansi-color-apply-on-region b e)))

(defun baw/ansi-colorify-buffer ()
  "Color ANSI control codes in entire buffer."
  (interactive)
  (baw/ansi-colorify-region (point-min) (point-max)))

(provide 'color)
;;; color.el ends here
