;;; package-utils.el --- Package utilities   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Brian Wilson

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

  ;; (cl-loop for url in '("http://marmalade-repo.org/packages/"
  ;;                       "http://melpa.milkbox.net/packages/"
  ;;                       "http://elpa.gnu.org/packages/")
  ;;          do (condition-case e
  ;;                 (kill-buffer (url-retrieve-synchronously url))
  ;;               (error (cl-return)))
  ;;          finally (cl-return t))



; package-archives
;(("melpa" . "http://melpa.org/packages/") ("gnu" . "https://elpa.gnu.org/packages/"))


(cl-loop for archive in package-archives
         do (progn
              (message (concat "Checking update time for " (car archive)))
              (let* ((archive-file (concat
                                   user-emacs-directory
                                   "elpa/archives/"
                                   (car archive) "/"
                                   ))
                     (archive-attrs (file-attributes archive-file)))
                (when archive-attrs
                  (message (format "%s archive (at %s) last updated at "))))))



; ("melpa" . "http://melpa.org/packages/")
; ("gnu" . "https://elpa.gnu.org/packages/")


; (format-time-string "%Y-%m-%d" (nth 5 (file-attributes "~/.emacs.d/elpa/archives/gnu/archive-contents")))



(provide 'package-utils)
;;; package-util.el ends here
