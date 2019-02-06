;;; compile.el --- Make compilation work for brian   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Brian Wilson

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



(defvar *build-filetype-plist*
  '("meson.build"
    (list 'command ""
          'build-dirs '("build"))
    "CMakeLists.txt"
    (list 'command "cmake --build"
          'build-dirs '("build" "btmp"))

    "Makefile"
    (list 'command "make"
          'build-dirs '())
    "makefile"
    (list 'command "make"
          'build-dirs '())
    "GNUMakefile"
    (list 'command "gnumake"
          'build-dirs '()))
  "Build files and how to build them.")


(defun find-build-file ()
  "Find a build-like file upwards from the current directory."
  (interactive)
  (let ((candidates (walk-tree-looking-for-build-files "." "~")))
    (if (not (null candidates))
        (message (format "%s/%s" (car (nth 0 (reverse candidates))) (cdr (nth 0 (reverse candidates)))))
      (error "Didn't find a build file!"))))


 (defun walk-tree-looking-for-build-files (current stop)
   "Walks up the dir tree from CURRENT to STOP looking for a build file, collecting founds in FOUND."
   (let ((current-dir (expand-file-name current))
         (stop-dir (expand-file-name stop)))
     (if (equal current-dir stop-dir)
         '()
       (let ((build-file (maybe-get-build-file current-dir *build-filetype-plist*))
             (parent-dir (expand-file-name (concat current-dir "/.."))))
         (if (not (null build-file))
             (cons (cons current-dir build-file)
                   (walk-tree-looking-for-build-files parent-dir stop-dir))
           (walk-tree-looking-for-build-files parent-dir stop-dir))))))


(defun maybe-get-build-file (dir build-files)
  "If no BUILD-FILES in DIR, nil, else (t . build-flle)."
  (let ((got-one (has-build-file dir build-files)))
    (if got-one
        (cdr got-one)
      nil)))


(defun has-build-file (dir build-filetype-plist)
  "Check to see if any of BUILD-FILETYPE-PLIST present in DIR."
  (let* ((files (directory-files dir))
         (match (seq-filter
                 (lambda (elt)
                   (not (null (lax-plist-get build-filetype-plist elt))))
                 files)))
    (if (null match)
        '()
      (cons t (car match)))))


(require 'ert)

(ert-deftest smoke ()
  "Smoke tests"
  (should (null (maybe-get-build-file "~/obsrc/flag/" *build-filetype-plist*)))
  (should (equal "meson.build" (maybe-get-build-file "~/obsrc/sfu-provisioner/" *build-filetype-plist*)))
  (should (equal "CMakeLists.txt" (maybe-get-build-file "~/obsrc/progress-bar-demo/" *build-filetype-plist*))))


(provide 'compile)
;;; compile.el ends here
