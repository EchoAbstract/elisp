;;; commands.el --- Brian's emacs commands           -*- lexical-binding: t; -*-
;; Author: <brian@polytopes.me>
;; Keywords: extensions, convenience, abbrev

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

(defvar *BAW/MACOS-LAUNCHER-APPLICATION*
  "open /Applications/Alfred\\ 3.app"
  "The command to launch apps on macOS (default: Alfred)")

;; (defvar *BAW/LINUX-LAUNCHER-APPLICATION*
;;   "dmenu_run"
;;   "The command to launch stuff on Linux (default: dmenu)")

(defvar *BAW/LINUX-LAUNCHER-APPLICATION*
  "synapse"
  "The command to launch stuff on Linux (default: synapse)")

; See: https://github.com/Wox-launcher/Wox
(defvar *BAW/WINDOWS-LAUNCHER-APPLICATION*
  "wox"                                 ; How do you do this on windows?
  "The command to launch stuff on windows (default: wox)")

(defun baw/launch-thing ()
  (interactive)
  ;; NB: This only works on linux/bsd, windows, and mac
  (let ((command (cond ((string-equal system-type "darwin")
                        *BAW/MACOS-LAUNCHER-APPLICATION*)
                       ((string-equal system-type "windows-nt")
                        *BAW/WINDOWS-LAUNCHER-APPLICATION*)
                       (t
                        *BAW/LINUX-LAUNCHER-APPLICATION*))))
    (shell-command command)))

;;;  (global-set-key (kbd "C-c C-o") 'launch-alfred)

(provide 'commands)
;;; commands.el ends here
