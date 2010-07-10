;;; mv-shell.el - keep buffers in sync with filename throughout 'mv'commands in shell-mode.

;; Copyright (C) 2010 Nathaniel Flath <nflath@gmail.com>

;; Author: Nathaniel Flath <nflath@gmail.com>
;; URL: http://github.com/nflath/mv-shell
;; Version: 1.0

;; This file is not part of GNU Emacs.

;;; Commentary:
;; mv-shell integrates with shell-mode in order to keep buffers in sync when
;; moving files around.  If you enter a 'mv' command on a file that has a buffer opened,
;; the buffer is also renamed and moved to the location the file is moved to.

;;; Installation

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;
;; (require 'mv-shell)
;; (mv-shell-mode 1)
;;

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defvar mv-shell-mv-regex "^mv[ \t\r\n]+\\([^ \t\r\n]+\\)[ \t\r\n]+\\([^ \t\r\n]+\\)[ \t\r\n]*$"
  "Regular expression matching 'mv' commands.  The first
  parenthetical subexpression must match the file being moved;
  the second the location it is being moved to." )

(defvar mv-shell-mode nil
  "Whether mv-shell-mode is enabled or not.")

(defun path-to-filename (full-path)
  "Returns just the filename in a path.  [EG, (path-to-filename
'/foo/bar/baz' returns 'baz'."
  (string-match "\\([^/ \t\r\n]+\\)[\t\r\n ]*$" full-path)
  (match-string 1 full-path))

(defun mv-shell-get-buffers-visiting-files-in-directory (directory)
  "Returns all buffers visiting files in the given directory."
  (remove-if-not (lambda (a) a)
                 (mapcar
                  (lambda (buffer)
                    (if (and (buffer-file-name buffer)
                             (string-match (regexp-opt (list (expand-file-name directory)))
                                           (expand-file-name (buffer-file-name buffer))))
                        buffer))
                  (buffer-list))))

(defun mv-shell-path-to-file (filename)
  "Works as path-to-file, except if the filename ends with / the / is stripped first."
  (if (string-match "/$" filename)
      (path-to-filename (substring filename 0 (1- (length filename))))
    (path-to-filename filename)))

(defun mv-shell-check-string (input-str)
  "Given an input string, checks if it is a 'mv' command.  If so,
and there is a buffer visiting the file being moved, rename the
buffer to the new file name and set it's location to the new
location.  Requires default-directory to be correct."
  (save-window-excursion
    (let ((input-str (string-trim input-str)))
      (if (string-match mv-shell-mv-regex input-str)
          (let* ((from (match-string 1 input-str))
                 (to-raw (match-string 2 input-str))
                 (to (expand-file-name (if (file-directory-p to-raw)
                                          (concat to-raw "/" (mv-shell-path-to-file from))
                                        to-raw))))
            (cond
             ((and (not (file-directory-p from))
                   (get-file-buffer from))

              (set-buffer (get-file-buffer from))
              (rename-buffer (mv-shell-path-to-file to))
              (set-visited-file-name to))
             ((file-directory-p from) ;;moving a directory
              (mapcar (lambda (buffer)
                        (let* ((buffer-name (expand-file-name (buffer-file-name buffer)))
                               (buffer-after-file (replace-regexp-in-string
                                                   (regexp-opt (list (expand-file-name from)))
                                                   ""
                                                   buffer-name)))
                          (set-buffer buffer)
                          (set-visited-file-name (concat to "/" buffer-after-file))))
                      (mv-shell-get-buffers-visiting-files-in-directory from)
                      ))))))))

(defun mv-shell-mode (&optional arg)
  "With a positive argument, turns on mv-shell-mode.  With a
negative argument, turns off mv-shell-mode.  With no argument,
toggles mv-shell-mode."
  (interactive)
  (cond
    ((or (and arg (> arg 0))
         (and (not arg) (not mv-shell-mode)))
      (progn
        (setq mv-shell-mode t)
        (add-hook 'comint-input-filter-functions 'mv-shell-check-string)
        (message "mv-shell mode enabled")))
    ((or (and arg (< arg 0))
         (and (not arg) mv-shell-mode))
     (progn
       (setq mv-shell-mode nil)
       (remove-hook 'comint-input-filter-functions 'mv-shell-check-string)
       (message "mv-shell mode disabled")))))

(define-minor-mode mv-shell-mode
  "Minor mode to keep buffers in sync across shell-mode 'mv'
commands."
  :init-value nil
  :global t
  :group 'mv-shell

  (if mv-shell-mode
      ;; activate
      (progn
        (add-hook 'comint-input-filter-functions 'mv-shell-check-string)
        (message "mv-shell mode enabled"))
    ;; deactivate
    (progn
      (remove-hook 'comint-input-filter-functions 'mv-shell-check-string)
      (message "mv-shell mode disabled"))))

(provide 'mv-shell)
;;; mv-shell.el ends here