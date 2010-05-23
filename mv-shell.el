;; mv-shell.el - keep buffers in sync with filename throughout 'mv'
;; command in shell-mode.
;;
;; Copyright (C) 2010 Nathaniel Flath <nflath@gmail.com>
;; Version: 0.1
;;
;; Commentary
;; mv-shell integrates with shell-mode in order to keep buffers in sync when
;; moving files around.  If you enter a 'mv' command on a file that has a buffer opened,
;; the buffer is also renamed and moved to the location the file is moved to.
;;
;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;
;; (require 'mv-shell)
;; (mv-shell-mode 1)
;;
;;
;; Changelog:
;; 0.1
;;  * Initial Release
;;
;; Code:

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
                                          (concat to-raw "/" (path-to-filename from))
                                        to-raw))))
            (when (and (get-file-buffer from)
                       (not (file-directory-p from)))
              (set-buffer (get-file-buffer from))
              (rename-buffer (path-to-filename to))
              (set-visited-file-name to)
              (save-buffer)))))))

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