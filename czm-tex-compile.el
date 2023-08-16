;;; czm-tex-compile.el --- Convenience functions compiling LaTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Keywords: tex

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

;; Convenience functions for compiling LaTeX.  Under construction.

;;; Code:


(defun czm-latexmk-this ()
  (interactive)
  (if (string-match "\\([^\.]+\\)\.tex" (buffer-name))
      (let ((name (match-string 1 (buffer-name))))
	(let ((bufname (concat "*eshell-" name "*")))
	  (if (get-buffer bufname)
	      (switch-to-buffer bufname)
	    (save-window-excursion
	      (eshell "new")
	      (rename-buffer bufname)
	      (insert (concat "latexmk -shell-escape -pvc -pdf -view=none " name ".tex"))
	      (eshell-send-input)))))))

(defun my-latexmk-this ()
  (interactive)
  (if (string-match "\\([^\.]+\\)\.tex" (buffer-name))
      (let ((name (match-string 1 (buffer-name)))
            (default-directory (file-name-directory (buffer-file-name))))
        (let ((bufname (concat "*latexmk-" name "*")))
          (with-current-buffer (get-buffer-create bufname)
            (compilation-start (format "latexmk -shell-escape -pvc -pdf -view=none %s.tex" name) 'compilation-mode)
            (display-buffer bufname))))))

(defun my-latexmk-this ()
  (interactive)
  (if (string-match "\\([^\.]+\\)\.tex" (buffer-name))
      (let ((name (match-string 1 (buffer-name)))
            (default-directory (file-name-directory (buffer-file-name))))
        (let ((bufname (concat "*latexmk-" name "*")))
          (with-current-buffer (get-buffer-create bufname)
            (unless (eq major-mode 'compilation-mode)
              (compilation-mode))
            (compilation-start (format "latexmk -shell-escape -pvc -pdf -view=none %s.tex" name) nil
                               (lambda (_mode-name) bufname))
            (display-buffer bufname)
            (local-set-key (kbd "x") 'my-kill-compilation))))))

(defun my-kill-compilation ()
  "Kill the current compilation process."
  (interactive)
  (when (get-buffer-process (current-buffer))
    (interrupt-process (get-buffer-process (current-buffer)))
    (message "Compilation process killed.")))

;; Maybe improve this at some point using the following:
;; (let ((entry (save-excursion
;;              (bibtex-beginning-of-entry)
;;              (bibtex-parse-entry))))
;;       (cdr (assoc "=key=" entry))

(defun my-paragraph-as-line ()
  (interactive)
  (let ((beg (point))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (replace-regexp-in-string "\n" "" (buffer-substring-no-properties beg end))))

(defvar-local czm/latex-log-state nil
  "Buffer-local variable with a cons containing last navigation time and log file position.")

(defun czm/latex-navigate-log-error (direction)
  "Helper function to navigate warnings in the log file.
DIRECTION should be either 'next or 'previous."
  (let* ((tex-file (buffer-file-name))
	 (log-file (concat (file-name-sans-extension tex-file) ".log"))
	 (already-open (find-buffer-visiting log-file))
	 (buf (or already-open (find-file-noselect log-file)))
	 (file-modification-time (nth 5 (file-attributes log-file)))
	 (last-navigation-time (car czm/latex-log-state))
	 (log-pos (cdr czm/latex-log-state))
	 line description)
    (with-current-buffer buf
      (save-excursion)
      (if (or (null last-navigation-time)
	      (time-less-p last-navigation-time file-modification-time))
	  (goto-char (if (eq direction 'previous) (point-max) (point-min)))
	(goto-char log-pos))
      (let ((search-fn
	     (if (eq direction 'previous) #'re-search-backward #'re-search-forward)))
	(when (funcall search-fn
		       (concat "^"
			       (regexp-opt
				'("! "
				  "LaTeX Warning: "))
			       "[^ ]")
		       nil t)
	  (goto-char (match-beginning 0))
	  (let ((error-p (looking-at "! ")))
	    (setq last-navigation-time (current-time))
	    (setq description
		  (if error-p
		      (buffer-substring-no-properties
		       (point) (line-end-position))
		    (my-paragraph-as-line)))
	    (if error-p
		(progn
		  (save-excursion
		    (re-search-forward "^l\\.\\([0-9]+\\) " nil t)
		    (let ((line-number (string-to-number (match-string 1)))
			  (line-prefix (buffer-substring-no-properties
					(point) (line-end-position))))
		      (setq line (cons line-number line-prefix))))
		  )
	      (when (string-match "input line \\([0-9]+\\)" description)
		(setq line (string-to-number (match-string 1 description)))))
	    (forward-line (if (eq direction 'previous) -1 1))
	    
	    (setq log-pos (point))))))
    (unless already-open
      (kill-buffer buf))
    (setq-local czm/latex-log-state (cons last-navigation-time log-pos))
    (when line
      (if (consp line)
	  (progn
	    (goto-line (car line))
	    (let* ((search-string (cdr line))
		   (truncated-search-string
		    (if (< (length search-string) 3)
			search-string
		      (substring search-string 3)
		      )))
	      (search-forward truncated-search-string nil t)))
	(goto-line line)))
    (message (or description "No further errors or warnings."))))

(defun czm/latex-previous-log-error ()
  "Move point to the previous LaTeX-warning line."
  (interactive)
  (czm/latex-navigate-log-error 'previous))

(defun czm/latex-next-log-error ()
  "Move point to the next LaTeX-warning line."
  (interactive)
  (czm/latex-navigate-log-error 'next))

(provide 'czm-tex-compile)
;;; czm-tex-compile.el ends here
