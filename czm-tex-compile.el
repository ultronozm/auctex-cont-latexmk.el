;;; czm-tex-compile.el --- Convenience functions for compiling LaTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-compile.el
;; Package-Requires: ((emacs "29.1"))
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

;; This package provides convenience functions for compiling a LaTeX
;; document continuously via latexmk and navigating the errors and
;; warnings recorded in the log file.  Customize the variable
;; `czm-tex-compile-command' to change the command used to compile the
;; document.
;;
;; My use-package declaration:
;;
;; (use-package czm-tex-compile
;;     :vc (:url "https://github.com/ultronozm/czm-tex-compile.el.git"
;;               :rev :newest)
;;     :after latex
;;     :bind
;;     ("C-c k" . czm-tex-compile)
;;     ("s-]" . czm-tex-compile-next-error)
;;     ("s-[" . czm-tex-compile-previous-error))

;;; Code:

(require 'esh-mode)

(defcustom czm-tex-compile-command
  "latexmk -shell-escape -pvc -pdf -view=none -e '$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/'"
  "Command to compile LaTeX documents."
  :type 'string
  :group 'czm-tex-compile)

(defun czm-tex-compile--kill-buffer-hook ()
  "Hook to kill the eshell buffer when the LaTeX buffer is killed."
  (when (string-match "\\([^\.]+\\)\.tex" (buffer-name))
    (let* ((name (match-string 1 (buffer-name)))
           (bufname (concat "*eshell-" name "*")))
      (when (get-buffer bufname)
        (let ((kill-buffer-query-functions '()))
          (with-current-buffer bufname
            (eshell-interrupt-process))
          (kill-buffer bufname))))))

;;;###autoload
(defun czm-tex-compile ()
  "Compile the current LaTeX document in an eshell buffer.
If the eshell buffer already exists, switch to it.  Otherwise,
create a new eshell buffer and compile the document in it.  The
eshell buffer is named *eshell-<name>*, where <name>.tex is the
name of the current LaTeX file."
  (interactive)
  (when (string-match "\\([^\.]+\\)\.tex" (buffer-name))
    (let* ((name (match-string 1 (buffer-name)))
           (bufname (concat "*eshell-" name "*")))
      (if (get-buffer bufname)
	  (switch-to-buffer bufname)
	(save-window-excursion
          (add-hook 'kill-buffer-hook #'czm-tex-compile--kill-buffer-hook)
	  (eshell "new")
	  (rename-buffer bufname)
	  (insert (concat czm-tex-compile-command " " name ".tex"))
	  (eshell-send-input))))))

(defvar-local czm-tex-compile--log-state nil
  "Cons containing last navigation time and log file position.")

(defun czm-tex-compile--paragraph-as-line ()
  "Return the current paragraph as a single line.
Used for navigating LaTeX warnings in the log file."
  (interactive)
  (let ((beg (point))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (replace-regexp-in-string "\n" "" (buffer-substring-no-properties beg end))))

;; TODO: look for the line <name>.bbl in the file, and don't jump to
;; line numbers found in log entries beyond that point (just display
;; them).

(defvar czm-tex-compile--debug nil
  "Whether to print debugging information.")

(defun czm-tex-compile--navigate-log-error (direction)
  "Helper function to navigate warnings in the log file.
DIRECTION should be either \='next or \='previous."
  (let* ((tex-file (buffer-file-name))
	 (log-file (concat (file-name-sans-extension tex-file)
                           ".log"))
	 (file-modification-time (nth 5 (file-attributes log-file)))
	 (last-navigation-time (car czm-tex-compile--log-state))
	 (log-pos (cdr czm-tex-compile--log-state))
	 line description)
    (with-temp-buffer
      (insert-file-contents log-file)
      (save-excursion)
      (if (or (null last-navigation-time)
	      (time-less-p last-navigation-time file-modification-time))
	  (goto-char (if (eq direction 'previous)
                         (point-max)
                       (point-min)))
	(goto-char log-pos))
      (let ((search-fn
	     (if (eq direction 'previous)
                 #'re-search-backward #'re-search-forward)))
        (when (eq direction 'next)
          (forward-line 2))
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
		       (point)
                       (line-end-position))
		    (czm-tex-compile--paragraph-as-line)))
	    (if error-p
		(progn
		  (save-excursion
		    (re-search-forward "^l\\.\\([0-9]+\\) " nil t)
		    (let ((line-number (when (match-string 1)
                                         (string-to-number (match-string 1))))
			  (line-prefix (buffer-substring-no-properties
					(point)
                                        (line-end-position))))
		      (setq line (cons line-number line-prefix)))))
	      (when (string-match "input line \\([0-9]+\\)" description)
		(setq line (string-to-number (match-string 1 description)))))
            (forward-line -1)
	    ;; (forward-line (if (eq direction 'previous) -1 1))
	    (setq log-pos (point))))))
    (setq-local czm-tex-compile--log-state (cons last-navigation-time log-pos))
    (when line
      (let ((pos
             (save-excursion
               (save-restriction
                 (goto-char (point-min))
                 (when-let ((line-number
                             (if (consp line)
                                 (car line)
                               line)))
                   (forward-line (1- line-number)))
                 (when (consp line)
                   (when-let* ((search-string (cdr line))
		               (truncated-search-string
		                (if (< (length search-string)
                                       3)
			            search-string
		                  (substring search-string 3))))
	             (search-forward truncated-search-string nil t)))
                 (point)))))
        (unless (<= (point-min)
                    pos (point-max))
          (widen))
        (goto-char pos)
        (recenter)))
    (message
     (concat (or description "No further errors or warnings.")
             (when czm-tex-compile--debug
               " -- "
               (format "%s" (cdr czm-tex-compile--log-state)))))))


(defun czm-tex-compile-process-log ()
  (let* ((tex-file (buffer-file-name))
	 (log-file (concat (file-name-sans-extension tex-file)
                           ".log"))
	 results)
    (with-temp-buffer
      (insert-file-contents log-file)
      (goto-char (point-min))
      (while (re-search-forward (concat "^" (regexp-opt '("! " "LaTeX Warning: "))
                                        "[^ ]")
                                nil t)
        (save-excursion
	  (goto-char (match-beginning 0))
	  (let* ((error-p (looking-at "! "))
                 (description (if error-p (buffer-substring-no-properties (point)
                                                                          (line-end-position))
                                (czm-tex-compile--paragraph-as-line)))
                 line prefix)
	    (if error-p
	        (progn
		  (save-excursion
                    (when
		        (re-search-forward "^l\\.\\([0-9]+\\) " nil t)
                      (setq line (when (match-string 1)
                                   (string-to-number (match-string 1))))
		      (setq prefix (buffer-substring-no-properties (point)
                                                                   (line-end-position))))))
	      (when (string-match "input line \\([0-9]+\\)" description)
	        (setq line (string-to-number (match-string 1 description)))))
            (when line
              (push (list error-p description line prefix)
                    results))))))
    (setq results
          (mapcar
           (lambda (result)
             (cl-destructuring-bind (error-p description line prefix)
                 result
               (list error-p
                     description
                     (if prefix
                         (let ((pos
                                (save-excursion
                                  (save-restriction
                                    (widen)
                                    (goto-char (point-min))
                                    (forward-line (1- line))
                                    ;; should probably just delete any
                                    ;; "..." at beginning
                                    (let ((truncated-prefix
                                           (substring prefix
                                                      (max 0 (- (length prefix)
                                                                3)))))
                                      (search-forward truncated-prefix nil t))))))
                           (when pos
                             (cons pos (1+ pos))))
                       (flymake-diag-region (current-buffer)
                                            line)))))
           results))
    results))

(defun czm-tex-compile--fresh-p ()
  "Return non-nil if logged errors should apply to current buffer.
This is the case if the current buffer is not modified, the
current buffer is a file, the current buffer has a log file, and
the log file is newer than the current buffer."
  (when-let* ((file (buffer-file-name))
              (log-file (concat (file-name-sans-extension file)
                                ".log")))
    (and
     (not (buffer-modified-p))
     (file-exists-p file)
     (file-exists-p log-file)
     (time-less-p (nth 5 (file-attributes file))
                  (nth 5 (file-attributes log-file))))))

(defun czm-tex-compile-flymake (report-fn &rest args)
  "Flymake backend for LaTeX based on latexmk.
REPORT-FN is the function called to report diagnostics.
ARGS are the keyword-value pairs concerning edits"
  (message "diagnostics time! %s" args)
  (when (czm-tex-compile--fresh-p)
    (let* ((log-data (czm-tex-compile-process-log))
           (diags (mapcar
                   (lambda (datum)
                     (cl-destructuring-bind (error-p description region)
                         datum
                       (flymake-make-diagnostic
                        (current-buffer)
                        (car region)
                        (cdr region)
                        (if error-p
                            :error
                          :warning)
                        description)))
                   log-data)))
      (funcall report-fn diags))))

(defun czm-tex-compile-setup-flymake-backend ()
  "Setup flymake backend."
  (add-hook 'flymake-diagnostic-functions #'czm-tex-compile-flymake
            nil t))

;;;###autoload
(defun czm-tex-compile-previous-error ()
  "Move point to the previous LaTeX-warning line."
  (interactive)
  (czm-tex-compile--navigate-log-error 'previous))

;;;###autoload
(defun czm-tex-compile-next-error ()
  "Move point to the next LaTeX-warning line."
  (interactive)
  (czm-tex-compile--navigate-log-error 'next))

(provide 'czm-tex-compile)
;;; czm-tex-compile.el ends here
