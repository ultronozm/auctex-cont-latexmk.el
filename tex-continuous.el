;;; tex-continuous.el --- run latexmk continuously, report errors via flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/tex-continuous.el
;; Package-Requires: ((emacs "27.1") (auctex "11.92"))
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

;; This package provides a minor mode that compiles a LaTeX document
;; via latexmk, reporting errors via `flymake'.
;;
;; Use M-x tex-continuous-toggle to toggle the minor mode and set up
;; flymake.  If you use flymake in tex documents for other reasons,
;; then you should instead use M-x tex-continuous-mode.
;;
;; Customize the variable `tex-continuous-command' to change the
;; compilation command.
;;
;; The compilation takes place in a buffer *pvc-filename*, so look
;; there if you need to see the output.

;;; Code:

(require 'tex)
(require 'flymake)

(defgroup tex-continuous nil
  "Run latexmk continuously, report errors via flymake."
  :group 'tex)

(defcustom tex-continuous-report-multiple-labels t
  "Non-nil means report multiple label errors via flymake."
  :type 'boolean)

(defcustom tex-continuous-command
  "latexmk -pvc -shell-escape -pdf -view=none -e '$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/'"
  "Command to compile LaTeX documents."
  :type 'string)

(defun tex-continuous-process-item (type file line message offset _context search-string
                                         _line-end bad-box _error-point ignore)
  "Process an error or warning for the current TeX document.
The arguments are as in `TeX-error-list'.  Return either nil or a
triple (ERROR-P DESCRIPTION (BEG . END)), where ERROR-P is non-nil if it
is an error rather than a warning."
  (and
   (not ignore)
   (stringp file)
   (or (not bad-box) TeX-debug-bad-boxes)
   (when-let
       ((region
         (save-restriction
           (widen)
           (cond
            ((file-equal-p file (buffer-file-name))
             (when line
               (if (eq type 'error)
                   (save-excursion
                     (goto-char (point-min))
                     (forward-line (+ line offset -1))
                     (unless (string= search-string " ")
                       (search-forward search-string nil t)
                       (cons (point) (1+ (point)))))
                 (flymake-diag-region (current-buffer) (+ line offset)))))
            ((file-equal-p file (TeX-master-file "aux"))
             (and tex-continuous-report-multiple-labels
                  (string-match-p "multiply defined" message)
                  (not (eq type 'error))
                  (let* ((label (progn
                                  (string-match "`\\(.*\\)'" message)
                                  (match-string 1 message)))
                         (label-re
                          (concat "\\\\label\\(?:\\[[^]]+\\]\\)?{"
                                  (regexp-quote label) "}")))
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward label-re nil t)
                        ;; Return the full line so the diagnostic is
                        ;; not covered by preview overlays when
                        ;; \\label appears after \\begin{equation}.
                        (cons (line-beginning-position)
                              (line-end-position)))))))))))
     (list (eq type 'error)
           (replace-regexp-in-string "\n" "" message)
           region))))

(defun tex-continuous--error-list (log-file)
  "Retrieve parsed TeX error list from LOG-FILE."
  (with-temp-buffer
    (insert-file-contents log-file)
    (goto-char (point-min))
    (while (re-search-forward "Warning:" nil t)
      (let ((fill-column most-positive-fixnum))
        (call-interactively 'fill-paragraph)))
    (TeX-parse-all-errors)
    TeX-error-list))

(defun tex-continuous-process-log ()
  "Process log file for current LaTeX document.
Return a list of triples as in the docstring of
`tex-continuous-process-item'."
  (delq nil (mapcar (lambda (item)
                      (apply #'tex-continuous-process-item item))
                    (tex-continuous--error-list (TeX-master-file "log")))))

(defun tex-continuous--compilation-buffer-name ()
  "Return the name of the buffer used for LaTeX compilation."
  (let ((master (abbreviate-file-name (expand-file-name (TeX-master-file)))))
    (format "*pvc-%s*" master)))

(defun tex-continuous--compilation-buffer ()
  "Return the buffer used for LaTeX compilation."
  (get-buffer (tex-continuous--compilation-buffer-name)))

(defconst tex-continuous--watching-str
  "=== Watching for updated files. Use ctrl/C to stop ..."
  "String indicating that latexmk is watching for updated files.")

(defun tex-continuous--fresh-p ()
  "Return non-nil if logged errors should apply to current buffer.
This is the case if the current buffer is not modified, the
current buffer is a file, the current buffer has a log file, the
log file is newer than the current buffer, and the current
latexmk compilation is in a \"Watching\" state."
  (when-let* ((file (buffer-file-name))
              (log-file (TeX-master-file "log")))
    (and
     (when-let ((buf (tex-continuous--compilation-buffer)))
       (with-current-buffer buf
         (goto-char (point-max))
         (forward-line -1)
         (equal (buffer-substring (point) (line-end-position))
                tex-continuous--watching-str)))
     (not (buffer-modified-p))
     (file-exists-p file)
     (file-exists-p log-file)
     (time-less-p (nth 5 (file-attributes file))
                  (nth 5 (file-attributes log-file))))))

(defvar tex-continuous-mode)

(defvar-local tex-continuous--report-fn nil
  "Function provided by Flymake for reporting diagnostics.")

(defun tex-continuous-flymake (report-fn &rest _args)
  "Flymake backend for LaTeX based on latexmk.
Save REPORT-FN in a local variable, called by `tex-continuous--timer' to
report diagnostics."
  (when tex-continuous-mode
    (setq tex-continuous--report-fn report-fn)))

(defvar tex-continuous--timer nil
  "Timer for reporting changes to the log file.")

(defun tex-continuous--timer-function ()
  "Report to the flymake backend if the current buffer is fresh."
  (and tex-continuous-mode
       tex-continuous--report-fn
       (tex-continuous--fresh-p)
       (funcall
        tex-continuous--report-fn
        (mapcar
         (lambda (datum)
           (cl-destructuring-bind (error-p description region) datum
             (flymake-make-diagnostic
              (current-buffer) (car region) (cdr region)
              (if error-p :error :warning)
              description)))
         (tex-continuous-process-log)))))

(defvar-local tex-continuous--subscribed-buffers nil
  "List of buffers subscribed to the current LaTeX compilation.")

(defun tex-continuous--unsubscribe ()
  "Unsubscribe from LaTeX compilation if the current buffer is in the list."
  (let ((buf (current-buffer))
        (comp-buf (tex-continuous--compilation-buffer))
        done)
    (when comp-buf
      (with-current-buffer comp-buf
        (setq tex-continuous--subscribed-buffers
              (cl-remove buf tex-continuous--subscribed-buffers))
        (when (null tex-continuous--subscribed-buffers)
          (setq done t)))
      (when done
        (let ((process (get-buffer-process comp-buf)))
          (when (process-live-p process)
            (interrupt-process process)
            (sit-for 0.1)
            (delete-process process))
          (kill-buffer comp-buf))))))

(defun tex-continuous--compilation-command ()
  "Return the command used to compile the current LaTeX document."
  (format "%s %s" tex-continuous-command (TeX-master-file "tex")))

;;;###autoload
(define-minor-mode tex-continuous-mode
  "If enabled, run LaTeX compilation on the current buffer."
  :lighter nil
  (cond
   (tex-continuous-mode
    (let ((buf (current-buffer)))
      (if-let ((comp-buf (tex-continuous--compilation-buffer)))
          (with-current-buffer comp-buf
            (push buf tex-continuous--subscribed-buffers))
        (unless (start-process-shell-command
                 "tex-continuous"
                 (tex-continuous--compilation-buffer-name)
                 (tex-continuous--compilation-command))
          (error "Failed to start LaTeX compilation"))
        (with-current-buffer (tex-continuous--compilation-buffer)
          (special-mode)
          (push buf tex-continuous--subscribed-buffers))))
    (add-hook 'kill-buffer-hook 'tex-continuous--unsubscribe nil t)
    (add-hook 'flymake-diagnostic-functions #'tex-continuous-flymake nil t)
    (when tex-continuous--timer
      (cancel-timer tex-continuous--timer)
      (setq tex-continuous--timer nil))
    (setq tex-continuous--timer
          (run-with-timer 2 1 #'tex-continuous--timer-function)))
   (t
    (tex-continuous--unsubscribe)
    (remove-hook 'kill-buffer-hook 'tex-continuous--unsubscribe t)
    (remove-hook 'flymake-diagnostic-functions #'tex-continuous-flymake t)
    (when tex-continuous--report-fn
      (setq tex-continuous--report-fn nil)))))

(defvar-local tex-continuous--saved-flymake-diagnostic-functions nil
  "Value of `flymake-diagnostic-functions' before calling `tex-continuous-toggle'.")

;;;###autoload
(defun tex-continuous-toggle ()
  "Toggle `tex-continuous-mode', and also `flymake-mode'."
  (interactive)
  (cond
   (tex-continuous-mode
    (tex-continuous-mode 0)
    (flymake-mode 0)
    (setq-local flymake-diagnostic-functions
                tex-continuous--saved-flymake-diagnostic-functions)
    (message "tex-continuous-mode and flymake-mode disabled"))
   (t
    (tex-continuous-mode 1)
    (setq tex-continuous--saved-flymake-diagnostic-functions flymake-diagnostic-functions)
    (setq-local flymake-diagnostic-functions '(tex-continuous-flymake t))
    (flymake-mode 1)
    (message "tex-continuous-mode and flymake-mode enabled"))))

(provide 'tex-continuous)
;;; tex-continuous.el ends here
