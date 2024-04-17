;;; czm-tex-compile.el --- run latexmk continuously, report errors via flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/czm-tex-compile.el
;; Package-Requires: ((emacs "29.1") (auctex))
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
;; via latexmk, reporting errors via `flymake'.  Customize the
;; variable `czm-tex-compile-command' to change the command used to
;; compile the document.
;;
;; My use-package declaration:
;;
;; (use-package czm-tex-compile
;;   :elpaca (:host github :repo "ultronozm/czm-tex-compile.el"
;;                  :depth nil)
;;   :bind
;;   ("C-c k" . czm-tex-compile-toggle))

;;; Code:

(require 'tex)
(require 'flymake)

(defgroup czm-tex-compile nil
  "Convenience functions for compiling LaTeX documents."
  :group 'tex)

(defcustom czm-tex-compile-command
  "latexmk -pvc -shell-escape -pdf -view=none -e '$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/'"
  "Command to compile LaTeX documents."
  :type 'string
  :group 'czm-tex-compile)

(defvar-local czm-tex-compile--report-fn nil
  "Function provided by Flymake for reporting diagnostics.")

(defvar-local czm-tex-compile--timer-enabled nil)

(defvar czm-tex-compile--timer nil
  "Timer for reporting changes to the log file.")

(defconst czm-tex-compile--watching-str
  "=== Watching for updated files. Use ctrl/C to stop ..."
  "String indicating that latexmk is watching for updated files.")

(defcustom czm-tex-compile-ignored-warnings
  '("Package hyperref Warning: Token not allowed in a PDF string")
  "List of warnings to ignore when parsing LaTeX log files."
  :type '(repeat string))

(defcustom czm-tex-compile-report-hbox-errors nil
  "Non-nil means report hbox errors via flymake."
  :type 'boolean
  :group 'czm-tex-compile)

(defcustom czm-tex-compile-report-multiple-labels t
  "Non-nil means report multiple label errors via flymake."
  :type 'boolean
  :group 'czm-tex-compile)

(defun czm-tex-compile--process-multiply-defined-warning (message)
  "Get position of multiply defined MESSAGE labels."
  (let ((label (progn
                 (string-match "`\\(.*\\)'" message)
                 (match-string 1 message))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (re-search-forward (concat "\\\\label{" label "}")
                                 nil t)
          (cons (line-beginning-position)
                (line-end-position)))))))

(defun czm-tex-compile--process-regular-error (context line)
  "Get position of error and handle CONTEXT and LINE.
Return a cons cell (BEG . END) indicating where the error happens, or
nil if the error is not found."
  (when-let* ((prefix (with-temp-buffer
                        (insert context)
                        (goto-char (point-min))
                        (when (re-search-forward "\nl\\.\\([0-9]+\\) " nil t)
                          (buffer-substring-no-properties
                           (point) (line-end-position)))))
              (pos
               (save-excursion
                 (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (forward-line (1- line))
                   (let ((truncated-prefix
                          (substring prefix (max 0 (- (length prefix) 3))))
                         (line-end (line-end-position))
                         (bol (point)))
                     (or (search-forward truncated-prefix line-end t)
                         bol))))))
    (cons pos (1+ pos))))

(defun czm-tex-compile--error-list (log-file)
  "Retrieve parsed TeX error list from LOG-FILE."
  (with-temp-buffer
    (insert-file-contents log-file)
    (goto-char (point-min))
    (while (re-search-forward "Warning:" nil t)
      (let ((fill-column most-positive-fixnum))
        (call-interactively 'fill-paragraph)))
    (TeX-parse-all-errors)
    TeX-error-list))

(defun czm-tex-compile-process-log ()
  "Process log file for current LaTeX document.
Returns a list of triples (ERROR-P DESCRIPTION REGION), where
ERROR-P is non-nil if the error is an error rather than a
warning, DESCRIPTION is what you'd expect, and REGION is a cons
cell (BEG . END) indicating where the error happens."
  (let* ((error-list (czm-tex-compile--error-list (TeX-master-file "log")))
         (processed-list
          (mapcar
           (lambda (item)
             (let ((error-p (eq (nth 0 item) 'error))
                   (file (nth 1 item))
                   (line (nth 2 item))
                   (message (nth 3 item))
                   (context (nth 5 item))
                   (_search-string (nth 6 item))
                   (is-bad-box (nth 8 item)))
               (when-let
                   ((region
                     (cond
                      ((file-equal-p file (buffer-file-name))
                       (and
                        line
                        (not (cl-some (lambda (ignored)
                                        (string-match-p ignored message))
                                      czm-tex-compile-ignored-warnings))
                        (stringp file)
                        (or (not is-bad-box)
                            czm-tex-compile-report-hbox-errors)
                        (if error-p
                            (czm-tex-compile--process-regular-error context line)
                          (flymake-diag-region (current-buffer) line))))
                      ((file-equal-p file (TeX-master-file "aux"))
                       (and czm-tex-compile-report-multiple-labels
                            (string-match-p "multiply defined" message)
                            (not error-p)
                            (czm-tex-compile--process-multiply-defined-warning message))))))
                 (list error-p
                       (replace-regexp-in-string "\n" "" message)
                       region))))
           error-list)))
    (delq nil processed-list)))

(defun czm-tex-compile--compilation-command ()
  "Return the command used to compile the current LaTeX document."
  (format "%s %s" czm-tex-compile-command (TeX-master-file "tex")))


(defun czm-tex-compile--compilation-buffer-name ()
  "Return the name of the buffer used for LaTeX compilation."
  (let ((master (abbreviate-file-name (expand-file-name (TeX-master-file)))))
    (format "*pvc-%s*" master)))

(defun czm-tex-compile--compilation-buffer ()
  "Return the buffer used for LaTeX compilation."
  (get-buffer (czm-tex-compile--compilation-buffer-name)))

(defun czm-tex-compile--fresh-p ()
  "Return non-nil if logged errors should apply to current buffer.
This is the case if the current buffer is not modified, the
current buffer is a file, the current buffer has a log file, the
log file is newer than the current buffer, and the current
latexmk compilation is in a \"Watching\" state."
  (when-let* ((file (buffer-file-name))
              (log-file (TeX-master-file "log")))
    (and
     (when-let ((buf (czm-tex-compile--compilation-buffer)))
       (with-current-buffer buf
         (goto-char (point-max))
         (forward-line -1)
         (equal (buffer-substring (point) (line-end-position))
                czm-tex-compile--watching-str)))
     (not (buffer-modified-p))
     (file-exists-p file)
     (file-exists-p log-file)
     (time-less-p (nth 5 (file-attributes file))
                  (nth 5 (file-attributes log-file))))))

(defun czm-tex-compile--timer-function ()
  "Report to the flymake backend if the current buffer is fresh."
  (when czm-tex-compile--timer-enabled
    (dolist (datum (czm-tex-compile-process-log))
      (cl-assert (not (null (nth 2 datum))) nil
                 "Region is nil in datum: %S" datum))
    (when (and czm-tex-compile--report-fn (czm-tex-compile--fresh-p))
      (funcall
       czm-tex-compile--report-fn
       (mapcar
        (lambda (datum)
          (cl-destructuring-bind (error-p description region) datum
            (flymake-make-diagnostic
             (current-buffer) (car region) (cdr region)
             (if error-p :error :warning)
             description)))
        (czm-tex-compile-process-log))))))

(defvar czm-tex-compile-mode)

(defun czm-tex-compile-flymake (report-fn &rest _args)
  "Flymake backend for LaTeX based on latexmk.
Save REPORT-FN in a local variable, called by
e`czm-tex-compile--timer' to report diagnostics."
  (when czm-tex-compile-mode
    (setq czm-tex-compile--report-fn report-fn)))

(defvar-local czm-tex-compile--subscribed-buffers nil
  "List of buffers subscribed to the current LaTeX compilation.")

;;;###autoload
(define-minor-mode czm-tex-compile-mode
  "If enabled, run LaTeX compilation on the current buffer."
  :lighter nil
  (cond
   (czm-tex-compile-mode
    (let ((buf (current-buffer)))
      (if-let ((comp-buf (czm-tex-compile--compilation-buffer)))
          (with-current-buffer comp-buf
            (push buf czm-tex-compile--subscribed-buffers))
        (unless (start-process-shell-command
                 "czm-tex-compile"
                 (czm-tex-compile--compilation-buffer-name)
                 (czm-tex-compile--compilation-command))
          (error "Failed to start LaTeX compilation"))
        (with-current-buffer (czm-tex-compile--compilation-buffer)
          (special-mode)
          (push buf czm-tex-compile--subscribed-buffers))))
    (add-hook 'kill-buffer-hook 'czm-tex-compile--unsubscribe nil t)
    (add-hook 'flymake-diagnostic-functions #'czm-tex-compile-flymake nil t)
    (when czm-tex-compile--timer
      (cancel-timer czm-tex-compile--timer)
      (setq czm-tex-compile--timer nil))
    (setq czm-tex-compile--timer
          (run-with-timer 2 1 #'czm-tex-compile--timer-function))
    (setq czm-tex-compile--timer-enabled t))
   (t
    (let ((buf (current-buffer))
          (comp-buf (czm-tex-compile--compilation-buffer))
          done)
      (with-current-buffer comp-buf
        (setq czm-tex-compile--subscribed-buffers
              (cl-remove buf czm-tex-compile--subscribed-buffers))
        (when (null czm-tex-compile--subscribed-buffers)
          (setq done t)))
      (when done
        (let ((process (get-buffer-process comp-buf)))
          (when (process-live-p process)
            (interrupt-process process)
            (sit-for 0.1)
            (delete-process process))
          (kill-buffer comp-buf)))
      (setq czm-tex-compile--timer-enabled nil))
    (when czm-tex-compile--report-fn
      (setq czm-tex-compile--report-fn nil)))))

(defvar-local czm-tex-compile--saved-flymake-diagnostic-functions nil
  "Value of `flymake-diagnostic-functions' before calling `czm-tex-compile-toggle'.")

;;;###autoload
(defun czm-tex-compile-toggle ()
  "Toggle `czm-tex-compile-mode', and also `flymake-mode'."
  (interactive)
  (cond
   (czm-tex-compile-mode
    (czm-tex-compile-mode 0)
    (flymake-mode 0)
    (setq-local flymake-diagnostic-functions
                czm-tex-compile--saved-flymake-diagnostic-functions)
    (message "czm-tex-compile-mode and flymake-mode disabled"))
   (t
    (czm-tex-compile-mode 1)
    (setq czm-tex-compile--saved-flymake-diagnostic-functions flymake-diagnostic-functions)
    (setq-local flymake-diagnostic-functions '(czm-tex-compile-flymake t))
    (flymake-mode 1)
    (message "czm-tex-compile-mode and flymake-mode enabled"))))

(provide 'czm-tex-compile)
;;; czm-tex-compile.el ends here
