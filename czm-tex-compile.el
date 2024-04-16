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
  "latexmk -shell-escape -pvc -pdf -view=none -e '$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/'"
  "Command to compile LaTeX documents."
  :type 'string
  :group 'czm-tex-compile)

(defvar-local czm-tex-compile--process nil
  "Process running the LaTeX compilation.")

(defvar-local czm-tex-compile--report-fn nil
  "Function provided by Flymake for reporting diagnostics.")

(defvar-local czm-tex-compile--timer nil
  "Timer for reporting changes to the log file.")

(defvar-local czm-tex-compile--compilation-buffer-name nil
  "Name of the buffer used for LaTeX compilation.")

(defun czm-tex-compile--kill-process ()
  "Kill the LaTeX compilation process associated with the buffer.
Also kill the timer for watching the log file."
  (when (process-live-p czm-tex-compile--process)
    (interrupt-process czm-tex-compile--process)
    (sit-for 0.1)
    (delete-process czm-tex-compile--process))
  (when (get-buffer czm-tex-compile--compilation-buffer-name)
    (kill-buffer czm-tex-compile--compilation-buffer-name))
  (when czm-tex-compile--timer
    (cancel-timer czm-tex-compile--timer)
    (setq czm-tex-compile--timer nil)))

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

(defun czm-tex-compile--process-multiply-defined-warning (message buf)
  "Get position of multiply defined MESSAGE labels in BUF."
  (let ((label (progn
                 (string-match "`\\(.*\\)'" message)
                 (match-string 1 message))))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (re-search-forward (concat "\\\\label{" label "}")
                                   nil t)
            (cons (line-beginning-position)
                  (line-end-position))))))))

(defun czm-tex-compile--process-regular-error (context line current-buf)
  "Get position of error and handle CONTEXT and LINE in CURRENT-BUF.
Return a cons cell (BEG . END) indicating where the error happens, or
nil if the error is not found."
  (let ((prefix nil))
    (with-temp-buffer
      (insert context)
      (goto-char (point-min))
      (when (re-search-forward "\nl\\.\\([0-9]+\\) " nil t)
        (setq prefix (buffer-substring-no-properties (point)
                                                     (line-end-position)))))
    (when prefix
      (let ((pos
             (with-current-buffer current-buf
               (save-excursion
                 (save-restriction
                   (widen)
                   (goto-char (point-min))
                   (forward-line (1- line))
                   (let ((truncated-prefix
                          (substring prefix
                                     (max 0 (- (length prefix)
                                               3))))
                         (line-end (line-end-position))
                         (bol (point)))
                     (or
                      (search-forward truncated-prefix line-end t)
                      bol)))))))
        (when pos
          (cons pos (1+ pos)))))))

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
  (mapcar
   (lambda (item)
     (let ((type (nth 0 item))
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
                (if (eq type 'error)
                    (czm-tex-compile--process-regular-error context line (current-buffer))
                  (flymake-diag-region (current-buffer) line))))
              ((file-equal-p file (TeX-master-file "aux"))
               (and czm-tex-compile-report-multiple-labels
                    (string-match-p "multiply defined" message)
                    (not (eq type 'error))
                    (let ((label (progn (string-match "`\\(.*\\)'" message)
                                        (match-string 1 message))))
                      (save-excursion
                        (save-restriction
                          (widen)
                          (goto-char (point-min))
                          (when (re-search-forward (concat "\\\\label{" label "}")
                                                   nil t)
                            (cons (line-beginning-position)
                                  (line-end-position)))))))))))
         (list (eq type 'error)
               (replace-regexp-in-string "\n" "" message)
               region))))
   (czm-tex-compile--error-list (TeX-master-file "log"))))

(defun czm-tex-compile--fresh-p ()
  "Return non-nil if logged errors should apply to current buffer.
This is the case if the current buffer is not modified, the
current buffer is a file, the current buffer has a log file, the
log file is newer than the current buffer, and the current
latexmk compilation is in a \"Watching\" state."
  (when-let* ((file (buffer-file-name))
              (log-file (TeX-master-file "log")))
    (and
     (when-let ((buf (get-buffer czm-tex-compile--compilation-buffer-name)))
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
      (seq-filter
       (lambda (datum) (not (null (nth 2 datum))))
       (czm-tex-compile-process-log))))))

(defvar czm-tex-compile-mode)

(defun czm-tex-compile-flymake (report-fn &rest _args)
  "Flymake backend for LaTeX based on latexmk.
Save REPORT-FN in a local variable, called by
e`czm-tex-compile--timer' to report diagnostics."
  (when (czm-tex-compile-mode)
    (setq czm-tex-compile--report-fn report-fn)))

;;;###autoload
(define-minor-mode czm-tex-compile-mode
  "If enabled, run LaTeX compilation on the current buffer."
  :lighter nil
  (cond
   (czm-tex-compile-mode
    (let ((name (and (string-match "\\([^\.]+\\)\.tex" (buffer-name))
                     (match-string 1 (buffer-name)))))
      (unless name
        (user-error "Buffer name does not match expected pattern"))
      (when (process-live-p czm-tex-compile--process)
        (interrupt-process czm-tex-compile--process)
        (sit-for 0.1)
        (delete-process czm-tex-compile--process))
      (setq czm-tex-compile--compilation-buffer-name
            (concat "*czm-tex-compile-" (expand-file-name name) "*"))
      (let ((command (concat czm-tex-compile-command " " name ".tex")))
        (setq czm-tex-compile--process
              (start-process-shell-command
               "czm-tex-compile" czm-tex-compile--compilation-buffer-name
               command)))
      (let ((current-buf (current-buffer)))
        (with-current-buffer (get-buffer czm-tex-compile--compilation-buffer-name)
          (special-mode)
          (setq-local czm-tex-compile--parent-buffer current-buf)
          (local-set-key (kbd "TAB")
                         (lambda ()
                           (interactive)
                           (set-window-buffer (selected-window) czm-tex-compile--parent-buffer)))))
      (add-hook 'kill-buffer-hook 'czm-tex-compile--kill-process nil t)
      (add-hook 'flymake-diagnostic-functions #'czm-tex-compile-flymake nil t)
      (when czm-tex-compile--timer
        (cancel-timer czm-tex-compile--timer)
        (setq czm-tex-compile--timer nil))
      (setq czm-tex-compile--timer
            (run-with-timer 2 1 #'czm-tex-compile--timer-function))))
   (t
    (czm-tex-compile--kill-process)
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
