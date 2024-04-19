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
  "Run latexmk continuously, report errors via flymake."
  :group 'tex)

(defcustom czm-tex-compile-report-multiple-labels t
  "Non-nil means report multiple label errors via flymake."
  :type 'boolean
  :group 'czm-tex-compile)

(defun czm-tex-compile-process-item (type file line message offset _context search-string
                                          _line-end bad-box _error-point ignore)
  "Process an error or warning for the current TeX document.
The arguments are as in in `TeX-error-list'.  Return either nil or a
triple (ERROR-P DESCRIPTION (BEG . END)), where ERROR-P is non-nil if
the error is an error rather than a warning, and the other elements of
the triple describe the error or warning."
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
             (and czm-tex-compile-report-multiple-labels
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
Return a list of triples as in the docstring of
`czm-tex-compile-process-item'."
  (delq nil (mapcar (lambda (item)
                      (apply #'czm-tex-compile-process-item item))
                    (czm-tex-compile--error-list (TeX-master-file "log")))))

(defun czm-tex-compile--compilation-buffer-name ()
  "Return the name of the buffer used for LaTeX compilation."
  (let ((master (abbreviate-file-name (expand-file-name (TeX-master-file)))))
    (format "*pvc-%s*" master)))

(defun czm-tex-compile--compilation-buffer ()
  "Return the buffer used for LaTeX compilation."
  (get-buffer (czm-tex-compile--compilation-buffer-name)))

(defconst czm-tex-compile--watching-str
  "=== Watching for updated files. Use ctrl/C to stop ..."
  "String indicating that latexmk is watching for updated files.")

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

(defvar czm-tex-compile-mode)

(defvar-local czm-tex-compile--report-fn nil
  "Function provided by Flymake for reporting diagnostics.")

(defun czm-tex-compile-flymake (report-fn &rest _args)
  "Flymake backend for LaTeX based on latexmk.
Save REPORT-FN in a local variable, called by
e`czm-tex-compile--timer' to report diagnostics."
  (when czm-tex-compile-mode
    (setq czm-tex-compile--report-fn report-fn)))

(defvar czm-tex-compile--timer nil
  "Timer for reporting changes to the log file.")

(defun czm-tex-compile--timer-function ()
  "Report to the flymake backend if the current buffer is fresh."
  (when czm-tex-compile-mode
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

(defvar-local czm-tex-compile--subscribed-buffers nil
  "List of buffers subscribed to the current LaTeX compilation.")

(defun czm-tex-compile--unsubscribe ()
  "Unsubscribe from LaTeX compilation if the current buffer is in the list."
  (let ((buf (current-buffer))
        (comp-buf (czm-tex-compile--compilation-buffer))
        done)
    (when comp-buf
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
          (kill-buffer comp-buf))))))

(defcustom czm-tex-compile-command
  "latexmk -pvc -shell-escape -pdf -view=none -e '$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/'"
  "Command to compile LaTeX documents."
  :type 'string
  :group 'czm-tex-compile)

(defun czm-tex-compile--compilation-command ()
  "Return the command used to compile the current LaTeX document."
  (format "%s %s" czm-tex-compile-command (TeX-master-file "tex")))

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
          (run-with-timer 2 1 #'czm-tex-compile--timer-function)))
   (t
    (czm-tex-compile--unsubscribe)
    (remove-hook 'kill-buffer-hook 'czm-tex-compile--unsubscribe t)
    (remove-hook 'flymake-diagnostic-functions #'czm-tex-compile-flymake t)
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
