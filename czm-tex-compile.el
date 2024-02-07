;;; czm-tex-compile.el --- Convenience functions for compiling LaTeX  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
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
;; via latexmk and reports errors and warnings to a `flymake' backend.
;; Customize the variable `czm-tex-compile-command' to change the
;; command used to compile the document.
;;
;; My use-package declaration:
;;
;; (use-package czm-tex-compile
;;   :elpaca (:host github :repo "ultronozm/czm-tex-compile.el"
;;                  :depth nil)
;;   :bind
;;   ("C-c k" . czm-tex-compile-toggle))

;;; Code:

(require 'esh-mode)

(defgroup czm-tex-compile nil
  "Convenience functions for compiling LaTeX documents."
  :group 'tex)

(defcustom czm-tex-compile-command
  "latexmk -shell-escape -pvc -pdf -view=none -e '$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/'"
  "Command to compile LaTeX documents."
  :type 'string)

(defvar-local czm-tex-compile-process nil
  "Process running the LaTeX compilation.")

(defvar-local czm-tex-compile-log-watch-descriptor nil)
(defvar-local czm-tex-compile-report-fn nil)
(defvar-local czm-tex-compile-log-watch-timer nil)
(defvar-local czm-tex-compile--compilation-buffer-name nil)

;;;###autoload
(define-minor-mode czm-tex-compile-mode
  "If enabled, run LaTeX compilation on the current buffer."
  :lighter " TexC"
  (if czm-tex-compile-mode
      (let ((name (and (string-match "\\([^\.]+\\)\.tex" (buffer-name))
                       (match-string 1 (buffer-name)))))
        (unless name
          (user-error "Buffer name does not match expected pattern"))
        (when (process-live-p czm-tex-compile-process)
          (interrupt-process czm-tex-compile-process)
          (sit-for 1)
          (delete-process czm-tex-compile-process))
        (setq czm-tex-compile--compilation-buffer-name (concat "*czm-tex-compile-" name "*"))
        (let ((command (concat czm-tex-compile-command " " name ".tex")))
          (setq czm-tex-compile-process
                (start-process-shell-command "czm-tex-compile" czm-tex-compile--compilation-buffer-name
                                             command)))
        (add-hook 'kill-buffer-hook 'czm-tex-compile--kill-process nil t)
        (add-hook 'flymake-diagnostic-functions #'czm-tex-compile-flymake nil t))
    (czm-tex-compile--kill-process)
    (when czm-tex-compile-log-watch-descriptor
      (file-notify-rm-watch czm-tex-compile-log-watch-descriptor)
      (setq czm-tex-compile-log-watch-descriptor nil))
    (when czm-tex-compile-log-watch-timer
      (cancel-timer czm-tex-compile-log-watch-timer)
      (setq czm-tex-compile-log-watch-timer nil))
    (when czm-tex-compile-report-fn
      (setq czm-tex-compile-report-fn nil))))

;;;###autoload
(defun czm-tex-compile-toggle ()
  "Toggle `czm-tex-compile-mode', and also `flymake'."
  (interactive)
  (if czm-tex-compile-mode
      (progn
        (czm-tex-compile-mode 0)
        (flymake-mode 0)
        (czm-tex-compile-relax-flymake-backends)
        (message "czm-tex-compile-mode and flymake-mode disabled"))
    (czm-tex-compile-mode 1)
    (czm-tex-compile-restrict-flymake-backends)
    (flymake-mode 1)
    (message "czm-tex-compile-mode and flymake-mode enabled")))


(defun czm-tex-compile--kill-process ()
  "Kill the LaTeX compilation process associated with the buffer."
  (when (process-live-p czm-tex-compile-process)
    (interrupt-process czm-tex-compile-process)
    (sit-for 1)
    (delete-process czm-tex-compile-process))
  (when (get-buffer czm-tex-compile--compilation-buffer-name)
    (kill-buffer czm-tex-compile--compilation-buffer-name)))

(require 'tex)


(defun testing ()
  (interactive)
  (while (re-search-forward "Warning:" nil t)
    ;; make it all appear on one line
    (end-of-line)
    (while (not (looking-at "\n\n"))
      (delete-char 1)
      (end-of-line))))

(defun czm-tex-compile-process-log ()
  "Process the log file for the current LaTeX document."
  (let* ((current-buf (current-buffer))
         (tex-file (buffer-file-name))
	        (log-file (concat (file-name-sans-extension tex-file)
                           ".log")))
    (with-temp-buffer
      (insert-file-contents log-file)
      (goto-char (point-min))
      (while (re-search-forward "Warning:" nil t)
        ;; make it all appear on one line
        (end-of-line)
        (while (not (looking-at "\n\n"))
          (delete-char 1)
          (end-of-line)))
      (let* ((error-list (progn (TeX-parse-all-errors)
                                TeX-error-list))
             (filtered (seq-filter
                        (lambda (item)
                          (and
                           (stringp (nth 1 item))
                           (equal (expand-file-name (nth 1 item))
                                  (expand-file-name tex-file))))
                        error-list))
             (stuff (mapcar
                     (lambda (item)
                       (let* ((error-p (eq (nth 0 item)
                                           'error))
                              (description-raw (nth (if error-p 3 5) item))
                              (description (if error-p description-raw
                                             (substring description-raw
                                                        0
                                                        ;; (progn (string-match " Warning: " description-raw)
                                                        ;;        (match-end 0))
                                                        -1)))
                              line prefix)
                         (if error-p
                             (with-temp-buffer
                               (insert (nth 5 item))
                               (goto-char (point-min))
                               (setq line (nth 2 item))
                               (when (re-search-forward "\nl\\.\\([0-9]+\\) " nil t)
		                               (setq prefix (buffer-substring-no-properties (point)
                                                                              (line-end-position)))))
	                          (when (string-match "input line \\([0-9]+\\)" description)
	                            (setq line (string-to-number (match-string 1 description)))))
                         (list
                          error-p
                          (replace-regexp-in-string
                           "\n" ""
                           description)
                          (when line
                            (if prefix
                                (let ((pos
                                       (with-current-buffer current-buf
                                         (save-excursion
                                           (save-restriction
                                             (widen)
                                             (goto-char (point-min))
                                             (forward-line (1- line))
                                             ;; should probably just delete any
                                             ;; "..." at beginning?
                                             (let ((truncated-prefix
                                                    (substring prefix
                                                               (max 0 (- (length prefix)
                                                                         3)))))
                                               (search-forward truncated-prefix nil t)))))))
                                  (when pos
                                    (cons pos (1+ pos))))
                              (flymake-diag-region current-buf
                                                   line))))))
                     filtered)))
        stuff))))

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

(defun czm-tex-compile-report (report-fn)
  "Call REPORT-FN if the current buffer is fresh."
  (let* ((log-data (czm-tex-compile-process-log))
         (diags (mapcar
                 (lambda (datum)
                   (cl-destructuring-bind (error-p description region)
                       datum
                     (flymake-make-diagnostic (current-buffer)
                                              (car region)
                                              (cdr region)
                                              (if error-p :error :warning)
                                              description)))
                 (seq-filter
                  (lambda (datum)
                    (not (null (nth 2 datum))))
                  log-data))))
    (funcall report-fn diags)
    t))

(defun czm-tex-compile-report-if-fresh ()
  "Call REPORT-FN if the current buffer is fresh."
  (when (and czm-tex-compile-report-fn
             (czm-tex-compile--fresh-p))
    (czm-tex-compile-report czm-tex-compile-report-fn)))

(defun czm-tex-compile-log-timer-fn ()
  "Call `czm-tex-compile-report-if-fresh' and cancels the timer."
  (when czm-tex-compile-log-watch-timer
    (cancel-timer czm-tex-compile-log-watch-timer))
  (czm-tex-compile-report-if-fresh))

(defun czm-tex-compile-log-change-handler (event)
  "Handle EVENT from log watcher.
If EVENT is `changed', then run `czm-tex-compile-log-timer-fn'
one second from now, so that the log has enough time to fully
update."
  (when (eq (nth 1 event)
            'changed)
    (when czm-tex-compile-log-watch-timer
      (cancel-timer czm-tex-compile-log-watch-timer))
    (setq czm-tex-compile-log-watch-timer
          (run-with-timer 1 1 #'czm-tex-compile-log-timer-fn))))

(require 'filenotify)

(defun czm-tex-compile-flymake (report-fn &rest _args)
  "Flymake backend for LaTeX based on latexmk.
REPORT-FN is the function called to report diagnostics.
ARGS are the keyword-value pairs concerning edits"
  (when (czm-tex-compile-mode)
    (setq czm-tex-compile-report-fn report-fn)
    (czm-tex-compile-report-if-fresh)
    (when czm-tex-compile-log-watch-descriptor
      (file-notify-rm-watch czm-tex-compile-log-watch-descriptor))
    (setq czm-tex-compile-log-watch-descriptor
          (file-notify-add-watch
           (concat (file-name-sans-extension (buffer-file-name))
                   ".log")
           '(change)
           #'czm-tex-compile-log-change-handler))))

(defun czm-tex-compile-restrict-flymake-backends ()
  "Restrict flymake backends to `czm-tex-compile-flymake'."
  (interactive)
  (setq-local flymake-diagnostic-functions '(czm-tex-compile-flymake t)))

(defun czm-tex-compile-relax-flymake-backends ()
  "Relax flymake backends to include `LaTeX-flymake'."
  (interactive)
  (setq-local flymake-diagnostic-functions '(czm-tex-compile-flymake LaTeX-flymake t)))



(provide 'czm-tex-compile)
;;; czm-tex-compile.el ends here
