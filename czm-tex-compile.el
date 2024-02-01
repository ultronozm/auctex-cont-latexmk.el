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
      (czm-tex-compile-restrict-flymake-backends)
      ;; (czm-tex-compile-setup-flymake-backend)
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

;; maybe delete this?
(defun czm-tex-compile--paragraph-as-line ()
  "Return the current paragraph as a single line.
Used for navigating LaTeX warnings in the log file."
  (interactive)
  (let ((beg (point))
	       (end (save-excursion
	              (forward-paragraph)
	              (point))))
    (replace-regexp-in-string "\n" "" (buffer-substring-no-properties beg end))))

(defun czm-tex-compile-process-log ()
  "Process the log file for the current LaTeX document."
  (let* ((current-buf (current-buffer))
         (tex-file (buffer-file-name))
	        (log-file (concat (file-name-sans-extension tex-file)
                           ".log"))
         (error-prefix "! ")
         (warning-prefix "LaTeX Warning: ")
	        results)
    (with-temp-buffer
      (insert-file-contents log-file)
      (let* ((error-list (progn (TeX-parse-all-errors)
                                TeX-error-list))
             (filtered (seq-filter
                        (lambda (item)
                          (equal (expand-file-name (nth 1 item))
                                 (expand-file-name tex-file)))
                        error-list))
             (stuff (mapcar
                     (lambda (item)
                       (let* ((error-p (eq (nth 0 item)
                                           'error))
                              (description-raw (nth (if error-p 3 5) item))
                              (description (if error-p description-raw
                                             (substring description-raw
                                                        (length "LaTeX Warning: ") -1)))
                              line prefix
                              region)
                         (if error-p
                             (with-temp-buffer
                               (insert (nth 5 item))
                               (goto-char (point-min))
                               (when (re-search-forward "^\nl\\.\\([0-9]+\\) " nil t)
                                 (setq line (when (match-string 1)
                                              (string-to-number (match-string 1))))
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
                     (flymake-make-diagnostic
                      (current-buffer)
                      (car region)
                      (cdr region)
                      (if error-p
                          :error
                        :warning)
                      description)))
                 (seq-filter
                  (lambda (datum)
                    (not (null (nth 2 datum)))
                    )
                  log-data))))
    (funcall report-fn diags)
    t))

(defun czm-tex-compile-report-if-fresh (report-fn)
  "Call REPORT-FN if the current buffer is fresh."
  (when (czm-tex-compile--fresh-p)
    (czm-tex-compile-report report-fn)))

(defvar czm-tex-compile-log-watch-descriptor nil)
(defvar czm-tex-compile-report-fn nil)
(defvar czm-tex-compile-log-watch-timer nil)

(defun czm-tex-compile-log-timer-fn ()
  "Call `czm-tex-compile-report-if-fresh' and cancels the timer."
  (when czm-tex-compile-log-watch-timer
    (cancel-timer czm-tex-compile-log-watch-timer))
  (czm-tex-compile-report-if-fresh czm-tex-compile-report-fn))

(defun czm-tex-compile-log-change-handler (event)
  "Handle EVENT from log watcher.
If EVENT is `changed', then run `czm-tex-compile-log-timer-fn'
one second from now, so that the log has enough time to fully
update."
  (when (eq (nth 1 event)
            'changed)
    (when czm-tex-compile-log-watch-timer
      (cancel-timer czm-tex-compile-log-watch-timer))
    (setq-local czm-tex-compile-log-watch-timer
                (run-with-timer 1 1 #'czm-tex-compile-log-timer-fn))))

(require 'filenotify)

(defun czm-tex-compile-flymake (report-fn &rest _args)
  "Flymake backend for LaTeX based on latexmk.
REPORT-FN is the function called to report diagnostics.
ARGS are the keyword-value pairs concerning edits"
  (czm-tex-compile-report-if-fresh report-fn)
  (setq-local czm-tex-compile-report-fn report-fn)
  (when czm-tex-compile-log-watch-descriptor
    (file-notify-rm-watch czm-tex-compile-log-watch-descriptor))
  (setq-local czm-tex-compile-log-watch-descriptor
              (file-notify-add-watch
               (concat (file-name-sans-extension (buffer-file-name))
                       ".log")
               '(change)
               #'czm-tex-compile-log-change-handler)))

(defun czm-tex-compile-setup-flymake-backend ()
  "Setup flymake backend."
  (add-hook 'flymake-diagnostic-functions #'czm-tex-compile-flymake
            nil t))

(defun czm-tex-compile-restrict-flymake-backends ()
  (interactive)
  (setq flymake-diagnostic-functions '(czm-tex-compile-flymake t)))

(defun czm-tex-compile-relax-flymake-backends ()
  (interactive)
  (setq flymake-diagnostic-functions '(czm-tex-compile-flymake LaTeX-flymake t)))



(provide 'czm-tex-compile)
;;; czm-tex-compile.el ends here
