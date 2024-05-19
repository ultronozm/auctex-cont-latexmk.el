;;; tex-continuous.el --- run latexmk continuously, report errors via flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/tex-continuous.el
;; Package-Requires: ((emacs "29.3") (auctex "14.0.5"))
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
;; Use `tex-continuous-toggle' to toggle the minor mode and set up
;; flymake.
;;
;; If you want to enable continuous compilation but prefer either not
;; to use the flymake backend or to manage it yourself (e.g., in
;; combination with other flymake backends), then instead use
;; `tex-continuous-mode' and add #'tex-continuous-flymake to
;; `flymake-diagnostic-functions' when you'd like.
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
  '("latexmk -pvc -shell-escape -pdf -view=none -e "
    ("$pdflatex=q/pdflatex %O -synctex=1 -interaction=nonstopmode %S/"))
  "Command to compile LaTeX documents.
This is a list consisting of strings or lists of strings.  It is
compiled to a single string by concatenating the strings and quoting the
lists, using system-specific quotes.  To produce the compilation
command, it is combined with an additional option to output build files
to a directory (if `TeX-output-dir' is set) and the name of the master
file."
  :type '(repeat (choice string (repeat string))))

(defun tex-continuous--compilation-command ()
  "Return the command used to compile the current LaTeX document."
  (let ((quote
         (if (memq system-type '(ms-dos windows-nt))
             "\""
           "'")))
    (concat
     (mapconcat (lambda (item)
                  (if (listp item)
                      (concat quote (mapconcat #'identity item) quote)
                    item))
                tex-continuous-command)
     (when TeX-output-dir
       (concat " -outdir=" (shell-quote-argument TeX-output-dir)))
     " "
     (shell-quote-argument (TeX-master-file "tex")))))

(defun tex-continuous--get-help (message)
  "Return the AUCTeX help string for MESSAGE."
  (let ((error-alist
         (append TeX-error-description-list
                 TeX-error-description-list-local)))
    (catch 'found
      (dolist (error error-alist)
        (when (string-match (car error) message)
          (throw 'found (cdr error)))))))

(defun tex-continuous-help-at-point ()
  "Display the AUCTeX help for the error at point."
  (interactive)
  (message "%s" (tex-continuous--get-help (help-at-pt-kbd-string))))

(defun tex-continuous--buffer-file-name ()
  (or buffer-file-name (buffer-file-name (buffer-base-buffer))))

(defun tex-continuous-process-item (type file line message offset _context
                                         search-string _line-end bad-box
                                         _error-point ignore)
  "Process an error or warning for the current TeX document.
The arguments are as in `TeX-error-list'.  Return either nil or a
triple (ERROR-P DESCRIPTION (BEG . END)), where ERROR-P is non-nil if it
is an error rather than a warning."
  (or
   (and
    (not ignore)
    (stringp file)
    (or (not bad-box) TeX-debug-bad-boxes)
    (when-let
        ((region
          (save-restriction
            (widen)
            (cond
             ((file-equal-p file (tex-continuous--buffer-file-name))
              (when line
                (if (eq type 'error)
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (+ line offset -1))
                      (unless (string= search-string " ")
                        (search-forward search-string nil t)
                        (cons (point) (1+ (point)))))
                  (flymake-diag-region (current-buffer) (+ line offset)))))
             ((file-equal-p file (TeX-master-output-file "aux"))
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
            region)))
   ;; Put errors without file or line at bottom of buffer.
   (when (eq type 'error)
     (list t
           (replace-regexp-in-string "\n" "" message)
           (cons (1- (point-max)) (point-max))))))

(defun tex-continuous--format-log-buffer ()
  "Format the current log buffer by joining lines suitably.
Adapted from `TeX-format-filter'"
  (goto-char (point-max))
  (while (> (point) (point-min))
    (end-of-line 0)
    (when (and (memq (- (point) (line-beginning-position)) '(79 80))
               (not (memq (char-after (1+ (point))) '(?\n ?\()))
               (not (and (eq (char-before) ?.)
                         (char-after (1+ (point)))
                         (not (eq ?w (char-syntax (char-after (1+ (point)))))))))
      (delete-char 1))))

(defun tex-continuous--error-list (log-file)
  "Retrieve parsed TeX error list from LOG-FILE."
  (with-temp-buffer
    (insert-file-contents log-file)
    (tex-continuous--format-log-buffer)
    (TeX-parse-all-errors)
    TeX-error-list))

(defun tex-continuous-process-log ()
  "Process log file for current LaTeX document.
Return a list of triples as in the docstring of
`tex-continuous-process-item'."
  (delq nil
        (mapcar (lambda (item)
                  (apply #'tex-continuous-process-item item))
                (tex-continuous--error-list (TeX-master-output-file "log")))))

(defun tex-continuous--compilation-buffer-name ()
  "Return the name of the buffer used for LaTeX compilation."
  (let ((master (abbreviate-file-name (expand-file-name (TeX-master-file)))))
    (format "*pvc-%s*" master)))

(defvar-local tex-continuous--compilation-buffer nil
  "The buffer used for LaTeX compilation.")

(defconst tex-continuous--watching-str
  "=== Watching for updated files. Use ctrl/C to stop ..."
  "String indicating that latexmk is watching for updated files.")

(defvar-local tex-continuous--last-update-time nil
  "Time of the last update in the compilation buffer.")

(defun tex-continuous--update-time (_beg _end _len)
  "Update the time of the last update in the compilation buffer."
  (setq tex-continuous--last-update-time (current-time)))

(defconst tex-continuous--wait-time 1
  "Time to wait before checking for changes in the log file.")

(defun tex-continuous--fresh-p ()
  "Return non-nil if logged errors should apply to current buffer.
This is the case if the current buffer is not modified, the current
buffer is a file, the current buffer has a log file, the log file is
newer than the current buffer, and the current latexmk compilation is
either in a watching state or has not updated recently."
  (when-let* ((file (tex-continuous--buffer-file-name))
              (log-file (TeX-master-output-file "log")))
    (and
     (when-let ((buf tex-continuous--compilation-buffer))
       (with-current-buffer buf
         (or
          (progn
            (goto-char (point-max))
            (forward-line -1)
            (equal (buffer-substring (point) (line-end-position))
                   tex-continuous--watching-str))
          (and (or
                tex-continuous--last-update-time
                (time-less-p (time-subtract (current-time)
                                            tex-continuous--last-update-time)
                             (seconds-to-time tex-continuous--wait-time)))))))
     (not (buffer-modified-p))
     (file-exists-p file)
     (file-exists-p log-file)
     (time-less-p (nth 5 (file-attributes file))
                  (nth 5 (file-attributes log-file))))))

(defvar tex-continuous-mode)

(defvar-local tex-continuous--report-fn nil
  "Function provided by Flymake for reporting diagnostics.")

(defun tex-continuous--clone-indirect-buffer-hook ()
  "Set `tex-continuous--report-fn' to nil after cloning an indirect buffer."
  (setq tex-continuous--report-fn nil))

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

(defun tex-continuous--unsubscribe (&optional nokill)
  "Unsubscribe from LaTeX compilation if the current buffer is in the list.
This kills the compilation buffer when its subscriber list becomes
empty, except when NOKILL is non-nil."
  (let ((buf (current-buffer))
        (comp-buf tex-continuous--compilation-buffer)
        done)
    (when (and comp-buf (buffer-live-p comp-buf))
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
          (unless nokill
            (kill-buffer comp-buf)))))))

(defvar-local tex-continuous--disable-function nil
  "Function to disable `tex-continuous' features.
This will be either `tex-continuous-mode-disable' or
`tex-continuous-turn-off'.")

(defun tex-continuous--disable ()
  "Disable `tex-continuous' features."
  (when tex-continuous--disable-function
    (funcall tex-continuous--disable-function)))

(defun tex-continuous--cancel-subscriptions ()
  "Cancel all subscriptions to LaTeX compilation.
This is called from the compilation buffer when it is killed."
  (dolist (buf tex-continuous--subscribed-buffers)
    (with-current-buffer buf
      (tex-continuous--disable))))

(defun tex-continuous-mode-disable ()
  "Disable `tex-continuous-mode' in all buffers."
  (tex-continuous-mode 0))

;;;###autoload
(define-minor-mode tex-continuous-mode
  "If enabled, run latexmk on the current tex file."
  :lighter nil
  (cond
   (tex-continuous-mode
    (let ((buf (current-buffer))
          (comp-buf-name (tex-continuous--compilation-buffer-name)))
      (if-let ((comp-buf (setq tex-continuous--compilation-buffer
                               (get-buffer comp-buf-name))))
          (with-current-buffer comp-buf
            (push buf tex-continuous--subscribed-buffers))
        (unless (start-process-shell-command
                 "tex-continuous"
                 comp-buf-name
                 (tex-continuous--compilation-command))
          (error "Failed to start LaTeX compilation"))
        (with-current-buffer (setq tex-continuous--compilation-buffer
                                   (get-buffer comp-buf-name))
          (special-mode)
          (add-hook 'after-change-functions #'tex-continuous--update-time nil t)
          (add-hook 'kill-buffer-hook #'tex-continuous--cancel-subscriptions nil t)
          (push buf tex-continuous--subscribed-buffers))))
    (add-hook 'kill-buffer-hook 'tex-continuous--unsubscribe nil t)
    (setq tex-continuous--disable-function 'tex-continuous-mode-disable)
    (add-hook 'after-set-visited-file-name-hook 'tex-continuous--disable nil t)
    (when tex-continuous--timer
      (cancel-timer tex-continuous--timer)
      (setq tex-continuous--timer nil))
    (setq tex-continuous--timer
          (run-with-timer 2 1 #'tex-continuous--timer-function)))
   (t
    (tex-continuous--unsubscribe)
    (remove-hook 'kill-buffer-hook 'tex-continuous--unsubscribe t)
    (remove-hook 'after-set-visited-file-name-hook 'tex-continuous--disable t)
    (when tex-continuous--report-fn
      (setq tex-continuous--report-fn nil)))))

(defvar-local tex-continuous--saved-flymake-diagnostic-functions nil
  "Saved value of `flymake-diagnostic-functions'.
Saved and restored by `tex-continuous-toggle'.")

(defun tex-continuous-turn-on ()
  "Enable `tex-continuous-mode' and `flymake-mode'.
Also set `flymake-diagnostic-functions' to `tex-continuous-flymake'."
  (interactive)
  (tex-continuous-mode 1)
  (setq tex-continuous--saved-flymake-diagnostic-functions
        flymake-diagnostic-functions)
  (setq-local flymake-diagnostic-functions '(tex-continuous-flymake))
  (flymake-mode 1)
  (setq tex-continuous--disable-function 'tex-continuous-turn-off)
  (add-hook 'clone-indirect-buffer-hook #'tex-continuous--clone-indirect-buffer-hook nil t)
  (message "tex-continuous-mode and flymake-mode enabled"))

(defun tex-continuous-turn-off ()
  "Disable `tex-continuous-mode' and `flymake-mode'.
Also restore `flymake-diagnostic-functions'."
  (interactive)
  (tex-continuous-mode 0)
  (flymake-mode 0)
  (remove-hook 'clone-indirect-buffer-hook #'tex-continuous--clone-indirect-buffer-hook t)
  (setq-local flymake-diagnostic-functions
              tex-continuous--saved-flymake-diagnostic-functions)
  (message "tex-continuous-mode and flymake-mode disabled"))

;;;###autoload
(defun tex-continuous-toggle ()
  "Toggle `tex-continuous-mode', and also `flymake-mode'."
  (interactive)
  (cond (tex-continuous-mode
         (tex-continuous-turn-off))
        (t
         (tex-continuous-turn-on))))

(provide 'tex-continuous)
;;; tex-continuous.el ends here
