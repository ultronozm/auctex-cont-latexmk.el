;;; auctex-cont-latexmk.el --- run latexmk continuously, report errors via Flymake  -*- lexical-binding: t; -*-

;; Copyright (C) 2023, 2024  Free Software Foundation, Inc.

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.2
;; URL: https://github.com/ultronozm/auctex-cont-latexmk.el
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
;; Use `auctex-cont-latexmk-toggle' to toggle the minor mode and set
;; up Flymake.
;;
;; If you want to enable continuous compilation but prefer either not
;; to use the Flymake backend or to manage it yourself (e.g., in
;; combination with other Flymake backends), then instead use
;; `auctex-cont-latexmk-mode' and add #'auctex-cont-latexmk-flymake to
;; `flymake-diagnostic-functions' when you'd like.
;;
;; Customize the variable `auctex-cont-latexmk-command' to change the
;; compilation command.
;;
;; The compilation takes place in a buffer *pvc-filename*, so look
;; there if you need to see the output.

;;; Code:

(require 'tex)
(require 'flymake)

(defgroup auctex-cont-latexmk nil
  "Run latexmk continuously, report errors via Flymake."
  :group 'tex)

(defvar auctex-cont-latexmk-mode)

;;; Flymake Backend

(defcustom auctex-cont-latexmk-report-multiple-labels t
  "Non-nil means report multiple label errors via Flymake."
  :type 'boolean)

(defun auctex-cont-latexmk--get-help (message)
  "Return the AUCTeX help string for MESSAGE."
  (let ((error-alist
         (append TeX-error-description-list
                 TeX-error-description-list-local)))
    (alist-get message error-alist nil nil #'string-match-p)))

(defun auctex-cont-latexmk-help-at-point ()
  "Display the AUCTeX help for the error at point."
  (interactive)
  (message "%s" (auctex-cont-latexmk--get-help (help-at-pt-kbd-string))))

(defun auctex-cont-latexmk-process-item (type file line message offset _context
                                              search-string _line-end bad-box
                                              _error-point ignore)
  "Process an error or warning for the current TeX document.
The arguments are as in `TeX-error-list'.  Return either nil or a
triple (ERROR-P DESCRIPTION (BEG . END)), where ERROR-P is non-nil if it
is an error rather than a warning."
  (or
   (when-let*
       (((not ignore))
        ((stringp file))
        ((or (not bad-box) TeX-debug-bad-boxes))
        (region
         (save-restriction
           (widen)
           (cond
            ((file-equal-p
              file
              (or buffer-file-name (buffer-file-name (buffer-base-buffer))))
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
             (and auctex-cont-latexmk-report-multiple-labels
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
           region))
   ;; Put errors without file or line at bottom of buffer.
   (when (eq type 'error)
     (list t
           (replace-regexp-in-string "\n" "" message)
           (cons (1- (point-max)) (point-max))))))

(defun auctex-cont-latexmk--format-log-buffer ()
  "Format the current log buffer by joining lines suitably.
Adapted from `TeX-format-filter'."
  (goto-char (point-max))
  (while (not (bobp))
    (end-of-line 0)
    (when (and (memq (- (point) (line-beginning-position)) '(79 80))
               (not (memq (char-after (1+ (point))) '(?\n ?\()))
               (not (and (eq (char-before) ?.)
                         (char-after (1+ (point)))
                         (not (eq ?w (char-syntax
                                      (char-after (1+ (point)))))))))
      (delete-char 1))))

(defun auctex-cont-latexmk--error-list (log-file)
  "Retrieve parsed TeX error list from LOG-FILE."
  (with-temp-buffer
    (insert-file-contents log-file)
    (auctex-cont-latexmk--format-log-buffer)
    (TeX-parse-all-errors)
    TeX-error-list))

(defun auctex-cont-latexmk-process-log ()
  "Process log file for current LaTeX document.
Return a list of triples as in the docstring of
`auctex-cont-latexmk-process-item'."
  (delq nil
        (mapcar (lambda (item)
                  (apply #'auctex-cont-latexmk-process-item item))
                (auctex-cont-latexmk--error-list (TeX-master-output-file "log")))))

(defvar-local auctex-cont-latexmk--report-fn nil
  "Function provided by Flymake for reporting diagnostics.")

(defvar-local auctex-cont-latexmk-force-enable nil
  "Whether to enable the Flymake backend unconditionally.
This is non-nil if we should enable the Flymake backend independent of
whether `auctex-cont-latexmk-mode' is enabled.  This may be useful for
testing or applying the backend in other contexts, e.g., in the context
of AUCTeX's built-in compilation functions.")

(defun auctex-cont-latexmk-flymake (report-fn &rest _args)
  "Flymake backend for LaTeX based on latexmk.
Save REPORT-FN in a local variable, called by
`auctex-cont-latexmk--timer' to report diagnostics."
  ;; At present, we check for `auctex-cont-latexmk-mode' just to avoid
  ;; spamming random buffers with report functions.  Could easily
  ;; replace this with some other check if we wanted to use the
  ;; Flymake backend provided here in other situations.
  (when (or auctex-cont-latexmk-mode auctex-cont-latexmk-force-enable)
    (setq auctex-cont-latexmk--report-fn report-fn)))

(defun auctex-cont-latexmk-send-report ()
  "Report to the Flymake backend."
  (funcall
   auctex-cont-latexmk--report-fn
   (mapcar
    (lambda (datum)
      (cl-destructuring-bind (error-p description region) datum
        (flymake-make-diagnostic
         (current-buffer) (car region) (cdr region)
         (if error-p :error :warning)
         description)))
    (auctex-cont-latexmk-process-log))))

(defun auctex-cont-latexmk--clone-indirect-buffer-hook ()
  "Set `auctex-cont-latexmk--report-fn' to nil after cloning an indirect buffer.
This should be added to `clone-indirect-buffer-hook' for any buffer in
which the Flymake backend is used.  This is because we don't want the
Flymake report function to propagate to indirect buffers."
  (setq auctex-cont-latexmk--report-fn nil))

;;; Continuous Compilation

(defcustom auctex-cont-latexmk-command
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

(defun auctex-cont-latexmk--compilation-command ()
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
                auctex-cont-latexmk-command)
     (when TeX-output-dir
       (concat " -outdir=" (shell-quote-argument TeX-output-dir)))
     " "
     (shell-quote-argument (TeX-master-file "tex")))))

(defun auctex-cont-latexmk--compilation-buffer-name ()
  "Return the name of the buffer used for LaTeX compilation."
  (let ((master (abbreviate-file-name (expand-file-name (TeX-master-file)))))
    (format "*pvc-%s*" master)))

(defvar-local auctex-cont-latexmk--compilation-buffer nil
  "The buffer used for LaTeX compilation.")

(defconst auctex-cont-latexmk--watching-str
  "=== Watching for updated files. Use ctrl/C to stop ..."
  "String indicating that latexmk is watching for updated files.")

(defvar-local auctex-cont-latexmk--last-update-time nil
  "Time of the last update in the compilation buffer.")

(defun auctex-cont-latexmk--update-time (_beg _end _len)
  "Update the time of the last update in the compilation buffer."
  (setq auctex-cont-latexmk--last-update-time (current-time)))

(defconst auctex-cont-latexmk--wait-time 1
  "Time to wait before checking for changes in the log file.")

(defun auctex-cont-latexmk--fresh-p ()
  "Return non-nil if logged errors should apply to current buffer.
This is the case if the current buffer is not modified, the current
buffer is a file, the current buffer has a log file, the log file is
newer than the current buffer, and the current latexmk compilation is
either in a watching state or has not updated recently."
  (when-let* ((file
               (or buffer-file-name (buffer-file-name (buffer-base-buffer))))
              (log-file (TeX-master-output-file "log")))
    (and
     (when-let ((buf auctex-cont-latexmk--compilation-buffer))
       (with-current-buffer buf
         (or
          (progn
            (goto-char (point-max))
            (forward-line -1)
            (equal (buffer-substring (point) (line-end-position))
                   auctex-cont-latexmk--watching-str))
          (and (or
                auctex-cont-latexmk--last-update-time
                (time-less-p (time-subtract (current-time)
                                            auctex-cont-latexmk--last-update-time)
                             (seconds-to-time auctex-cont-latexmk--wait-time)))))))
     (not (buffer-modified-p))
     (file-exists-p file)
     (file-exists-p log-file)
     (time-less-p (nth 5 (file-attributes file))
                  (nth 5 (file-attributes log-file))))))

(defvar auctex-cont-latexmk--timer nil
  "Timer for reporting changes to the log file.")

(defun auctex-cont-latexmk--timer-function ()
  "Report to the Flymake backend if the current buffer is fresh."
  (and auctex-cont-latexmk-mode
       auctex-cont-latexmk--report-fn
       (auctex-cont-latexmk--fresh-p)
       (auctex-cont-latexmk-send-report)))

(defvar-local auctex-cont-latexmk--subscribed-buffers nil
  "List of buffers subscribed to the current LaTeX compilation.")

(defun auctex-cont-latexmk--unsubscribe (&optional nokill)
  "Unsubscribe from LaTeX compilation if the current buffer is in the list.
This kills the compilation buffer when its subscriber list becomes
empty, except when NOKILL is non-nil."
  (let ((buf (current-buffer))
        (comp-buf auctex-cont-latexmk--compilation-buffer)
        done)
    (when (and comp-buf (buffer-live-p comp-buf))
      (with-current-buffer comp-buf
        (setq auctex-cont-latexmk--subscribed-buffers
              (seq-remove (lambda (b) (eq b buf))
                          auctex-cont-latexmk--subscribed-buffers))
        (when (null auctex-cont-latexmk--subscribed-buffers)
          (setq done t)))
      (when done
        (let ((process (get-buffer-process comp-buf)))
          (when (process-live-p process)
            (interrupt-process process)
            (sit-for 0.1)
            (delete-process process))
          (unless nokill
            (kill-buffer comp-buf)))))))

(defvar-local auctex-cont-latexmk--disable-function nil
  "Function to disable `auctex-cont-latexmk' features.
This will be either `auctex-cont-latexmk-mode-disable' or
`auctex-cont-latexmk-turn-off'.")

(defun auctex-cont-latexmk--disable ()
  "Disable `auctex-cont-latexmk' features."
  (when auctex-cont-latexmk--disable-function
    (funcall auctex-cont-latexmk--disable-function)))

(defun auctex-cont-latexmk--cancel-subscriptions ()
  "Cancel all subscriptions to LaTeX compilation.
This is called from the compilation buffer when it is killed."
  (dolist (buf auctex-cont-latexmk--subscribed-buffers)
    (with-current-buffer buf
      (auctex-cont-latexmk--disable))))

(defun auctex-cont-latexmk-mode-disable ()
  "Disable `auctex-cont-latexmk-mode' in all buffers."
  (auctex-cont-latexmk-mode 0))

;;;###autoload
(define-minor-mode auctex-cont-latexmk-mode
  "If enabled, run latexmk on the current tex file."
  :lighter nil
  (cond
   (auctex-cont-latexmk-mode
    (let ((buf (current-buffer))
          (comp-buf-name (auctex-cont-latexmk--compilation-buffer-name)))
      (if-let ((comp-buf (setq auctex-cont-latexmk--compilation-buffer
                               (get-buffer comp-buf-name))))
          (with-current-buffer comp-buf
            (push buf auctex-cont-latexmk--subscribed-buffers))
        (unless (start-process-shell-command
                 "auctex-cont-latexmk"
                 comp-buf-name
                 (auctex-cont-latexmk--compilation-command))
          (error "Failed to start LaTeX compilation"))
        (with-current-buffer (setq auctex-cont-latexmk--compilation-buffer
                                   (get-buffer comp-buf-name))
          (special-mode)
          (add-hook 'after-change-functions #'auctex-cont-latexmk--update-time nil t)
          (add-hook 'kill-buffer-hook
                    #'auctex-cont-latexmk--cancel-subscriptions nil t)
          (push buf auctex-cont-latexmk--subscribed-buffers))))
    (add-hook 'kill-buffer-hook 'auctex-cont-latexmk--unsubscribe nil t)
    (setq auctex-cont-latexmk--disable-function 'auctex-cont-latexmk-mode-disable)
    (add-hook 'after-set-visited-file-name-hook 'auctex-cont-latexmk--disable nil t)
    (when auctex-cont-latexmk--timer
      (cancel-timer auctex-cont-latexmk--timer)
      (setq auctex-cont-latexmk--timer nil))
    (setq auctex-cont-latexmk--timer
          (run-with-timer 2 1 #'auctex-cont-latexmk--timer-function)))
   (t
    (auctex-cont-latexmk--unsubscribe)
    (remove-hook 'kill-buffer-hook 'auctex-cont-latexmk--unsubscribe t)
    (remove-hook 'after-set-visited-file-name-hook 'auctex-cont-latexmk--disable t)
    (when auctex-cont-latexmk--report-fn
      (setq auctex-cont-latexmk--report-fn nil)))))

(defvar-local auctex-cont-latexmk--saved-flymake-diagnostic-functions nil
  "Saved value of `flymake-diagnostic-functions'.
Saved and restored by `auctex-cont-latexmk-toggle'.")

(defvar-local auctex-cont-latexmk--saved-flymake-mode nil
  "Saved value of `flymake-mode'.
Saved and restored by `auctex-cont-latexmk-toggle'.")

(defcustom auctex-cont-latexmk-retained-flymake-backends
  '(eglot-flymake-backend)
  "Flymake backends to retain when enabling `auctex-cont-latexmk-mode'."
  :type 'boolean)

(defun auctex-cont-latexmk-turn-on ()
  "Enable `auctex-cont-latexmk-mode' and set up Flymake."
  (interactive)
  (auctex-cont-latexmk-mode 1)
  (setq auctex-cont-latexmk--saved-flymake-diagnostic-functions
        flymake-diagnostic-functions)
  (setq auctex-cont-latexmk--saved-flymake-mode (if flymake-mode 1 0))
  (setq-local flymake-diagnostic-functions
              (append
               '(auctex-cont-latexmk-flymake)
               (seq-intersection flymake-diagnostic-functions
                                 auctex-cont-latexmk-retained-flymake-backends)))
  (flymake-mode 1)
  (setq auctex-cont-latexmk--disable-function 'auctex-cont-latexmk-turn-off)
  (add-hook 'clone-indirect-buffer-hook
            #'auctex-cont-latexmk--clone-indirect-buffer-hook nil t)
  (message "auctex-cont-latexmk-mode enabled"))

(defun auctex-cont-latexmk-turn-off ()
  "Disable `auctex-cont-latexmk-mode' and restore flymake settings."
  (interactive)
  (auctex-cont-latexmk-mode 0)
  (flymake-mode auctex-cont-latexmk--saved-flymake-mode)
  (remove-hook 'clone-indirect-buffer-hook
               #'auctex-cont-latexmk--clone-indirect-buffer-hook t)
  (setq-local flymake-diagnostic-functions
              auctex-cont-latexmk--saved-flymake-diagnostic-functions)
  (message "auctex-cont-latexmk-mode disabled"))

;;;###autoload
(defun auctex-cont-latexmk-toggle ()
  "Toggle `auctex-cont-latexmk-mode' and its Flymake backend."
  (interactive)
  (cond (auctex-cont-latexmk-mode
         (auctex-cont-latexmk-turn-off))
        (t
         (auctex-cont-latexmk-turn-on))))

(provide 'auctex-cont-latexmk)
;;; auctex-cont-latexmk.el ends here
