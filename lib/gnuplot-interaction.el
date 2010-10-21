;;; gnuplot-interaction.el --- major mode for editing Gnuplot programs
;; Time-stamp: <1998-09-02 15:01:47 ecl>

;; Copyright (C) 1998 Emilio C. Lopes.

;; Author: Emilio Lopes <Emilio.Lopes@Physik.TU-Muenchen.DE>
;; Created: 20 Aug 1998
;; Version: 1.0
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; If you have not received a copy of the GNU General Public License
;; along with this software, it can be obtained from the GNU Project's
;; World Wide Web server (http://www.gnu.org/copyleft/gpl.html), from
;; its FTP server (ftp://ftp.gnu.org/pub/gnu/GPL), by sending an eletronic
;; mail to this program's maintainer or by writting to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:

;; General
;; =======
;;
;; This library provides support for editing Gnuplot input files,
;; including interaction with an underlying Gnuplot subprocess.
;;
;; Please send bug reports, suggestions, improvements, etc. to the
;; author of this library (see e-mail address above), so that they
;; can be incorporated in future versions.
;;
;;
;; Installation
;; ============
;;
;; Put this file ("gnuplot-interaction.el") in a directory listed in
;; your `load-path' and byte-compile it.
;;
;; Add the following to your "~/.emacs":
;;
;;      (autoload 'gnuplot-interaction-mode "gnuplot-interaction"
;;        "Major mode for editing Gnuplot input files." t nil)
;;
;;      (add-to-list 'auto-mode-alist
;;                   '("\\.gpt?\\'" . gnuplot-interaction-mode))
;;
;; The last two lines make `gnuplot-interaction-mode' the default
;; major-mode for files whose name end with either ".gpt" or ".gp".
;; 
;; See the GNU Emacs on-line documentation for details.
;;
;; Note that this library requires "gnuplot.el".

;;; Code:

(require 'gnuplot)

;; User variables
(defvar gnuplot-interaction-mode-hook nil
  "*Hook run when `gnuplot-interaction-mode' is invoked.")

(defvar gnuplot-interaction-auto-reset t
  "*If non-nil, automatically reset Gnuplot before sending buffer contents.")

;; Internal variables
(defvar gnuplot-interaction-gnuplot-buffer nil
  "Name of the buffer where the Gnuplot process is running.")
(make-variable-buffer-local 'gnuplot-interaction-gnuplot-buffer)

(defvar gnuplot-interaction-font-lock-keywords
  '(("\\(^\\|;+\\)[ \t]*\\(set\\|show\\|cd\\|pwd\\|pause\\|load\\|save\\|s?plot\\)" 2 font-lock-keyword-face)
    ("#\\(.*\\)" . font-lock-comment-face))
  "Default expressions to highlight in gnuplot-interaction mode.")

(defvar gnuplot-interaction-mode-map nil
  "Keymap used in `gnuplot-interaction-mode'.")
(unless gnuplot-interaction-mode-map
  (setq gnuplot-interaction-mode-map (make-sparse-keymap))
  (mapcar (lambda (alist)
            (define-key gnuplot-interaction-mode-map (car alist) (cdr alist)))
          '(("\C-c\C-b" . gnuplot-interaction-send-buffer)
            ("\C-c\C-p" . gnuplot-interaction-send-up-to-point)
            ("\C-c\C-r" . gnuplot-interaction-send-region)
            ("\C-c\C-j" . gnuplot-interaction-send-line)
            ("\C-c\C-d" . gnuplot-interaction-reset)
            ("\C-c\C-q" . gnuplot-interaction-quit-process)
            ("\C-c\C-k" . gnuplot-interaction-kill-process)
            ("\C-c\C-l" . gnuplot-interaction-display-gnuplot-buffer))))

;; Functions
(defun gnuplot-interaction-mode ()
  "Major mode for editing Gnuplot input files.
Commands:
\\{gnuplot-interaction-mode-map}"
  (interactive)
  (kill-all-local-variables)            ; also runs `change-major-mode-hook'
  (setq comment-start "#")
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#[ \t]*")
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(gnuplot-interaction-font-lock-keywords nil t))
  (make-local-hook 'change-major-mode-hook)
  (add-hook 'change-major-mode-hook 'gnuplot-interaction-kill-gnuplot-buffer t t)
  (make-local-hook 'kill-buffer-hook)
  (add-hook 'kill-buffer-hook 'gnuplot-interaction-kill-gnuplot-buffer t t)
  (use-local-map gnuplot-interaction-mode-map)
  (setq mode-name "Gnuplot Interaction")
  (setq major-mode 'gnuplot-interaction-mode)
  (setq gnuplot-interaction-gnuplot-buffer
        (generate-new-buffer-name (format "*Gnuplot %s*" (buffer-name))))
  (gnuplot-start-process gnuplot-interaction-gnuplot-buffer)
  (with-current-buffer gnuplot-interaction-gnuplot-buffer
    (gnuplot-mode))
  (run-hooks 'gnuplot-interaction-mode-hook))

(defun gnuplot-interaction-send-line (arg)
  "Send current line to the Gnuplot process.
With a numeric prefix argument also send the next ARG-1 lines (or the
previous -ARG lines if ARG is negative)."
  (interactive "p")
  (gnuplot-interaction-send-region
   (save-excursion
     (if (> arg 0)
         (beginning-of-line)
       (end-of-line))
     (point))
   (save-excursion (forward-visible-line arg) (point))))

(defun gnuplot-interaction-send-string (string)
  "Send STRING to the Gnuplot process."
  (interactive)
  (process-send-string gnuplot-interaction-gnuplot-buffer string))

(defun gnuplot-interaction-send-region (beg end &optional arg)
  "Send region to the Gnuplot process.
With prefix ARG, or if `gnuplot-interaction-auto-reset' is non-nil, send
a `reset' first."
  (interactive "r\nP")
  (when (or arg gnuplot-interaction-auto-reset) (gnuplot-interaction-reset))
  (process-send-region gnuplot-interaction-gnuplot-buffer beg end))

(defun gnuplot-interaction-send-up-to-point (here &optional arg)
  "Send the region from buffer begin up to the point to the Gnuplot process.
With prefix ARG, or if `gnuplot-interaction-auto-reset' is non-nil, send
a `reset' first."
  (interactive "d\nP")
  (when (or arg gnuplot-interaction-auto-reset) (gnuplot-interaction-reset))
  (gnuplot-interaction-send-region (point-min) here))

(defun gnuplot-interaction-send-buffer (arg)
  "Send buffer to the Gnuplot process.
With prefix ARG, or if `gnuplot-interaction-auto-reset' is non-nil, send
a `reset' first."
  (interactive "P")
  (when (or arg gnuplot-interaction-auto-reset) (gnuplot-interaction-reset))
  (gnuplot-interaction-send-region (point-min) (point-max)))

(defun gnuplot-interaction-reset ()
  "Send a `reset' to the Gnuplot process."
  (interactive)
  (gnuplot-interaction-send-string "reset\n"))

(defun gnuplot-interaction-quit-process ()
  "Send the QUIT signal to the Gnuplot process."
  (interactive)
  (quit-process gnuplot-interaction-gnuplot-buffer))

(defun gnuplot-interaction-kill-process ()
  "Send the KILL signal to the Gnuplot process."
  (interactive)
  (kill-process gnuplot-interaction-gnuplot-buffer))

(defun gnuplot-interaction-display-gnuplot-buffer ()
  "Make the Gnuplot buffer visible in a window."
  (interactive)
  (display-buffer gnuplot-interaction-gnuplot-buffer))

(defun gnuplot-interaction-kill-gnuplot-buffer ()
  "Send the QUIT signal to the Gnuplot process and kill its buffer."
  (when (get-buffer-process gnuplot-interaction-gnuplot-buffer)
    (gnuplot-interaction-quit-process))
  (delete-windows-on gnuplot-interaction-gnuplot-buffer t)
  (kill-buffer gnuplot-interaction-gnuplot-buffer))

(provide 'gnuplot-interaction)

;;; gnuplot-interaction.el ends here
