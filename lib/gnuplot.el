;;; gnuplot.el --- run Gnuplot interactively in a Emacs buffer
;; Time-stamp: <1998-08-24 16:53:52 ecl>

(require 'comint)

;;; User variables
(defvar gnuplot-program "gnuplot"
  "*Name of the executable used to run Gnuplot.")

(defvar gnuplot-switches nil
  "*List of strings containing options to pass to `gnuplot-program'.")

(defvar gnuplot-prompt-regexp "^\\(gnuplot\\|multiplot\\)?> "
  "*Regular expression used to match the Gnuplot prompt.")

(defvar gnuplot-buffer "*Gnuplot*"
  "*Name of the Gnuplot buffer.")

(defvar gnuplot-mode-hook nil
  "*Hook run when `gnuplot-mode' is invoked.")

;;; Internal variables
(defvar gnuplot-mode-map nil
  "Keymap used in `gnuplot-mode.'")
(unless gnuplot-mode-map
  (setq gnuplot-mode-map comint-mode-map))

;;; Functions

(defun gnuplot-mode ()
  "Major mode for running Gnuplot interactively in a Emacs buffer."
  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (use-local-map gnuplot-mode-map)
  (setq comint-prompt-regexp gnuplot-prompt-regexp)
  (setq comint-process-echoes t)
  (setq major-mode 'gnuplot-mode)
  (setq mode-name "Gnuplot")
  (run-hooks 'gnuplot-mode-hook))
(put 'gnuplot-mode 'mode-class 'special)

(defun gnuplot-start-process (buffer)
  "Start a Gnuplot process in buffer BUFFER.
Return the process object."
  (apply 'start-process "gnuplot" buffer gnuplot-program gnuplot-switches))

;;;###autoload
(defun gnuplot ()
  "Run Gnuplot interactively.
If a Gnuplot process is already running in buffer `gnuplot-buffer',
just switch to it.
The name of the executable used to run Gnuplot is determined by the
variable `gnuplot-program'. Extra arguments to `gnuplot-program' can
be passed using the variable `gnuplot-switches'."
  (interactive)
  (unless (comint-check-proc gnuplot-buffer)
    (gnuplot-start-process gnuplot-buffer)
    (with-current-buffer gnuplot-buffer
      (gnuplot-mode)))
  (pop-to-buffer gnuplot-buffer))

(provide 'gnuplot)

;;; gnuplot.el ends here
