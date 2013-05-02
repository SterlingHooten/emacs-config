;;; gack.el --- grepping with Ack

;; Copyright (C) 2012  Emilio C. Lopes

;; Author: Emilio C. Lopes <Emilio.Lopes@partner.bmw.de>
;; Keywords: tools, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple wrapper to run `ack' (http://betterthangrep.com) using `grep'.

;;; Code:

(defvar ack-history nil
  "History for the `ack' command.")

(defun ack (command-args)
  (interactive
   (let ((sap (thing-at-point 'symbol))
         (ack-command "ack --nogroup --with-filename --all "))
     (list (read-shell-command "Run ack (like this): "
                               (if sap (concat ack-command sap) ack-command)
                               'ack-history))))
  (let ((/dev/null (if (and (fboundp 'w32-shell-dos-semantics)
                            (w32-shell-dos-semantics))
                       null-device
                     "/dev/null")))
    (compilation-start (concat "< " /dev/null " " command-args) 'grep-mode)))

(provide 'gack)
;;; gack.el ends here
