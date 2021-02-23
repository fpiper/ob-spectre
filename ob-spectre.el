;;; ob-spectre.el --- Babel Functions for spectre

;; Author: Ferdinand Pieper <mail@pie.tf>
;; Keywords: literate programming, reproducible research

;; Copyright (c) 2020 Ferdinand Pieper

;; License: GPL v3, or any later version
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating spectre simulations.

;;; Requirements:

;; - spectre :: https://www.cadence.com

;;; Code:

(require 'ob)

(define-derived-mode spectre-mode spice-mode "Spectre")

(defvar org-babel-spectre-command "spectre"
  "Name of the command to execute spectre.")

(defvar org-babel-spectre-command-line-args " -format=nutascii "
  "Default command line arguments for spectre.

Surround the actual arguments with spaces.")

(defvar org-babel-spectre-nutascii2csv-command "sed -r 's/^([a-Z])/# \\1/' | sed -r 's/^(# Variables:\\t(0\\t)|(\\t)\\t[0-9]+\\t)([a-Z._:0-9]*).*/\\2\\3\\4/' | tr '\\n' '\\r' | sed -e 's/\\r\\t/\t/g' | tr '\\r' '\\n' | sed '/^#/d' | sed 's/^\s//' "
  "Shell command to convert a nutascii file to csv.")

(defvar org-babel-spectre-license-env-vars "CDS_LIC_DIR=55002@license1.ims-as.uni-hannover.de:55004@license1.ims-as.uni-hannover.de CDS_LIC_DIR_modshare=55004@license1.ims-as.uni-hannover.de:1:55002@license1.ims-as.uni-hannover.de:1 LM_LICENSE_FILE=55002@license1.ims-as.uni-hannover.de:55004@license1.ims-as.uni-hannover.de LM_LICENSE_FILE_modshare=55004@license1.ims-as.uni-hannover.de:1:55002@license1.ims-as.uni-hannover.de:1"
  "Environment variables to set for spectre license information.")

(defun org-babel-variable-assignments:spectre (params)
  "Return list of spectre statements assigning the block's variables."
  (let ((vars (org-babel--get-vars params)))
    (when vars
      (list
       (concat
        "parameters "
        (mapconcat
         (lambda (pair)
           (format "%s=%s"
	           (car pair)
	           (cdr pair)))
         vars
         " "))))))

(defun org-babel-execute:spectre (body params)
  "Execute a block of Spectre code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((body (org-babel-expand-body:generic
                body params (org-babel-variable-assignments:spectre params)))
         (csv (assq :convert params))
         (out-file (cdr (assq :file params)))
         (circuit-file (org-babel-temp-file "spectre-" ".scs"))
         (circuit-file-local-sans (file-name-sans-extension (file-name-nondirectory (file-local-name circuit-file))))
         (raw  (or (and (not csv) out-file)
                   (concat
                    circuit-file-local-sans
                    ".out")))
         (result))
    (with-temp-file circuit-file (insert body))
    (let ((ret (org-babel-spectre-evaluate circuit-file raw))
          (result (if csv
                      (org-babel-spectre-nutascii2csv raw out-file)
                    raw)))
      (when (not (equal "no" (cdr (assq :cleanup params))))
        (org-babel-spectre-cleanup raw circuit-file-local-sans csv))
      result)))

(defun org-babel-spectre-evaluate (file raw)
  "Call spectre to evaluate FILE and save raw results to RAW."
  (org-babel-eval (concat
                   org-babel-spectre-license-env-vars
                   " "
                   org-babel-spectre-command
                   org-babel-spectre-command-line-args
                   " -outdir "
                   (file-local-name default-directory)
                   " -raw "
                   raw
                   " "
                   (file-local-name file)) ""))

(defun org-babel-spectre-cleanup (raw circuit-file-sans csv)
  "Remove unused result files."
  (when csv (delete-file raw))
  (delete-file (concat circuit-file-sans ".log"))
  (delete-directory (concat raw ".psf") t)
  ;; (delete-file "spectre.dc")
  )

(defun org-babel-spectre-nutascii2csv (raw &optional out)
  "Convert a nutascii formatted file to csv file and save result
in OUT."
  (let ((out-file (or out raw)))
    (with-temp-buffer
      (insert-file-contents raw)
      (shell-command-on-region (point-min) (point-max) org-babel-spectre-nutascii2csv-command (current-buffer))
      (buffer-string))
    ;; (save-excursion
    ;;   (switch-to-buffer out-file)
    ;;   (write-file out-file)
    ;;   (kill-buffer out-file))
    ))

(provide 'ob-spectre)
;;; ob-spectre.el ends here
