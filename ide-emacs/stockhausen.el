;;;
;;; Author:
;;;   Leif Kornstaedt <kornstae@ps.uni-sb.de>
;;;
;;; Copyright:
;;;   Leif Kornstaedt, 2001
;;;
;;; Last change:
;;;   $Date$ by $Author$
;;;   $Revision$
;;;

;;
;; TODO:
;;
;; -- Attach a filter to the stot process to parse error messages.
;; -- Implement a compilation-parse-errors-function.
;; -- Do not make global key bindings.
;;

(require 'comint)
(require 'sml-mode)

;; Customization

(eval-and-compile
  (condition-case ()
      (require 'custom)
    (error nil))
  (if (and (featurep 'custom)
	   (fboundp 'custom-declare-variable)
	   (fboundp 'custom-declare-group))
      nil
    (defmacro defgroup (&rest args)
      nil)
    (defmacro defcustom (symbol value doc &rest args)
      (` (defvar (, symbol) (, value) (, doc))))))

(eval-and-compile
  (eval '(defgroup stockhausen nil
	   "Stockhausen Interactive Toplevel."
	   :group 'languages
	   :prefix "stockhausen-")))

(eval-and-compile
  (eval '(defcustom stockhausen-stot-command "stot"
	   "*Command used to start the Stockhausen interactive toplevel."
	   :type 'string
	   :group 'stockhausen)))
(put 'stockhausen-stot-command 'variable-interactive
     "sCommand used to start the Stockhausen interactive toplevel: ")

(eval-and-compile
  (eval '(defcustom stockhausen-other-buffer-size 35
	   "*Percentage of screen to use for Stockhausen window."
	   :type 'integer
	   :group 'stockhausen)))
(put 'stockhausen-other-buffer-size 'variable-interactive
     "nPercentage of screen to use for Stockhausen window: ")

(eval-and-compile
  (defvar stockhausen-stot-buffer "*Stockhausen Interactive Toplevel*"
    "Name of the Stockhausen interactive toplevel buffer."))

;; Running the Interactive Toplevel

;--** (defun stockhausen-stot-filter (proc string) ...)

;--** (defun stockhausen-compilation-parse-errors ...)

(defun stockhausen-show-buffer (buffer)
  (if (and buffer (not (get-buffer-window buffer)))
      (let ((win (or (get-buffer-window stockhausen-stot-buffer)
		     (split-window (get-largest-window)
				   (/ (* (window-height (get-largest-window))
					 (- 100 stockhausen-other-buffer-size))
				      100)))))
	(set-window-buffer win buffer)
	(set-buffer buffer)
	(set-window-point win (point-max))
	(bury-buffer buffer))))

(defun run-stockhausen ()
  "Start the Stockhausen interactive toplevel."
  (interactive)
  (let ((buffer (get-buffer-create stockhausen-stot-buffer)))
    (comint-exec buffer stockhausen-stot-buffer
		 stockhausen-stot-command nil nil)
;;--** support compilation mode
;    (save-excursion
;      (set-buffer buffer)
;      (compilation-mode)
;      (set (make-local-variable 'compilation-parse-errors-function)
;	   'stockhausen-compilation-parse-errors))
    (let ((proc (get-buffer-process buffer)))
      (process-kill-without-query proc nil)
      (set-process-filter proc nil)) ;--** 'stockhausen-stot-filter
    (stockhausen-show-buffer buffer)))

(defun stockhausen-evaluate-string (string filename line)
  ;;--** escape dots at start of line
  (let ((proc (get-buffer-process stockhausen-stot-buffer)))
    (if (not proc)
	(error "Stockhausen is not running."))
    (comint-send-string proc (concat filename "\n"
				     (number-to-string line) "\n"
				     string "\n;\n"))))

(defun stockhausen-evaluate-region (start end)
  "Evaluate the current region in the Stockhausen interactive toplevel."
  (interactive "r")
  (stockhausen-evaluate-string (buffer-substring start end)
			       (or (buffer-file-name) (buffer-name))
			       (1+ (count-lines 1 start))))

(defun stockhausen-line-region (arg)
  ;; Return starting and ending positions of ARG lines surrounding point.
  ;; Positions are returned as a pair ( START . END ).
  (save-excursion
    (let (start end)
      (cond ((> arg 0)
	     (beginning-of-line)
	     (setq start (point))
	     (forward-line (1- arg))
	     (end-of-line)
	     (setq end (point)))
	    ((= arg 0)
	     (setq start (point))
	     (setq end (point)))
	    ((< arg 0)
	     (end-of-line)
	     (setq end (point))
	     (forward-line arg)
	     (setq start (point))))
      (cons start end))))

(defun stockhausen-evaluate-line (arg)
  "Evaluate the current line in the Stockhausen interactive toplevel.
With ARG, evaluate that many lines.  If ARG is negative, evaluate that many
preceding lines as well as the current line."
  (interactive "p")
  (let ((region (stockhausen-line-region arg)))
    (stockhausen-evaluate-region (car region) (cdr region))))

(defun stockhausen-paragraph-region (arg)
  ;; Return starting and ending positions of ARG paragraphs surrounding point.
  ;; Positions are returned as a pair ( START . END ).
  (save-excursion
    (let (start end)
      (cond ((> arg 0)
	     (backward-paragraph 1)
	     (setq start (point))
	     (forward-paragraph arg)
	     (setq end (point)))
	    ((= arg 0)
	     (setq start (point))
	     (setq end (point)))
	    ((< arg 0)
	     (forward-paragraph (1- arg))
	     (setq start (point))
	     (backward-paragraph (1- arg))
	     (setq end (point))))
      (cons start end))))

(defun stockhausen-evaluate-paragraph (arg)
  "Evaluate the current paragraph in the Stockhausen interactive toplevel.
If the point is exactly between two paragraphs, evaluate the preceding
paragraph.  With ARG, evaluate that many paragraphs.  If ARG is negative,
evaluate that many preceding paragraphs as well as the current paragraph."
  (interactive "p")
  (let ((region (stockhausen-paragraph-region arg)))
    (stockhausen-evaluate-region (car region) (cdr region))))

(defun stockhausen-evaluate-buffer ()
  "Evaluate the current buffer in the Stockhausen interactive toplevel."
  (interactive)
  (stockhausen-evaluate-region (point-min) (point-max)))

;; Key bindings

(define-key sml-mode-map (kbd "C-c r") 'run-stockhausen)
(define-key sml-mode-map (kbd "C-c C-r") 'stockhausen-evaluate-region)
(define-key sml-mode-map (kbd "C-c C-l") 'stockhausen-evaluate-line)
(define-key sml-mode-map (kbd "C-c C-p") 'stockhausen-evaluate-paragraph)
(define-key sml-mode-map (kbd "C-c C-b") 'stockhausen-evaluate-buffer)
