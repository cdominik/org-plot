;;; org-plot.el --- support for plotting from org-mode

;; Author: Eric Schulte <schulte dot eric at gmail dot com>

;; This file is Not part of GNU Emacs.

;;; Comments:

;; this file is to seperate/preserve that which is working for me day
;; to day isolated from that which will become the core of org-plot

;;; Code:
(require 'org)

;;--------------------------------------------------------------------------------
;; gnuplot integration into org tables
(defun org-plot/gnuplot (&optional x-col)
  "Plot the current table using gnuplot.  Use a prefix argument
to specify a column to use for the x-coordinates, to use the row
number for the x-coordinates provide a prefix argument of 0."
  (interactive "p")
  (unless (org-at-table-p)
    (error "No table at point"))
  (require 'org-exp)
  (require 'gnuplot)
  (org-table-align) ;; make sure we have everything we need
  (let* ((beg (org-table-begin))
	 (end (org-table-end))
	 (cols (save-excursion
		 (goto-char end)
		 (backward-char 3)
		 (org-table-current-column)))
	 (data-beg (if (and 
			(goto-char beg)
			(re-search-forward org-table-dataline-regexp end t)
			(re-search-forward org-table-hline-regexp end t)
			(re-search-forward org-table-dataline-regexp end t))
		       (match-beginning 0)
		     beg))
	 (skip (- (line-number-at-pos data-beg) (line-number-at-pos beg)))
	 (exp-format (format "orgtbl-to-tsv :skip %d" skip))
	 (file (make-temp-file "org-plot")))
    ;; export table
    (org-table-export file exp-format)
    (with-temp-buffer
      ;; write script
      (insert (org-plot/gnuplot-script file x-col cols))
      ;; graph table
      (gnuplot-mode)
      (gnuplot-send-buffer-to-gnuplot)
      (bury-buffer (get-buffer "*gnuplot*")))
    (delete-file file)))

; write a gunplot script
(defun org-plot/gnuplot-script (file x-col num-cols)
  (let ((plot-str "'%s' using %s:%d with lines title '%d'")
	script)
    (dotimes (col (+ 1 num-cols))
      (unless (or (and x-col (equal col x-col)) (equal col 0))
	(setf script (cons (format plot-str file (or (and x-col (format "%d" x-col)) "") col col) script))))
    (concat "plot " (mapconcat 'identity (reverse script) "\\\n    ,"))))

;;; org-table integration
(defun org-plot/table-rows ()
  "Return the number of rows in the current table"
  (- (line-number-at-pos (org-table-end)) (line-number-at-pos (org-table-begin))))

(defun org-plot/table-cols ()
  "Return the number of columns in the current table"
  (save-excursion (goto-char (org-table-end))
		     (backward-char 1)
		     (org-table-previous-field)
		     (org-table-current-column)))

(defun org-plot/table-map-cells (function)
  "Call function on all cells in the current table.  Traverse by
column, top-down (like reading japanese) ."
  (let ((begin-line (line-number-at-pos (org-table-begin)))
	field-value)
    (save-excursion
      ;; for each column
      (dotimes (col (org-plot/table-cols))
	;; for each row
	(dotimes (row (org-plot/table-rows))
	  (goto-line (+ begin-line row))
	  ;; for each cell call function on the field value
	  (eval (list function (+ 1 col) row (org-table-get-field (+ 1 col)))))))))

;;; plot grids of data
(defun org-plot/rest ()
  (interactive)
  (let* ((beg (point))
	 (end (- (org-table-end) 2))
	 (rows (+ 1 (- (line-number-at-pos end) (line-number-at-pos beg))))
	 (file (make-temp-file "plot-grid")))
    (message
     (format "beg=%d end=%d rows=%d" (line-number-at-pos beg) (line-number-at-pos end) rows))
    (save-excursion (org-table-copy-region beg end))
    ;; write copied region to temp buffer
    (with-temp-buffer
      (insert "||") (goto-char 2)
      (org-mode) (org-return) (goto-char 2)
      (org-table-paste-rectangle)
      ;; save table to gnuplot format
      (org-plot/grid file))
    (with-temp-buffer
      ;; plot table
      (insert (org-plot/gnuplot-3d-script rows file))
      ;; graph table
      (gnuplot-mode) (gnuplot-send-buffer-to-gnuplot)
      (switch-to-buffer-other-window "*gnuplot*")
      (bury-buffer "*gnuplot*") (delete-window)
      (delete-file file))))

(defun org-plot/gnuplot-3d-script (num-rows file)
  (concat "set pm3d map"
	  (format "\nset yrange [0.5:%f]" (+ num-rows 0.5))
	  (format "\nset ytics (%s)" (mapconcat
				      (lambda (row) (format "%d" row))
				      (loop for n from 1 to num-rows collect n)
				      ","))
	  "\nsplot \"" file "\" with pm3d title \"\""))

(defun org-plot/grid (file)
  "This will plot a grid using gnuplot.  In order to keep gnuplot
from smoothing out most of our surface we have to specify all
four corners of the grid."
  (interactive)
  (get-buffer-create "*data*")
  (flet ((gnuplot-row (col row value)
		      (format "%f  %f  %f\n%f  %f  %f\n"
			      col (- row 0.5) value ;; lower edge
			      col (+ row 0.5) value))) ;; upper edge
    (let ((collector (get-buffer-create "*org-plot/grid temp buffer*"))
	  (last-row 0)
	  front-edge back-edge)
      (org-plot/table-map-cells
       (lambda (col row el)
	 (save-excursion
	   (switch-to-buffer collector)
	   (setf row (+ 1 row))
	   (when (> last-row row) ;; only insert once per whole col group
	     (insert back-edge) (insert "\n") ;; back edge
	     (insert front-edge) (insert "\n") ;; front edge
	     (setf back-edge "") (setf front-edge ""))
	   (setf back-edge
		 (concat back-edge
			 (gnuplot-row (- col 1) row (string-to-number el))))
	   (setf front-edge
		 (concat front-edge
			 (gnuplot-row col row (string-to-number el))))
	   (setf last-row row))))
      (switch-to-buffer collector)
      (write-file file)
      (kill-buffer collector))))

(provide 'org-plot)
;;; org-plot.el ends here