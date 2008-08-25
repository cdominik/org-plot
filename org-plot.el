;;; org-plot.el --- support for plotting from org-mode

;; Author: Eric Schulte <schulte dot eric at gmail dot com>

;; This file is Not part of GNU Emacs.

;;; Comments:

;;; Code:
(require 'org)
(require 'org-exp)
(require 'gnuplot)

(defun debug (el)
  (message (format "%S" el))
  el)

(defvar org-plot/gnuplot-default-options
  '((:plot-type . "2d")
    (:with . "lines")
    (:xcol . 1))
  "Gnuplot options accessible through `org-plot', common options
are specifically supported, while all other options are
accessible through specification of generic script lines, or
specification of custom script files.  Possible options are...

plot-type: ---- specify whether the plot will be '2d' '3d or
                'grid'

script: ------- if you want total controll you can specify a
                script file which will be used to plot, before
                plotting every instance of $datafile in the
                specified script will be replaced with the path
                to the generated data file.  Note even if you set
                this option you may still want to specify the
                plot type, as that can impact the content of the
                data file.

set: ---------- specify a gnuplot option to be set when graphing

title: -------- specify the title of the plot

xcol: --------- specify which column of the table to use as the x
                axis

ycols: !------- specify which columns of the table to plot on the
                y axis, default to all cols aside from the xcol

with: --------- specify a with option to be inserted for every
                col being plotted (e.g. lines, points, boxes,
                impulses, etc...) defaults to 'lines'

output-file: -- if you want to plot to a file specify the path to
                the desired output file

col-labels: --- list of labels to be used for the ycols (defaults
                to column headers if they exist)
")

;;--------------------------------------------------------------------------------
;; utility
(defun org-plot/goto-nearest-table ()
  "Move the point to the beginning of nearest table.  First look
back until hitting an empty line, then forward until a table is
found."
  (interactive)
  (move-beginning-of-line 1)
  (while (not (or (org-at-table-p) ;; backwards
		  (> 0 (forward-line -1))
		  (equal (length (thing-at-point 'line)) 1))))
  (while (not (or (org-at-table-p) ;; forwards
		  (< 0 (forward-line 1)))))
  (goto-char (org-table-begin)))

(defun org-plot/collect-options (&optional params)
  (interactive)
  (let (a (start 0) (line (thing-at-point 'line)))
    (save-match-data
      (when (string-match "#\\+PLOT: +\\([^\r\n]+\\)$" line)
	(setq a (match-string 1 line))
	(while (string-match
		":\\([^ \t\r\n]+\\) +\\([^:\r\n]+\\)"
		a start)
	  (setq key (match-string 1 a) value (match-string 2 a)
		start (match-end 0)
		params (plist-put params (intern key) value)))))
    params))

(defun org-plot/header-labels ()
  (interactive)
  (save-excursion
    (let ((beg (org-table-begin))
	  (end (org-table-end))
	  col-labels)
      (when (and (goto-char beg)
		 (re-search-forward org-table-dataline-regexp end t)
		 (re-search-forward org-table-hline-regexp end t)
		 (move-beginning-of-line 0))
	(dotimes (col (org-plot/table-cols))
	  (setf col-labels (cons (org-table-get-field (+ 1 col)) col-labels))))
      (reverse col-labels))))

(defun org-plot/gnuplot-to-2d-data (data-file)
  (save-excursion
    (let* ((beg (org-table-begin)) (end (org-table-end))
	   (data-beg (if (and (goto-char beg)
			      (re-search-forward org-table-dataline-regexp end t)
			      (re-search-forward org-table-hline-regexp end t)
			      (re-search-forward org-table-dataline-regexp end t))
			 (match-beginning 0) beg))
	   (skip (- (line-number-at-pos data-beg) (line-number-at-pos beg))))
      (org-table-export data-file (format "orgtbl-to-tsv :skip %d" skip)))))

(defun org-plot/gnuplot-to-3d-data (data-file)
  )

(defun org-plot/gnuplot-to-grid-data (data-file)
  )

(defun org-plot/gnuplot-script (params)
  (let* ((cols (save-excursion
		 (goto-char (org-table-end))
		 (backward-char 3)
		 (org-table-current-column)))
	 (with (or (plist-get params :with)
		   (and (or (equal type '3d)
			    (equal type 'grid))
			"pm3d")))
	 (type (intern (plist-get params :plot-type)))
	 (set (plist-get params :set))
	 (title (plist-get params :title))
	 (output-file (plist-get params :output-file))
	 (xcol (plist-get params :xcol))
	 (col_labels (plist-get params :col-labels))
	 (plot-str "'%s' using %s:%d with %s title '%s'")
	 (script "") plot-lines)
    (flet ((add-to-script (line) (setf script (format "%s\n%s" script line))))
      (unless (equal type '2d) ('grid (add-to-script "set pm3d"))) ;; type
      (when set ;; set
	(mapcar
	 (lambda (pair)
	   (setf script (format "%s\nset %s %s" script (car pair) (cdr pair))))
	 set))
      (when title (setf script (format "%s\nset title %s" script title))) ;; title
      ;; output_file
      (when output-file

	
	
	)

      (dotimes (col (+ 1 num-cols))
	(unless (or (and x-col (equal col x-col)) (equal col 0))
	  (setf script (cons (format plot-str file (or (and x-col (format "%d" x-col)) "") col col) script))))
      (concat "plot " (mapconcat 'identity (reverse script) "\\\n    ,"))
      )))

;;--------------------------------------------------------------------------------
;; gnuplot integration into org tables
(defun org-plot/gnuplot (&optional params)
  "Plot table using gnuplot. Gnuplot options can be specified
with PARAMS.  If not given options will be taken from the +PLOT
line directly before or after the table."
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (org-plot/goto-nearest-table)
    ;; set default options
    (mapcar 
     (lambda (pair)
       (unless (plist-member params (car pair))
	 (setf params (plist-put params (car pair) (cdr pair)))))
     org-plot/gnuplot-default-options)
    ;; get any plot options adjacent to the table
    (save-excursion ;; before table
      (while (and (equal 0 (forward-line -1))
		  (looking-at "#\\+"))
	(setf params (org-plot/collect-options params))))
    (save-excursion ;; after table
      (goto-char (org-table-end))
      (while (and (equal 0 (forward-line 1))
		  (looking-at "#\\+"))
	(setf params (org-plot/collect-options params))))
    ;; if headers set them as column labels in params
    (setf params (plist-put params :col_labels (org-plot/header-labels)))
    (let ((data-file (make-temp-file "org-plot")))
      ;; get the data from the table (very different for 3d)
      (case (intern (plist-get params :plot-type))
	('2d   (org-plot/gnuplot-to-2d-data   data-file))
	('3d   (org-plot/gnuplot-to-3d-data   data-file))
	('grid (org-plot/gnuplot-to-grid-data data-file)))
      ;; write script
      (with-temp-buffer
	;; write script
	(if (plist-get :script) ;; user script
	    (progn (insert-file-contents)
		   (goto-char (point-min))
		   (while (re-search-forward "$datafile" nil t)
		     (replace-match data-file nil nil)))
	  (insert (org-plot/gnuplot-script params)))
	;; graph table
	(gnuplot-mode)
	(gnuplot-send-buffer-to-gnuplot))
      ;; cleanup
      (bury-buffer (get-buffer "*gnuplot*"))
      (delete-file data-file))))

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
      (dotimes (col (org-plot/table-cols)) ;; for each column
	(dotimes (row (org-plot/table-rows)) ;; for each row
	  (goto-line (+ begin-line row))
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