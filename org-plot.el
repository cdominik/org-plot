;;; org-plot.el --- support for plotting from org-mode

;; Author: Eric Schulte <schulte dot eric at gmail dot com>

;; This file is Not part of GNU Emacs. yet...

;;; Comments:

;; borrows ideas and a couple of lines of code from org-exp.el

;;; Code:
(require 'org)
(require 'org-exp)
(require 'gnuplot)
(require 'cl)

(defun debug (el) (message (format "%S" el)) el)

;;--------------------------------------------------------------------------------
;; org-plot/gnuplot options
;; 
;;  Gnuplot options accessible through `org-plot', common options
;; are specifically supported, while all other options are
;; accessible through specification of generic script lines, or
;; specification of custom script files.  Possible options are...
;; 
;; set: ---------- specify any gnuplot option to be set when graphing
;; title: -------- specify the title of the plot
;; ind: ---------- specify which column of the table to use as the x
;;                 axis
;; deps: --------- specify (as a comma seperated list with no spaces)
;;                 which columns of the table to graph against the
;;                 ind (defaults to all other columns)
;; type: --------- specify whether the plot will be '2d' '3d or
;;                 'grid'
;; with: --------- specify a with option to be inserted for every
;;                 col being plotted (e.g. lines, points, boxes,
;;                 impulses, etc...) defaults to 'lines'
;; file: --------- if you want to plot to a file specify the path to
;;                 the desired output file
;; labels: ------- list of labels to be used for the deps (defaults
;;                 to column headers if they exist)
;; line ---------- specify an entire line to be inserted in the
;;                 gnuplot script
;; map ----------- when plotting 3d or grid types, set this to true to
;;                 graph a flat mapping rather than a 3d slope
;; script: ------- if you want total controll you can specify a
;;                 script file which will be used to plot, before
;;                 plotting every instance of $datafile in the
;;                 specified script will be replaced with the path
;;                 to the generated data file.  Note even if you set
;;                 this option you may still want to specify the
;;                 plot type, as that can impact the content of the
;;                 data file.

(defvar org-plot/gnuplot-default-options
  '((:plot-type . 2d)
    (:with . lines)
    (:ind . 0)))

(defun org-plot/add-options-to-plist (p options)
  "Parse an OPTONS line and set values in the property list P."
  (let (o)
    (when options
      (let ((op '(("type"   . :plot-type)
		  ("script" . :script)
		  ("line"   . :line)
		  ("set"    . :set)
		  ("title"  . :title)
		  ("ind"    . :ind)
		  ("deps"   . :deps)
		  ("with"   . :with)
		  ("file"   . :file)
		  ("labels" . :labels)
		  ("map"    . :map)))
	    (multiples '("set" "line"))
	    (regexp ":\\([\"][^\"]+?[\"]\\|[(][^)]+?[)]\\|[^ \t\n\r;,.]*\\)")
	    (start 0)
	    o)
	(while (setq o (pop op))
	  (if (member (car o) multiples) ;; keys with multiple values
	      (while (string-match
		      (concat (regexp-quote (car o)) regexp)
		      options start)
		(setq start (match-end 0))
		(setq p (plist-put p (cdr o)
				   (cons (car (read-from-string
					       (match-string 1 options)))
					 (plist-get p (cdr o)))))
		p)
	    (if (string-match (concat (regexp-quote (car o)) regexp)
			      options)
		(setq p (plist-put p (cdr o)
				   (car (read-from-string
					 (match-string 1 options)))))))))))
  p)

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
  (let ((line (thing-at-point 'line)))
    (if (string-match "#\\+PLOT: +\\(.*\\)$" line)
	(org-plot/add-options-to-plist params (match-string 1 line))
      params)))

(defun org-plot/header-labels ()
  (interactive)
  (save-excursion
    (let ((beg (org-table-begin))
	  (end (org-table-end))
	  labels)
      (when (and (goto-char beg)
		 (re-search-forward org-table-dataline-regexp end t)
		 (re-search-forward org-table-hline-regexp end t)
		 (move-beginning-of-line 0))
	(dotimes (col (org-plot/table-cols))
	  (setf labels (cons (org-table-get-field (+ 1 col)) labels))))
      (reverse labels))))

(defun org-plot/gnuplot-to-2d-data (data-file)
  (save-excursion
    (let* ((beg (org-table-begin)) (end (org-table-end))
	   (data-beg (if (and (goto-char beg)
			      (re-search-forward org-table-dataline-regexp end t)
			      (re-search-forward org-table-hline-regexp end t)
			      (re-search-forward org-table-dataline-regexp end t))
			 (match-beginning 0) beg))
	   (skip (- (line-number-at-pos data-beg) (line-number-at-pos beg))))
      (org-table-export data-file (format "orgtbl-to-tsv :skip %d" skip))))
  nil)

(defun org-plot/gnuplot-to-3d-data (data-file)
  ;; TODO
  )

(defun org-plot/gnuplot-to-grid-data (data-file)
  (interactive)
  (let* ((beg (org-table-begin)) (end (- (org-table-end) 3))
	 (data-beg (+ 1 (if (and (goto-char beg)
				 (re-search-forward org-table-dataline-regexp end t)
				 (re-search-forward org-table-hline-regexp end t)
				 (re-search-forward org-table-dataline-regexp end t))
			    (match-beginning 0) beg)))
	 (ind (plist-get params :ind))
	 (deps (if (plist-member params :deps) (plist-get params :deps)))
	 (rows (+ 1 (- (line-number-at-pos end) (line-number-at-pos data-beg))))
	 (num-cols (save-excursion (goto-char end) (org-table-current-column)))
	 (counter 0) row-vals)
    (save-excursion (org-table-copy-region data-beg end))
    ;; write copied region to temp buffer
    (with-temp-buffer
      (insert "||") (goto-char 2)
      (org-mode) (org-return) (goto-char 2)
      (org-table-paste-rectangle)
      (when (> ind 0)
	(save-excursion (goto-char 3) ;; collect values of ind col
			(dotimes (row rows)
			  (setq row-vals (cons (org-table-get-field ind) row-vals))
			  (forward-line 1)))
	(setq row-vals (reverse row-vals))
	(save-excursion (goto-char 2) ;; remove ind col
			(dotimes (n (- ind 1)) (org-cycle))
			(org-table-delete-column)))
      ;; TODO: if deps: then remove all other columns
      ;; save table to gnuplot format
      (get-buffer-create "*data*")
      (let ((collector (get-buffer-create "*org-plot/grid temp buffer*"))
	    (last-row 0)
	    front-edge back-edge)
	(flet ((gnuplot-row (col row value)
			    (format "%f  %f  %f\n%f  %f  %f\n"
				    col (- row 0.5) value ;; lower edge
				    col (+ row 0.5) value))) ;; upper edge
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
	  (save-excursion ;; one last time
	    (switch-to-buffer collector)
	    (insert back-edge) (insert "\n") ;; back edge
	    (insert front-edge) (insert "\n") ;; front edge
	    (setf back-edge "") (setf front-edge ""))
	  (switch-to-buffer collector) ;; close up shop
	  (write-file data-file) (kill-buffer collector))))
    ;; return the label lines to add to the script
    (if row-vals
	(format "set ytics (%s)"
		(mapconcat
		 (lambda (el)
		   (setf counter (+ 1 counter))
		   (format "\"%s\" %d" el counter))
		 row-vals ", ")))))

(defun org-plot/gnuplot-script (data-file num-cols params &optional script-hack)
  (let* ((type (plist-get params :plot-type))
	 (with (if (equal type '2d)
		   (plist-get params :with)
		 'pm3d))
	 (sets (plist-get params :set))
	 (lines (plist-get params :line))
	 (map (plist-get params :map))
	 (title (plist-get params :title))
	 (file (plist-get params :file))
	 (ind (plist-get params :ind))
	 (deps (if (plist-member params :deps) (plist-get params :deps)))
	 (col-labels (plist-get params :labels))
	 (plot-str "'%s' using %s:%d with %s title '%s'")
	 (plot-cmd (case type
		     ('2d "plot")
		     ('3d "splot")
		     ('grid "splot")))
	 (script "reset") plot-lines)
    (flet ((add-to-script (line) (setf script (format "%s\n%s" script line))))
      (if script-hack (add-to-script script-hack))
      (when file ;; output file
	(add-to-script (format "set term %s" (file-name-extension file)))
	(add-to-script (format "set output '%s'" file)))
      (case type ;; type
	('2d ())
	('3d ())
	('grid (if map
		   (add-to-script "set pm3d map")
		 (add-to-script "set pm3d"))))
      (when title (add-to-script (format "set title '%s'" title))) ;; title
      (when lines (mapcar (lambda (el) (add-to-script el)) lines)) ;; line
      (when sets ;; set
	(mapcar (lambda (el) (add-to-script (format "set %s" el))) sets))
      (case type ;; plot command
	('2d (dotimes (col num-cols) 
	       (unless (and (equal type '2d)
			    (or (and ind (equal (+ 1 col) ind))
				(and deps (not (member (+ 1 col) deps)))))
		 (setf plot-lines
		       (cons
			(format plot-str data-file
				(or (and ind (format "%d" ind)) "")
				(+ 1 col) with
				(or (nth col col-labels) (format "%d" (+ 1 col))))
			plot-lines)))))
	('3d )
	('grid
	 (setq plot-lines (list (format "'%s' with %s title ''" data-file with)))))
      (add-to-script
       (concat plot-cmd " " (mapconcat 'identity (reverse plot-lines) "\\\n    ,")))
      script)))

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
    ;; if headers set them as column labels in params
    (setf params (plist-put params :labels (org-plot/header-labels)))
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
    ;; get the data from the table (very different for 3d)
    (let ((data-file (make-temp-file "org-plot"))
	  (num-cols (save-excursion
		      (goto-char (org-table-end))
		      (backward-char 3)
		      (org-table-current-column)))
	  script-hack) ;; because some datadumps return relevant
      ;; information for the script
      (setq script-hack
	    (case (plist-get params :plot-type)
	      ('2d   (org-plot/gnuplot-to-2d-data data-file))
	      ('3d   (org-plot/gnuplot-to-3d-data data-file))
	      ('grid (org-plot/gnuplot-to-grid-data data-file))))
      ;; write script
      (with-temp-buffer
	;; write script
	(if (plist-get params :script) ;; user script
	    (progn (insert-file-contents)
		   (goto-char (point-min))
		   (while (re-search-forward "$datafile" nil t)
		     (replace-match data-file nil nil)))
	  (insert (org-plot/gnuplot-script data-file num-cols params script-hack)))
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

(provide 'org-plot)
;;; org-plot.el ends here