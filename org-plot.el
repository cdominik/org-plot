;;; org-plot.el --- support for plotting from org-mode

;; Author: Eric Schulte <schulte dot eric at gmail dot com>

;; This file is Not part of GNU Emacs. yet...

;;; Comments:

;; borrows ideas and a couple of lines of code from org-exp.el

;; thanks to the org-mode mailing list for suggestions

;;; Code:
(require 'org)
(require 'org-exp)
(require 'gnuplot)
(require 'cl)

;; (defun debug (el) (message (format "%S" el)) el)

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

(defun org-plot/goto-nearest-table ()
  "Move the point to the beginning of nearest table.  First look
back until hitting an empty line, then forward until a table is
found."
  (interactive) (move-beginning-of-line 1)
  (while (not (or (org-at-table-p) (< 0 (forward-line 1)))))
  (goto-char (org-table-begin)))

(defun org-plot/collect-options (&optional params)
  (interactive)
  (let ((line (thing-at-point 'line)))
    (if (string-match "#\\+PLOT: +\\(.*\\)$" line)
	(org-plot/add-options-to-plist params (match-string 1 line))
      params)))

(defun org-plot-quote-tsv-field (s)
  "Quote field for export to gnuplot."
  (if (string-match org-table-number-regexp s) s
    (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")))

(defun org-plot/gnuplot-to-data (table data-file)
  (with-temp-file
      data-file (insert (orgtbl-to-generic
			 table
			 (org-combine-plists
			  '(:sep "\t" :fmt org-plot-quote-tsv-field)
			  params))))
  nil)

(defun org-plot/gnuplot-to-grid-data (table data-file)
  (interactive)
  (let* ((ind (- (plist-get params :ind) 1))
	 (deps (if (plist-member params :deps)
		   (mapcar (lambda (val) (- val 1)) (plist-get params :deps))
		 (let (collector)
		   (dotimes (col (length (first table)))
		     (setf collector (cons col collector)))
		   collector)))
	 row-vals (counter 0))
    (when (>= ind 0) ;; collect values of ind col
      (setf row-vals (mapcar (lambda (row) (setf counter (+ 1 counter))
			       (cons counter (nth ind row))) table)))
    (when (or deps (>= ind 0)) ;; remove non-plotting columns
      (setf deps (delq ind deps))
      (setf table (mapcar (lambda (row)
			    (dotimes (col (length row))
			      (unless (memq col deps)
				(setf (nth col row) nil)))
			    (delq nil row))
			  table)))
    ;; write table to gnuplot grid datafile format
    (with-temp-file data-file
      (let ((num-rows (length table)) (num-cols (length (first table)))
	    front-edge back-edge)
	(flet ((gnuplot-row (col row value)
			    (setf col (+ 1 col)) (setf row (+ 1 row))
			    (format "%f  %f  %f\n%f  %f  %f\n"
				    col (- row 0.5) value ;; lower edge
				    col (+ row 0.5) value))) ;; upper edge
	  (dotimes (col num-cols)
	    (dotimes (row num-rows)
	      (setf back-edge
		    (concat back-edge
			    (gnuplot-row (- col 1) row (string-to-number
							(nth col (nth row table))))))
	      (setf front-edge
		    (concat front-edge
			    (gnuplot-row col row (string-to-number
						  (nth col (nth row table)))))))
	    ;; only insert once per row
	    (insert back-edge) (insert "\n") ;; back edge
	    (insert front-edge) (insert "\n") ;; front edge
	    (setf back-edge "") (setf front-edge "")))))
    row-vals))

(defun org-plot/gnuplot-script (data-file num-cols params)
  (let* ((type (plist-get params :plot-type))
	 (with (if (equal type 'grid)
		   'pm3d
		 (plist-get params :with)))
	 (sets (plist-get params :set))
	 (lines (plist-get params :line))
	 (map (plist-get params :map))
	 (title (plist-get params :title))
	 (file (plist-get params :file))
	 (ind (plist-get params :ind))
	 (text-ind (plist-get params :textind))
	 (deps (if (plist-member params :deps) (plist-get params :deps)))
	 (col-labels (plist-get params :labels))
	 (x-labels (plist-get params :xlabels))
	 (y-labels (plist-get params :ylabels))
	 (plot-str "'%s' using %s%d%s with %s title '%s'")
	 (plot-cmd (case type
		     ('2d "plot")
		     ('3d "splot")
		     ('grid "splot")))
	 (script "reset") plot-lines)
    (flet ((add-to-script (line) (setf script (format "%s\n%s" script line))))
      (when file ;; output file
	(add-to-script (format "set term %s" (file-name-extension file)))
	(add-to-script (format "set output '%s'" file)))
      (case type ;; type
	('2d ())
	('3d (if map (add-to-script "set map")))
	('grid (if map
		   (add-to-script "set pm3d map")
		 (add-to-script "set pm3d"))))
      (when title (add-to-script (format "set title '%s'" title))) ;; title
      (when lines (mapcar (lambda (el) (add-to-script el)) lines)) ;; line
      (when sets ;; set
	(mapcar (lambda (el) (add-to-script (format "set %s" el))) sets))
      (when x-labels ;; x labels (xtics)
	(add-to-script
	 (format "set xtics (%s)"
		 (mapconcat (lambda (pair)
			      (format "\"%s\" %d" (cdr pair) (car pair)))
			    x-labels ", "))))
      (when y-labels ;; y labels (ytics)
	(add-to-script
	 (format "set ytics (%s)"
		 (mapconcat (lambda (pair)
			      (format "\"%s\" %d" (cdr pair) (car pair)))
			    y-labels ", "))))
      (case type ;; plot command
	('2d (dotimes (col num-cols) 
	       (unless (and (equal type '2d)
			    (or (and ind (equal (+ 1 col) ind))
				(and deps (not (member (+ 1 col) deps)))))
		 (setf plot-lines
		       (cons
			(format plot-str data-file
				(or (and (not text-ind) ind
					 (> ind 0) (format "%d:" ind)) "")
				(+ 1 col)
				(if text-ind (format ":xticlabel(%d)" ind) "")
				with
				(or (nth col col-labels) (format "%d" (+ 1 col))))
			plot-lines)))))
	('3d
	 (setq plot-lines (list (format "'%s' matrix with %s title ''"
					data-file with))))
	('grid
	 (setq plot-lines (list (format "'%s' with %s title ''"
					data-file with)))))
      (add-to-script
       (concat plot-cmd " " (mapconcat 'identity (reverse plot-lines) "\\\n    ,")))
      script)))

;;--------------------------------------------------------------------------------
;; facad functions
(defun org-plot/gnuplot (&optional params)
  "Plot table using gnuplot. Gnuplot options can be specified
with PARAMS.  If not given options will be taken from the +PLOT
line directly before or after the table."
  (interactive)
  (save-window-excursion
    (delete-other-windows)
    (when (get-buffer "*gnuplot*") ;; reset *gnuplot* if it already running
      (save-excursion
	(set-buffer "*gnuplot*") (goto-char (point-max)) 
	(gnuplot-delchar-or-maybe-eof nil)))
    (org-plot/goto-nearest-table)
    ;; set default options
    (mapcar 
     (lambda (pair)
       (unless (plist-member params (car pair))
	 (setf params (plist-put params (car pair) (cdr pair)))))
     org-plot/gnuplot-default-options)
    ;; collect table and table information
    (let* ((data-file (make-temp-file "org-plot"))
	   (table (org-table-to-lisp))
	   (num-cols (length (first table))))
      (while (equal 'hline (first table)) (setf table (cdr table)))
      (when (equal (second table) 'hline)
	(setf params (plist-put params :labels (first table))) ;; headers to labels
	(setf table (delq 'hline (cdr table)))) ;; clean non-data from table
      ;; collect options
      (save-excursion (while (and (equal 0 (forward-line -1))
				  (looking-at "#\\+"))
			(setf params (org-plot/collect-options params))))
      ;; dump table to datafile (very different for grid)
      (case (plist-get params :plot-type)
	('2d   (org-plot/gnuplot-to-data table data-file))
	('3d   (org-plot/gnuplot-to-data table data-file))
	('grid (let ((y-labels (org-plot/gnuplot-to-grid-data table data-file)))
		 (when y-labels (plist-put params :ylabels y-labels)))))
      ;; check for text ind column
      (let ((ind (- (plist-get params :ind) 1)))
	(when (and (>= ind 0) (equal '2d (plist-get params :plot-type)))
	  (if (> (length
		  (delq 0 (mapcar
			   (lambda (el)
			     (if (string-match org-table-number-regexp el)
				 0 1))
			   (mapcar (lambda (row) (nth ind row)) table)))) 0)
	      (plist-put params :textind t))))
      ;; write script
      (with-temp-buffer
	(if (plist-get params :script) ;; user script
	    (progn (insert-file-contents)
		   (goto-char (point-min))
		   (while (re-search-forward "$datafile" nil t)
		     (replace-match data-file nil nil)))
	  (insert
	   (org-plot/gnuplot-script data-file num-cols params)))
	;; graph table
	(gnuplot-mode)
	(gnuplot-send-buffer-to-gnuplot))
      ;; cleanup
      (bury-buffer (get-buffer "*gnuplot*"))(delete-file data-file))))

(provide 'org-plot)
;;; org-plot.el ends here