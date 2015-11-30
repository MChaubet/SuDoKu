;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sudokuSize* 3)
(defparameter *nbSquare* 3)
(defparameter *grid* (createGrid *sudokuSize*))

;; Table of hash
;; (loop for key being the hash-keys of *my-hash*
;;		using (hash-value value)
;;		do (format t "The value associated with the key ~S is ~S~%" key value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; usefull function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Diapo page 142
(defmacro while (test &rest body)
	(do ()
		((not, test))
		,@body))
;; Utilisation
;;
		
;; Diapo page 143
(defmacro for (init test update &body body)
	'(progn
		,init
		(while, test
			,@body
			,update)))	
;; Utilisation			
;; (for (setf *i* 4) (>= *i* 0) (decf *i*) (print1 *i*))

;; Diapo page 144
(defmacro ntimes (n &body body)
	(let ((g (gensym)))
	'(do ((,g 0 (1+ ,g)))
		((>= ,g ,n))
		,@body)))
;; Utilisation
;;

;; Function for create grid
(defun createGrid ()
	(make-array (list (* *size* *size*) (* *size* *size*))))
	
;; Function for read file
(defun readFile (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
     while line do (format t "~a~%" line))
      (close in))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; class SUDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sudoku() ()) ;; HashTable of ((nbSquare * sudokuSize) * (nbSquare * sudokuSize))
					   ;; Nb of empty case
					   ;; Name of a file // If exist

;; Maybe not usefull
(defclass cell() ())   ;; Value
					   ;; position

;; Modification pour les grandes cases
(defmethod checkValue ((grid sudoku) line column n)
	(:documentation "Check if a value is valid for line of column")
	(if (equal column -1)
		(dotimes (i *size* t)
			(and (= n (getValue grid line i))
				(return nil))))
		(if (equal line -1)
			(dotimes (i *size* t)
				(and (= n (getValue grid i column))
					(return nil)))))
(defmethod checkCase ((grid sudoku) line column n)
	(:documentation "Check of a value is valid for square"))

;; Use ntimes macro
(defun drawStarLine (n)
	(format t (make-string n)

(+ 1 (floor y *squaresize*) (* *squaresize* (floor x *squaresize*))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Principale function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sudoku (grid)
	(let ((sudokuGrid (createGrid))
