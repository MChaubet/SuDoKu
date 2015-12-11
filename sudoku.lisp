;;;; sudoku.lisp --- Sudoku Puzzle
;;
;; Copyright (C) 2015 Mathieu Chaubet, Mathieu Lacoste
;;
;; Author: Mathieu Chaubet <mathieu.chaubet.travail@gmail.com>
;; Author: Mathieu Lacoste <mat.lacoste33@gmail.com>
;; GIT: https://github.com/MChaubet/SuDoKu
;; Version: 1.0
;; Created: 31-11-2015


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sudokuSize* 3 )
(defparameter *nbSquare* 3 )
(defparameter *lengthArray* (* *sudokuSize* *nbSquare*) )
(defparameter *lettersList* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y ))

(defun isGridValid (grid)
  "Check if the size of grid is valid")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rules of SUDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun letterToNumber (column)
  "Take the alphanumeric and send back the number of the column"
  (position column *lettersList*))

(defun numberToLetter (column)
  "Take the number of the column and send back the alphanumeric"
  (car (subseq *lettersList* column (1+ column))))

(defun insertNewValue (value cell grid)
  "Insert a new value into the sudoku grid"
  (let ((column (letterToNumber (car cell)))
	(line (1- (car (cdr cell)))))
   (setf (aref grid line column) value)))


;;; Arguments   : line -> line check ------- if -1 check column
;;;               column -> column check --- if -1 check line
;;;               n -> value test

(defun checkValue (grid line column n)
  "Check if a value is valid for line of column"
  (let ((find nil))
    (dotimes (i *lengthArray* t)
      (when (= n (aref grid line i))
	(setq find T)))
    (dotimes (i *lengthArray* t)
      (when (= n (aref grid i column))
	(setq find T)))
    (or find (checkCase grid line column n))))


;;; Arguments   : line -> line ---------- | For spot square
;;;               column -> column ------ |
;;;               n -> value test

(defun checkCase (grid line column n)
  "Check of a value is valid for square"
  (let ((caseX (car (list (floor line *sudokuSize*))))
	(caseY (car (list (floor column *sudokuSize*))))
	(find nil))
    (dotimes (i *sudokuSize*)
      (dotimes (j *sudokuSize*)
	(when (= n (aref grid 
			 (+ i (* caseX *nbSquare*))
			 (+ j (* caseY *nbSquare*))))
	  (setq find T))))
    find))


(defun countEmptyCells (grid)
  "Count number of empty cell of the sudoku"
  (let ((emptyCell 0))
    (dotimes (i *lengthArray*)
      (dotimes (j *lengthArray*)
	(when (= 0 (aref grid i j))
	  (incf emptyCell))))
    emptyCell))
	     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Print of SUDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun drawStarLine (n)
  "Print n stars"
  (format nil (make-string n :initial-element #\*)))

(defun delimiter ()
  (drawStarLine (+ (1+ *nbSquare*) (* 3 (1+ *lengthArray*)))))

(defun drawLine (grid line)
  "Print a specific line of sudoku"
  (let ((string (make-array 0)))
    (dotimes (i *lengthArray*)
      (if (equal (floor i *sudokuSize*) 0)
	  (concatenate 'string "| ~A " (aref grid line i))
	  (concatenate 'string " ~A " (aref grid line i))))
    string))

(defun drawSudokuLines ()
  "Print all line of sudoku")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Principale function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Explication : Principal function of the game, load grid if inquire,
;;;               Else load a existing grid

(defun sudoku (&optional grid)
  (if (equal grid nil)
      (sudoku (readFile "~/SuDoKu/easy.txt"))
      (if (isGridValid grid)
	  (launchGame grid)
	  (format t "Erreur : grille non conforme"))))


(defun launchGame (grid)
  "Loop game"
  (let ((cellChoice 0)
	(valueChoice 0)
	(nbEmptyCells (countEmptyCells grid)))

    (loop do
      (showGrid grid)
      (print nbEmptyCells)

      (loop do 
	   (format t "C L?")
	   (setq cellChoice (list (read) (read)))
	   
	   (format t "Value?")
	   (setq valueChoice (read))
	 while (= (checkValue) nil))

      (insertNewValue valueChoice cellChoice grid)
      (setq nbEmptyCells (- nbEmptyCells 1))
	  while (/= 0 nbEmptyCells))))
