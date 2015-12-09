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
(defparameter *lettersList* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; useful function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tentativeDeBoGos (fileName)
  "Create a grid with initial content"
  (let ((grid (make-array (list *lengthArray* *lengthArray*)))
	(rien 0))))


;; Remove atfer correct implementation of loadSudoku
(defun readFile (fileName)
  "Read a existing file"
  (let ((in (open fileName :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
     while line do (format t "~a~%" line))
      (close in))))

(defun loadSudoku (filename)
  "Load a sudoku with file"
  (let ((x (read-in (open filename))))
    x
    (progn
      (format t "Sudoku file does not have 81 elements")
      (quit))))

(defun isGridValid (grid)
  "Check if the size of grid is valid")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rules of SUDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convertColumn (column)
  "Take the alphanumeric and send back the number of the column"
  (position column *lettersList*))

(defun insertNewValue (value cell grid)
  "Insert a new value into the sudoku grid"
  (let ((column (convertColumn (car cell)))
	(line (1- (car (cdr cell)))))

   (setf (aref grid line column) value)))


;;; Arguments   : line -> line check ------- if -1 check column
;;;               column -> column check --- if -1 check line
;;;               n -> value test

(defun checkValue (grid line column n)
  "Check if a value is valid for line of column"
  (if (equal column -1)
      (dotimes (i *size* t)
	(when (= n (aref grid line i))   ;; change for an array #2A
	  (return nil))))
  (if (equal line -1)
      (dotimes (i *size* t)
	(when (= n (aref grid i column)) ;; change for an array #2A
	  (return nil)))))


;;; Arguments   : line -> line ---------- | For spot square
;;;               column -> column ------ |
;;;               n -> value test

(defun checkCase (grid line column n)
  "Check of a value is valid for square"
  (let ((caseX (car (list (floor line *sukoduSize*))))
	(caseY (car (list (floor column *sudokuSize*)))))
    (do ((i (* *sudokuSize* caseX) (1+ i)))
	((< i (* *sudokuSize* (1+ caseX))))
      (do ((j (* *sudokuSize* caseY) (1+ j)))
	  ((< j (* *sudokuSize* (1+ caseY))))
	(when (= n (aref grid i j))
	  (return nil))))))


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


(defun drawLine (number)
  "Print a specific line of sudoku")


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
