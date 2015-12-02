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

(defparameter *sudokuSize* 3)
(defparameter *nbSquare* 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; useful function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun createGrid (grid)
  "Create a grid with initial content"
  (make-array (* (* *size* *size*) (* *size* *size*)) :initial-content grid))


;; Remove atfer correct implementation of loadSudoku
(defun readFile (fileName)
  "Read a existing file"
  (let ((in (open fileName :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
     while line do (format t "~a~%" line))
      (close in))))


;; Reduice large expression for calculate number of case
;; Or put in a varible
(defun loadSudoku (filename)
  "Load a sudoku with file"
  (let ((x (read-in (open filename :if-does-not-exist nil))))
    (if (= (list-length x) (* (* *sudokuSize* *nbSquare*) (* *sudokuSize* *nbSquare*) ))
	x
	(progn 
	  (format t "Sudoku file does not have 81 elements") ;; Put a calculate variable for 81
          (quit)))))


(defun isGridValid (grid)
  "Check if the size of grid is valid")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rules of SUDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun insertNewValue (value cell grid)
  "Insert a new value into the sudoku grid")


;;; Arguments   : line -> line check ------- if -1 check column
;;;               column -> column check --- if -1 check line
;;;               n -> value test

(defun checkValue (grid line column n)
  "Check if a value is valid for line of column"
  (if (equal column -1)
      (dotimes (i *size* t)
	(and (= n (getValue grid line i))   ;; change for an array
	     (return nil))))
  (if (equal line -1)
      (dotimes (i *size* t)
	(and (= n (getValue grid i column)) ;; change for an array
	     (return nil)))))


;;; Arguments   : line -> line ---------- | For spot square
;;;               column -> column ------ |
;;;               n -> value test

(defun checkCase (grid line column n)
  "Check of a value is valid for square")

;;; Function for spot the square
;;; (+ 1 (floor y *squaresize*) (* *squaresize* (floor x *squaresize*))))


(defun checkWin (grid)
  "Check if the sudoku is finish")


(defun countEmptyCells (grid)
  "Count number of empty cell of the sudoku")


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
      (readFile "~/SuDoKu/easy.txt")
      nil))


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
	   (setq cellChoice (read))
	   
	   (format t "Value?")
	   (setq valueChoice (read))
	 while (= (checkValue) nil))

      (insertNewValue valueChoice cellChoice grid)
      (setq nbEmptyCells (- nbEmptyCells 1))
	  while (/= 0 nbEmptyCells))))
