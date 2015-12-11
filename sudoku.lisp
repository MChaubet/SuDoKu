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
(defparameter *letters* '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Solver of SUDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun randomValue (l)
  (let ((value))
    (loop do
	 (setf value (nth (random (length l)) l))
       while (= value 0))
    value))

(defun possibleValues (grid i j)
  (let ((values (make-list *lengthArray* :initial-element 0)))
    (dotimes (cpt *lengthArray*)
      (when (equal nil (checkValue grid i j (1+ cpt)))
	(setf (nth cpt values) (1+ cpt))))
    values))

(defun solve-random (grid)
  (let ((errorPossibleValue nil))
   (dotimes (i *lengthArray*)
    (dotimes (j *lengthArray*)
      (when (zerop (aref grid i j))
	(let ((values (possibleValues grid i j)))
	  (if (eq values nil)
	      (setf errorPossibleValue T)
	      (setf (aref grid i j) (randomValue values)))))))
   (if (equal errorPossibleValue T)
       (print "Error")
       (print "Correct"))
   errorPossibleValue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rules of SUDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun letterToNumber (column)
  "Take the alphanumeric and send back the number of the column"
  (position column *letters*))

(defun numberToLetter (column)
  "Take the number of the column and send back the alphanumeric"
  (car (subseq *letters* column (1+ column))))

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
  (make-string n :initial-element #\*))

(defun delimiter ()
  (drawStarLine (+ (1+ *nbSquare*) (* 3 (1+ *lengthArray*)))))

(defun drawLetters ()
  (let ((string ""))
    (setq string (concatenate 'string string "   "))
    (dotimes (i *lengthArray*)
      (when (= 0 (mod i *sudokuSize*))
	(setq string (concatenate 'string string "|")))
      (setq string (concatenate 'string string (format nil " ~a " (numberToLetter i)))))
    (setq string (concatenate 'string string "|"))
    string))

(defun drawLine (grid line)
  "Print a specific line of sudoku"
  (let ((string ""))
    (setq string (concatenate 'string string (format nil " ~a " (1+ line))))
    (dotimes (i *lengthArray*)
      (when (= 0 (mod i *sudokuSize*))
	(setq string (concatenate 'string string "|")))
      (setq string (concatenate 'string string (format nil " ~a " (aref grid line i)))))
    (setq string (concatenate 'string string "|"))
    string))

(defun drawSudoku (grid)
  "Print all line of sudoku"
  (print (drawLetters))
  (dotimes (i *lengthArray*)
    (when (= 0 (mod i *sudokuSize*))
      (print (delimiter)))
    (print (drawLine grid i)))
  (print (delimiter)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Principale function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Explication : Principal function of the game, load grid if inquire,
;;;               Else load a existing grid

(defun sudoku (grid)
  "Loop game"
  (let ((cellChoice 0)
	(valueChoice 0)
	(nbEmptyCells (countEmptyCells grid)))

    (loop do
      (drawSudoku grid)
      (print nbEmptyCells)

      (loop do 
	   (format t "C L?")
	   (setq cellChoice (list (read) (read)))
	   
	   (format t "Value?")
	   (setq valueChoice (read))
	 while (eq (checkValue grid (1- (car (cdr cellChoice))) (letterToNumber (car cellChoice)) valueChoice) T))

      (insertNewValue valueChoice cellChoice grid)
      (setq nbEmptyCells (- nbEmptyCells 1))
	  while (/= 0 nbEmptyCells))))