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


(defun readFile (fileName)
  "Read a existing file"
  (let ((in (open fileName :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
     while line do (format t "~a~%" line))
      (close in))))


;; Reduice large expression for calculate number of case
;; Or put in a varible
(defun load-sudoku (filename)
  "Load a sudoku with file"
  (let ((x (read-in (open filename :if-does-not-exist nil))))
    (if (= (list-length x) (* (* *sudokuSize* *nbSquare*) (* *sudokuSize* *nbSquare*) ))
	x
	(progn 
	  (format t "Sudoku file does not have 81 elements") ;; Put a calculate variable for 81
          (quit)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Rules of SUDOKU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(+ 1 (floor y *squaresize*) (* *squaresize* (floor x *squaresize*))))


(defun checkWin (grid)
  (:documentation "Check if the sudoku is finish"))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; PARTIE LACOSTE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Récupérer grille depuis un fichier
(defun collectGrid ()
  ())

; Vérifie si la grille corespond bien au standard TAILLE*TAILLE*TAILLE (et autre
(defun isGridValid (grid)
  ())

; Compte le nombre de cellules vides dans la grille
(defun countEmptyCells (grid)
  ())

; Affiche la grille
(defun showGrid (grid)
  ())

; Insère la nouvelle valeur à la case indiquée dans la grille
(defun insertNewValue (value cell grid)
  ())

; Lance la boucle principale du jeu
(defun launchGame (grid)
  (let ((cellChoice 0)
	(valueChoice 0)
	(nbEmptyCells (countEmptyCells grid)))


    (loop do
	 (showGrid grid)
	 (print nbEmptyCells)

	 (print "C L?")

	 (setq cellChoice (read))

	 (print "Value?")
	 (setq valueChoice (read))

	 (insertNewValue valueChoice cellChoice grid)
	 (setq nbEmptyCells (- nbEmptyCells 1))
       while (/= 0 nbEmptyCells))))

; Fonction principale du jeu. Vérifie si on a donné un grille en paramètre (si on veut en charger à partir d'un fichier), puis lance le jeu.
(defun sudoku (&rest grid)
(if (equal grid nil)
    (sudoku (collectGrid))
    (if (isGridValid grid)
	(launchGame grid)
	"Erreur : grille non conforme")))
