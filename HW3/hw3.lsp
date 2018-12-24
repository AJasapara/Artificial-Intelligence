;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; Description: This function returns true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
; Logic: This function counts each row for 2s and 3s and if 
; there is even one of either, it returns nil because at least a 
; box or a goalkeeper are not at a goal.
(defun goal-test (s)
    (cond 
    	((null s) t)
    	((> (+ (count 2 (first s)) (count 3 (first s))) 0) nil)
    	(t (goal-test (rest s)))
  	)
  );end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; Description: This function returns the value of the square at row r and column c in state s.
; Logic: This function cuts away the first r rows and c cols, and returns the value
; at that square.
(defun get-square (s r c)
	(cond 
		((or (< r 0) (< c 0) (not(nthcdr c (first(nthcdr r s))))) 1)
		(t (first (nthcdr c (first(nthcdr r s)))))
	)
)

; Description: This function sets the value of the square at row r and column c in state s to v.
; Logic: This function appends the first r-1 rows and c-1 cols to the new value v, and appends to that
; the remainder of the rows and columns.
(defun set-square (s r c v)
	(let ((col (first(nthcdr r s))))
	(append (butlast s (- (length s) r)) (list (append (butlast col (- (length col) c)) (list v) (nthcdr (+ c 1) col) nil)) (nthcdr (+ r 1) s)))
)

; Description: This function takes a state s and a direction d (0 is up, 1 is down, 2 is left, 3 is right)
; and returns a new state where the keeper has successfully moved in that direction or nil if he cannot
; Logic: This function checks the 4 squares around the keeper for an empty space or a goal (where it can successfully move), 
; a wall (where it cannot move), or a box. If there is a box, it checks the space after that box in the same direction to 
; determine if the box can move, where if it can (due to a goal or empty space), the function returns that state. Otherwise, 
; it returns nil.
(defun try-move (s d)
	(let* ((pos (getKeeperPosition s 0))
		(y (car pos))
		(x (cadr pos)))
		(let* (
			(currSquare (cond ((= (get-square s x y) 3) 0) (t 4)))
			(new_x (cond ((= d 2) (- x 1)) ((= d 3) (+ x 1)) (t x)))
		 	(new_y (cond ((= d 0) (- y 1)) ((= d 1) (+ y 1)) (t y)))
		 	(far_x (cond ((= d 2) (- x 2)) ((= d 3) (+ x 2)) (t x))) 
		    (far_y (cond ((= d 0) (- y 2)) ((= d 1) (+ y 2)) (t y))))
		 	(let* ((near (get-square s new_x new_y)) (far (get-square s far_x far_y))) 
	 			(cond 
	 				((= near 0) (set-square(set-square s new_x new_y 3) x y currSquare))
	 				((= near 1) nil)
	 				((= near 2) 
	 					(cond
	 						((= far 0) (set-square(set-square(set-square s new_x new_y 3) x y currSquare) far_x far_y 2))
	 						((= far 4) (set-square(set-square(set-square s new_x new_y 3) x y currSquare) far_x far_y 5))
	 						(t nil)			
	 					)
	 				)
	 				((= near 4) (set-square(set-square s new_x new_y 6) x y currSquare))
	 				(t 
	 					(cond
	 						((= far 0) (set-square(set-square(set-square s new_x new_y 6) x y currSquare) far_x far_y 2))
	 						((= far 4) (set-square(set-square(set-square s new_x new_y 6) x y currSquare) far_x far_y 5))
	 						(t nil)			
	 					)
	 				)
	 			)
		 	)
		)
	)
)

; Description: This function returns a list of the valid states that the keeper can move to from state s.
; Logic: This function creates a list by calling try-move on each of the four directions, and removes the nils.
(defun next-states (s)
    (cleanUpList (list (try-move s 2) (try-move s 1) (try-move s 3) (try-move s 0)))
 )

; Description: This function computes the trivial admissible heuristic.
; Logic: Return 0.
(defun h0 (s)
	0
)

; Description: This function computes the number of misplaced boxes in s.
; Logic: This function counts the number of 2s (boxes) in s.
; This heuristic is admissible because no matter what it will take at least 
; as many moves as there are misplaced boxes to move each box to a goal. Thus,
; h1 will always be less than the actual number of moves it would take to complete 
; the game.
(defun h1 (s)
	(cond 
    	((null s) 0)
    	(t (+ (count 2 (first s)) (h1 (rest s))))
  	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; Description: This function uses the minimum distance it would take for the keeper/boxes to reach a goal state 
; in s as the heuristic.
; Logic: This function obtains a list of the locations of the keeper, boxes, and goals by calling getLocs, and 
; then passes in these locations to sumMin to obtain the minimum possible distance for each keeper/box to reach 
; a goal.
(defun h504742401 (s)
	(let* ((locs (getLocs s 0)))
		(sumMin (first locs) (second locs))
	)
)
 
; Description: This function calculates the (row col) of the keeper and the boxes in s in the first part of 
; the list and the (row col) of the goals in the second part of the list.
; Logic: This function goes through each row in s starting at index row, and calls getLocCols on it. It then 
; appends all these rows together. An example of the format is shown below:
; ( ((r, c), (r, c), (r, c))  ((r, c) (r,c) (r,c)) )
;   |    Player + boxes    |        Goals 		 |
(defun getLocs (s row)
	(cond
		((null s) '(nil nil))
		(t (let* ((col (getLocCols (first s) row 0)) (more (getLocs (rest s) (+ row 1))))
			(list (append (first col) (first more)) (append (second col) (second more)))
			)
		)
	)
)

; Description: This function calculates the col of the keeper and the boxes in r and appends it to the passed-in row 
; in the first part of the list. It then does the same for goals in the second part of the list. 
; Logic: It checks each element for its value and appends it to the proper part of the overall list. It then calls itself 
; on the remainder of the list, eventually proceeding through the entire thing.
(defun getLocCols (r row col)
	(cond ((null r) '(nil nil))
	(t
			(cond
			((or (= (car r) 2) (= (car r) 3)) (let* ((l (getLocCols (cdr r) row (+ col 1))))
				(list (append (first l) (list(list row col))) (second l))))
			((= (car r) 4) (let* ((l (getLocCols (cdr r) row (+ col 1)))) (list (first l) (append (second l) (list(list row col))))))
			(t (getLocCols (cdr r) row (+ col 1)))
			)
	     
	   )
	)
)

; Description: This function takes a list of the keeper and boxes as l1 and a list of the goals as l2, and calculates the 
; minimum sum of the distance required for each keeper and box to reach a goal.
; Logic: This function calls getMin for each element in l1 to calculate its minimum distance to a goal, and then sums up 
; all these minimums.
(defun sumMin (l1 l2)
	(cond
		((null l1) 0)
		(t (+ (sumMin (rest l1) l2) (getMin l2 (first (first l1)) (second(first l1)) -1)))
	)
)

; Description: This function takes a list of the goals as l, the row and column of the box/keeper as r and c respectively,
; and the current minimum (starts at -1 as base case). It uses these parameters to calculate the minimum distance to a goal.
; minimum sum of the distance required for each keeper and box to reach a goal.
; Logic: This function calculates the distance between r and c, and each element in l, updating the minimum as it recurses through
; the list. Upon scanning through the entire list, it returns the minimum.
(defun getMin (l r c minimum)
	(cond 
		((null l) minimum)
		(t (let* ((row (first(first l))) (col (second(first l))))
			(let* ((rowSum (cond ((> (- r row) 0) (- r row)) (t (- row r))))
				(colSum (cond ((> (- c col) 0) (- c col)) (t (- col c)))))
				(cond 
					((or (< (+ rowSum colSum) minimum) (= minimum -1)) (getMin (rest l) r c (+ rowSum colSum)))
					(t (getMin (rest l) r c minimum))
				)
			)
		))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)

(setq s1 '((1 1 1 1 1)
	(1 4 0 4 1)
	(1 0 2 0 1)
	(1 0 3 0 1)
	(1 0 0 0 1)
	(1 1 1 1 1)))

(setq s2 '((1 1 1 1 1)
(1 0 0 4 1) (1 0 2 3 1) (1 0 0 0 1) (1 4 0 0 1) (1 1 1 1 1) ))

(setq s3 '((1 1 1 1 1)
(1 0 0 6 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 0 4 1) (1 1 1 1 1) ))

(setq s4 '((1 1 1 1 1)
(1 4 2 4 1) (1 0 0 0 1) (1 0 0 0 1) (1 4 5 3 1) (1 1 1 1 1) ))

(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
