; Arpit Jasapara
; Computer Science 161
; Professor Adnan Darwiche
; Homework 1

; Overall Solution Explanation: For all of my solutions (except SPLIT-LIST) I have an overall conditional 
; with base cases and recursive cases. The base cases account for the end of the recursive sequence where 
; the list has reached its final few elements or the list was small to begin with. The recursive sequences 
; reduce the given arguments to the base case while collecting basic information to return the final value.
; SPLIT-LIST is the only solution that does not need a recursive case since we only split the list once, 
; and return the list of two lists.

; Arguments: N is a number that we check for, 
;			TREE is the list representation of the tree that we are searching through.
; Return Value: This function returns T for true if N is found in TREE,
;				otherwise it returns NIL for false.

(defun TREE-CONTAINS (N TREE) 
	(cond 
		((NULL TREE) NIL)
		((numberp TREE) (= TREE N))
		((= (length TREE) 1) (= (first TREE) N))
		((= N (second TREE)) T)
		((> N (second TREE)) (TREE-CONTAINS N (third TREE)))
		(t (TREE-CONTAINS N (first TREE)))
	)
)

; Arguments: TREE is the list representation of the tree that we are searching through for the minimum.
; Return Value: This function returns the minimum element if TREE is not NIL,
;				otherwise it returns NIL.
(defun TREE-MIN (TREE) 
	(cond 
		((NULL TREE) NIL)
		((numberp TREE) TREE)
		(t (TREE-MIN (first TREE)))
	)
)

; Arguments: TREE is the list representation of the tree that we are traversing through in preoder fashion.
; Return Value: This function returns a list representing the preorder traversal of TREE if not NIL,
;				otherwise it returns NIL.
(defun TREE-ORDER (TREE) 
	(cond 
		((NULL TREE) NIL)
		((numberp TREE) (list TREE))
		(t (append (list (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))
	)
)

; Arguments: L is the list that we are finding a sub-list for, 
;			START is the start index where the sub-list will begin,
;			LEN is the length that the sub-list will be.
; Return Value: This function returns a list representing the sub-list of L from index START with length LEN if LEN is not 0,
;				otherwise it returns NIL for false.
(defun SUB-LIST (L START LEN) 
	(cond 
		((= 0 LEN) NIL)
		((and (= 0 START) (= 1 LEN)) (list(first L)))
		((= 0 START) (append (list(first L)) (SUB-LIST (rest L) START (- LEN 1))))
		(t (SUB-LIST (rest L) (- START 1) LEN))
	)
)

; Arguments: L is the list that we are splitting roughly in half to create a list of the two halves.
; Return Value: This function returns a list of L1 and L2 where the result of (append L1 L2) is L,
;				and the length of L1 minus length of L2 is 0 or 1.
(defun SPLIT-LIST (L) 
	(cond
		((evenp (length L)) (cons (SUB-LIST L 0 (/ (length L) 2)) (list (SUB-LIST L (/ (length L) 2) (/ (length L) 2)))))
		(t (cons (SUB-LIST L 0 (/ (+ (length L) 1) 2)) (list(SUB-LIST L (/ (+ (length L) 1) 2) (/ (- (length L) 1) 2)))))
	)
)

; Arguments: TREE is the list representation of the tree that we are searching through for its height.
; Return Value: This function returns the height of TREE, as defined by the distance from its root
;				to its furthest leaf.
; NOTE: L stands for the left branch of the TREE node and R stands for the right branch of the TREE node.
; The variable names were shortened for readability purposes.
(defun BTREE-HEIGHT (TREE) 
	(cond
		((atom TREE) 0)
		(t (let ((L (BTREE-HEIGHT (first TREE))) (R (BTREE-HEIGHT (second TREE))))
			(cond 
				((> L R)  (+ L 1))
				(t (+ R 1))
			)
		))
	)
)

; Arguments: LEAVES is the list representation of a collection of leaf nodes.
; Return Value: This function returns a list that represents a binary tree indicated by LEAVES where 
;				the number of leaves in any internal node's left branch (the first element in the list)
;				minus the number of leaves in its right branch (the second element in the list) is 0 or 1.
(defun LIST2BTREE (LEAVES) 
	(cond
		((= (length LEAVES) 1) (first LEAVES))
		((<= (length LEAVES) 2) LEAVES)
		(t (cons (LIST2BTREE(first(SPLIT-LIST LEAVES))) (cons (LIST2BTREE(second(SPLIT-LIST LEAVES))) NIL)))
	)
)

; Arguments: TREE is the list representation of a binary tree where internal lists represent branches and internal nodes.
; Return Value: This function returns a list that flattens TREE from a binary tree into a collection 
;				of leaf nodes (represented as a list of atoms).
(defun BTREE2LIST (TREE) 
	(cond
		((atom TREE) (list TREE))
		((and (not (listp (first TREE))) (not (listp (second TREE)))) TREE)
		((atom (second TREE)) (append (BTREE2LIST (first TREE)) (list (second TREE))))
		(t (append (BTREE2LIST (first TREE)) (BTREE2LIST (second TREE))))
	)
)

; Arguments: E1 is the first LISP expression whose atoms are all numbers,
;			E2 is the second such LISP expression, which the function determines if it is identical to E1.
; Return Value: This function returns T for true if E1 is identical to E2 based on the '=' operator,
;				otherwise it returns NIL for false.
(defun IS-SAME (E1 E2) 
	(cond
		((and (NULL E1) (NULL E2)) t)
		((or (NULL E1) (NULL E2)) NIL)
		((and (atom E1) (atom E2) (= E1 E2)) t)
		((and (listp E1) (listp E2)) (and (IS-SAME (first E1) (first E2)) (IS-SAME (rest E1) (rest E2))))
		(t NIL)
	)
)