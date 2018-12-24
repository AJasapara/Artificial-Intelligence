; Description: This function loads the "parse_cnf.lsp" file.
; Logic: It calls load on "parse_cnf.lsp".
(defun load-cnf()
	(load "parse_cnf.lsp")
)

; Description: This function determines if delta is satisfiable by a list
; of literals from 1 to n.
; Logic: This function calls nextElement to get a valid assignment, which 
; it fills by calling completeRes, or it gets nil, which means that delta 
; has a contradiction.
(defun sat? (n delta)
	(let* ((res (nextElement (first delta) (rest delta))))
		(cond
			((null res) nil)
			(t (completeRes 1 n res res))
		)
	)
)

; Description: This function checks all of delta and removes the clauses 
; that the element satisfies. Thus, this function eventually determines 
; if the element belongs in the result set or not.
; Logic: This function checks all of delta and removes clauses that 
; the element satisfies, and adds the clauses it doesn't to delta_new 
; so that the next element can check the rest.
(defun clauses (element delta delta_new)
	(cond
		((null delta) (cond
			((null delta_new) (list element))
			(t (let* ((next (nextElement (first delta_new) (rest delta_new))))
				(cond
					((null next) nil)
					(t (append (list element) next))
				)
			))
		))
		(t (let* ((found (elementSearch element (first delta))))
			(cond
				((= found element) (clauses element (rest delta) delta_new))
				((= found 0) (clauses element (rest delta) (append delta_new (list (first delta)))))
				(t (let* ((negative (removeNegative found (first delta))))
					(cond
						((null negative) nil)
						(t (clauses element (rest delta) (append delta_new (list negative))))
					)
				))
			)
		))
	)
)

; Description: This function completes the result set with arbitrary values
; for the literals that did not need to have a specific assignment in delta.
; Logic: This function checks res for each i till n, and when it doesn't find 
; it, it adds that i to final_res.
(defun completeRes (i n res final_res)
	(cond
		((> i n) final_res)
		((null res) (completeRes (+ i 1) n (append final_res (list i)) (append final_res (list i))))
		((or (= i (first res)) (= (- 0 i) (first res))) (completeRes (+ i 1) n final_res final_res))
		(t (completeRes i n (rest res) final_res))
	)
)

; Description: This function checks if the next element in the clause needs to be
; called or if a satisfactory result was found.
; Logic: This function calls clauses on each element if it returns an unsuccessful pairing.
; If it runs through the whole clause, it returns nil to indicate that it could not 
; find one.
(defun nextElement (l delta_new)
	(cond
		((null l) nil)
		(t (let* ((res (clauses (first l) delta_new '())))
			(cond
				((null res) (nextElement (rest l) delta_new))
				(t res)
			)
		))
	)
)

; Description: This function searches for element in l.
; Logic: This function returns 0 if the element isn't found,
; element if it is, and negative element if the negative 
; version of element is found.
(defun elementSearch (element l)
	(cond
		((null l) 0)
		((= (first l) element) element)
		((= (first l) (- 0 element)) (- 0 element))
		(t (elementSearch element (rest l)))
	)
)

; Description: This function removes the indicated element in l.
; Logic: This function runs through the list, and upon finding 
; the first occurrence of element, returns the rest of the list.
(defun removeNegative (element l)
	(cond
		((= (first l) element) (rest l))
		(t (append (list(first l)) (removeNegative element (rest l))))
	)
)


; GENERAL TESTING FUNCTIONS
; These functions are not part of the homework but are used to test the validity of my solutions.
; Description: This function checks if res does not violate delta.
; Logic: This function calls solutionHelper on the first value in res and delta to get the 
; delta without the clauses satisfied by each element. If delta runs out, res is valid. If res
; runs out, res is invalid.
(defun testSolution (res delta)
	(cond
		((null delta) t)
		((null res) nil)
		(t (testSolution (rest res) (solutionHelper (first res) delta '())))
	)
)

; Description: This function checks all of delta for the given element, removing 
; clauses that are satisfied by the element.
; Logic: This function calls elementSearch to determine if an element was found. If 
; so, call solutionHelper on the rest of delta. If not, add that clause to the resulting 
; delta_res, and call solutionHelper on the rest of delta.
(defun solutionHelper(element delta delta_res)
	(cond
		((null delta) delta_res)
		(t (let* ((found (elementSearch element (first delta))))
			(cond
				((= element found) (solutionHelper element (rest delta) delta_res))
				(t (solutionHelper element (rest delta) (append delta_res (list (first delta)))))
			)
		))
	)
)