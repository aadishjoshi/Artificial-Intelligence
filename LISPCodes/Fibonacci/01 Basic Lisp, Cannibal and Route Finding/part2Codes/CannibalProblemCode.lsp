(solve(make-cannibal-problem
	:initial-state (make-cannibal-state :m1 3 :c1 3)))
(solve (make-cannibal-problem) 'breadth-first-search)
(solve (make-cannibal-problem) 'uniform-cost-search)
(solve (make-cannibal-problem) 'no-returns-breadth-first-search)
(solve (make-cannibal-problem) 'no-duplicates-breadth-first-search)
(solve (make-cannibal-problem) 'no-cycles-depth-first-search)

(setq searchers '(breadth-first-search
uniform-cost-search
no-returns-breadth-first-search
no-duplicates-breadth-first-search
no-cycles-depth-first-search))
(compare-search-algorithms #'make-cannibal-problem searchers)
(compare-search-algorithms #'make-cannibal-problem searchers :n 1)
