(setq p1 (make-romanian-problem :initial-state 'Bucharest :goal 'Lugoj))
(solve (make-romanian-problem :initial-state 'Bucharest :goal 'Lugoj) 'A*-search)
(solve (make-romanian-problem :initial-state 'Bucharest :goal 'Lugoj) 'TREE-A*-SEARCH)
(solve (make-romanian-problem :initial-state 'Bucharest :goal 'Lugoj) 'TREE-IDA*-SEARCH)
(setq searchers '(A*-search TREE-A*-SEARCH
			       TREE-IDA*-SEARCH))
(compare-search-algorithms #'(lambda () p1)
			      searchers :n 1)