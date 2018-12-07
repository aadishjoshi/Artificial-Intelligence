(solve (make-romanian-problem :initial-state 'Arad :goal 'Bucharest) 'A*-search)
(solve (make-romanian-problem :initial-state 'Arad :goal 'Bucharest) 'GREEDY-SEARCH)
(solve (make-romanian-problem :initial-state 'Arad :goal 'Bucharest) 'TREE-A*-SEARCH)
(solve (make-romanian-problem :initial-state 'Arad :goal 'Bucharest) 'TREE-IDA*-SEARCH)

(setq searchers '(A*-search GREEDY-SEARCH TREE-A*-SEARCH
			       TREE-IDA*-SEARCH))
(compare-search-algorithms #'make-romanian-problem searchers)