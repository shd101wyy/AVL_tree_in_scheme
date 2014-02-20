Simple AVL tree by Yiyi Wang

;; make avl tree
(define x (make-avl-tree))
;; insert 
((x 'set) "Hi" 3)
((x 'set) "B" 12)
((x 'set) "A" 15)
((x 'set) "I" 16)
((x 'set) "J" 17)
((x 'set) "ASD" 122)
((x 'set) "Hi" 666) ;; change hi value
;; remove
((x 'remove) "J")
;; ref
(display ((x 'ref) "Hi"))
(display ((x 'ref) "A"))
(display ((x 'ref) "B"))
(display ((x 'ref) "I"))
(display ((x 'ref) "J"))
(display ((x 'ref) "A"))
(display ((x 'ref) "ASD"))