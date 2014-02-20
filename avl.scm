#lang scheme
;; avl tree by shd101wyy
;; string>? string<? string=? compare string
;; simple avl tree
;; ref set remove 
(define (make-avl-tree)
  ;; define node
  (define (Node k v)
    (define key k)
    (define value v)
    (define left '())
    (define right '())
    (define height 0)
    (lambda [msg]
      (cond [(eq? msg 'get-key)
	     (lambda [] key)]
	    [(eq? msg 'get-value)
	     (lambda [] value)]
	    [(eq? msg 'left)
	     (lambda [] left)]
	    [(eq? msg 'right)
	     (lambda [] right)]
	    [(eq? msg 'set)
	     (lambda [k v]
	       (set! key v)
	       (set! value v))]
	    [(eq? msg 'set-left)
	     (lambda [l]
	       (set! left l))]
	    [(eq? msg 'set-right)
	     (lambda [r]
	       (set! right r))]
	    [(eq? msg 'height)
	     (lambda [] height)]
	    [(eq? msg 'set-height)
	     (lambda [h] (set! height h))])))
  
  ;; getter
  (define (left node) (vector-ref node 2))
  (define (right node) (vector-ref node 3))
  (define (key node) (vector-ref node 0))
  (define (value node) (vector-ref node 1))
  (define (height node) (if (null? node) -1 (vector-ref node 4)))
  (define (parent node) (vector-ref node 5))
  ;; setter
  (define (set-left node v) (vector-set! node 2 v) (set-parent v node))
  (define (set-right node v) (vector-set! node 3 v) (set-parent v node))
  (define (set-key node v) (vector-set! node 0 v))
  (define (set-value node v) (vector-set! node 1 v))
  (define (set-height node v) (vector-set! node 4 v))
  (define (set-parent node v) (if (null? node) 'done (vector-set! node 5 v)))
  ;; make node
  (define (make-node key value left right parent)
    (vector key value left right (+ 1 (max (height left) (height right)))  parent))
  ;; make root
  (define (make-root key value) (vector key value '() '() 0 '()))
  
  
  
  (define root '())

  (define (find k)
    (find_ root k))
  ;; find value by key
  (define (find_ node k)
    (if (null? node)
	'() ;; didn't find
	(let ([node-key (key node)])
	  (if (string=? node-key k)
	      (value node)
	      (if (string<? k node-key)
		  (find_ (left node) ;; < so find left
			 k)
		  (find_ (right node) ;; > so find right
			 k))))
	))
 
  ;; rotate left
  ;;
  ;; parent                      parent
  ;;    \                             \
  ;;      temp     =>   
  ;;      /  \ 
  ;; left_c   node2
  ;;             \
  ;;              right
   
  (define (rotate-left node)
    (letrec ([temp node]
	     [node2 (right node)]
	     )
      (if (null? (parent temp)) ;; try to rotate root
          (begin (set! root node2) ;; update root
                 (set-parent root '())
                 (set-right temp (left node2))
                 (set-left node2 temp)
                 ) 
          ;; parent is not root
          (begin (set-right (parent node) node2) 
                 (set-right temp (left node2))
                 (set-left node2 temp)))
      ;; parent set child
      ;;(set-right (parent temp) node2)
      ;; update height
      (set-height temp (+ 1 (max (height (left temp)) (height (right temp)))))
      (set-height node2 (+ 1 (max (height (left node2)) (height (right node2)))))
      ))
  ;; rotate right
  (define (rotate-right node)
    (letrec ([temp node]
	     [node2 (left node)])
       (if (null? (parent temp)) ;; try to rotate root
          (begin (set! root node2) ;; update root
                 (set-parent root '())
                 (set-left temp (right node2))
                 (set-right node2 temp)
                 ) 
          ;; parent is not root
          (begin (set-left (parent node) node2) 
                 (set-left temp (right node2))
                 (set-right node2 temp)))
      ;; parent set child
      ;;(set-left (parent temp) node2)
      ;; update height
      (set-height temp (+ 1 (max (height (left temp)) (height (right temp)))))
      (set-height node2 (+ 1 (max (height (left node2)) (height (right node2)))))
      ))

  ;; rotate rightLeft
  (define (rotate-right-left node)
    (rotate-right (right node))
    (rotate-left node))
  ;; rotate leftRight
  (define (rotate-left-right node)
    (rotate-left (left node))
    (rotate-right node))
  ;; balance node
  (define (balance node)
    (let ([balance# (- (height (left node))
                       (height (right node)))])
      (cond [(= balance# 2) ;; adjust left side
             (if (= 1 
                    (- (height (left (left node)))
                       (height (right (left node)))))
                 (rotate-right node)
                 (rotate-left-right node)
                 )]
            [(= balance# -2) ;; adjust right side
             (if (= -1
                    (- (height (left (right node)))
                       (height (right (right node)))))
                 (rotate-left node)
                 (rotate-right-left node)
                 )]
            [else ;; no need to adjust 
             '()])
      ))
  ;; insert
  (define (insert key value)
    (insert_ root key value '() #t))
  (define (insert_ node k value parent left?)
    (if (null? root) ;; root doesn't exist
	(set! root (make-root k value)) ;; set root	
	(if (null? node) ;; node doesn't exist
	    (if left? 
		(set-left  parent (make-node k value '() '() parent)) ;; set left
		(set-right parent (make-node k value '() '() parent))) ;; set right
            (if (string=? k (key node))
                ;; same
                (set-value node value)
                ;; not same
                (begin (if (string<? k (key node))
                           (begin ;; left  
                             (insert_ (left node)
                                      k
                                      value
                                      node
                                      #t)
                             (balance node))
                           (begin ;; right  
                             (insert_ (right node)
                                      k
                                      value
                                      node
                                      #f)
                             (balance node)))  
                       ;; update height
                       (set-height node (+ 1 (max (height (left node)) (height (right node)))))
                       )
                )
            ))) 
  
  (define (find-biggest-on-left-side node)
    (if (null? (right node))
        node
        (find-biggest-on-left-side (right node))))
  ;; remove
  (define (remove k)
    (remove_ k root))
  (define (remove_ k node)
    (if (null? node)
        '() ;; didn't remove, key doesnt exist
        (if (eq? (key node) k)
            ;; find
            (let [(null-left? (null? (left node)))
                  (null-right? (null? (right node)))]
              (cond [(and null-left? null-right?) ;; empty
                     (if (null? (parent node))
                         (set! root '()) ;; reset root
                         (if (eq? (left (parent node)) node)
                             (set-left (parent node) '()) ;; delete this node, left
                             (set-right (parent node) '())))] ;; right 
                    ;; has one child on the left side
                    [null-right? 
                     (if (null? (parent node))
                         ;; update root
                         (begin (set! root (left node))
                                (set-parent root '()))
                         (if (eq? (left (parent node)) node)
                             (set-left (parent node) (left node))
                             (set-right (parent node) (left node)))
                         )
                     ]
                    ;; has one child on the right side
                    [null-left? 
                     (if (null? (parent node))
                         ;; update root
                         (begin (set! root (right node))
                                (set-parent root '()))
                         (if (eq? (left (parent node)) node)
                             (set-left (parent node) (right node))
                             (set-right (parent node) (right node)))
                         )]
                    [else ;; has two child.    this part unfinished
                     (let [(y (find-biggest-on-left-side (left node)))]
                       (set-left y (left node)) ;; set children
                       (set-right y (right node)) ;; set children
                       (set-parent y (parent node)) ;; set parent
                       (if (null? (parent node))
                           ;; reset root
                           (set! root y)
                           ;; it's not root
                           (if (eq? node (left (parent node)))
                               [begin  ;; node is left child of parent
                                 (set-left (parent node)
                                           y)
                                 (balance y)]
                               [begin  ;; node is right child of parent
                                 (set-right (parent node)
                                            y)
                                 (balance y)]))
                       )]))
            ;; unfound
            (if (string<? k (key node))
                (remove_ k (left node))  ;; search left
                (remove_ k (right node)));; search right
            ))
    )
  
  (lambda [msg]
    (cond [(eq? msg 'set)
	   (lambda [key value]
	     (insert key value))]
	  [(eq? msg 'ref)
	   (lambda [key]
	     (find key))]
          [(eq? msg 'remove)
           (lambda [key]
             (remove key))]
          [(eq? msg 'get-root)
           (lambda [] root)])))


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
;;(display (( ((x 'get-root)) 'left)))

























