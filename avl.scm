#lang scheme
;; avl tree by shd101wyy
;; string>? string<? string=? compare string

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
          (begin (set-right temp (left node2))
                 (set-left node2 temp)))
      ;; update height
      (set-height temp (+ 1 (max (height (left temp)) (height (right temp)))))
      (set-height node2 (+ 1 (max (height (left temp)) (height (right temp)))))
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
          (begin (set-left temp (right node2))
                 (set-right node2 temp)))
          ;; update height
      (set-height temp (+ 1 (max (height (left temp)) (height (right temp)))))
      (set-height node2 (+ 1 (max (height (left temp)) (height (right temp)))))
      ))

  ;; rotate rightLeft
  (define (rotate-right-left node)
    (rotate-right (right node))
    (rotate-left node))
  ;; rotate leftRight
  (define (rotate-left-right node)
    (rotate-left (left node))
    (rotate-right node))
  
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
	    (begin (if (string<? k (key node))
		       (begin ;; left  
			 (insert_ (left node)
				  k
				  value
				  node
				  #t)
			 (letrec ([balance (- (height (left node ))
					      (height (right node)))]
				  [left-balance (- (height (left (left node)))
                                                   (height (right (left node))))])
				  (if (= balance 2)
				      (if (= left-balance 1)
					  (rotate-right node)
					  (rotate-left-right node))
                                      '())))
		       (begin ;; right  
			 (insert_ (right node)
				  k
				  value
				  node
				  #f)
			 (letrec ([balance (- (height (left node))
					      (height (right node)))]
				  [right-balance (- (height (left (right node)))
                                                    (height (right (right node))))])
				  (if (= balance 2)
				      (if (= right-balance 1)
					  (rotate-left node)
					  (rotate-right-left node))
                                      '()))))  
		   ;; update height
                   (set-height node (+ 1 (max (height (left node)) (height (right node)))))
                   ))))  
  (lambda [msg]
    (cond [(eq? msg 'insert)
	   (lambda [key value]
	     (insert key value))]
	  [(eq? msg 'ref)
	   (lambda [key]
	     (find key))]
          [(eq? msg 'get-root)
           (lambda [] root)])))


;; make avl tree
(define x (make-avl-tree))
;; insert 
((x 'insert) "Hi" 3)
((x 'insert) "B" 12)
((x 'insert) "A" 15)
((x 'insert) "I" 16)
((x 'insert) "J" 17)
;; get
(display ((x 'ref) "Hi"))
(display ((x 'ref) "A"))
(display ((x 'ref) "B"))
(display ((x 'ref) "I"))
(display ((x 'ref) "J"))
;;(display (( ((x 'get-root)) 'left)))

























