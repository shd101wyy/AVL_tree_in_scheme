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

  (define root '())

  (define (find key)
    (find_ root key))
  ;; find value by key
  (define (find_ node key)
    (if (null? node)
	'() ;; didn't find
	(let ([node-key ((node 'get-key))])
	  (if (string=? node-key key)
	      ((node 'get-value))
	      (if (string<? key node-key)
		  (find_ ((node 'left)) ;; < so find left
			 key)
		  (find_ ((node 'right)) ;; > so find right
			 key))))
	))
  (define (heightOrNeg1 node)
    (if (null? node)
	-1
	((node 'height))))
  ;; rotate left
  (define (rotate-left node)
    (letrec ([temp node]
	     [node ((node 'right))]
	     )
      ((temp 'set-right) ((node 'left)))
      ((node 'set-left) temp)
      ;; update height
      (letrec ([max1 (if (> (heightOrNeg1 ((temp 'left)))
			    (heightOrNeg1 ((temp 'right))))
			 (heightOrNeg1 ((temp 'left)))
			 (heightOrNeg1 ((temp 'right))))]
	       [max2 (if (> (heightOrNeg1 ((node 'left)))
			    (heightOrNeg1 ((node 'right))))
			 (heightOrNeg1 ((node 'left)))
			 (heightOrNeg1 ((node 'right))))]
	       )
	((temp 'set-height) (+ max1 1))
	((node 'set-height) (+ max2 1))
	) 
      ))
  ;; rotate right
  (define (rotate-right node)
    (letrec ([temp node]
	     [node ((node 'left))])
      ((temp 'set-left) ((node 'right)))
      ((node 'set-right) temp)
      ;; update height
      (letrec ([max1 (if (> (heightOrNeg1 ((temp 'left)))
			    (heightOrNeg1 ((temp 'right))))
			 (heightOrNeg1 ((temp 'left)))
			 (heightOrNeg1 ((temp 'right))))]
	       [max2 (if (> (heightOrNeg1 ((node 'left)))
			    (heightOrNeg1 ((node 'right))))
			 (heightOrNeg1 ((node 'left)))
			 (heightOrNeg1 ((node 'right))))]
	       )
	((temp 'set-height) (+ max1 1))
	((node 'set-height) (+ max2 1))
	) 
      ))
  ;; rotate rightLeft
  (define (rotate-right-left node)
    (rotate-right ((node 'right)))
    (rotate-left node))
  ;; rotate leftRight
  (define (rotate-left-right node)
    (rotate-left ((node 'left)))
    (rotate-right node))
  
  ;; insert
  (define (insert key value)
    (insert_ root key value '() #t))
  (define (insert_ node key value parent left?)
    (if (null? root) ;; root doesn't exist
	(set! root (Node key value))	
	(if (null? node) ;; node doesn't exist
	    (if left? 
		((parent 'set-left) (Node key value)) ;; set left
		((parent 'set-right) (Node key value))) ;; set right
	    (begin (if (string<? key ((node 'get-key)))
		       (begin ;; left  
			 (insert_ ((node 'left))
				  key
				  value
				  node
				  #t)
			 (letrec ([balance (- (heightOrNeg1 ((node 'left)))
					      (heightOrNeg1 ((node 'right))))]
				  [left-balance (- (heightOrNeg1 ((((node 'left)) 'left)))
                                                   (heightOrNeg1 ((((node 'left)) 'right))))])
				  (if (= balance 2)
				      (if (= left-balance 1)
					  (rotate-right node)
					  (rotate-left-right node))
                                      '())))
		       (begin ;; right  
			 (insert_ ((node 'right))
				  key
				  value
				  node
				  #f)
			 (letrec ([balance (- (heightOrNeg1 ((node 'left)))
					      (heightOrNeg1 ((node 'right))))]
				  [right-balance (- (heightOrNeg1 ((((node 'right)) 'left)))
                                                    (heightOrNeg1 ((((node 'right)) 'right))))])
				  (if (= balance 2)
				      (if (= right-balance 1)
					  (rotate-left node)
					  (rotate-right-left node))
                                      '()))))  
		   ;; update height
		   (let ([max (if (> (heightOrNeg1 ((node 'left)))
				     (heightOrNeg1 ((node 'right))))
				  (heightOrNeg1 ((node 'left)))
				  (heightOrNeg1 ((node 'right))))])
			 ((node 'set-height) (+ max 1)))))))  
  (lambda [msg]
    (cond [(eq? msg 'insert)
	   (lambda [key value]
	     (insert key value))]
	  [(eq? msg 'ref)
	   (lambda [key]
	     (find key))])))


;; make avl tree
(define x (make-avl-tree))
;; insert 
((x 'insert) "Hi" 3)
((x 'insert) "A" 12)
((x 'insert) "B" 15)
;; get
(display ((x 'ref) "Hi"))
(display ((x 'ref) "A"))
(display ((x 'ref) "BBB"))

