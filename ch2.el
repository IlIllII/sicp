;; NOTE: Rational Numbers
(defun linear-combination(a b x y)
  (add (mul a x) (mul b y)))


(defun make-rat(n d)
  (cons n d))

(defun numer(x)
  (car x))

(defun denom(x)
  (cdr x))

(defun add-rat(x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun sub-rat(x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun mul-rat(x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defun div-rat(x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))

(defun equal-rat-p(x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;; cons = construct, car = contents of address part of register, cdr = contents of decrement part of register (could-er)
(defvar x (cons 1 2))

(car x)

(cdr x)


(defvar y (cons 3 4))
(defvar z (cons x y))

z
(car z)
(cdr z)

(print z)
(car (car z))
(car (cdr z))
(cdr (cdr z))
(car (cdr z))

(defun print-rat(x)
  (print (concat
	  (prin1-to-string 'x)
	  ": "
	  (prin1-to-string (numer x))
	  "/"
	  (prin1-to-string (denom x)))))

(print-rat (make-rat 1 2))


(defvar one-half (make-rat 1 2))

(defvar one-third (make-rat 1 3))

(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))


(defun gcd(a b)
  (cond ((> b a) (gcd b a))
	((= (% a b) 0) b)
	(t (gcd b (% a b)))))

(gcd 12 24)

(defun make-rat(n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (make-rat 12 24))

(defun abs(x)
  (if (>= x 0)
      x
    (- x)))

;; 2.1
(defun make-rat-signed(n d)
  (let ((g (gcd (abs n) (abs d))))
    (if (or (and (= (abs n) n) (= (abs d) d)) (and (not (= (abs n) n)) (not (= (abs d) d))))
	(cons (/ (abs n) g) (/ (abs d) g))
      (cons (- (/ (abs n) g)) (/ (abs d) g)))))

(make-rat (- 1) 2)
(print-rat (make-rat (- 1) 2))

;; 2.2
(defun make-segment(point-a point-b)
  (cons point-a point-b))

(defun start-segment(line)
  (car line))

(defun end-segment(line)
  (cdr line))

(defun midpoint-segment(line)
  (make-point (/ (+ (x-point (start-segment line)) (x-point (end-segment line))) 2)
	      (/ (+ (y-point (start-segment line)) (y-point (end-segment line))) 2)))

(defun make-point(x y)
  (cons x y))

(defun x-point(point)
  (car point))

(defun y-point(point)
  (cdr point))

(defun print-point(p)
  (print (concat
	  "("
	  (prin1-to-string (x-point p))
	  ", "
	  (prin1-to-string (y-point p))
	  ")")))

(print-point (make-point 1 2))


(defun make-rect(p1 p2)
  (cons p1 p2))

(defun bottom-left(rect)
  (car rect))

(defun top-right(rect)
  (cdr rect))

(defun top-left(rect)
  (cons (x-point (bottom-left rect))
	(y-point (top-right rect))))

(defun bottom-right(rect)
  (cons (x-point (top-right rect))
	(y-point (bottom-left rect))))

(defun distance(p1 p2)
  (sqrt (+ (square (- (x-point p1) (x-point p2)))
	   (square (- (y-point p1) (y-point p2))))))

(defun square(a)
  (* a a))

(defun sqrt(a)
  (let ((tolerance 0.0001))
    (defun sqrt-helper(a guess)
      (print guess)
      (cond ((< (abs (- (square guess) a)) tolerance) guess)
	    (t (sqrt-helper a (/ (+ (/ a guess) guess) 2)))))
    (sqrt-helper a 2.0)))

(sqrt 4)
(sqrt 16)
(sqrt 20)


(defun perimeter(rect)
  (+ (distance (bottom-left rect) (top-left rect))
     (distance (top-left rect) (top-right rect))
     (distance (top-right rect) (bottom-right rect))
     (distance (bottom-right rect) (bottom-left rect))))

(defun area(rect)
  (* (distance (bottom-left rect) (top-left rect))
     (distance (bottom-left rect) (bottom-right rect))))

(defun my-cons(a b)
  (lambda(m)
    (cond ((= m 0) a)
	  ((= m 1) b)
	  (t (print "error")))))

(defun my-car(z)
  (funcall z 0))

(setq lexical-binding t)

(defun my-cdr(z)
  (funcall z 1))

(my-car (my-cons 1 2))
(my-cdr (my-cons 1 2))

(define (cons x y))

(defun new-cons(x y)
  (lambda (m) (funcall m x y)))

(defun new-car(z)
  (funcall z (lambda (p q) p)))

(new-car (new-cons 2 2))

(defun new-cdr(z)
  (funcall z (lambda (p q) q)))

(new-cdr (new-cons 2 5))

(defun exp-cons(a b)
  (* (expt 2 a) (expt 3 b)))

(defun exp-car(x)
  (defun car-iter(x count)
    (if (= 0 (% x 2))
	(car-iter (/ x 2) (+ count 1))
      count))
  (car-iter x 0))

(defun exp-cdr(x)
  (defun cdr-iter(x count)
    (if (= 0 (% x 3))
	(cdr-iter (/ x 3) (+ count 1))
      count))
  (cdr-iter x 0))

(exp-cons 2 3)
(exp-car (exp-cons 2 3))
(exp-cdr (exp-cons 2 3))

(defvar zero (lambda (f) (lambda (x) x)))

(defun add-one(n)
  (lambda (f) (lambda (x) (funcall f ((funcall n f) x)))))

(add-one zero)

(defun add-interval(x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval(x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval(x y)
  (if (spans-zero y)
      (error "Can't divide intervals by zero!"))
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

(defun make-interval(a b)
  (cons a b))

(defun upper-bound(interval)
  (cdr interval))

(defun lower-bound(interval)
  (car interval))

(defun sub-interval(x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

(defun spans-zero(interval)
  (and (< (lower-bound interval) 0) (> (upper-bound interval 0))))

(defun width(interval)
  (/ (- (upper-bound interval) (lower-bound interval)) 2.0))

(width (sub-interval
	(make-interval 2 4)
	(make-interval 4 6)))

(print-interval (div-interval
		 (make-interval 1 2)
		 (make-interval 1 2)))

(defun print-interval(interval)
  (print (concat "["
		 (int-to-string (lower-bound interval))
		 " - "
		 (int-to-string (upper-bound interval))
		 "]")))


(defun make-center-width(c w)
  (make-interval (- c w) (+ c w)))

(defun center(i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(defun make-center-percent(c p)
  (make-interval (- c (* c p)) (+ c (* c p))))

(defun percent(i)
  (/ (abs (- (upper-bound i) (center i))) (center i)))


(cons 1 (cons 2 (cons 3 (cons 4 nil)))) ;; this is a list
(list 1 2 3 4) ;; the same thing


(defun list-ref (items n)
  (if (= n 0)
      (car items)
    (list-ref (cdr items) (- n 1))))

(defvar squares (list 1 4 9 16 25))
(list-ref squares 3)

(defun length (items)
  (if (null items)
      0
    (+ 1 (length (cdr items)))))

(length squares)

(defun length-iter (items)
  (defun len-inner (a count)
    (if (null a)
	count
      (len-inner (cdr a) (+ 1 count))))
  (len-inner items 0))

(length-iter squares)

(defun append-list (list1 list2)
  (if (null list1)
      list2
    (cons (car list1) (append-list (cdr list1) list2))))

(defvar odds (list 1 3 5 7 9))

(append-list squares odds)

(defun last-pair (of)
  (if (null (cdr of))
      of
    (last-pair (cdr of))))

(last-pair odds)

(defun reverse-list (to-reverse)
  (defun reverse-iter (original reversed)
    (if (null original)
	reversed
      (reverse-iter (cdr original) (cons (car original) reversed))))
  (reverse-iter to-reverse '()))

(reverse-list odds)

(defun cc (amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-more? coin-values)) 0)
	(t
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))


(defun except-first-denomination (denominations)
  (cdr denominations))

(defun first-denomination (denominations)
  (car denominations))

(defun no-more? (denominations)
  (null denominations))

(defvar denominations (list 50 25 10 5 1))

(cc 100 denominations)

(cc 100 (reverse-list denominations))

;; TODO exercise 2.20


(defun takes-a-list (param1 param2 &rest param3)
  (print param3))

(takes-a-list 1 2 3 4 5 6 7 8 9)

(defun same-parity (first &rest rest)
  (defun parity-iter (par rest results)
    (if (null rest)
	results
      (parity-iter par
		   (cdr rest)
		   (if (= par (% (car rest) 2))
		       (append-list results `(,(car rest)))
		     results))))
  (parity-iter (% first 2) rest '()))

(same-parity 0 2 3 4 5 6 7 8 9 10)


(defun scale-list (items factor)
  (if (null items)
      nil
    (cons (* (car items) factor)
	  (scale-list (cdr items) factor))))

(scale-list (list 0 1 2 3 4 5 6 7 8 9 10) 10)

(defun j-map (proc items)
  (if (null items)
      nil
    (cons (funcall proc (car items))
	  (j-map proc (cdr items)))))

(j-map (lambda (x) (+ x 2)) (list 0 1 2 3 4 5))

(defun sq (x)
  (* x x))

(defun square-list (items)
  (if (null items)
      nil
    (cons (sq (car items)) (square-list (cdr items)))))

(defun square-list-2 (items)
  (j-map 'sq items))

(square-list (list 1 2 3 4 5))
(square-list-2 (list 1 2 3 4 5))

(defun for-each (procedure items)
  (if (null items)
      nil
    (progn (funcall procedure (car items))
	   (for-each procedure (cdr items)))))

(for-each 'print (list 1 2 3 4 5 6 7))

(cons (list 1 2) (list 3 4))

(defvar x-list (cons (list 1 2) (list 3 4)))

(defun count-leaves (items)
  (cond ((null items) 0)
	((atom items) 1)
	(t (+ (count-leaves (car items))
	      (count-leaves (cdr items))))))

(not (atom '(1 . 2)))
(not (atom 2))

(length x-list)
(count-leaves x-list)
(list x-list x-list)
(length (list x-list x-list))
(count-leaves (list x-list x-list))

(list 1 (list 2 (list 3 4)))
(list (list (list (list 1 2 3 4))))

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
(car (car '((7))))

(defvar xx (list 1 2 3))
(defvar yy (list 4 5 6))

;; NOTE Interesting!

(append xx yy) ;; (1 2 3 4 5 6)
(cons xx yy) ;; ((1 2 3) 4 5 6)
(list xx yy) ;; ((1 2 3) (4 5 6))

;; TODO Exercise 2.27


(defun reverse-list-deep (to-reverse)
  (defun reverse-iter (original reversed)
    (if (null original)
	reversed
      (reverse-iter
       (cdr original)
       (cons (if (listp (car original))
		 (reverse-list-deep (car original))
	       (car original))
	     reversed))))
  (reverse-iter to-reverse '()))


(defvar listx (list (list 1 2) (list 3 4)))

(reverse-list listx)
(reverse-list-deep listx)

;; TODO Ex 2.28

(defun fringe (x)
  (let ((res '()))
      (defun fringe-iter (x)
	(if (null (car x))
	    nil
	  (progn
	    (if (not (atom (car x)))
		(fringe-iter (car x))
	      (setq res (append res (list (car x)))))
	    (fringe-iter (cdr x)))))
      (fringe-iter x)
      res))

(defvar xxx (list (list 1 2) (list 3 4)))
(defvar xxxx (list xxx xxx))

(append (list 1 2) (list 1))

(fringe xxx)
(fringe xxxx)

(defun make-mobile (left right)
  (list left right))

(defun make-branch (length structure)
  (list length structure))

(defun left-branch (binary-mobile)
  (car binary-mobile))

(defun right-branch (binary-mobile)
  (cdr binary-mobile))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (cdr branch))

(defun total-weight (binary-mobile)
  (let ((res 0))
    (defun check-weight-or-recur (structure)
      (if (atom structure)
	  (setq res (+ res structure))
	(total-weight-helper structure)))
    (defun total-weight-helper (binary-mobile)
      (let ((left (branch-structure (left-branch binary-mobile)))
	    (right (branch-structure (right-branch binary-mobile))))
	(check-weight-or-recur left)
	(check-weight-or-recur right)))
    (total-weight-helper binary-mobile)
    res))

;; TODO Ex 2.30

(defun scale-tree (tree factor)
  (cond ((null tree) nil)
	((atom tree) (* tree factor))
	(t (cons (scale-tree (car tree) factor)
		 (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 2)

(defun scale-tree-map (tree factor)
  (j-map (lambda (sub-tree)
	   (if (not (atom sub-tree))
	       (scale-tree-map sub-tree factor)
	     (* sub-tree factor)))
	 tree))

(scale-tree-map (list 1 (list 2 (list 3 4) 5) (list 6 7)) 2)

(defun square-tree-no-recur (tree)
  (cond ((null tree) nil)
	((atom tree) (square tree))
	(t (cons (square-tree-no-recur (car tree))
		 (square-tree-no-recur (cdr tree))))))

(defun square-tree (tree)
  (j-map (lambda (subtree)
	   (if (not (atom subtree))
	       (square-tree subtree)
	     (square subtree)))
	 tree))

(square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(square-tree-no-recur (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(defun tree-map (func tree)
  (cond ((null tree) nil)
	((atom tree) (funcall func tree))
	(t (cons (tree-map func (car tree))
		 (tree-map func (cdr tree))))))


(tree-map #'square (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(defun subsets (s)
  (if (null s)
      (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (j-map (lambda (rest)
			   (cons (car s) rest))
			 rest)))))

(subsets (list 1 2 3))

(defun is-odd (n)
  (= (% n 2) 1))

(defun sum-odd-squares (tree)
  (cond ((null tree) 0)
	((atom tree) (if (is-odd tree) (square tree) 0))
	(t (+ (sum-odd-squares (car tree))
	      (sum-odd-squares (cdr tree))))))

(sum-odd-squares (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(defun is-even (n)
  (not (is-odd n)))



(defun even-fibs (n)
  (defun next (k)
    (if (> k n)
	nil
      (let ((f (fib k)))
	(if (is-even f)
	    (cons f (next (+ k 1)))
	  (next (+ k 1))))))
  (next 0))

(even-fibs 5)

(defun filter (predicate sequence)
  (cond ((null sequence) nil)
	((funcall predicate (car sequence)) (cons (car sequence)
					  (filter predicate (cdr sequence))))
	(t (filter predicate (cdr sequence)))))

(filter #'is-odd (list 1 2 3 4 5 6 7))

(defun accumulate (op initial sequence)
  (if (null sequence)
      initial
    (funcall op (car sequence)
	     (accumulate op initial (cdr sequence)))))

(accumulate #'+ 0 (list 1 2 3 4 5))

(accumulate #'+ 0 (filter #'is-odd (list 1 2 3 4 5)))

(defun enumerate-interval (lo hi)
  (if (> lo hi)
      nil
    (cons lo (enumerate-interval (+ 1 lo) hi))))

(enumerate-interval 1 7)

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(t (append (enumarate-tree (car tree))
		   (enumarate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))


(defun sum-odd-sq (tree)
  (accumulate #'+ 0 (j-map #'square (filter #'is-odd (enumerate-tree tree)))))

(sum-odd-squares (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(sum-odd-sq (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; TODO 2.33
