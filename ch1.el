;; (exp 4 2) =>

(defun fast-exp(a b n)
  (cond ((= n 0) a)
	((is-even n) (fast-exp a (* b b) (/ n 2)))
	(t (fast-exp (* a b) b (- n 1)))))

(defun is-even(n)
  (= (% n 2) 0))

(fast-exp 1 3 4)

(defun mult(a b)
  (if (= b 0)
      0
    (+ a (mult a (- b 1)))))

(mult 3 4)

(defun double(x)
  (+ x x))

(defun halve(x)
  (/ x 2))

(defun fast-mult(a b)
  (cond ((= b 0) 0)
	((= b 1) a)
	((is-even b) (mult (double a) (halve b)))
	(t (mult (+ a b) (- b 1)))))

(* 4 6)
(fast-mult 4 6)

(* 6 9)
(fast-mult 9 6)


(defun fast-mult-iter(a b c)
  (cond ((= b 0) c)
	((is-even b) (fast-mult-iter (double a) (halve b) c))
	(t (fast-mult-iter a (- b 1) (+ c a)))))

(fast-mult-iter 9 6 0)

(defun fib(n)
  (fib-iter 1 0 0 1 n))

(defun fib-iter(a b p q count)
  (cond ((= count 0) b)
	((is-even count)
	 (fib-iter a
		   b
		   ;; <??>      ; compute p'
		   ;; <??>      ; compute q'
		   (/ count 2)))
	(t (fib-iter (+ (* b q) (* a q) (* a p))
		     (+ (* b p) (* a q))
		     p
		     q
		     (- count 1)))))


(defun smallest-divisor(n)
  (find-divisor n 2))

(defun find-divisor(n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides test-divisor n) test-divisor)
	(t (find-divisor n (next test-divisor)))))

(defun next(n)
  (cond ((= n 2) 3)
	(t (+ n 2))))

(defun divides(a b)
  (= (% b a) 0))

(defun square(x)
  (* x x))

(smallest-divisor 7)

(defun is-prime(n)
  (= (smallest-divisor n) n))

(is-prime 7)

(defun expmod(base exp m)
  (cond ((= exp 0) 1)
	((is-even exp) (% (square (expmod base (/ exp 2) m)) m))
	(t (% (square (expmod base (- exp 1) m)) m))))

(defun is-even(n)
  (= 0 (% n 2)))

(defun fermat-test(n)
  (defun try-it(a)
    (= a (expmod a n n)))
  (try-it (+ 1 (random (- n 1)))))


(fermat-test 4)
(random 4)

(defun fast-prime(n times)
  (cond ((= times 0) t)
	((fermat-test n) (fast-prime n (- times 1)))
	(t nil)))

(fast-prime 7 5)

(smallest-divisor 19999)
(current-time)

(defun timed-prime-test(n)
  (print n)
  (start-prime-test n (current-time)))

(defun start-prime-test(n start-time)
  (if (is-prime n)
      (report-prime (time-since start-time))))

(defun report-prime(elapsed-time)
  (print " *** ")
  (print elapsed-time))

(timed-prime-test 199999)

(defun sum-integers(a b)
  (if (> a b)
      0
    (+ a (sum-integers (+ a 1) b))))

(sum-integers 0 10)

(defun sum-cubes(a b)
  (if (> a b)
      0
    (+ (cube a) (sum-cubes (+ a 1) b))))

(defun pi-sum(a b)
  (if (> a b)
      0
    (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))


(defun sum(term a next b)
  (if (> a b)
      0
    (+ (funcall term a)
       (sum term (funcall next a) next b))))

(defun inc(n)
  (+ n 1))

(defun sum-cubess(a b)
  (sum 'cube a 'inc b))

(defun cube(n)
  (* n n n))

(defun sum-integerss(a b)
  (sum 'identity a 'inc b))

(defun identity(a) a)

(sum-cubess 1 10)

(sum-integerss 1 10)

(defun pi-sum(a b)
  (defun pi-term(x)
    (/ 1.0 (* x (+ x 2))))
  (defun pi-next(x)
    (+ x 4))
  (sum 'pi-term a 'pi-next b))

(* 8 (pi-sum 1 1000))

(defun integral(f a b dx)
  (defun add-dx(x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) 'add-dx b)
     dx))

(integral 'cube 0 1 0.01)
