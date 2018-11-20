(define factorial
  (lambda(n)
    (if (= n 0)
      1
      (* n (factorial (- n 1))))))

(define (sinhFunc x m)
  (/ (expt x (+ 1 (* 2 m))) (factorial(+ 1 (* 2 m)))))
(define (coshFunc y k)
  (/ (expt y (* 2 k)) (factorial(* 2 k))))

(define pi 3.141592653589793238)
(define degToRad
  (lambda (deg)
    (define rad (* pi (/ deg 180)))
     rad
  ))

(define i -1)
(define (sinh x)
  (set! i (+ i 1))
  (if(> i 30)
     (begin
       (set! i -1)
       0)
     (+ (sinhFunc (degToRad x) i) (sinh x))))

(define j -1)
(define (cosh x)
  (set! j (+ j 1))
  (if(> j 30)
     (begin
       (set! j -1)
       0)
     (+ (coshFunc (degToRad x) j) (cosh x))))

(define (isEnough lst)
  (cond
    ((null? lst) #F)
    ((< (length lst) 3) #F)
    (#T)))
  
(define (pre-order lst)
  (if (not (isEnough lst))
      '()
      (cons (car lst) (append (pre-order (car(cdr lst))) (pre-order (car(cdr(cdr lst))))))))

(define (in-order lst)
  (if (not (isEnough lst))
      '()
      (append (in-order (car(cdr lst))) (list (car lst)) (in-order (car(cdr(cdr lst)))))))

(define (post-order lst)
  (if (not (isEnough lst))
      '()
      (append (post-order (car(cdr lst))) (post-order (car(cdr(cdr lst)))) (list (car lst)))))

