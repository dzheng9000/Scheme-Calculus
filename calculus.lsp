(define plus? (lambda (num)
	(cond
		((eq? num '+)#t)
		(else #f)
	)
			   )
)

(define x? (lambda (ex)
	(cond
		((eq? ex 'x)#t)
		(else #f)
	)
			   )
)

(define non-num-co? (lambda (num)
	(cond 
		((and (symbol? num) (not (eq? num 'x)) (not (eq? num '^))) #t)
		(else #f)
	)
	)
)

(define reverseSpecial (lambda (poly)
	(cond
		((null? poly) '())
		(else (cons (reverse(car poly)) (reverseSpecial (cdr poly)))
	)
)))

(define last (lambda (poly)
  (cond ((null? (cdr poly)) (car poly))
        (else (last (cdr poly))))
		)
)
;; (a x ^ 5) (b x ^ 5) (2 x ^ 3) (6 x ^ 2) (x ^ 2) (3 x) (7)	
	
(define derive (lambda (poly var first_num last_num len out)
	(cond 
	((null? poly) (flatten out)) 
	((and (eq? len 1) (number? (car poly))) '() ) 
	((and (eq? last_num var)(eq? len 1)) (list '1)) 
	((x? last_num) (list first_num ))
	((and (non-num-co? (car poly)) (eq? len 4) (eq? last_num 2)) (list last_num first_num var))
	((and (number? (car poly)) (eq? len 4) (eq? last_num 2)) (list (* first_num last_num) var))
	((and (eq? len 3) (eq? last_num 2)) list('2 var)) 
	((and (number? (car poly)) (null? out)) (derive (cdr poly) var first_num last_num len (list (* (car poly) last_num) ))) ;; OKAY
	( (and (number? last_num) (non-num-co? (car poly)) ) (derive (cdr poly) var first_num last_num len (list last_num (car poly) ))) ;; OKAY appends variable a , b, c... to beginning of the list due to power rule
	((and (number? (car poly)) (>= (length out) 1)) (derive (cdr poly) var first_num last_num len (list out (- (car poly) 1)))) ;;subtracts 1
	(else (derive (cdr poly) var first_num last_num len(list out (car poly)))))))
;; expo = (a x ^ 5 + b x ^ 4 + 2 x ^ 3 + 6 x ^ 2 + 3 x + 7)	
(define deriv (lambda (poly var)
	(flatten (make-plus (remove-all '() (differentiate (reverseSpecial (reverse(make-paren (remove-all '* poly) '() '()))) var (length (make-paren poly '() '()))) ) (length(make-paren poly '()'() )         ) ))))

(define make-plus (lambda (poly len)
	(cond
	((null? poly) '())
	((eq? (length poly) 1) (car poly))
	(else (cons (car poly) (cons '+ (make-plus(cdr poly) len)))))))
	
(define differentiate(lambda (poly var len) 
	(cond
	
	((null? poly) '())
	(else (cons (derive (car poly) var (caar poly) (last (car poly)) (length (car poly)) '() ) (differentiate(cdr poly) var len)))))) 
	

(define make-paren 
	(lambda (expo temp holder) 
	(cond
		((and (null? expo) (not (null? holder))) (cons holder temp))
		((null? expo) (reverseSpecial (reverse temp))) 
		((plus? (car expo)) (make-paren (cdr expo) (cons holder temp) '() ))
		
		(else
		(make-paren (cdr expo) temp (cons (car expo) holder)))
	
	)
	))

(define flatten
    (lambda (list)
      (cond
        ((null? list) 
         '())
        ((pair? (car list))
         (append (flatten (car list)) (flatten (cdr list))))
        (else
          (cons (car list) (flatten (cdr list))))
        )))
		
(define remove-all
    (lambda (item lst)
      (cond
        ((null? lst) '())
        ((equal? item (car lst)) (remove-all item (cdr lst)))
        ((pair? (car lst)) 
         (cons (remove-all item (car lst)) (remove-all item (cdr lst))))
        (else (cons (car lst) (remove-all item (cdr lst)))))
))
