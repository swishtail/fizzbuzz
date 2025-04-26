(define pairs
  (list
   (list 3 "Fizz")
   (list 5 "Buzz")
   (list 7 "Hiss")
   (list 11 "Howl")))

(define (fizzbuzz pairs n)
  (define (print-fizzbuzz x)
    (for-each (lambda (line)
                (begin (display line)
                       (newline)))
              x))
  
  (define (enumerate low high)
    (if (> low high)
        '()
        (cons low (enumerate (+ low 1) high))))
  
  (define (fold-right op initial x)
    (if (null? x)
        initial
        (op (car x)
            (fold-right op initial (cdr x)))))
  
  (define (test-number pairs number)
    (define (test-pair pair)
      (if (zero? (remainder number (car pair)))
          (cadr pair)
          ""))
    
    (let ((output-string
           (fold-right string-append
                       ""
                       (map (lambda (pair)
                              (test-pair pair))
                            pairs))))
      (if (zero? (string-length output-string))
          number
          output-string)))
  
  (print-fizzbuzz
   (map (lambda (k)
          (test-number pairs k))
        (enumerate 1 n))))

(fizzbuzz pairs 100)
