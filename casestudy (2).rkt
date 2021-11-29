#lang racket

;############################################################ Matrix creation #################################################### - nishitha 

(define (creatematrix r c )
  (define mat '()) ;outer array 
  (for ([i (in-range 0 r)])
    (define temp '()) 
    (for ([j (in-range 0 c)])
      (define a(read-line (current-input-port) 'any))  ; taking input 
      (set! temp (append temp (list (string->number a)))) ; reassignment temp 
      )
    (display temp)
    (if (null? mat) ; avoiding empty array at start of matrix 
        (set! mat (list temp))
            (set! mat (append mat (list temp)))) 
    (display mat) ; displaying 
     )
  mat )

;############################################################ helpers ####################################################

(define (rows matrix)
  (length matrix))

(define (col matrix)
   (length (car matrix)))

;############################################################ transpose ####################################################

(define (transpose matrix)                    
  (for/list ((i (length (list-ref matrix 0)))) ; sends result in form of a list 
    (for/list ((il matrix)) ; access the rows of the matrix                    
      (list-ref il i)))) ; access all the rows of ith column (il row ith col)

;############################################################ extract diagonal elements ####################################################

(define (diagonal matrix) 
  (if (null? matrix) ; if matrix null condition
      '()
      (cons (caar matrix) 
            (diagonal (map cdr (cdr matrix)))))); recursive

;############################################################ addition ####################################################- - parvana 

(define (addmatrix matrix1 matrix2)
  (if (and (= (rows matrix1) (rows matrix2)) (=(col matrix1) (col matrix2))) ; checking rows and columns are equal 
      (map (lambda (matrix1 matrix2) (map + matrix1 matrix2)) matrix1 matrix2) ; using inbuilt map function 
      (display "Dimensions do not match")))

;############################################################ subtraction ####################################################

(define (submatrix matrix1 matrix2)
  (if (and (= (rows matrix1) (rows matrix2)) (=(col matrix1) (col matrix2))) ; checking rows and columns are equal 
      (map (lambda (matrix1 matrix2) (map - matrix1 matrix2)) matrix1 matrix2)
      (display "Dimensions do not match"))) ; else part

;############################################################ helpers ####################################################

(define (change-at l x y to)
  (for/list ([row l] [i (length l)]) ; access every row of each matrix 
    (for/list ([e row] [j (length row)]) ; each and every element of that particular row 
      (if (and (= x i) (= y j))
          to
          e)))); returns a matrix

;############################################################ identifying upper triangle matrix ####################################################

(define (upper matrix)
   (define count (rows matrix))
  (if (= (rows matrix) (col matrix)); if square matrix 
 
  (for ([i (in-range 0 (rows matrix))])
    (for ([j (in-range 0 (- (rows matrix) count))]) ; as last row will have rows-1 number of zeroes 
       (set! matrix (change-at matrix i j 0)) ; changing (i,j) position to 0 
    )
    (set! count (- count 1))
    (display matrix); displaying matrix 
    )
  (display "not a square matrix")))

;############################################################ identifying lower triangle matrix ####################################################

(define (lower matrix)
   (define count 1)
  (if (= (rows matrix) (col matrix)); square matrix 
  (for ([i (in-range 0 (rows matrix))]) ; first row will have rows-1 number of 0 
    (for ([j (in-range count (rows matrix))])
       (set! matrix (change-at matrix i j 0))
    )
    (set! count (+ count 1))
    (display matrix) ; displaying matrix 
    )
  (display "not a square matrix")))

;############################################################ multiplication #################################################### -- divya 

(define (matrixmult matrix1 matrix2)
  (if (= (col matrix1)(rows matrix2)) ; checking condition clo1= rows2 
       (map(lambda (row)
        (apply map
       (lambda column
         (apply + (map * row column))) ; when lists we use apply function 
       matrix2)) 
   matrix1)
      (display "error")))

;############################################################ rank of matrix ####################################################
(define (has-no-zero row)
  (if (null? row)
      0
      (begin 
        (if (= (car row) 0)
            (has-no-zero (cdr row))
            1))))
(define (rank matrix count)
  (if (null? matrix)
      count
      (rank (cdr matrix ) (+ count (has-no-zero (car matrix))))
      )
      
  )

;############################################################ helpers ####################################################

(define (access-elem l x y)
(define fla 0)
(define w 0)
(for/list ([row l] [i (length l)])
(for/list ([e row] [j (length row)])
(if (and (= x i) (= y j))
(set! fla e)
(+ w 1) )))fla)

;############################################################ helpers #################################################### - - hridhi 
(define (cofac l x y)
(define fla '())
(define w 0)
(define tmat '())
(for/list ([row l] [i (length l)])
(for/list ([e row] [j (length row)])
(if (or (= x i) (= y j))
    (begin
      
    (set! w 1))
    (begin
      
      (if (null? tmat)
          (set! tmat (list e))
      (set! tmat (append tmat (list e))))
     
      )
    ))
  (if (null? fla)
      ; avoiding empty array at start of matrix
      (begin 
      (if (null? tmat)
          (set! w 1)

          (set! fla (list tmat))
          ))
        (if (null? tmat)
            (set! w 1)
            (set! fla (append fla (list tmat)))))
 ;(set! fla (append fla (list tmat)));add row to matr
  (set! tmat '()))(det fla))


(define (det mat)
  (define a (caar mat))
  (define b (cadar mat))
  (define c (caadr mat))
  (define d (cadadr mat))
  (set! a (* a d ))
  (set! b (* b c))
  (set! a (- a b))
  a)

;############################################################ determinant #################################################### 

(define (deter mat)
  ;for first row only ?
  (define x 0)
  (define ans 0)
  (define t 0)
  (for ([i (in-range 0 (rows mat))])
    (set! x (cofac mat 0 i))
    (set! t (access-elem mat 0 i));acces elemenet
    (set! x (* x t))
    (if (even? i)
        (set! ans (+ ans x)
              )
        (set! ans (- ans x)))
    )
  ans)


;############################################################ helpers ####################################################  - - nikitha 

(define (change-t l x y r)
  (for/list ([row l] [i (length l)]) ; access every row of each matrix 
    (for/list ([e row] [j (length row)]) ; each and every element of that particular row 
      (if (and (= x i) (= y j))
          (begin
          (/ e r))
          e)))); 

;############################################################ inverse of matrix ####################################################
(define (inverse mat)
  (define r (deter mat))
  (define mat2 '())
  (define mat1 '())
  (define count 0)
  (define w 0)
  (define temp 0)
  (if (= 0 r)
      (display "error")
      (begin
        (for ([i (in-range 0 (rows mat))])
          (for ([j (in-range 0 (col mat))])
            (set! temp (cofac mat i j))
            (if (even? count)
                (set! w 1)
                (set! temp (* -1 temp)))
            (if (null? mat2)
                (set! mat2 (list temp))
                (set! mat2 (append mat2 (list temp))))
            (set! count (+ 1 count )))
          (if (null? mat1)
              (set! mat1 (list mat2))
              (set! mat1 (append mat1 (list mat2))))
          (set! mat2 '()))))
  (set! mat1 (transpose mat1))
    (for ([i (in-range 0 (rows mat1))])
    (for ([j (in-range 0 (col mat1))])
      (set! mat1(change-t mat1 i j r)))) mat1)
  

;############################################################ execution ####################################################

;(define x (creatematrix 3 3))
;(define y (creatematrix 2 3))
;(addmatrix x y)
