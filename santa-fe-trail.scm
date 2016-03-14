(use gauche.record)
(use gauche.sequence) ; shuffle
(use srfi-27) ; random-integer
(use srfi-42) ; list-ec

;variable
(define *width* 32)
(define *height* 32)
(define *food-count* 89)

(define *ant* '())
(define *ants* '())

(define *ant-builder* 
  (^(proc-gene) 
    (let1 gene (proc-gene)
      (make-ant 'S 0 0 400 0 gene (count-tree gene))
      )
    )
  )

(define *master-foods* (make-hash-table 'equal?))

(define *trace* #f)


;function


;util {{{

(define (quote-it x)
      (list 'quote x))


(define (quote-off l)
  (if (null? l)
    l
    (if (eq? (car l) (car ''()))
      (cadr l)
      l
      )
    )
  )

(define (flatten x)
  (cond 
    ((null? x) '())
    ((not (pair? x)) (list x))
    (else (append (flatten (car x)) (flatten (cdr x))))
    )
  )


;}}}

;food {{{

(define *foods* (make-hash-table 'equal?))

(define (random-food left top width height)
  (add-food (+ left (random-integer width)) (+ top (random-integer height)))
  )

(define (add-food x y)
  (hash-table-put! *master-foods* (cons x y) #t)
  )

(define (add-foods)
  ;(dotimes (i *food-count*)
  ;  (random-food 0 0 *width* *height*)
  ;  )
  (add-food 1 0)
  (add-food 2 0)
  (add-food 3 0)
  (add-food 3 1)
  (add-food 3 2)
  (add-food 3 3)
  (add-food 3 4)
  (add-food 3 5)
  (add-food 4 5)
  (add-food 5 5)
  (add-food 6 5)
  (add-food 8 5)
  (add-food 9 5)
  (add-food 10 5)
  (add-food 11 5)
  (add-food 12 5)
  (add-food 12 6)
  (add-food 12 7)
  (add-food 12 8)
  (add-food 12 9)
  (add-food 12 11)
  (add-food 12 12)
  (add-food 12 13)
  (add-food 12 14)
  (add-food 12 17)
  (add-food 12 18)
  (add-food 12 19)
  (add-food 12 20)
  (add-food 12 21)
  (add-food 12 22)
  (add-food 12 23)
  (add-food 11 24)
  (add-food 10 24)
  (add-food 9 24)
  (add-food 8 24)
  (add-food 7 24)
  (add-food 4 24)
  (add-food 3 24)
  (add-food 1 25)
  (add-food 1 26)
  (add-food 1 27)
  (add-food 1 28)
  (add-food 2 30)
  (add-food 3 30)
  (add-food 4 30)
  (add-food 5 30)
  (add-food 7 29)
  (add-food 7 28)
  (add-food 8 27)
  (add-food 9 27)
  (add-food 10 27)
  (add-food 11 27)
  (add-food 12 27)
  (add-food 13 27)
  (add-food 14 27)
  (add-food 16 26)
  (add-food 16 25)
  (add-food 16 24)
  (add-food 16 21)
  (add-food 16 20)
  (add-food 16 19)
  (add-food 16 18)
  (add-food 17 15)
  (add-food 20 14)
  (add-food 20 13)
  (add-food 20 10)
  (add-food 20 9)
  (add-food 20 8)
  (add-food 20 7)
  (add-food 21 5)
  (add-food 22 5)
  (add-food 24 4)
  (add-food 24 3)
  (add-food 25 2)
  (add-food 26 2)
  (add-food 27 2)
  (add-food 29 3)
  (add-food 29 4)
  (add-food 29 6)
  (add-food 29 9)
  (add-food 29 12)
  (add-food 28 14)
  (add-food 27 14)
  (add-food 26 14)
  (add-food 23 15)
  (add-food 24 18)
  (add-food 27 19)
  (add-food 26 22)
  (add-food 23 23)
  )

;}}}

;ant {{{
(define-record-type ant #t #t (dir) (x) (y) (energy) (eaten) (gene) (len))

(define-method ant-show (a)
  (display "ANT:(front = ")
  (display (ant-dir a))
  (display ")")
  (display ":(pos = ")
  (display (ant-pos a))
  (display ")")
  (display ":(energy = ")
  (display (ant-energy a))
  (display ")")
  (display ":(eaten = ")
  (display (ant-eaten a))
  (display ")")
  (display ":(ahead = ")
  (display (ant-ahead a))
  (display ")")
  (display ":(length = ")
  (display (ant-len a))
  (display ")")
  (print)
  )

(define-method ant-pos (a)
  (cons (ant-x a) (ant-y a))
  )

(define-method ant-front (a)
  (let1 d (ant-dir a)
    (cond
      ((eq? 'E d) (cons (+ 1 (ant-x a)) (ant-y a)))
      ((eq? 'W d) (cons (- 1 (ant-x a)) (ant-y a)))
      ((eq? 'S d) (cons (ant-x a) (+ 1 (ant-y a))))
      ((eq? 'N d) (cons (ant-x a) (- 1 (ant-y a))))
      )
    )
  )

(define-method ant-dead (a)
  (<= (ant-energy a) 0)
  )

(define-method ant-eat (a)
  (let1 pos (ant-pos a)
    (when (hash-table-get *foods* pos #f)
      (inc! (ant-eaten a))
      (hash-table-delete! *foods* pos)
      )
    )
  )

(define-method ant-ahead (a)
  (hash-table-get *foods* (ant-front a) #f)
  )

(define-method ant-move (a)
  (dec! (ant-energy a))
  (let1 d (ant-dir a)
    (cond
      ((eq? 'E d) (when (< (ant-x a) (- *width* 1)) (inc! (ant-x a))))
      ((eq? 'W d) (when (> (ant-x a) 0) (dec! (ant-x a))))
      ((eq? 'S d) (when (< (ant-y a) (- *height* 1)) (inc! (ant-y a))))
      ((eq? 'N d) (when (> (ant-y a) 0) (dec! (ant-y a))))
      )
    )
  )

(define-method ant-left (a)
  (dec! (ant-energy a))
  (let1 d (ant-dir a)
    (cond
      ((eq? 'E d) (set! (ant-dir a) 'N))
      ((eq? 'W d) (set! (ant-dir a) 'S))
      ((eq? 'S d) (set! (ant-dir a) 'E))
      ((eq? 'N d) (set! (ant-dir a) 'W))
      )
    )
  )

(define-method ant-right (a)
  (dec! (ant-energy a))
  (let1 d (ant-dir a)
    (cond
      ((eq? 'E d) (set! (ant-dir a) 'S))
      ((eq? 'W d) (set! (ant-dir a) 'N))
      ((eq? 'S d) (set! (ant-dir a) 'W))
      ((eq? 'N d) (set! (ant-dir a) 'E))
      )
    )
  )


;}}}

;gene rules {{{

(define (move)
  (when (not (ant-dead *ant*))
    (ant-move *ant*)
    (ant-eat *ant*)
    (when *trace*
      (draw-trace)
      )
    )
  )

(define (left)
  (when (not (ant-dead *ant*))
    (ant-left *ant*)
    (when *trace*
      (draw-trace)
      )
    )
  )

(define (right)
  (when (not (ant-dead *ant*))
    (ant-right *ant*)
    (when *trace*
      (draw-trace)
      )
    )
  )


(define (if-food-ahead x y)
  (if (ant-ahead *ant*)
    (exec x)
    (exec y)
    )
  )

(define (prog-2 x y)
  (exec x)
  (exec y)
  )

(define (prog-3 x y z)
  (exec x)
  (exec y)
  (exec z)
  )

(define (exec p)
  (eval p interaction-environment)
  )


(define (make-gene)
  (case (random-integer 9) 
    ((0 1) '(move))
    ((2 3) '(left))
    ((4 5) '(right))
    ((6) (list 'prog-2 (quote-it (make-gene)) (quote-it (make-gene))))
    ((7) (list 'prog-3 (quote-it (make-gene)) (quote-it (make-gene)) (quote-it (make-gene)))) 
    ((8) (list 'if-food-ahead (quote-it (make-gene)) (quote-it (make-gene))))
    )
  )


;}}}




;tree {{{

(define atom?
  (lambda (x) 
    (and (not (null? x)) (not (pair? x))))
  )


(define (count-tree tree)
  (if (null? tree)
    0
    (if (atom? tree)
      (if (eq? tree (car ''()))
        0
        1
        )
      (+ (count-tree (car tree)) (count-tree (cdr tree)))
      )
    )
  )

(define (get-tree t n)
  (let1 x 0
    (define (get-tree-in tree nth cont)
      (if (null? tree)
        (cont)
        (let1 qt (quote-off tree)
          (if (atom? (car qt))
            (begin
              (set! x (+ 1 x))
              (if (= nth x)
                qt
                (get-tree-in (cdr qt) nth cont)
                )
              )
            (get-tree-in (car qt) nth (^() (get-tree-in (cdr qt) nth cont)))
            )
          )
        )
      )
      (get-tree-in t n (^() ()))
    )
  )



(define (rep-tree t n new)
  (let1 x 0
    (define (rep-tree-in tree nth scont mcont)
      (if (null? tree)
        (scont)
        (let1 qt (quote-off tree)
          (if (atom? (car qt))
            (begin
              (set! x (+ 1 x))
              (if (= nth x)
                (mcont (quote-it new))
                (rep-tree-in (cdr qt) nth scont (^x (mcont (cons (car qt) x))))
                )
              )
            (rep-tree-in (car qt) nth (^() (rep-tree-in (cdr qt) nth scont (^x (mcont (cons (car qt) x))))) (^x (mcont (cons x (cdr qt)))))
            )
          )
        )
      )
      (rep-tree-in t n (^() ()) (^x (quote-off x)))
    )
  )


;}}}

;world {{{

(define (draw-trace)
  (newline)
  (newline)
  (dotimes (y *height*)
    (newline)
    (display "|")
    (dotimes (x *width*)
      (display 
        (cond 
          ((and (= x (ant-x *ant*)) (= y (ant-y *ant*))) #\A)
          ((hash-table-get *foods* (cons x y) #f) #\*)
          (else #\space)
          )
        )
      )
    (display "|")
    )
  (newline)
  (ant-show *ant*)
  (sys-nanosleep 100000000)
  )

(define (run-ant ant)
  (set! *foods* (hash-table-copy *master-foods*))
  (while (not (ant-dead ant))
    (exec (ant-gene ant))
    )
  (hash-table-clear! *foods*)
  )

(define (init)
  (list-ec (: i 100) (*ant-builder* make-gene))
  )

(define (select ants)
  ;(drop (sort ants (^(x y) (< (ant-gene-length x) (ant-gene-length y)))) 80)
  (drop (sort ants 
          (^(x y) 
            (if (= (ant-eaten x) (ant-eaten y))
              (> (ant-len x) (ant-len y))
              (< (ant-eaten x) (ant-eaten y))
             ))
          ) 80)
  )

(define (crossovers ants)
  (append 
    (flatten (map (^x (crossover (car x) (cadr x))) (slices (shuffle ants) 2)))
    (flatten (map (^x (crossover (car x) (cadr x))) (slices (shuffle ants) 2)))
    (flatten (map (^x (crossover (car x) (cadr x))) (slices (shuffle ants) 2)))
    (flatten (map (^x (crossover (car x) (cadr x))) (slices (shuffle ants) 2)))
    (flatten (map (^x (crossover (car x) (cadr x))) (slices (shuffle ants) 2)))
    )
  )

(define (crossover ant1 ant2)
  (let*
    (
     (l1 (ant-len ant1))
     (l2 (ant-len ant2))
     (n1 (if (= 1 l1) 1 (+ 2 (random-integer (- l1 1)))))
     (n2 (if (= 1 l2) 1 (+ 2 (random-integer (- l2 1)))))
     (g1 (ant-gene ant1))
     (g2 (ant-gene ant2))
     )
    (cons
      (*ant-builder* (^() (rep-tree g1 n1 (get-tree g2 n2)))) 
      (*ant-builder* (^() (rep-tree g2 n2 (get-tree g1 n1)))) 
      )
    )
  
  )

(define (mutatations ants)
  (map
    (^a 
      (when (= 0 (random-integer 50))
        (let* 
          (
           (l (ant-len a))
           (n (if (= 1 l) 1 (+ 2 (random-integer (- l 1)))))
           (g (ant-gene a))
           )
          (set! (ant-gene a) (rep-tree g n (make-gene)))
          (set! (ant-len a) (count-tree (ant-gene a)))
          )
        )
      a
      )
    ants)
  )

(define (next-generation ants)
  (mutatations (crossovers (select ants)))
  )

(define (prosper ants)
  (set! *ants* ants)
  (dolist (a *ants*)
    (set! *ant* a)
    (run-ant a)
    )
  )

(define (show-ants)
  (dolist (a (sort *ants* (^(x y) (< (ant-eaten x) (ant-eaten y)))))
    (ant-show a)
    )
  )

(define (show-top-ant g)
  (display "G:")
  (display g)
  (display "   ")
  (let1 a (car (sort *ants* (^(x y) (> (ant-eaten x) (ant-eaten y)))))
    (ant-show a)
    )
  )

(define (trace-top-ant)
  (let*
    (
     (origin (car (sort *ants* (^(x y) (> (ant-eaten x) (ant-eaten y))))))
     (new (*ant-builder* (^() (ant-gene origin))))
     )
    (set! *trace* #t)
    (set! *ant* new)
    (run-ant new)
    (print (ant-gene new))
    (set! *trace* #f)
    )
  )

;}}}



;try




(define (evolution)
  (let1 g 1
    (read-line)
    (add-foods)
    (prosper (init))
    (show-top-ant g)
    (show-ants)
    (let loop ()
      ;(draw-world)
      (newline)
      (display ">")
      (flush)
      (let1 str (read-line)
        (unless (equal? str "quit")
          (let1 x (string->number str)
            (newline)
            (if x 
              (dotimes (i x)
                (inc! g)
                (prosper (next-generation *ants*))
                (show-top-ant g)
                ;(display #\.)
                (flush)
                )
              (begin
                (inc! g)
                (prosper (next-generation *ants*))
                (show-top-ant g)
                )
              )
            (newline)
            (display "show top ant?(y/N) > ")
            (flush)
            (when (equal? (read-line) "y")
              (trace-top-ant)
              )
            ;(show-ants)
            )
          (loop)
          )
        )
      )
    )
  )




