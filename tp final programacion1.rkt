;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |tp final programacion1|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;-----------------------------------------------------------------------------
;1
(define(implica p q)
  (cond
       [(and(equal? p #t)(equal? q #f))#f]
       [else #t]
       ))
  
(define(equivalente p q)(equal? p q))
;-----------------------------------------------------------------------------
;2
;lista_verdaderos: recibe una lista de listas y devuelve las listas internas con #true como primer elemento.
(define (lista_verdaderos l)
    (cond [(empty? l) '()]
        [(cons? l)(cons (cons #true (first l))
              (lista_verdaderos (rest l)))]))

;lista_falsos: recibe una lista de listas y devuelve las listas internas con #false como primer elemento.  
  (define (lista_falsos l)
    (cond [(empty? l) '()]
                [(cons? l)(cons (cons #false (first l))
              (lista_falsos (rest l)))]))

;Caso Base: cuando n = 0, lista de empty.
(define (valuaciones n)
     (cond
      [(zero? n)
        (list '())]
       [(positive? n)
                  (append (lista_verdaderos (valuaciones (sub1 n)))
                  (lista_falsos (valuaciones (sub1 n))))])) 

;-----------------------------------------------------------------------------
;3
; A : List(Boolean) -> Boolean
; A representa la formula proposicional:((p1->p3)^(p2->p3))<->((p1∨p2)-> p3)
(check-expect(A(list #t #f #t))#t)
(check-expect(A(list #f #f #t))#t)
(check-expect(A(list #f #f #f))#t)

(define
  (A l)
  (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
  (equivalente (and (implica p1 p3)
                    (implica p2 p3))
               (implica (or p1 p2)
                        p3))))
; B : List(Boolean)-> Boolean
; B representa la formula proposicional:((p1^p2)->p3)<->((p1->p3)^(p2->p3))
(check-expect(B(list #t #f #t))#t)
(check-expect(B(list #t #f #f))#f)
(check-expect(B(list #f #f #f ))#t)

(define
  (B l)
   (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
     (equivalente (implica (and p1 p2)p3)(and(implica p1 p3)(implica p2 p3)))))
; C: List(Boolean)-> Boolean
; C representa la formula proposicional: (¬p1∨¬p2)<->(p1^p2)
(check-expect(C(list #t #t #t))#f)
(check-expect(C(list #t #f #t))#f)
(check-expect(C(list #f #f #t))#f)

(define
  (C l)
   (let ([p1 (first l)]
        [p2 (second l)])
     (equivalente (or (not p1)(not p2))(and p1 p2))))
;-----------------------------------------------------------------------------
;4

; evaluar: (List(Boolean)-> Boolean) Number -> (List(Boolean))
; Dada una formula proposicional P (que admita listas de booleanos)
; Evaluará la proposición en todas las posibilidades existentes, dependiendo cuantas variables se ingrese
; Las posibilidades son listas de booleanos provenientes de la función "valuaciones"
(check-expect(evaluar A 3)(list #t #t #t #t #t #t #t #t))
(check-expect(evaluar B 3)(list #t #t #t #f #t #f #t #t))
(check-expect(evaluar C 2)(list #f #f #f #f))
(define(evaluar p n)(map p (valuaciones n)))

;-----------------------------------------------------------------------------
;5
;compruebav: Boolean Boolean -> Boolean
; Dados dos datos booleanos, mediante and comprueba si ambos son verdaderos
(check-expect(compruebav #t #t)#t)
(check-expect(compruebav #t #f)#f)
(check-expect(compruebav #f #f)#f)

(define(compruebav x y)(and x y))

;compruebaf: Boolean Boolean -> Boolean
; Dados dos datos booleanos, mediante or comprueba si alguno de los dos son verdaderos
(check-expect(compruebaf #t #t)#t)
(check-expect(compruebaf #t #f)#t)
(check-expect(compruebaf #f #f)#f)

(define(compruebaf x y)(or x y))
; tautología: List(boolean)-> (boolean)
; Dada una lista de booleanos y de acuerdo a la def. de tautología, comprobará si todos sus elementos son #True
; En caso de serlo devolverá #True, de lo contrario, #False
(check-expect(tautología? A 3)#t)
(check-expect(tautología? B 3)#f)
(check-expect(tautología? C 2)#f)

(define (tautología? p n)(local(; evaluat:  (List(Boolean)->Boolean) N -> List(Boolean)
                              ; Dados un número y un predicado, evaluará ese mismo predicado en todas sus posibilidades
                              (define(evaluat p n)(evaluar p n))
                              )(foldr compruebav #t (evaluat p n))))
; contradicción?: List(boolean)-> (boolean)
; Dada una lista de booleanos y de acuerdo a la def. de contradicción, comprobará si todos sus elementos son #False
; En caso de serlo devolverá #True, de lo contrario, #False
(check-expect(contradicción? A 3)#f)
(check-expect(contradicción? B 3)#f)
(check-expect(contradicción? C 2)#t)

(define (contradicción? p n)(local(; evaluac: (List(Boolean)->Boolean) N -> List(Boolean)
                                 ;  Dados un número y un predicado, evaluará ese mismo predicado en todas sus posibilidades
                                 (define(evaluac x y)(evaluar p n)))
                                  (not(foldr compruebaf #f (evaluac p n)))))

; satisfactible?: List(boolean)-> (boolean)
; Dada una lista de booleanos y de acuerdo a la def. de satisfactible, comprobará si existe algún #True en la lista
; En caso de serlo devolverá #True, de lo contrario, #false
(check-expect(satisfactible? A 3)#t)
(check-expect(satisfactible? B 3)#t)
(check-expect(satisfactible? C 2)#f)

(define (satisfactible? p n)(local(; evaluas: (List(Boolean)->Boolean) N -> List(Boolean)
                                   ; Dados un número y un predicado, evaluará ese mismo predicado en todas sus posibilidades
                                   (define(evaluas p n)(evaluar p n))
                                   )(member? #t (evaluas p n))))

