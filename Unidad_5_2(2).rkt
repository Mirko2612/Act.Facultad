;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Unidad_5_2(2)|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;2
(define (pares l)
  (filter even? l))
(pares (list 4 6 3 7 5 0))
;3
(define (clasificar l)(<(string-length l) 5))
(define (cortas l)
  (filter clasificar l))
(cortas (list "Lista" "de" "palabras" "sin" "sentido"))
;4
(define MAX 5)
(define (cerquita l)(and(< (posn-x l) MAX)(< (posn-y l)MAX)))

(define (cerca l)
  (filter cerquita l))
(cerca (list (make-posn 3 5) (make-posn 1 2) (make-posn 0 1) (make-posn 5 6)))
;5
(define (positivos l)
  (filter positive? l))
(positivos (list -5 37 -23 0 12))

;7
(define (distancia l) (sqrt(+ (sqr (posn-x l)) (sqr(posn-y l)) ) ) ) 
(define (distancias l)
  (map distancia l))
(distancias (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5)))
;8
(define (ancho l)(image-width l))
(define (anchos l)
  (map ancho l))
(anchos (list (circle 30 "solid" "red") (rectangle 10 30 "outline" "blue")))
;11
(define (longitud l)(string-length l))
(define (longitudes l)
  (map longitud l))
(longitudes(list "hola" "cómo" "estás?"))
;13
(define (prod l) (foldr * 1 l))
(prod (list 1 2 3 4 5))
;14
(define (pegar l) (foldr string-append "" l))
(pegar (list "Las " "lis" "tas " "son " "complicadas" "."))
;15
(define (maxi l)(foldr max 0 l))
(maxi (list 23 543 325 0 75))
;16
(define (todos-verdaderos l)(foldr boolean=? #t l))
(todos-verdaderos (list #t #f #t))

;17
(define (largos l)(length l))
(largos (list 1 "hola" #t -5))
;local
(define (eliminar l m)
  (local (; distinto-m?: Number -> Boolean
          ; Determina si un número n es distinto a m
          (define (distinto-m? n) (not (= m n))))
    (filter distinto-m? l)))
(eliminar (list 0 0 1 -5 0 2.5 0) 0)
;18
(define (mayores l m)
  (local (
          (define (mayor? n) (> n m)))
            (filter mayor? l)))
(mayores (list -5 0 8 5 6 2) 5)
;19
(define (largas l g)
  (local (
          (define (largo? n) (> (string-length n) g)))
    (filter largo? l)))
(largas (list "Hola" "estudiantes" "de" "LCC" "Rosario") 4)
;20
(define (lejos l g)
  (local(
         (define (lejos? n) (> (sqrt(+ (sqr (posn-x n)) (sqr(posn-y n)) ) ) g)))
    (filter lejos? l)))
(lejos (list (make-posn 3 5) (make-posn 1 2) (make-posn 0 1) (make-posn 5 6)) 4)
;21
(define (sumar l g)
  (local(
         (define (operacion n)(+ n g)))
         (map operacion l)))
(sumar (list 5 3 -4) 10)
;22
(define (elevar l g)
  (local(
         (define (cuadrado n)(expt n g)))
         (map cuadrado l)))
(elevar (list 5 3 -4) 2)
;23
(define (cuadrados l)(map sqr l))
(define (suma l)(foldr + 0 l))
(define (sumcuad l)(suma (cuadrados l)))
(sumcuad (list 11 13 9))
;24
(define (dist-origen n) (sqrt(+ (sqr (posn-x n)) (sqr(posn-y n)) )))
(define (list-dist n) (map dist-origen n))
(define (sumdist n)
  (suma (list-dist n)))
(sumdist(list (make-posn 3 4) (make-posn 0 3)))
;25
(define (multPos l)
  (foldr * 1 (filter positive? l)))
(multPos (list 3 -2 4 0 1 -5))
;26
(define (todosp l)
  (cond[(empty? l)l]
       [(and(cons? l)(= 0 (first l))) (cons(first l)(todosp(rest l)))]
       [(and(cons? l)(positive? (first l)))(cons (first l)(todosp (rest l)))]
       [(and(cons? l)(negative? (first l)))(cons (* -1 (first l))(todosp (rest l)))]))
(define (sumAbs l)(suma (todosp l)))
(sumAbs (list 3 -2 4 0 1 -5))
;27
(define (raices l)(map sqrt (filter positive? l)))
 (raices (list 16 -4 9 0))
;28
(define (calcu-area l)(* (image-width l)(image-height l)))
(define (clasificador n)(>(image-width n)(image-height n)))
(define (ancha? l)(filter clasificador l))
(define (areas l)(map calcu-area (ancha? l)))
(define (saa l)
  (suma (areas l)))
(saa (list (circle 20 "solid" "red")
              (rectangle 40 20 "solid" "blue")
              (rectangle 10 20 "solid" "yellow")
              (rectangle 30 20 "solid" "green")))
;29
(define (algunpos l) (foldr boolean=? #t (map positive? (map suma l))))
(algunpos (list(list 1 3 -4 -2) (list 1 2 3 -5) (list 4 -9 -7 8 -3)))
(algunpos (list (list -1 2 -3 4 -5) empty (list -3 -4)))
;30
(define (mayor4 l)(> (length l)4))
(define (long-lists l)(foldr boolean=? #t (map mayor4 l)))
(long-lists (list (list 1 2 3 4 5) (list 1 2 3 4 5 6) (list 87 73 78 83 33)))
(long-lists (list '() '() (list 1 2 3)))

;32
;Area de definiciones
; alumno es (List(String)),(list(Number)),(List(Number)) donde
; nombre es la lista de nombres de los alumnos ingresados
; nota es la lista de las notas correspondientes a cada alumno ingresado
; faltas es la lista de las faltas que poseen los alumnos correspondientes
(define-struct alumno [nombre nota faltas])

;Area de funciones
; condicion: (List(Number)) -> String
; Se verificará mediante condicionales la situacion de cada alumno
(check-expect(condicion (make-alumno "pedro" 8 2))"promovido");Caso de prueba
(define (condicion l)
  (cond[(>= (alumno-nota l) 8)"promovido"]
       [(and(>= (alumno-nota l) 6)(< (alumno-nota l) 8))"regular"]
       [else "libre"]))

; v_condicion: (List(struct)) -> (List(Strings))
; Con la funcion asociada "map" aplica la funcion "condicion" a todos los elementos de la lista
(check-expect(v_condicion(list (make-alumno "Ada Lovelace" 10 20)))(list"promovido"))
(define (v_condicion l)(map condicion l))
(v_condicion (list (make-alumno "Ada Lovelace" 10 20)
                  (make-alumno "Carlos Software" 6.5 12)
                  (make-alumno "Carlos Software" 3.5 12)))

; libre? (List(String)) -> Boolean
; Si el elemento de la lista coincide con el string devolverá #false sino #true
(check-expect(libre?(make-alumno "pedro" 2 2))#false)
(define (libre? l)
  (cond
       [(string=? "libre" (condicion l)) #f]
       [else #t]))

; regular? (List(String)) -> Boolean
; Si el elemento de la lista coincide con el string devolverá #true sino #false
(check-expect(regular?(make-alumno "pedro" 6 2))#true)
(define (regular? l)
  (cond
       [(string=? "regular" (condicion l)) #t]
       [else #f]))

; promovido? (List(String)) -> Boolean
; Si el elemento de la lista coincide con el string devolverá #true sino #false
(check-expect(promovido?(make-alumno "pedro" 10 2))#true)
(define (promovido? l)
  (cond[(string=? "promovido"(condicion l)) #t]
      [else #f]))

; exitos: (List(Struct)) -> Boolean
; Comparará si todos los booleanos de la lista son iguales a #true, en caso de no serlo devuelve #true
(check-expect(exitos (list(make-alumno "Ada Lovelace" 10 20)))#t)
(define (exitos l) (foldr boolean=? #t (map libre? l)))
(exitos (list (make-alumno "Juan Computación" 4 13)
             (make-alumno "Carlos Software" 6.5 12)
             (make-alumno "Ada Lovelace" 10 20)))

; acceder: List(Struct) -> (List(Number))
; Accede mediante map a las faltas de cada alumno de la lista
(check-expect(acceder(list (make-alumno "Juan Computación" 7 2)))(list 2))
(define (acceder l)(map alumno-faltas l))

; acceder2: List(Struct) -> (List(String))
; Accede mediante map a los nombres de cada alumno de la lista
(check-expect(acceder2(list (make-alumno "Juan Computación" 7 2)))(list "Juan Computación"))
(define (acceder2 l)(map alumno-nombre l))

; faltas-regulares: List(Struct) -> List(Number)
; Sumará todas las faltas de los alumnos regulares mediante el fooldr "suma",
; antes filtrará mediante la función regular? si el alumno cumple las condiciones
(check-expect(faltas-regulares(list (make-alumno "Juan Computación" 7 2)))2)
(define (faltas-regulares l) (suma (acceder(filter regular? l))))
(faltas-regulares (list (make-alumno "Juan Computación" 7 2)
                        (make-alumno "Carlos Software" 3.5 4)
                        (make-alumno "Ada Lovelace" 10 1)))

; faltas?: Struct -> Boolean
; Evaluará si las faltas del alumno son mayores o iguales a 3
(check-expect(faltas?(make-alumno "Juan Computación" 7 2))#f)
(define (faltas? l)(>= (alumno-faltas l) 3))

; promovidos-ausentes: List(Struct) -> List(Struct)
; Filtrará mediante la funcion faltas? aquellos alumnos promovidos (filtrado mediante promovido?) que superen las 3 faltas
(check-expect(promovidos-ausentes(list (make-alumno "Juan Computación" 9 3)))(list (make-alumno "Juan Computación" 9 3)))
(define(promovidos-ausentes l)(filter faltas?(filter promovido? l)))
 (promovidos-ausentes (list (make-alumno "Juan Computación" 9 3)
                        (make-alumno "Ada Lovelace" 10 1)
                        (make-alumno "Carlos Software" 3.5 2)))
