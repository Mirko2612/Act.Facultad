;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname racket_Unidad_2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;UNIDAD 2
;PRACTICA 2

;2
;distancia_puntos: Number Number Number Number -> Number
;Recibe 2 puntos y devuelve su distancia
;entrada: (1,7)(4,3), salida: 5
;entrada: (2,3) (5,-1), salida: 5
(define (distancia_puntos x1 y1 x2 y2)(sqrt(+(sqr(- x2 x1))(sqr(- y2 y1)))))
(distancia_puntos 1 7 4 3)
(distancia_puntos 2 3 5 -1)

;3
;vol_cubo: Number -> Number
;Recibe la longitud de una arista y devuelve el volumen del cubo al que pertenece
;entrada: 3, salida: 27
;entrada 4, salida: 64
(define (vol_cubo x)(expt x 3))
(vol_cubo 3)
(vol_cubo 4)

;4
;area_cubo: Number -> Number
;Recibe la longitud de una arista y devuelve el area del cubo al que pertenece
;entrada: 3, salida: 9
;entrada 4, salida: 16
(define (area_cubo x)(sqr x))
(area_cubo 3)
(area_cubo 4)

;5
;string-insert: String Number -> String
;Recibe un string y un numero i, en el cual se iterará esa posicion y, se agreará un "-" en dicha posicion
;entrada: "Holamundo" 4, salida: "Hola-mundo"
;entrada: "Github" 3, salida: "Git-hub"
(define (string-insert x i)(string-append(substring x 0 i)"-"(substring x i)))
(string-insert "Holamundo" 4)
(string-insert "Github" 3)

;6
;string-last: String -> String
;Recibe una cadena de caracteres y se extrae el ultimo
;entrada: Sol, salida: l
;entrada: 30 de marzo de 2024, salida: 4
(define (string-last x)(substring x (-(string-length x)1)(string-length x)))
(string-last "Sol")
(string-last "30 de marzo de 2024")

;7
;string-remove-last: String -> String
;Recibe una cadena de caracteres y se elimina el ultimo
;entrada: Sol, salida: So
;entrada: 30 de marzo de 2024, salida: 30 de marzo de 202
(define (string-remove-last x)(substring x 0 (-(string-length x)1)))
(string-remove-last "Sol")
(string-remove-last "30 de marzo de 2024")

;8
;(check-expect (vol_cubo 3)27)

;9
;AREA DE DEFINICIONES:
(define valor_cuota 650)
;desc_2p: Number -> Number
;Multiplica el valor de la cuota por el 10%, obteniendo el descuento que se le debe hacer en caso de que se anoten 2 personas
;entrada: 650, salida: 65
(define desc_2p (* valor_cuota 0.10))
;desc_i: Number-> Number
;Multiplica el valor de la cuota por el 20%, obteniendo el descuento que se le debe hacer en caso de que se anoten 3 o mas personas
;entrada: 3 650, salida: 130
(define desc_i (* valor_cuota 0.20))
;desc_2m: Number -> Number
;Multiplica el valor de la cuota por el 15%, obteniendo el descuento que se le debe hacer en caso de que se paguen 2 meses juntos
;entrada: 650, salida: 97,5
(define desc_2m (* valor_cuota 0.15))
;desc_i: Number-> Number
;Multiplica el valor de la cuota por el 25%, obteniendo el descuento que se le debe hacer en caso de que se cancelen 3 o mas meses
;entrada: 3 650, salida: 162.5
(define desc_im (* valor_cuota 0.25))

(define desc_lim (* valor_cuota 0.35))
;AREA DE FUNCIONES
;cant_amigos Number -> Number 
;Calcula el valor de la cuota dependiendo de los amigos que se anoten, multiplicando x cantidad de amigos por el valor total de la cuota con desucentos incluidos
;entrada 5, salida: 2600
(define (cant_amigos x)(cond[(= x 1)valor_cuota]
                            [(= x 2)(- valor_cuota desc_2p)]
                            [(>= x 3)(- valor_cuota desc_i)]))
;CUERPO PRINCIPAL DEL PROGRAMA
;monto_persona Number Number -> Number
;Multiplica la cantidad de meses que se decide pagar por la resta entre: el valor de la cuota original; el descuento por la cantidad de amigos y el descuento por los meses a pagar
;entrada 2 2, salida: 975
(define (monto_persona a b)(cond[(and(= a 1)(= b 1))valor_cuota]
                                [(and(>= a 3)(>= b 3))(*(- valor_cuota desc_lim)b)]
                                [(and(= a 2)(= b 2))(*(- (cant_amigos a) desc_2m) 2)]
                                [else (*(- (cant_amigos a) desc_im)b)]))

;10
;AREA DE DEFINICIONES
(define lim_1m 13)
(define lim1_6m 10)
(define lim6_12m 11)
(define lim1_5a 11.5)
(define lim5_10a 12.6)
(define lim_10a 13)
;AREA DE FUNCIONES
;anemia? Number Number -> Boolean
;Compara, una vez ingresada la edad (en meses), el valor de la hemoglobina (en g/dl) con el nivel minimo establecido por edad
;entrada 7 10, salida: true
(define (anemia? a b)(cond[(<= a 1)(not(>= b lim_1m))]
                          [(<= a 6)(not(>= b lim1_6m))]
                          [(<= a 12)(not(>= b lim6_12m))]
                          [(<= a 60)(not(>= b lim1_5a))]
                          [(<= a 120)(not(>= b lim5_10a))]
                          [else (not(>= b lim_10a))]
                          ))

;12
;AREA DE DEFINICIONES
(define c_ciudad_2 8)
(define c_ruta_2 11)
(define c_ciudad_3 8.8)
(define c_ruta_3 12.1)
;AREA DE FUNCIONES
;autonomia Number Number -> String
;Multiplica la cantidad de litros restantes en el tanque por los kilometros que puede recorrer con 1 solo litro según la marca indicada
;Entrada 2 20, salida:"Autonomía en ciudad: 160km, Autonomía en ruta: 220km"
(define (autonomia x y)(cond[(= x 2)(string-append "Autonomía en ciudad: "(number->string (* y c_ciudad_2))"km,"
                                                   " Autonomía en ruta: " (number->string (* y c_ruta_2))"km")]
                            [(= x 3)(string-append "Autonomía en ciudad: "(number->string (* y c_ciudad_3))"km,"
                                                   " Autonomía en ruta: " (number->string (* y c_ruta_3))"km")]))
(autonomia 2 20)
(autonomia 3 20)