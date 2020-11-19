#lang racket
;; Nombre: crearMatrizLogica
;; Parametros: n (valor entero para crear la matriz nxn acorde a este valor)
;; Descripcion: Esta funcion es la base para poder ejecutar el algortimo. Crea una matriz con todos los datos necesarios de cada casilla,
;; como su cantidad de aristas, si ya fue visitado ya tambien los destinos posibles desde la la casilla a partir de todas las funciones que se llaman
;; luego de llamar a esta, pero basicamnete solo revisa que el valor de n sea mayor a 5 y sino llama a crearMatrizAux

(define (crearMatrizLogica n)
  (cond [(< n 5) "Solo se aceptan matrices nxn con n >= 5"] 
        [else (crearMatrizAux 1 1 n '())]))


;; Nombre: crearMatrizAux
;; Parametros: i (valor inicial de recorrido para filas), j (valor inicial de recorrido para columnas), n_og (dimension de la matriz), matriz (la matriz logica)
;; Descripcion: Esta funcion se encarga de crear las filas y las columnas de la matriz por medio de un incremento en los valores i y j. Ademas a la hora de crear
;; las columnas utiliza la funcion calcularArista y obtenerDestinos de forma que cada casilla tenga el siguiente formato para poder realizar el algoritmo:
;; (cantidad_aristas (i j) visitado (posiblesDestinos)). Cada elemento de la matriz es una lista con un entero representando la cantidad de aristas,
;; un par ordenado representando su posicion en la matriz, un entero que una vez diferente de 0 significa que la casilla fue visitada y una lista de
;; pares ordenados conteniendo movimientos que se puede realizar desde la actual casilla

(define (crearMatrizAux i j n_og matriz)
  (cond [(> i n_og) matriz]
        [(<= j n_og) (crearMatrizAux i (+ j 1) n_og (append matriz (cons (list (calcularArista i j n_og) (list i j) 0 (obtenerDestinos '() i j (calcularArista i j n_og) n_og)) '() )))]
        [else (crearMatrizAux  (+ i 1) 1 n_og matriz)]))



;; Nombre: calcularArista
;; Parametros: i j n_og
;; Descripcion: Dependiendo de su posicion en la matriz a cada casilla se le asigna su cantidad de aristas, es decir la cantidad de movimientos que se pueden
;; realizar desde la casilla

(define (calcularArista i j n_og)
  (cond [(and (= i 1) (= j 1)) 2]
        [(and (= i 1) (= j n_og)) 2]
        [(and (= i n_og) (= j 1)) 2]
        [(and (= i n_og) (= j n_og)) 2]
        [(and (> j 2) (< j (- n_og 1)) (> i 2) (< i (- n_og 1))) 8]
        [(or (and (= i 2) (> j 2) (< j (- n_og 1))) (and (= i (- n_og 1)) (> j 2) (< j (- n_og 1)))) 6]
        [(or (and (= j 2) (> i 2) (< i (- n_og 1))) (and (= j (- n_og 1)) (> i 2) (< i (- n_og 1)))) 6]
        [(or (and (= i 1) (= j 2)) (and (= i 1) (= j (- n_og 1))) (and (= i (- n_og 1)) (= j 1)) (and (= i (- n_og 1)) (= j n_og))
             (and (= i 2) (= j 1)) (and (= i 2) (= j n_og)) (and (= i n_og) (= j 2)) (and (= i n_og) (= j (- n_og 1)))) 3]
        [else 4]))



;; Nombre: obtenerDestinos
;; Parametros: i j cant_aristas n_og
;; Descripcion: Como cada casilla tiene la misma cantidad de destinos que cantidad de aristas esta funcion funciona como un filtro donde
;; dependiendo de la cantidad de aristas de la respectiva casilla se llama a una distinta funcion acorde a su cantidad de aristas como lo son
;; las funciones destinosXaristas. Estas funciones calculan dependiendo de su posicion en la matriz los destinos posibles de cada casilla en especifico.

(define (obtenerDestinos listaDestinos i j cant_aristas n_og)
  (cond [(= cant_aristas 2) (destinos2aristas i j n_og '())]
        [(= cant_aristas 3) (destinos3aristas i j n_og '())]
        [(= cant_aristas 4) (destinos4aristas i j n_og '())]
        [(= cant_aristas 6) (destinos6aristas i j n_og '())]
        [(= cant_aristas 8) (destinos8aristas i j n_og '())]))

(define (destinos2aristas i j n_og listaDestinos)
  (cond [(and (= i 1) (= j 1)) (append listaDestinos (cons (list  2 3) '()) (cons (list  3 2) '()))]
        [(and (= i 1) (= j n_og)) (append listaDestinos (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 1) (- j 2)) '()))]
        [(and (= i n_og) (= j 1)) (append listaDestinos (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (- i 1) (+ j 2)) '()))]
        [else (append listaDestinos (cons (list  (- i 1) (- j 2)) '()) (cons (list  (- i 2) (- j 1)) '()))]))

(define (destinos3aristas i j n_og listaDestinos)
  (cond [(and (= i 1) (= j 2))(append listaDestinos (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '()) (cons (list  (+ i 1) (+ j 2)) '()))]
        [(and (= i 1) (= j (- n_og 1))) (append listaDestinos (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '()) (cons (list  (+ i 1) (- j 2)) '()))]
        [(and (= i (- n_og 1)) (= j 1)) (append listaDestinos (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (+ i 1) (+ j 2)) '()) (cons (list  (- i 1) (+ j 2)) '()))]
        [(and (= i (- n_og 1)) (= j n_og)) (append listaDestinos (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (- i 1) (- j 2)) '()) (cons (list  (- i 2) (- j 1)) '()))]
        [(and (= i 2) (= j 1)) (append listaDestinos (cons (list  (+ i 1) (+ j 2)) '()) (cons (list  (- i 1) (+ j 2)) '()) (cons (list  (+ i 2) (+ j 1)) '()))]
        [(and (= i 2) (= j n_og)) (append listaDestinos (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (- i 1) (- j 2)) '()) (cons (list  (+ i 2) (+ j 1)) '()))]
        [(and (= i n_og) (= j 2)) (append listaDestinos (cons (list  (- i 2) (- j 1)) '()) (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (- i 1) (+ j 2)) '()))]
        [(and (= i n_og) (= j (- n_og 1))) (append listaDestinos (cons (list  (- i 2) (- j 1)) '()) (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (- i 1) (- j 2)) '()))] ))

(define (destinos4aristas i j n_og listaDestinos)
  (cond [(= i 1 ) (append listaDestinos (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '()) (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (+ i 1) (+ j 2)) '()))]
        [(= i n_og) (append listaDestinos (cons (list  (- i 2) (- j 1)) '()) (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (- i 1) (- j 2)) '()) (cons (list  (- i 1) (+ j 2)) '()))]
        [(= j 1) (append listaDestinos (cons (list  (+ i 2) (+ j 1)) '()) (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (- i 1) (+ j 2)) '()) (cons (list  (+ i 1) (+ j 2)) '()))]
        [(= j n_og) (append listaDestinos (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (- i 2) (- j 1)) '()) (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (- i 1) (- j 2)) '()))]
        [(and (= i 2) (= j 2)) (append listaDestinos (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '()) (cons (list  (+ i 1) (+ j 2)) '()) (cons (list  (- i 1) (+ j 2)) '()))]
        [(and (= i 2) (= j (- n_og 1))) (append listaDestinos (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '()) (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (- i 1) (- j 2)) '()))]
        [(and (= i (- n_og 1)) (= j 2)) (append listaDestinos (cons (list  (- i 2) (- j 1)) '()) (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (+ i 1) (+ j 2)) '()) (cons (list  (- i 1) (+ j 2)) '()))]
        [(and (= i (- n_og 1)) (= j (- n_og 1))) (append listaDestinos (cons (list  (- i 2) (- j 1)) '()) (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (- i 1) (- j 2)) '()))]))

(define (destinos6aristas i j n_og listaDestinos)
  (cond [(= i 2) (append listaDestinos (cons (list  (- i 1) (+ j 2)) '()) (cons (list  (- i 1) (- j 2)) '()) (cons (list  (+ i 1) (+ j 2)) '())
                                       (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '()))]
        [(= i (- n_og 1)) (append listaDestinos (cons (list  (- i 1) (+ j 2)) '()) (cons (list  (- i 1) (- j 2)) '()) (cons (list  (+ i 1) (+ j 2)) '())
                                                (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (- i 2) (- j 1)) '()) (cons (list  (- i 2) (+ j 1)) '()))]
        [(= j 2) (append listaDestinos (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (- i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '())
                                       (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 1) (+ j 2)) '()) (cons (list  (- i 1) (+ j 2)) '()))]
        [(= j (- n_og 1)) (append listaDestinos (cons (list  (- i 2) (+ j 1)) '()) (cons (list  (- i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '())
                                       (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (- i 1) (- j 2)) '()))]))

(define (destinos8aristas i j n_og listaDestinos)
  (append listaDestinos  (cons (list  (- i 1) (- j 2)) '()) (cons (list  (- i 1) (+ j 2)) '()) (cons (list  (- i 2) (- j 1)) '()) (cons (list  (- i 2) (+ j 1)) '())
          (cons (list  (+ i 2) (- j 1)) '()) (cons (list  (+ i 2) (+ j 1)) '()) (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (+ i 1) (+ j 2)) '())))