#lang racket


(define (PDC n pos)
  (cond [(and (number? n) (list? pos)) (pasoDos (crearListaLogica n) pos 0 (crearMatriz 1 1 n '()) n)]
        [else "Porfavor introduzca parametros validos"]))

;; Nombre: pasoDos
;; Parametros: listaLogica pos paso matrizSol n
;; Descripcion: En base al algoritmo desarrollado esta funcion consta de p a fase 2 de este algoritmo, aqui aun no se realiza ninguna condicion de desempate,
;; sino que la funcion retorna una lista con las casillas a als que el caballo se puede mover que NO han sido visitadas y que tienen la menor cantidad de aristas
;; en caso de que el largo de la lista sea igual a uno significa que el caballo puede moverse ahi
;; Retorna: una lista de casillas disponibles para realizar un movimiento

(define (pasoDos listaLogica pos paso matrizSol n)
  (cond [(> paso (* n n)) listaLogica]
        [else (getCasillasAristas pos listaLogica (getNOvisitados pos listaLogica (getDestinos listaLogica (car pos) (cadr pos)) '() (calcularAristas (car pos) (cadr pos) n)) '() (length (getNOvisitados pos listaLogica (getDestinos listaLogica (car pos) (cadr pos)) '() (calcularAristas (car pos) (cadr pos) n))) (length (getNOvisitados pos listaLogica (getDestinos listaLogica (car pos) (cadr pos)) '() (calcularAristas (car pos) (cadr pos) n))) n) ]))
        


;; Nombre: getCasillasAristas
;; Parametros: pos listaLogica noVisitados(lista generada por la funcion getNOvisitados) destinosPosibles contador flag  n
;; Descripcion: Realiza un trabajo similar a getNOvisitados, pero en vez de buscar que casillas no han sido visitadas busca las de menor cantidad de aristas
;; Retorna: Una lista de pares ordenados con los pares ordenados que perteneces a las casillas con menos aristas

(define (getCasillasAristas pos listaLogica noVisitados destinosPosibles contador flag n)
  (cond [(= contador 0) destinosPosibles]
        [(= contador  flag) (getCasillasAristas pos listaLogica (cdr noVisitados) (append destinosPosibles (list (car noVisitados))) (- contador 1) flag n)]
        [(and (not (null? noVisitados)) (< (calcularAristas (car (car noVisitados)) (cadr (car noVisitados)) n) (calcularAristas (car (car destinosPosibles)) (cadr (car destinosPosibles)) n))) (getCasillasAristas pos listaLogica (cdr noVisitados) (list (car noVisitados)) (- contador 1) flag n)]
        [(and (not (null? noVisitados)) (= (calcularAristas (car (car noVisitados)) (cadr (car noVisitados)) n) (calcularAristas (car (car destinosPosibles)) (cadr (car destinosPosibles)) n))) (getCasillasAristas pos listaLogica (cdr noVisitados) (append destinosPosibles (list (car noVisitados))) (- contador 1) flag n)]
        [else (getCasillasAristas pos listaLogica (cdr noVisitados) destinosPosibles (- contador 1) flag n)]))

;; Nombre getNOvisitados
;; Parametros: pos listaLogica destinos(lista general de posibles destinos) noVisitados contador
;; Descripcion: De la lista general que cada casilla tiene la funcion se encarga de encontrar dentro de esta lista de destinos los destinos que no han sido visitados y los mete dentro de la lista noVisitados
;; Retorna: Una lista con los destinos posibles desde la casilla actual que NO han siod visitados

(define (getNOvisitados pos listaLogica destinos noVisitados contador)
  (cond [(= contador 0) noVisitados]
        [(= (visitado? listaLogica (caar destinos) (cadar destinos)) 0) (getNOvisitados pos listaLogica (cdr destinos) (append noVisitados (list (car destinos))) (- contador 1))]
        [else (getNOvisitados pos listaLogica (cdr destinos) noVisitados (- contador 1))]))


;; Nombre: actualizarLista
;; Parametros: listaLogica i j pos paso listaActualizada n
;; Descripcion: Esta funcion se debe de llamar a la hora de realizar un movimiento del caballo. Se encarga de construir una nueva lista logica en la que se ve actualizado el valor
;; de visitado con el parametro paso en la casilla actual
;; nota(creo): remover en los destinosPosibles de la casilla actual la casilla a la cual se va a mover
(define (actualizarLista listaLogica i j pos paso listaActualizada n)
  (cond [(> i n) listaActualizada]
        [(and (<= j n) (= (revisarPosicion (car pos) (cadr pos) listaLogica) 1))
         (actualizarLista (cdr listaLogica) i (+ j 1) pos paso (append listaActualizada (cons (list (calcularAristas i j n) (list i j) paso (crearDestinos '() i j (calcularAristas i j n) n)) '() )) n)]
        [(<= j n) (actualizarLista (cdr listaLogica) i (+ j 1) pos paso (append listaActualizada (list (car listaLogica))) n)]
        [else (actualizarLista  listaLogica (+ i 1) 1 pos paso listaActualizada n)]))

;; Nombre: crearListaLogica
;; Parametros: n (valor entero para crear la matriz nxn)
;; Descripcion: Esta funcion es la base para poder ejecutar el algortimo. Crea una matriz con todos los datos necesarios de cada casilla,
;; como su cantidad de aristas, si ya fue visitado ya tambien los destinos posibles desde la la casilla a partir de todas las funciones que se llaman
;; luego de llamar a esta, pero basicamnete solo revisa que el valor de n sea mayor a 5 y sino llama a crearMatrizAux
;; Retorna: una lista logica

(define (crearListaLogica n)
  (cond [(< n 5) "Solo se aceptan matrices nxn con n >= 5"] 
        [else (crearListaAux 1 1 n '())]))


;; Nombre: crearListaAux
;; Parametros: i (valor inicial de recorrido para filas), j (valor inicial de recorrido para columnas), n_og (dimension de la matriz), lista (la lista logica)
;; Descripcion: Esta funcion se encarga de crear las filas y las columnas de la matriz por medio de un incremento en los valores i y j. Ademas a la hora de crear
;; las columnas utiliza la funcion calcularArista y obtenerDestinos de forma que cada casilla tenga el siguiente formato para poder realizar el algoritmo:
;; (cantidad_aristas (i j) visitado (posiblesDestinos)). Cada elemento de la matriz es una lista con un entero representando la cantidad de aristas,
;; un par ordenado representando su posicion en la matriz, un entero que una vez diferente de 0 significa que la casilla fue visitada y una lista de
;; pares ordenados conteniendo movimientos que se puede realizar desde la actual casilla

(define (crearListaAux i j n_og lista)
  (cond [(> i n_og) lista]
        [(<= j n_og) (crearListaAux i (+ j 1) n_og (append lista (cons (list (calcularAristas i j n_og) (list i j) 0 (crearDestinos '() i j (calcularAristas i j n_og) n_og)) '() )))]
        [else (crearListaAux  (+ i 1) 1 n_og lista)]))



;; Nombre: calcularArista
;; Parametros: i j n_og
;; Descripcion: Dependiendo de su posicion en la matriz a cada casilla se le asigna su cantidad de aristas, es decir la cantidad de movimientos que se pueden
;; realizar desde la casilla
;; Retorna: un entero equivalente al numero de aristas de la casilla

(define (calcularAristas i j n_og)
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



;; Nombre: crearDestinos
;; Parametros: i j cant_aristas n_og
;; Descripcion: Como cada casilla tiene la misma cantidad de destinos que cantidad de aristas esta funcion funciona como un filtro donde
;; dependiendo de la cantidad de aristas de la respectiva casilla se llama a una distinta funcion acorde a su cantidad de aristas como lo son
;; las funciones destinosXaristas. Estas funciones calculan dependiendo de su posicion en la matriz los destinos posibles de cada casilla en especifico.
;; Retorna: una lista de destinos 

(define (crearDestinos listaDestinos i j cant_aristas n_og)
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



;; Nombre: getDestinos
;; Parameteros: lista (lista logica), i(fila del elemento), j(columna del elemento)
;; Descripcion: Basandose en los parametros i j esta funcion busca esta casilla dentro de la lista logica y retorna una lista con los destinos de dicha casilla
;; Retorna: Una lista con destinos


(define (getDestinos lista i j)  
  (cond [(= (revisarPosicion i j lista) 1)(car (cdddr (car lista)))]
        [else (getDestinos (cdr lista) i j)]))

(define (revisarPosicion i j lista)
  (cond [(and (= i (car (car (cdr (car lista))))) (= j (cadr (car (cdr (car lista)))))) 1]
        [else 0]))



;; Nombre: visitado?
;; Parametros: lista (lista logica), i(fila del elemento), j(columna del elemento)
;; Descripcion: Basicamente busca dentro de la lista basanadose en las posiciones i j de la casilla el valor de visitado de la casilla
;; y si el valor de visitado es distinto de 0 retorna un 0, sino un 1
;; Retorna: 1 o 0


(define (visitado? lista i j)
  (cond [(= (revisarPosicion i j lista) 1) (checkVisitado (car (cddr (car lista))))]
        [else (visitado? (cdr lista) i j)]))

(define (checkVisitado bool)
  (cond [(= bool 0) 0]
        [else 1]))


;; Nombre: crearMatriz
;; Paramtetros: n (dimension nxn de la matriz) i (filas), j (columnas), matriz (matriz que se retorna como solucion)
;; Retorna: una matriz nxn en el que cada elemento es una lista vacia

(define (crearMatriz i j n matriz)
  (cond [(> i n) matriz]
        [(<= j n) (crearMatriz i (+ j n) n (append matriz (list (crearFila i 1 n '())) ))]
        [else (crearMatriz  (+ i 1) 1 n matriz)]))

(define (crearFila i j n fila)
  (cond [(> j n) fila]
        [else (crearFila i (+ j 1) n (append fila (cons '() '())))]))