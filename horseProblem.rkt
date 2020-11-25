#lang racket

;; Nombre: PDC-Sol
;; Parametros: n pos
;; Descripcion: Esta funcion desencadena todo el algoritmo en busca de una solucion desde la casillas actual (pos) en una matriz nxn
(define (PDC n pos)
  (cond [(and (number? n) (list? pos)) (PDC_aux (crearListaLogica n) pos 1 n  pos '())]
        [else "Porfavor introduzca parametros validos"]))

(define (PDC2 n pos)
  (cond [(and (number? n) (list? pos)) (PDC2_aux (crearListaLogica n) pos 1 n)]
        [else "Porfavor introduzca parametros validos"]))

(define (PDC2_aux listaLog pos paso n)
  (backtracking listaLog (getNodo listaLog pos) pos paso n '()))

(define (PDC_aux listaLogica pos paso n posinicial res)
  (paso_dos pos (actualizarLista listaLogica 1 1 pos paso '() n) paso (getCasillasAristas listaLogica (getNOvisitados pos listaLogica (getDestinos listaLogica (car pos) (cadr pos)) '() (calcularAristas (car pos) (cadr pos) n)) '() (length (getNOvisitados pos listaLogica (getDestinos listaLogica (car pos) (cadr pos)) '() (calcularAristas (car pos) (cadr pos) n))) (length (getNOvisitados pos listaLogica (getDestinos listaLogica (car pos) (cadr pos)) '() (calcularAristas (car pos) (cadr pos) n))) n) n posinicial))

;; Nombre: paso_dos
;; Parametros: pos listaLogica pos paso paso2res(lista generada con getCasillasAristas) n
;; Descripcion: En base al algoritmo desarrollado esta funcion consta de la fase 2 de este algoritmo, aqui aun no se realiza ninguna condicion de desempate,
;; sino que la funcion retorna una lista con las casillas a als que el caballo se puede mover que NO han sido visitadas y que tienen la menor cantidad de aristas
;; en caso de que el largo de la lista sea igual a uno significa que el caballo puede moverse ahi
;; Retorna: una lista de casillas disponibles para realizar un movimiento

(define (paso_dos pos listaLogica paso paso2res n posinicial)
  (cond ;[(= paso 22) (list paso paso2res (crearMatriz 1 1 n '() listaLogica))]
        [(= (length paso2res) 1)
         (paso_dos (list (car (car paso2res)) (cadr (car paso2res))) (actualizarLista listaLogica 1 1 (car paso2res) (+ paso 1) '() n) (+ paso 1) (getCasillasAristas listaLogica (getNOvisitados (car paso2res) listaLogica (getDestinos listaLogica (car (car paso2res)) (cadr (car paso2res))) '() (calcularAristas (car (car paso2res)) (cadr (car paso2res)) n)) '() (length (getNOvisitados (car paso2res) listaLogica (getDestinos listaLogica (car (car paso2res)) (cadr (car paso2res))) '() (calcularAristas (car (car paso2res)) (cadr (car paso2res)) n))) (length (getNOvisitados (car paso2res) listaLogica (getDestinos listaLogica (car (car paso2res)) (cadr (car paso2res))) '() (calcularAristas (car (car paso2res)) (cadr (car paso2res)) n))) n) n posinicial) ]
        ;[else (paso_dos pos listaLogica paso (cdr paso2res) n posinicial (res append (paso_dos (list (car (car paso2res)) (cadr (car paso2res))) (actualizarLista listaLogica 1 1 (car paso2res) (+ paso 1) '() n) (+ paso 1) (car paso2res) n posinicial res)))]
        [else (paso_tres pos listaLogica paso (getCasillasEsquinas listaLogica paso2res '() (length paso2res) (length paso2res) n) n posinicial)]
        ))

;; Nombre: paso_tres
;; Parametros: pos listaLogica pos paso paso3res(lista generada con getCasillasEsquinas) n
;; Descripcion: Esta funcion es una de las condiciones de desempate de las diferentes casillas disponibles para moverse, este desempate se basa
;; en buscar la casilla mas cercana a una esquina, y si aun hay varias que cumplen la condicion se procede al paso_4
;; Retorna: una lista con posibles destinos

(define (paso_tres pos listaLogica paso paso3res n posinicial)
  (cond [(= (length paso3res) 1) (paso_dos (list (car (car paso3res)) (cadr (car paso3res))) (actualizarLista listaLogica 1 1 (car paso3res) (+ paso 1) '() n) (+ paso 1) (getCasillasAristas listaLogica (getNOvisitados (list (car (car paso3res)) (cadr (car paso3res))) listaLogica (getDestinos listaLogica (car (list (car (car paso3res)) (cadr (car paso3res)))) (cadr (list (car (car paso3res)) (cadr (car paso3res))))) '() (calcularAristas (car (list (car (car paso3res)) (cadr (car paso3res)))) (cadr (list (car (car paso3res)) (cadr (car paso3res)))) n)) '() (length (getNOvisitados (list (car (car paso3res)) (cadr (car paso3res))) listaLogica (getDestinos listaLogica (car (list (car (car paso3res)) (cadr (car paso3res)))) (cadr (list (car (car paso3res)) (cadr (car paso3res))))) '() (calcularAristas (car (list (car (car paso3res)) (cadr (car paso3res)))) (cadr (list (car (car paso3res)) (cadr (car paso3res)))) n))) (length (getNOvisitados (list (car (car paso3res)) (cadr (car paso3res))) listaLogica (getDestinos listaLogica (car (list (car (car paso3res)) (cadr (car paso3res)))) (cadr (list (car (car paso3res)) (cadr (car paso3res))))) '() (calcularAristas (car (list (car (car paso3res)) (cadr (car paso3res)))) (cadr (list (car (car paso3res)) (cadr (car paso3res)))) n))) n) n posinicial)]
        [else (paso_cuatro pos listaLogica paso (getCasillasPared listaLogica paso3res '() (length paso3res) (length paso3res) n) n posinicial)]))

;; Nombre: paso_cuatro
;; Parametros: pos listaLogica paso paso4res(listaGenerada con getCasillasPared) n
;; Descripcion: El paso 4 y penultimo criterio de desempate del algoritmo se basa en escoger las casillas basandose en su cercania a una de las paredes del tablero, en este caso
;; se utilizara la columna 1 de la matriz para este criterio de desempate

(define (paso_cuatro pos listaLogica paso paso4res n posinicial)
  (cond [(= (length paso4res) 1) (paso_dos (list (car (car paso4res)) (cadr (car paso4res))) (actualizarLista listaLogica 1 1 (car paso4res) (+ paso 1) '() n) (+ paso 1) (getCasillasAristas listaLogica (getNOvisitados (list (car (car paso4res)) (cadr (car paso4res))) listaLogica (getDestinos listaLogica (car (list (car (car paso4res)) (cadr (car paso4res)))) (cadr (list (car (car paso4res)) (cadr (car paso4res))))) '() (calcularAristas (car (list (car (car paso4res)) (cadr (car paso4res)))) (cadr (list (car (car paso4res)) (cadr (car paso4res)))) n)) '() (length (getNOvisitados (list (car (car paso4res)) (cadr (car paso4res))) listaLogica (getDestinos listaLogica (car (list (car (car paso4res)) (cadr (car paso4res)))) (cadr (list (car (car paso4res)) (cadr (car paso4res))))) '() (calcularAristas (car (list (car (car paso4res)) (cadr (car paso4res)))) (cadr (list (car (car paso4res)) (cadr (car paso4res)))) n))) (length (getNOvisitados (list (car (car paso4res)) (cadr (car paso4res))) listaLogica (getDestinos listaLogica (car (list (car (car paso4res)) (cadr (car paso4res)))) (cadr (list (car (car paso4res)) (cadr (car paso4res))))) '() (calcularAristas (car (list (car (car paso4res)) (cadr (car paso4res)))) (cadr (list (car (car paso4res)) (cadr (car paso4res)))) n))) n) n posinicial)]     
        [(> (length paso4res) 1) (paso_cinco pos listaLogica paso (getCasillaCuadrante pos paso4res '() 10) n posinicial)]
        [else listaLogica]))

;; Nombre: paso_cinco
;; Parametros: pos listaLogica paso paso5res(lista generada con getCasillasCuadrante) n posinicial
;; Descripcion: El ultimo criterio de desempate del algoritmo se basa en escoger las casillas basandose en escoger sus movimientos segun la posicion de los destinos posibles
;; de acuerdo al sentido horario de un reloj

(define (paso_cinco pos listaLogica paso paso5res n posinicial)
  (paso_dos (list (car (car paso5res)) (cadr (car paso5res))) (actualizarLista listaLogica 1 1 (car paso5res) (+ paso 1) '() n) (+ paso 1) (getCasillasAristas listaLogica (getNOvisitados (list (car (car paso5res)) (cadr (car paso5res))) listaLogica (getDestinos listaLogica (car (list (car (car paso5res)) (cadr (car paso5res)))) (cadr (list (car (car paso5res)) (cadr (car paso5res))))) '() (calcularAristas (car (list (car (car paso5res)) (cadr (car paso5res)))) (cadr (list (car (car paso5res)) (cadr (car paso5res)))) n)) '() (length (getNOvisitados (list (car (car paso5res)) (cadr (car paso5res))) listaLogica (getDestinos listaLogica (car (list (car (car paso5res)) (cadr (car paso5res)))) (cadr (list (car (car paso5res)) (cadr (car paso5res))))) '() (calcularAristas (car (list (car (car paso5res)) (cadr (car paso5res)))) (cadr (list (car (car paso5res)) (cadr (car paso5res)))) n))) (length (getNOvisitados (list (car (car paso5res)) (cadr (car paso5res))) listaLogica (getDestinos listaLogica (car (list (car (car paso5res)) (cadr (car paso5res)))) (cadr (list (car (car paso5res)) (cadr (car paso5res))))) '() (calcularAristas (car (list (car (car paso5res)) (cadr (car paso5res)))) (cadr (list (car (car paso5res)) (cadr (car paso5res)))) n))) n) n posinicial))

;; Nombre: getCasillascuadrante
;; Parametros: pos paso5res res prioridad
;; Descripcion: Segun la prioridad donde 1 es el primer movimiento en sentido horario y 8 el ultimo la funcion se encarga de guardar en la variable res
;; el destino con la prioridad mas cercana a 1

(define (getCasillaCuadrante pos paso5res res prioridad)
  (cond [(null? paso5res) res]
        [(and (= (car (car paso5res)) (- (car pos) 2)) (= (cadr (car paso5res)) (+ (cadr pos) 1)) (< 1 prioridad)) (getCasillaCuadrante pos (cdr paso5res) (cons (list (car (car paso5res)) (cadr (car paso5res)))'()) 1)] 
        [(and (= (car (car paso5res)) (- (car pos) 1)) (= (cadr (car paso5res)) (+ (cadr pos) 2)) (< 2 prioridad)) (getCasillaCuadrante pos (cdr paso5res) (cons (list (car (car paso5res)) (cadr (car paso5res)))'()) 2)]
        [(and (= (car (car paso5res)) (+ (car pos) 1)) (= (cadr (car paso5res)) (+ (cadr pos) 2)) (< 3 prioridad)) (getCasillaCuadrante pos (cdr paso5res) (cons (list (car (car paso5res)) (cadr (car paso5res)))'()) 3)]
        [(and (= (car (car paso5res)) (+ (car pos) 2)) (= (cadr (car paso5res)) (+ (cadr pos) 1)) (< 4 prioridad)) (getCasillaCuadrante pos (cdr paso5res) (cons (list (car (car paso5res)) (cadr (car paso5res)))'()) 4)]
        [(and (= (car (car paso5res)) (+ (car pos) 2)) (= (cadr (car paso5res)) (- (cadr pos) 1)) (< 5 prioridad)) (getCasillaCuadrante pos (cdr paso5res) (cons (list (car (car paso5res)) (cadr (car paso5res)))'()) 5)]
        [(and (= (car (car paso5res)) (+ (car pos) 1)) (= (cadr (car paso5res)) (- (cadr pos) 2)) (< 6 prioridad)) (getCasillaCuadrante pos (cdr paso5res) (cons (list (car (car paso5res)) (cadr (car paso5res)))'()) 6)]
        [(and (= (car (car paso5res)) (- (car pos) 1)) (= (cadr (car paso5res)) (- (cadr pos) 2)) (< 7 prioridad)) (getCasillaCuadrante pos (cdr paso5res) (cons (list (car (car paso5res)) (cadr (car paso5res)))'()) 7)]
        [(and (= (car (car paso5res)) (- (car pos) 2)) (= (cadr (car paso5res)) (- (cadr pos) 1)) (< 8 prioridad)) (getCasillaCuadrante pos (cdr paso5res) (cons (list (car (car paso5res)) (cadr (car paso5res)))'()) 8)]
        [else res]))

;; Nombre: getCasillasPared
;; Parametros: listaLogica noVisitados destinosPosibles contador flag n
;; Descripcion: Esta funcion lo que realiza es encontrar dentro de noVisitados las casillas mas cercanas a la pared de la izquierda del tablero
;; Retorna: Una lista con los posibles destinos

(define (getCasillasPared listaLogica noVisitados destinosPosibles contador flag n)
  (cond [(= contador 0) destinosPosibles]
        [(= contador flag) (getCasillasPared listaLogica (cdr noVisitados) (append destinosPosibles (list (car noVisitados))) (- contador 1) flag n)]
        [(and (not (null? noVisitados)) (< (distancia2puntos (car (car noVisitados)) (cadr (car noVisitados)) n 99999 6) (distancia2puntos (car (car destinosPosibles)) (cadr (car destinosPosibles)) n 99999 6)))
         (getCasillasPared listaLogica (cdr noVisitados) (list (car noVisitados)) (- contador 1) flag n)]
        [(and (not (null? noVisitados)) (= (distancia2puntos (car (car noVisitados)) (cadr (car noVisitados)) n 99999 6) (distancia2puntos (car (car destinosPosibles)) (cadr (car destinosPosibles)) n 99999 6)))
         (getCasillasPared listaLogica (cdr noVisitados) (append destinosPosibles (list (car noVisitados))) (- contador 1) flag n)]
        [else (getCasillasPared listaLogica (cdr noVisitados) destinosPosibles (- contador 1) flag n)]))

;; Nombre: getCasillasEsquinas
;; Parametros: listaLogica noVisitados(lista generada por la funcion getNOvisitados) destinosPosibles contador flag  n
;; Descripcion: Realiza un trabajo similar a getCasillasAristas, pero en vez de retornar una lista con los destinos disponibles de menor arista,
;; obtiene mas bien una lista con los destinos posibles que esten mas cerca de las esquinas.

(define (getCasillasEsquinas listaLogica noVisitados destinosPosibles contador flag n)
  (cond [(= contador 0) destinosPosibles]
        [(= contador flag) (getCasillasEsquinas listaLogica (cdr noVisitados) (append destinosPosibles (list (car noVisitados))) (- contador 1) flag n)]
        [(and (not (null? noVisitados)) (< (distancia2puntos (car (car noVisitados)) (cadr (car noVisitados)) n 99999 0) (distancia2puntos (car (car destinosPosibles)) (cadr (car destinosPosibles)) n 99999 0)))
         (getCasillasEsquinas listaLogica (cdr noVisitados) (list (car noVisitados)) (- contador 1) flag n)]
        [(and (not (null? noVisitados)) (= (distancia2puntos (car (car noVisitados)) (cadr (car noVisitados)) n 99999 0) (distancia2puntos (car (car destinosPosibles)) (cadr (car destinosPosibles)) n 99999 0)))
         (getCasillasEsquinas listaLogica (cdr noVisitados) (append destinosPosibles (list (car noVisitados))) (- contador 1) flag n)]
        [else (getCasillasEsquinas listaLogica (cdr noVisitados) destinosPosibles (- contador 1) flag n)]))

;; Nombre: distancia2puntos
;; Parametros: i j n distancia(valor por retornar) contador
;; Descripcion: Si el contador de esta funcion empieza en 1 la funcion se encarga de calcular la distancia de i y j a cada una de las esquinas,
;; guardando el menor valor siempre en la variable distancia. Solo el contador empieza en 6 realiza el mismo procedimiento pero no calculado con las esquinas, sino con las paredes
;; Retorna: Entero

(define (distancia2puntos i j n distancia contador)
  (cond [(= contador 5) distancia]
        [(= contador 10) distancia]
        [(and (= contador 1) (<= (sqrt (+ (expt (- i 1) 2) (expt (- j 1) 2))) distancia)) (distancia2puntos i j n (sqrt (+ (expt (- i 1) 2) (expt (- j 1) 2))) (+ contador 1))]
        [(and (= contador 2) (<= (sqrt (+ (expt (- i 1) 2) (expt (- j n) 2))) distancia)) (distancia2puntos i j n (sqrt (+ (expt (- i 1) 2) (expt (- j n) 2))) (+ contador 1))]
        [(and (= contador 3) (<= (sqrt (+ (expt (- i n) 2) (expt (- j 1) 2))) distancia)) (distancia2puntos i j n (sqrt (+ (expt (- i n) 2) (expt (- j 1) 2))) (+ contador 1))]
        [(and (= contador 4) (<= (sqrt (+ (expt (- i n) 2) (expt (- j n) 2))) distancia)) (distancia2puntos i j n (sqrt (+ (expt (- i n) 2) (expt (- j n) 2))) (+ contador 1))]
        [(and (= contador 6) (<= (- i 1) distancia)) (distancia2puntos i j n (- i 1) (+ contador 1))]
        [(and (= contador 7) (<= (- n i) distancia)) (distancia2puntos i j n (- n i) (+ contador 1))]
        [(and (= contador 8) (<= (- j 1) distancia)) (distancia2puntos i j n (- j 1) (+ contador 1))]
        [(and (= contador 9) (<= (- n j) distancia)) (distancia2puntos i j n (- n j) (+ contador 1))]
        [else (distancia2puntos i j n distancia (+ contador 1))]))

;; Nombre: getCasillasAristas
;; Parametros: listaLogica noVisitados destinosPosibles contador flag  n
;; Descripcion: Realiza un trabajo similar a getNOvisitados, pero en vez de buscar que casillas no han sido visitadas busca las de menor cantidad de aristas
;; Retorna: Una lista de pares ordenados con los pares ordenados que pertenecen a las casillas con menos aristas

(define (getCasillasAristas listaLogica noVisitados destinosPosibles contador flag n)
  (cond [(= contador 0) destinosPosibles]
        [(= contador  flag) (getCasillasAristas listaLogica (cdr noVisitados) (append destinosPosibles (list (car noVisitados))) (- contador 1) flag n)]
        [(and (not (null? noVisitados)) (< (calcularAristas (car (car noVisitados)) (cadr (car noVisitados)) n) (calcularAristas (car (car destinosPosibles)) (cadr (car destinosPosibles)) n)))
         (getCasillasAristas listaLogica (cdr noVisitados) (list (car noVisitados)) (- contador 1) flag n)]
        [(and (not (null? noVisitados)) (= (calcularAristas (car (car noVisitados)) (cadr (car noVisitados)) n) (calcularAristas (car (car destinosPosibles)) (cadr (car destinosPosibles)) n)))
         (getCasillasAristas listaLogica (cdr noVisitados) (append destinosPosibles (list (car noVisitados))) (- contador 1) flag n)]
        [else (getCasillasAristas listaLogica (cdr noVisitados) destinosPosibles (- contador 1) flag n)]))

;; Nombre getNOvisitados
;; Parametros: pos listaLogica destinos(lista general de posibles destinos) noVisitados contador
;; Descripcion: De la lista general que cada casilla tiene la funcion se encarga de encontrar dentro de esta lista de destinos los destinos que no han sido visitados y los mete dentro de la lista noVisitados
;; Retorna: Una lista con los destinos posibles desde la casilla actual que NO han siod visitados

(define (getNOvisitados pos listaLogica destinos noVisitados contador)
  (cond [(= contador 0) noVisitados]
        [(= (visitado? listaLogica (caar destinos) (cadar destinos)) 0) (getNOvisitados pos listaLogica (cdr destinos) (append noVisitados (list (car destinos))) (- contador 1))]
        [else (getNOvisitados pos listaLogica (cdr destinos) noVisitados (- contador 1))]))

;; Nombre: actualizarLista
;; Parametros: listaLogica i(contador) j(contador) pos(posicion que hay que actualizar) paso listaActualizada n
;; Descripcion: Esta funcion se debe de llamar a la hora de realizar un movimiento del caballo. Se encarga de construir una nueva lista logica en la que se ve actualizado el valor
;; de visitado con el parametro paso en la casilla actual
;; nota(creo): remover en los destinosPosibles de la casilla actual la casilla a la cual se va a mover

(define (actualizarLista listaLogica i j pos paso listaActualizada n badway pasado)
  (cond [(> i n) listaActualizada]
        [(and (<= j n) (= (revisarPosicion (car pos) (cadr pos) listaLogica) 1))
         (actualizarLista (cdr listaLogica) i (+ j 1) pos paso (append listaActualizada (cons (list (calcularAristas i j n) (list i j) paso (crearDestinos '() i j (calcularAristas i j n) n ) badway pasado) '() )) n badway pasado)]
        [(<= j n) (actualizarLista (cdr listaLogica) i (+ j 1) pos paso (append listaActualizada (list (car listaLogica))) n badway pasado)]
        [else (actualizarLista  listaLogica (+ i 1) 1 pos paso listaActualizada n badway pasado)]))



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
        [(<= j n_og) (crearListaAux i (+ j 1) n_og (append lista (cons (list (calcularAristas i j n_og) (list i j) 0 (crearDestinos '() i j (calcularAristas i j n_og) n_og ) '() '() ) '() )))]
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
        [(and (= i 2) (= j n_og)) (append listaDestinos (cons (list  (+ i 1) (- j 2)) '()) (cons (list  (- i 1) (- j 2)) '()) (cons (list  (+ i 2) (- j 1)) '()))]
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

(define (crearMatriz i j n matriz listaLogica)
  (cond [(> i n) matriz]
        [(<= j n) (crearMatriz i (+ j n) n (append matriz (list (crearFila i 1 n '() listaLogica)) ) (cddr (cddddr listaLogica)))]
        [else (crearMatriz  (+ i 1) 1 n matriz listaLogica)]))

(define (crearFila i j n fila listaLogica)
  (cond [(> j n) fila]
        [else (crearFila i (+ j 1) n (append fila (cons (car (cddr (car listaLogica))) '())) (cdr listaLogica))]))

;;

;; Nombre: editSpot
;; Paramtetros: (lista) recibe la lista a la que se le va a hacer el cambio,
;;(pos) posicion en la lista que se va a cambiar, (content) valor que se quiere insertar
;; Retorna: Lista con cambios efectuados

(define(editSpot lista pos content)
  (editSpotAux lista pos content 0 '())
  )

;; Nombre: editSpotAux
;; Paramtetros: (lista) recibe la lista a la que se le va a hacer el cambio,
;;(pos) posicion en la lista que se va a cambiar, (content) valor que se quiere insertar
;;(cntr) counter, (res)lista que almacena el resultado
;; Descripcion: Va truncando la lista hasta llegar a la posicion deseada, agrega el valor y sigue anadiendo los valores de la lista hasta llegar a '()
;; Retorna: Lista con cambios efectuados

(define (editSpotAux lista pos content cntr res)
  (cond [(equal? lista '())res]
        [(= pos cntr) (editSpotAux (cdr lista) pos content (+ cntr 1) (append res (list content)))]
        [else (editSpotAux (cdr lista) pos content (+ cntr 1) (append res (list(car lista))))]
       )
  )

;;Nombre: getPos
;;Parametros: (lista)lista con el dato deseado (pos) ubicacion del valor deseado
;;Descripcion: invoca a la funcion auxiliar
;;Retorna: resultado de la funcion aux
(define(getPos lista pos)
  (getPosAux lista pos 0)
  )

;;Nombre: getPosAux
;;Parametros: (lista)lista deseada (pos)posicion del dato (cntr) contador
;;Descripcion: aumenta un contador hasta llegar a la posicion deseada mientras trunca la lista y retorna el valor
;;Retorna: valor en la posicion de la lista
(define(getPosAux lista pos cntr)
  (cond [(= pos cntr)(car lista)]
        [else (getPosAux (cdr lista) pos (+ cntr 1 ))]
        ))

;;Nombre: en
;;Parametros: (valor) una lista (lista) lista en la que se hace la busqueda
;;Descripcion: verifica que un valor este dentro de la lista
;;Retorna: booleano
(define (en valor lista)
  (cond[(equal? lista '()) false]
       [(equal? valor (list(car lista))) true]
       [else (en valor (cdr lista))]
       )
  )
;;Nombre:enLista
;;Parametros:(ListaB) lista de valores que se desean buscar (ListaA)
;;Descripcion:Recorre  la lista de valores deseados dentro de la (ListaB)
;;Retorna:booleano
(define (enLista ListaB ListaA)
  (cond [(equal? ListaB '()) false]
        [(en (list(car ListaB)) ListaA) true]
        [else(enLista (cdr ListaB) ListaA )]
        )
  )

;;Nombre: eliminarL
;;Parametros: (x)valor por eliminar (lista)lista victima
;;Descripcion: recorre la lista hasta encontrar el valor deseado y lo remueve
;;Retorna: Lista sin el valor por deseado
(define(eliminarL x lista)
  (cond[(equal? '() lista)'()]
       [(equal? x (list(car lista))) (cdr lista)]
       [else (append (list(car lista)) (eliminarL x (cdr lista)))]
       )
  )

;;Nombre:getMovibles
;;Parametros: (Movs)Argumento con posibles movimientos (MovsSinRespuesta)Movimientos descartados por algoritmo
;;Descripcion: Comparacion de listas de movimiento que permite al algoritmo de backtrackin ignorar las decisiones sin respuesta o tomadas anteriormente
;;Retorna: (posible mejor movimiento)
(define (getMovibles Movs MovsSinRespuesta)
  (cond[(equal? MovsSinRespuesta '()) Movs]
       [(en(list(car MovsSinRespuesta)) Movs) (getMovibles (eliminarL (list(car MovsSinRespuesta)) Movs ) (cdr MovsSinRespuesta))]
       [else (getMovibles Movs (cdr MovsSinRespuesta) )])
  )


(define (backtracking listaLog LLnodo pos paso n res)
  (cond[(and (or (= paso 0)(= paso 1)) (equal? '() (getPosibleMoves LLnodo))) res];;condicion de respuesta
       [(equal? '() (getPosibleMoves LLnodo)) (backtracking (devueltaBT listaLog LLnodo pos paso n) (getNodo listaLog (cadr(cddddr LLnodo))) (cadr(cddddr LLnodo)) (- paso 1) n res ) ];;condicion de devolverse
       [(equal? paso (* n n)) (encSol listaLog LLnodo pos paso n res )];;condicion de append res
       [else(mover listaLog LLnodo pos paso n res )];;condicion de moverse
    
    ))

(define (getNodo listaLog pos)
  (cond [(equal? listaLog '()) '() ]
        [(equal? (cadar listaLog) pos) (car listaLog)]
        [else(getNodo (cdr listaLog) pos)])
  )
(define(encSol listaLog LLnodo pos paso n res )
  (backtracking (devueltaBT listaLog LLnodo pos paso n) (getNodo listaLog (cadr(cddddr LLnodo))) (cadr(cddddr LLnodo)) (- paso 1) n (append res listaLog))
  )

(define(devueltaBT ListaLog LLnodo Pos Paso n)
  (actualizarLista (actualizarLista ListaLog 1 1 Pos 0 '() n '() '()) 1 1 (cadr(cddddr LLnodo)) (- Paso 1) '() n (append (car(cddddr (getNodo ListaLog (cadr(cddddr LLnodo))))) Pos ) (cadr(cddddr (getNodo ListaLog (cadr(cddddr LLnodo))))))
  )

(define(mover listaLog LLnodo pos paso n res )
  (backtracking (actualizarLista listaLog 1 1 (caar(cdddr pos)) (+ paso 1) '() n (car(cddddr(getNodo(caar(cdddr pos))))) (pos)))
  )

(define (getPosibleMoves nodo)
  (getMovibles (cadddr nodo) (car (cddddr nodo)) )
  )