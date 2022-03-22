#! /usr/local/bin/clisp

(defun split-by-one-space (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

(setq estadoInicial (mapcar #'parse-integer (split-by-one-space (first *args*))))
(setq EI (copy-list estadoInicial))
(setq estadoFinal (mapcar #'parse-integer (split-by-one-space (second *args*))))
(setq EF (copy-list estadoFinal))
(print estadoInicial)
(print estadoFinal)



(defun positionBlank (estado con)
;; Halla la posicion del numero 0 en un estado. �con� es un contador
;; que iniciara en 1
(if (/= (car estado) 0) 
	(positionBlank (cdr estado) (incf con))
(return-from positionBlank con)
))

(defun posicionNumero (estado con num)
;; Halla la posicion del numero num en un estado. �con� es un contador
;; que iniciara en 1
(if (/= (car estado) num) 
	(posicionNumero (cdr estado) (incf con) num)
(return-from posicionNumero con)
))

;; definimos el nodo inicial
		;;(setq estadoInicial (*extrae lista del estado inicial*))


;; definimos el estado meta (no hay nodo meta)
			;;(setq estadoMeta (*extrae lista del estado meta*))

(defun unaInversion(estado cant)
;; suma el numero de inversiones que tiene la ficha cant
(setq elemento (car estado))
(cond
    ((= elemento cant))
    ((< elemento cant) (unaInversion (cdr estado) cant))
    (t (incf s) (unaInversion (cdr estado) cant))))
    
(defun inversiones(estado cant)
;; utiliza unaInversion para calcular el total de inversiones sumandolas
(cond
    ((= cant 0) s)
    (t (unaInversion estado cant) (inversiones estado (decf cant)))))


(defun esPosible (estIn estFin)
;; Determina si el puzzle se puede resolver dependiendo del numero de inversiones 
(setq fila (ceiling (/ (positionBlank estIn 1) 4))) (setq s 0) (setq s1 (+ (inversiones estIn 15) fila))
(setq fila (ceiling (/ (positionBlank estFin 1) 4))) (setq s 0) (setq s2 (+ (inversiones estFin 15) fila))
(setq s3 (- s1 s2))
(cond
    ((= (mod s3 2) 0) t)
    (t nil)))

(defun encuentraLugares(estado cant)
;; Devuelve las columnas y filas de cada lugar en 
;; forma de una lista. recibe el estado en forma de lista y 
;; la cantidad el numero de fichas 
(cond
    ((= cant 0) lugares)
    (t 
    	(setq aux (posicionNumero estado 1 cant)) 
		(if (=(mod aux 4)0) 
			(push 4 lugares)
			(push (mod aux 4) lugares))
    	(push (ceiling (/ aux 4)) lugares) 
    	(encuentraLugares estado (decf cant))
    )
))


(defun sumaAbsolutos(lst)
;; Suma el valor absoluto de todos los numeros de una lista en forma lineal
(setq elemento (car lst))
(cond
    ((null elemento) s)
    (t(incf s (abs elemento)) (sumaAbsolutos (cdr lst)))))

;; define los lugares del estado meta antes de definir manhattan
(setq lugares '())
(setq lugaresfinal (encuentraLugares estadoFinal 15))


(defun manhattan(estado)
;; Encuentra suma la distancia manhattan de todas las fichas respecto al estado ;;final.
(setq lugares '() s 0)
(setq lugaresEstado (encuentraLugares estado 15))
(setq diferencia (mapcar #'- lugaresFinal lugaresEstado))
(sumaAbsolutos diferencia)
)


(defun heuristicaH (B G)
(setq H (+ B G))
(return-from heuristicaH H)
)


(defun mismoEstado (nodo1 nodo2)
;; verifica si dos nodos tienen el mismo estado. Regresa valor booleano
(cond
	((equal (car nodo1) (car nodo2)) t)
	(t nil)
))

(defun esMeta (nodo estadoMeta)
;; verifica si el nodo llego al estado meta.
;; Llama a la funcion mismoEstado, para lo cual genera un pseudonodo auxiliar
(setq aux (list estadoMeta))
(mismoEstado nodo aux)
)

(defun mejorEstado (abierto)
;; obtiene el nodo con el valor mas bajo en heuristicaH. 
;; Si empate regresa el que tenga menor heur�sticaG
(setq mejorNodo (car abierto))

(loop for n in abierto do
	(setq h1 (fifth (cadr mejorNodo)) h2 (fifth (cadr n)))
	(cond
		((> h1 h2) (setq mejorNodo n))
		((= h1 h2)   
			(setq g1 (seventh (cadr mejorNodo)) g2 (seventh (cadr n)))
			(cond
				((> g1 g2) (setq mejorNodo n))
				(t)
			)
		)
		(t)
	)
)
(return-from mejorEstado mejorNodo)
)



(defun iniciaNodo (estado padre operacion)
;; dada una lista estado regresa un nuevo nodo. se utilizara para la funcion 
;; que genera sucesores, por lo que tendremos siempre al padre como parametro
;; debe asignarse el resultado de esta funcion de la manera:
;; (setq nombreNodo (iniciaNodo estado padre operacion))

;; definimos los valores de las heuristicas

;; el valor de la heuristicaB del padre +1
(setq heuB (+ (sixth (cadr padre)) 1))

;; la heuristica Greedy con manhattan
(setq heuG (heuristicaG estado))

;; la heuristica final 
(setq heuH (heuristicaH heuB heuG))

(setq caracteristicas 
	(list 
		(incf cuentaNodos) 
		(caadr padre) 
		operacion 
		(positionBlank estado 1) 
		heuH
		heuB
		heuG
	)
)
(setq hijo (list estado caracteristicas))
(return-from iniciaNodo hijo )
)


(defun quitaElemento(ele lst)
;; Quita un elemento dado de una lista dada y redefine la lista
(loop for nodo in lst do
	(if (equal (car nodo) (car ele)) (setq abierto (remove nodo lst ':test #'equal)))
)) 


(defun realizaOperacion (estado operacion pbAnterior)
;; realiza la operacion sobre el estado dado
;; regresa una lista con el nuevo estado

(setq estadoNuevo (copy-list estado))

;;ahora halla cual sera la posicion alterada 
(cond
	((eq operacion 1)
		(setq posicionAlterada (- pbAnterior 4))
	)
	((eq operacion 2)
		(setq posicionAlterada (+ pbAnterior 1))
	)
	((eq operacion 3)
		(setq posicionAlterada (+ pbAnterior 4))
	)
	((eq operacion 4)
		(setq posicionAlterada (- pbAnterior 1))
	)
)
;; sabiendo estas dos posiciones podemos hacer los cambios en estadoNuevo
(setq aux1 (- posicionAlterada 1))
(setq valorAlterado (nth aux1 estado))
(setf (nth (- posicionAlterada 1) estadoNuevo) 0 (nth (- pbAnterior 1) estadoNuevo) valorAlterado)
(return-from realizaOperacion estadoNuevo)
)

(defun generaHijos (nodo)
;; genera todos los posibles hijos de nodo
;; aun si estos ya fueron explorados
;; se eliminara a los ya explorados al momento de entrar en abierto
;; regresa la lista de hijos

;; define estado y caracteristicas para manipular mejor
(setq estado (car nodo) caracteristicas (cadr nodo))
;; definimos el position blank del nodo
(setq pb (fourth caracteristicas))
;; nombra cada operacion
(setq o1 1 o2 2 o3 3 o4 4)
;; define la lista de hijos
(setq hijos '())

;; revisa en cada caso si la operacion es viable antes de generar el hijo

;; define algunas posiciones que se alteraran por cada operacion (pa_i es posicion alterada 
;; al realizar la operacion i)
(setq pa1 (- pb 4))
(setq pa3 (+ pb 4))

;; hijo para operacion 1
(when (> pa1 0)
	(setq estadoHijo (realizaOperacion estado o1 pb))
	(setq hijo1 (iniciaNodo estadoHijo nodo o1))
	(push hijo1 hijos)
)
;; hijo para operacion 2
;; definimos una lista de bordes derechos y un vector de position blank
(setq bordesDerechos '(4 8 12 16) pbVector (list pb pb pb pb))
;; hacemos prueba para saber si estamos en un borde derecho
(setq esBorde (mapcar #'eq bordesDerechos pbVector))

(when (equal esBorde '(nil nil nil nil))

	(setq estadoHijo (realizaOperacion estado o2 pb))
	(setq hijo2 (iniciaNodo estadoHijo nodo o2))
	(push hijo2 hijos)

)

;; hijo para operacion 3
(when (< pa3 13)
	
	(setq estadoHijo (realizaOperacion estado o3 pb))
	(setq hijo3 (iniciaNodo estadoHijo nodo o3))
	(push hijo3 hijos)
)

;; hijo para operacion 4
;; definimos una lista de bordes derechos y un vector de position blank
(setq bordesIzquierdos '(1 5 9 13) pbVector (list pb pb pb pb))
;; hacemos prueba para saber si pb esta en un borde derecho
(setq esBorde (mapcar #'eq bordesIzquierdos pbVector))
(when (equal esBorde '(nil nil nil nil))
	(setq estadoHijo (realizaOperacion estado o4 pb))
	(setq hijo4 (iniciaNodo estadoHijo nodo o4))
	(push hijo4 hijos)
)
(return-from generaHijos hijos)
)

(defun buscaIguales(nodo lst)
;verfica si el estado ya est� en alguna de las listas
(setq contador1 0)
(loop for i in lst do
	(if (mismoEstado nodo i) (incf contador1))
)
(if (eq contador1 0) 
	(return-from buscaIguales nil) (return-from buscaIguales t))
)

;(defun meteHijos (listaHijos listaAbierto listaCerrado)
;; mete a los hijos de un estado en abierto si es que no estan
;; en cerrado
;(dolist (hijo listaHijos)
;	(when (not(buscaIguales hijo listaCerrado))
;	(push hijo listaAbierto)
;	)
;))

(defun meteHijos(hijos listaCerrado)
;; mete los hijos si no estan en estadoCerrado
(setq entran '())
(loop for h in hijos do
	(if (not (member (car h) listaCerrado ':test #'equal)) (push h entran))
)
(return-from meteHijos entran)
)

(defun buscaPadre (padre lista)
;; regresa el nodo que tenga como nombre el numero
;; "padre" en una lista
(dolist (nodo lista)
	(when (eq padre (caadr nodo))
		(return-from buscaPadre nodo)
	)
))



(defun trayectoria (nodoFinal)
;; regresa una lista que contiene los nodos de las operaciones
;; que se realizaron para llegar al nodoFinal
(setq trayec '())
(push nodoFinal trayec)
(setq padre (second(cadr nodoFinal)))

(loop while (/= padre 0) do
	(setq nodoPadre (buscaPadre padre cerrado))
	(push nodoPadre trayec)
	(setq padre (second (cadr nodoPadre)))
)
(return-from trayectoria trayec)
)



(defun A_STAR (estadoInicial estadoFinal)
	(format t "~%Comienzo~% ~%")
	
	(setq inicial 
	(list estadoInicial (list 1 0 0 (positionBlank estadoInicial 1) 0 0 0))
	)
	
	(setq final estadoFinal)
	(format t "~%Start state: ~%")
	(print inicial)
	(format t "Goal state: ~%")
	(print final) 

	(if (not (esPosible estadoInicial estadoFinal))
		(progn
			;;detects infeasible puzzle
			(format t "~%Tas loko ue!!~%")
			(return-from A_STAR "El puzzle no tiene solucion")
		)
	)
	(setq cuentaNodos 1)
	(setq estadosExpandidos 0)
	
	;(if (null inicial)
      ;  (format t "~%Failure!!~%")
     ;   (return-from A_STAR "Failure")
    ;)   
	
	(setq abierto (list inicial))
	(setq cerrado '())
	(setq estadoCerrado '())

	(loop while (not(null abierto)) do

		(setq actual (mejorEstado abierto))
		(if (esMeta actual final)
			(progn
				(trayectoria actual)
				(print 'trayectoria)
				(print trayec)
				(print 'estadosExpandidos)
				(print estadosExpandidos)
                (print(mapcar #'third (mapcar #'cadr trayec)))
				(return-from A_STAR (mapcar #'third (mapcar #'cadr trayec)))
			)
		)
		(incf estadosExpandidos)
		
		(setq hijos (generaHijos actual)) ;genera los hijos de actual
		
		(setq listaAux (meteHijos hijos estadoCerrado))
		(dolist (h listaAux)
			(push h abierto)
		)
		
		(push actual cerrado)
		(push (car actual) estadoCerrado)
		;mete los hijos en cerrado
		(quitaElemento actual abierto)
		;lo saca de abierto
	)
)

(defun generaRenglones (lst)
(setq renglones '())
(push (nthcdr 12 (reverse (nthcdr (-(length lst) 16) (reverse lst)))) renglones)
(push (nthcdr 8 (reverse (nthcdr (-(length lst) 12) (reverse lst)))) renglones)
(push (nthcdr 4 (reverse (nthcdr (-(length lst) 8) (reverse lst)))) renglones)
(push (nthcdr 0 (reverse (nthcdr (-(length lst) 4) (reverse lst)))) renglones))

(defun generaColumnas (estado)
(setq columnas '())
(setq aux '())
(push (nth 15 estado) aux)(push (nth 11 estado) aux)(push (nth 7 estado) aux)(push (nth 3 estado) aux)
(push aux columnas)
(setq aux '())
(push (nth 14 estado) aux)(push (nth 10 estado) aux)(push (nth 6 estado) aux)(push (nth 2 estado) aux)
(push aux columnas)
(setq aux '())
(push (nth 13 estado) aux)(push (nth 9 estado) aux)(push (nth 5 estado) aux)(push (nth 1 estado) aux)
(push aux columnas)
(setq aux '())
(push (nth 12 estado) aux)(push (nth 8 estado) aux)(push (nth 4 estado) aux)(push (nth 0 estado) aux)
(push aux columnas))



(defun lugaresPorLista (lista1 lista2)
(setq auxPl '())
(if (=(cadddr lista1) 0) (push 0 auxPl) (push (lugaresPorObjeto (cadddr lista1) lista2 1) auxPl))
(if (=(caddr lista1) 0) (push 0 auxPl)(push (lugaresPorObjeto (caddr lista1) lista2 1) auxPl))
(if (=(cadr lista1) 0) (push 0 auxPl)(push (lugaresPorObjeto (cadr lista1) lista2 1) auxPl))
(if (=(car lista1) 0) (push 0 auxPl)(push (lugaresPorObjeto (car lista1) lista2 1) auxPl))
auxPl)



(defun lugaresPorObjeto (objeto lista2 cont)
(cond
    ((null lista2) 0)
    ((= objeto (car lista2)) cont)
    (t (lugaresPorObjeto objeto (cdr lista2) (incf cont)))))


(defun lugaresTodaLista (lista1 lista2)
(setq auxTl '())
(push (lugaresPorLista (cadddr lista1) (cadddr lista2)) auxTl)
(push (lugaresPorLista (caddr lista1) (caddr lista2)) auxTl)
(push (lugaresPorLista (cadr lista1) (cadr lista2)) auxTl)
(push (lugaresPorLista (car lista1) (car lista2)) auxTl)
auxTl)


(defun inversionesTodaLista (lst)
(setq s 0)
(inversionesUnalista (car lst))
(inversionesUnalista (cadr lst))
(inversionesUnalista (caddr lst))
(inversionesUnalista (cadddr lst))
s)


(defun inversionesUnaLista (lst)
(if (/= (cadr lst) 0) (unaInversion lst (cadr lst)))
(if (/= (caddr lst) 0) (unaInversion lst (caddr lst)))
(if (/= (cadddr lst) 0) (unaInversion lst (cadddr lst))))

(setq columnasFinal (generaColumnas estadoFinal))
(setq renglonesFinal (generaRenglones estadoFinal))

(defun calculaConflictoLineal (estado)
(setq colEstado (generaColumnas estado) renEstado (generaRenglones estado) confLin 0)
(incf confLin (inversionesTodaLista (lugaresTodaLista colEstado columnasFinal)))
(incf confLin (inversionesTodaLista (lugaresTodaLista renEstado renglonesFinal)))
confLin)


(defun HeuristicaG (estado)
(setq heuristicaprueba (+ (manhattan estado) (* (calculaConflictoLineal estado) 2)))
)

(A_STAR estadoInicial estadoFinal)