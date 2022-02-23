;Instituto Tecnológico de Costa Rica
;Inteligencia Artificial 
;Roy Chavarría Garita - 2018034199

#lang racket

;------------------------------------------------------------------------------------------------------------------------------------
;a) Escriba una función recursiva que duplique cada elemento de una lista

;Descripción: Se declara una lista auxiliar en donde se inserta dos veces el primer elemento de la lista, esto con todos los demás.

;Descripción de cada parámetro de entrada: l es una lista de los elementos que se desean duplicar.

;Descripción de salida: una lista con los elementos ya duplicados.

;Ejemplo de como se ejecuta: (duplicate '(1 2 3)) o (duplicate '(a 3 c))

(define (duplicate l)
  (cond [(empty? l) '()]
        [else(append (append (list (first l)) (list (first l))) (duplicate (rest l)) )]))


;------------------------------------------------------------------------------------------------------------------------------------
;b) Escriba una función recursiva que empaquete duplicados consecutivos de elementos en sub-listas.

;Descripción: La funcion cuenta con 3 listas en la funcion auxiliar, una con elementos duplicados,
              ; una que me ayuda a crear la sublista de los repetidos
              ;y una en donde voy metiendo los empaquetados para dar la respuesta.

;Descripción de cada parámetro de entrada: una lista con elementos duplicados, una lista vacia para respuesta y una
                                           ;lista que me ayuda a empaquetar los duplicados.

;Descripción de salida: Una lista con los duplicados empaquetados.

;Ejemplo de como se ejecuta: (pack '(1 1 1 1 2 2 2 2 3 3 3)) o (pack '("a" "a" "a" "b" "b" "b"))

(define (pack-aux l temp1 temp2)
  (cond [(empty? l) (append temp2 (list temp1))]
        [(equal? (first l) (first temp1)) (pack-aux (rest l) (append temp1 (list (first l))) temp2)]
        [else (pack-aux (rest l) (list (first l)) (append temp2 (list temp1)))])
  )

(define (pack l)
  (cond [(empty? l) '()]
        [else(pack-aux (rest l) (append '() (list( first l))) '())]
  ))

;------------------------------------------------------------------------------------------------------------------------------------

;c)  Escriba una función recursiva que codifique el contenido de una lista. Es decir, los
     ;elementos duplicados consecutivos de la lista se deben codificar como sub-listas de la
     ;forma (N E) donde N es el número de duplicados del elemento E.

;Descripción: Reccorre elemento por elemento guardando en un registro la cantidad de elementos iguales antes de encontrar
              ; uno diferente, cuando encuentra uno diferente lo empaqueta en una lista de retorno y continua su camino.

;Descripción de cada parámetro de entrada: -l: la lista con repetidos.
                                          ;-la: lista de la forma (N E) donde N es el número de duplicados del elemento E.
                                          ;-num: el numero que me guarda las apariciones de los repetidos, se reinicia cada diferente
                                          ;-res: lista que contiene el par ordenado (N E).
;Descripción de salida: Una lista de listas en donde cada sublista es de la forma (N E)

;Ejemplo de como se ejecuta:(encode '(A A A A B C C A A D E E E E)) o (encode '(1 1 1 1 2 2 2 3 3 4 5 6 7))

(define (encode-aux l la num res)
  (cond [(empty? l) (append res (list (append (list num) la)))]
        [(equal? (first l) (first la)) (encode-aux (rest l) la (+ num 1) res)]
        [else (encode-aux (rest l) (list (first l)) 1 (append res (list (append (list num) la))) )])
  )

(define (encode l)
  (cond [(empty? l) '()]
        [else(encode-aux (rest l) (list (first l)) 1 '())]
  ))


;------------------------------------------------------------------------------------------------------------------------------------

;d) Escriba una función recursiva que decodifique una lista codificada. Es decir, dada una
    ;lista codificada, decodifique esta y la vuelva a su contenido original.

;Descripción: Tiene una funcion que desempaqueta solo una lista, luego la funcion principal, hace que esa funcion se
              ;recorra una a una aplicandole la funcion, el resultado es guardado en la primer sublista, al final
              ;se retorna se le aplica la funcion a la primera sublista y se le pega el resto que ya está desempaquetado.

;Descripción de cada parámetro de entrada: -l: lista con sublistas de pares (n, v) donde n es un numero y v es un valor

;Descripción de salida: Una unica lista en donde está desempaquetado las sublistas.

;Ejemplo de como se ejecuta: (decode '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)))


(define (decode-one l)
 (cond [(empty? l) '()]
       [(= 0 (first l)) (rest (rest l))]
       [else (decode-one (append (append (list (- (first l) 1)) (list (first (rest l)))) (rest l)))]
       )
  )

(define (decode l)
  (cond [(empty? (rest l)) (append (decode-one (append (list (first (first l))) (list (first (rest (first l)))) )) (rest (rest (first l))) ) ]
        [else(decode (append (list (append (first l) (decode-one (first (rest l)) ) )) (rest (rest l))) )])
  )


;------------------------------------------------------------------------------------------------------------------------------------

;PARTE 2) 

;Descripción: 
;Descripción de cada parámetro de entrada: 
;Descripción de salida: 
;Ejemplo de como se ejecuta:


;Estructura de un nodo (Arbol):
(struct nodo (id nombre valor peso hijos) #:transparent)

;Creación de nodos con información (Arbol):
(define n10 (nodo 10 "Carlos" 9812 12 '()))
(define n9 (nodo 9 "Gustavo" 3567 11 '()))
(define n8 (nodo 8 "Maria" 34 13 '()))
(define n7 (nodo 7 "Jordan" 986 8 '()))
(define n6 (nodo 6 "Juan" 54512 3 '()))
(define n5 (nodo 5 "Pedro" 923232 2 '()))
(define n4 (nodo 4 "Veronica" 2323 1 (list n10)))
(define n3 (nodo 3 "Pepe" 9939 17 (list n8 n9)))
(define n2 (nodo 2 "Nereo" 4332 4 (list n5 n6 n7)))
(define n1 (nodo 1 "Feith" 111 0 (list n2 n3 n4)))

;Listar todos los nodos (list-all-nodes): recibe una estructura de tipo árbol y
                                         ;despliega el contenido completo de esta (nodos y aristas). 
(define (mostrar-info-nodos n)
  (cond [(null? n) (print "Vacio")]
        [else(begin (print (nodo-id n))
                    (print "-")
                    (print (nodo-nombre n))
                    (print "-")
                    (print (nodo-valor n))
                    )]))

(define (list-all-nodes arb)
  (cond [(equal? '() arb) null]
        [else(begin (mostrar-info-nodos arb)
                    (display "\n")
                    (print (nodo-peso arb))
                    (display "\n")
                    (display "\n")
                    (map list-all-nodes (nodo-hijos arb))
                    )])
  )

;'(n1 12 '((n2 5 '(n5 n6 n7)) (n3 3 '(n8 n9)) (n4 7 '(n10))))
;(node-id (tree-root arb1))
;(nodo-id (first (nodo-hijos n1)))

;Buscar un nodo (find-node): devuelve todos los datos asociados a un nodo. Recibe
                            ;el árbol y el nodo a buscar.
(define (find-node-aux l n)
  (cond[(empty? l) '()]
       [else (begin (find-node (first l) n)
                    (find-node-aux (rest l) n))]))

(define (find-node arb n)
  (cond[(equal? arb n) (mostrar-info-nodos arb)]
       [else(find-node-aux (nodo-hijos arb) n)]))



;Insertar un nuevo nodo en una posición determinada en el árbol (insert-node).
  ;Recibe el árbol, el nodo padre y los datos del nodo a insertar.


(define n11 (nodo 11 "Camila" 0433 7 '()))
(define n12 (nodo 12 "Ricardo" 0433 7 '()))

(define (insert-node-aux l padre n)
  (cond[(empty? l) '()]
       [else (begin (insert-node (first l) padre n)
                    (insert-node-aux (rest l) padre n))]))

(define (insert-node arb padre n)
   (cond[(equal? arb padre) ( struct-copy nodo arb [ hijos (append (nodo-hijos arb) (list n))] )]
       [else(insert-node-aux (nodo-hijos arb) padre n)]))

;Eliminar un nodo (delete-node). Recibe el árbol y el nodo a eliminar. La función
     ;retorna el nuevo árbol. 


(define (delete-node-aux l n)
  (cond[(equal? '() l) '()]
       [(equal? (first l) n) (struct-copy nodo (first l) [hijos (rest l)])]
       [else (begin (delete-node (first l) n)
                    (delete-node-aux (rest l) n))]))

(define (delete-node arb n)
   (cond[(equal? arb n) (struct-copy nodo arb [id 0]
                                               [nombre ""]
                                               [valor 0]
                                               [peso 0]
                                               [hijos '()])]
       [else(delete-node-aux (nodo-hijos arb) n)]))

;Buscar el ancestro (ancestor). Recibe un árbol y el nodo para el cual se desea
            ;buscar el ancestro.

(define (ancestor-aux l n padre)
  (cond[(equal? '() l) '()]
       [(equal? (first l) n) (mostrar-info-nodos padre)]
       [else( begin (ancestor-aux (rest l) n padre)
                    (ancestor (first l) n))]))

(define (ancestor arb n)
  (cond[(equal? arb n) arb]
       [else( ancestor-aux (nodo-hijos arb) n arb)]))