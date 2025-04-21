;; FUNCIÓ GENERACIÓ DEL LABERINT
(defun generar (nom n m)
    (setq rs (make-random-state t))
    ;; s'obté aleatòriament la casella d'entrada
    (setq fila (+ 2 (random n rs))) ;; + 2 per evitar que estigui a la primera fila
    (setq columna (+ 2 (random m rs)))
    ;; es comprova que no estigui a una vorera (amb els condicionals)
    (dfs (inicialitzar-matriu n m) 
        (cond ((= (+ n 1) fila) (- fila 2)) ((= n fila) (- fila 1)) (t fila)) 
        (cond ((= (+ m 1) columna) (- columna 2)) ((= m columna) (- columna 1)) (t columna)))
)

;; crear una matriu de les dimensions n*m plena de parets
(defun inicialitzar-matriu (n m)
    (cond ((> n 1) (append (list (afegir-fila m)) (inicialitzar-matriu (- n 1) m)))
        (t (list (afegir-fila m)))
    )
)

;; crea una llista de parets de longitud m
(defun afegir-fila (m)
    (cond ((> m 1) (cons 'paret  (afegir-fila (- m 1))))
        (t (list 'paret))
    )
)

;; algorisme DFS per a generar laberint (sense pila)
(defun dfs (l f c) ;; l:matriu f:fila casella inici c:columna casella inici
    (print l)
    ;; es canvia el valor de de la casella entrada
    (setq l1 (canvia f l (canvia c (car l) 'entrada)))
    ;; crear cami
)

;; crea un cami per al laberint
(defun crea-cami (l f c)
    ;; es tria aleatòriament una casella adjacent a l'actual
    (setq rs (make-random-state t))
    (setq eleccio (+ 1 (random 4 rs)))
    (cond ((= eleccio 1) (+ f 1))
        ((= eleccio 2) (- f 1))
        ((= eleccio 3) (+ c 1))
        ((= eleccio 4) (- c 1)))
    ;; es canvia el valor si es compleixen els requisits
    (cond (and (= 'paret ()) (mirar caselles veines)) (crea-cami l))
    
)

;; tornar l'enesim valor d'una llista de dos dimensions
(defun get-valor (l f c)
    (cond ((and (= f 1) (= c 1)) (car (car l)))
    ((and (= f 1) (> c 1)) (get-valor (list (cdr (car l))) f (- c 1)))
    ((= f 1) (get-valor (car l) f c))
    ((> f 1) (get-valor (cdr l) (- f 1) c)))
)

;; tornar la llista l amb la fila f i columna c modificada amb el valor x
(defun set-valor (l f c x)
    (cond ((= f 1) (cons (set-valor-fila (car l) c x) (cdr l)))
    (t (cons (car l) (set-valor (cdr l) (- f 1) c x))))
)

;; funció auxiliar que modifica la posicio c d'una llista l amb el valor x
( defun set-valor-fila (l c x)
    (cond ((= c 1) (cons x (cdr l)))
    (t (cons (car l) (set-valor-fila (cdr l) (- c 1) x)))
    )
)


;;canvia l'enèsim element d'una llista per l'element donat
(defun canvia (on l per)
    (cond ((= on 1) (cons per (cdr l)))
        (t (cons (car l) 
            (canvia (- on 1)
                (cdr l)
                per)))))

;; FUNCIÓ EXPLORACIÓ INTERACTIVA DE LABERINTS
(defun explora (nom))