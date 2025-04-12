;; FUNCIÓ GENERACIÓ DEL LABERINT
(defun generar (nom n m)
    (setq rs (make-random-state t))
    ;; s'obté aleatòriament la casella d'entrada
    (setq fila (+ 1(random n rs)))
    (setq columna (+ 1(random m rs)))
    (print fila)
    (print columna)
    (dfs (inicialitzar-matriu n m) fila columna)
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
    ;;(crea-cami (l1))
)

;; crea un cami per al laberint
(defun crea-cami (l)
    ;; es tria aleatòriament una casella adjacent a l'actual
    (setq rs (make-random-state t))
    (setq eleccio (+ 1 (random 4 rs)))
    ;; es canvia el valor si es compleixen els requisits
    (cond (and (encara es paret) (mirar caselles veines)) (es canvia el valor de la casella))
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