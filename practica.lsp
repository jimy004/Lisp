;; FUNCIÓ GENERACIÓ DEL LABERINT
(defun generar (nom n m)
    (dfs (inicialitzar-matriu n m))
)

;; crear una matriu de les dimensions n*m plena de parets
(defun inicialitzar-matriu (n m)
    (cond ((> n 1) (list (inicialitzar-matriu (- n 1) m) (afegir-fila m)))
        (t (afegir-fila m))
    )
)

;; crea una llista de parets de longitud m
(defun afegir-fila (m)
    (cond ((> m 1) (cons 'paret  (afegir-fila (- m 1))))
        (t (list 'paret))
    )
)

;; algorisme DFS per a generar laberint (sense pila)
(defun dfs (l))

;; FUNCIÓ EXPLORACIÓ INTERACTIVA DE LABERINTS
(defun explora (nom))