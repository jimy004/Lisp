;; FUNCIÓ GENERACIÓ DEL LABERINT
(defun generar (nom n m)
    (setq rs (make-random-state t))
    ;; s'obté aleatòriament la casella d'entrada
    (setq fila (+ 2 (random n rs))) ;; + 2 per evitar que estigui a la primera fila
    (setq columna (+ 2 (random m rs)))
    ;; es comprova que no estigui a una vorera (amb els condicionals)
    (setq laberint (dfs (inicialitzar-matriu n m) (cond ((= (+ n 1) fila) (- fila 2)) ((= n fila) (- fila 1)) (t fila)) (cond ((= (+ m 1) columna) (- columna 2)) ((= m columna) (- columna 1)) (t columna))))
    (putprop 'caracters #\# 'paret)
    (putprop 'caracters #\. 'cami)
    (putprop 'caracters #\e 'entrada)
    (putprop 'caracters #\s 'sortida)
    (escriure (tradueix laberint) nom)
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
    ;; crear cami
    (setq laberint (crear-cami (set-valor l f c 'entrada) f c (opcions l f c)))
    ;; s'obté una casella 'sortida aleatoria
    (opcions-sortida laberint)
)

;; crea un cami per al laberint
(defun crear-cami (l f c o)
    (setq rs (make-random-state t))
    (cond ((= 0 (llarg o)) l)
        (t (setq eleccio (agafa-opcio o (random (llarg o) rs)))
        (cond ;; es posa 'cami a:
            ((and (= eleccio 1) (eq 'paret (get-valor l (+ f 1) c)) (mirar-veins l (+ f 1) c eleccio)) (crear-cami (set-valor l (+ f 1) c 'cami) (+ f 1) c (esborra 2 (opcions l (+ f 1) c))));; casella adjacent inferior
            ((and (= eleccio 2) (eq 'paret (get-valor l (- f 1) c)) (mirar-veins l (- f 1) c eleccio)) (crear-cami (set-valor l (- f 1) c 'cami) (- f 1) c (esborra 1 (opcions l (- f 1) c))));; casella adjacent superior
            ((and (= eleccio 3) (eq 'paret (get-valor l f (+ c 1))) (mirar-veins l f (+ c 1) eleccio)) (crear-cami (set-valor l f (+ c 1) 'cami) f (+ c 1) (esborra 4 (opcions l f (+ c 1)))));; casella adjacent posterior
            ((and (= eleccio 4) (eq 'paret (get-valor l f (- c 1))) (mirar-veins l f (- c 1) eleccio)) (crear-cami (set-valor l f (- c 1) 'cami) f (- c 1) (esborra 3 (opcions l f (- c 1)))));; casella adjacent anterior
            (t (crear-cami l f c (esborra eleccio o))))))
)
        

(defun tradueix (l)
    (cond ((eq (car l) nil) nil)
    (t (cons (tradueix-llista (car l)) (tradueix (cdr l)))))
)

(defun opcions-sortida (l) 
    (setq rs (make-random-state t))
    (setq fila (+ 2 (random (- (llarg l) 1) rs))) ;; + 2 per evitar que estigui a la primera fila
    (setq columna (+ 2 (random (- (llarg (car l)) 1) rs))) ;; ... a la primera columna
    (cond ((eq 'cami (get-valor l fila columna)) (set-valor l fila columna 'sortida))
    (t (opcions-sortida l)))
)



;;mirar posibilitats per no sortir del mapa
(defun opcions (l f c)
    (setq limithoritzontal (llarg (car l)))
    (setq limitvertical (llarg l))
    
    (cond ((and (= f 2) (= c 2)) (list 1 3))
    ((and (= f 2) (= c (- limithoritzontal 1))) (list 1 4))
    ((= f 2) (list 1 3 4)) 
    ((and (= f (- limitvertical 1)) (= c 2)) (list 2 3))
    ((and (= f (- limitvertical 1)) (= c (- limithoritzontal 1))) (list 2 4))
    ((= f (- limitvertical 1)) (list 2 3 4))
    ((= c (- limithoritzontal 1)) (list  1 2 4))
    ((= c 2) (list 1 2 3))
    (t (list 1 2 3 4))
    )
)

;;afagar una opcio
(defun agafa-opcio (l n)
    (cond ((= n 0) (car l)) 
    (t (agafa-opcio (cdr l) (- n 1))))
)

;; torna t si les caselles adjacent son totes paret
(defun mirar-veins (l f c e)
    (cond ((= e 1) (cond ((and (diferents (get-valor l (+ f 1) c)) (diferents (get-valor l f (+ c 1))) (diferents (get-valor l f (- c 1)))) t) 
    (t nil)))
    ((= e 2) (cond ((and (diferents (get-valor l (- f 1) c)) (diferents (get-valor l f (+ c 1))) (diferents (get-valor l f (- c 1)))) t) 
    (t nil)))
    ((= e 3) (cond ((and (diferents (get-valor l (+ f 1) c))  (diferents (get-valor l (- f 1) c)) (diferents (get-valor l f (+ c 1)))) t) 
    (t nil)))
    ((= e 4) (cond ((and (diferents (get-valor l (+ f 1) c))  (diferents (get-valor l (- f 1) c)) (diferents (get-valor l f (- c 1)))) t) 
    (t nil)))
    )
)

;; torna t si son paret
(defun diferents (a)
    (cond ((eq 'entrada a) nil)
    ((eq 'cami a) nil)
    (t t)))

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
(defun set-valor-fila (l c x)
    (cond ((= c 1) (cons x (cdr l)))
    (t (cons (car l) (set-valor-fila (cdr l) (- c 1) x)))
    )
)

;; tradueix una llista als possibles caracters
(defun tradueix-llista (l)
    (cond ((eq (cdr l) nil) (list (get 'caracters (car l)) #\newline))
    (t (cons (get 'caracters (car l)) (tradueix-llista (cdr l))))))

;;calcula la llargaria de una llista
(defun llarg (l)
    (cond ((null l) 0)
    (t (+ 1 (llarg (cdr l))))))

(defun esborra (x l)
    (cond ((null l) nil)
    ((equal x (car l)) (cdr l))
    (t (cons (car l)
    (esborra x (cdr l))))))

(defun escriure (l nom)
    (cond ((eq (car l) nil) nil)
    (t (afegeix nom (car l)) (escriure (cdr l) nom))
    )
    
)

(defun afegeix (nom contingut)
    (let ((fp (open nom :direction :output
                        :if-exists :append 
                        :if-does-not-exist :create)))
        (escriu-intern fp contingut)
        (close fp)))

;;escriure el fitxer de text
(defun escriu (nom contingut)
    (let ((fp (open nom :direction :output)))
        (escriu-intern fp contingut)
        (close fp)))

(defun escriu-intern (fp contingut)
    (cond ((null contingut) nil)
        (t (write-char (car contingut) fp)
            (escriu-intern fp (cdr contingut)))))

;; FUNCIÓ EXPLORACIÓ INTERACTIVA DE LABERINTS
(defun explora (nom))