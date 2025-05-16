;; FUNCIÓ GENERACIÓ DEL LABERINT
(defun generar (nom n m)
    (setq rs (make-random-state t))
    ;; s'obté aleatòriament la casella d'entrada
    (setq fila (+ 2 (random n rs))) ;; + 2 per evitar que estigui a la primera fila
    (setq columna (+ 2 (random m rs))) ;; + ...  a la primera columna
    ;; es comprova que no estigui a una vorera (amb els condicionals)
    (setq laberint (dfs (inicialitzar-matriu n m) 
    (cond ((= (+ n 1) fila) (- fila 2)) ((= n fila) (- fila 1)) (t fila)) ;; condicional per limitar la posicio de la fila
    (cond ((= (+ m 1) columna) (- columna 2)) ((= m columna) (- columna 1)) (t columna)))) ;; condicional per limitar la posicio de la columna
    ;; escrivim les propietats per poder escriure el laberint en un fitxer
    (putprop 'caracters #\# 'paret)
    (putprop 'caracters #\. 'cami)
    (putprop 'caracters #\e 'entrada)
    (putprop 'caracters #\s 'sortida)
    ;;traducció
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
    (setq rs (make-random-state t))
    (setq laberint (crear-cami (set-valor l f c 'entrada) f c (opcions l f c) rs))
    ;; s'obté una casella 'sortida aleatoria
    (opcions-sortida laberint)
)

;; crea un cami per al laberint
(defun crear-cami (l f c o rs) ;; l:matriu f:fila casella actual c:columna casella actual o:opcions per visitar rs:semilla per el random
    (cond ((= 0 (llarg o)) l)
        (t (setq eleccio (agafa-opcio o (random (llarg o) rs))) ;; es tria una opció disponible
        (cond ;; es posa 'cami a:
            ((and (= eleccio 1) ;; casella adjacent inferior
            (eq 'paret (get-valor l (+ f 1) c)) (mirar-veins l (+ f 1) c eleccio)) ;; si encara es paret i l'unic cami en els veins es la casella actual
            (crear-cami  (crear-cami (set-valor l (+ f 1) c 'cami) (+ f 1) c (opcions l (+ f 1) c) rs) f c (esborra eleccio o) rs)) ;; s'explora el seguent cami i després l'actual borrant l'eleccio triada
            ((and (= eleccio 2) ;; casella adjacent superior
            (eq 'paret (get-valor l (- f 1) c)) (mirar-veins l (- f 1) c eleccio)) 
            (crear-cami (crear-cami (set-valor l (- f 1) c 'cami) (- f 1) c (opcions l (- f 1) c) rs)  f c (esborra eleccio o) rs))
            ((and (= eleccio 3) ;; casella adjacent posterior
            (eq 'paret (get-valor l f (+ c 1))) (mirar-veins l f (+ c 1) eleccio)) 
            (crear-cami (crear-cami (set-valor l f (+ c 1) 'cami) f (+ c 1) (opcions l f (+ c 1)) rs) f c (esborra eleccio o) rs))
            ((and (= eleccio 4) ;; casella adjacent anterior
            (eq 'paret (get-valor l f (- c 1))) (mirar-veins l f (- c 1) eleccio)) 
            (crear-cami (crear-cami (set-valor l f (- c 1) 'cami) f (- c 1) (opcions l f (- c 1)) rs) f c (esborra eleccio o) rs))
            
            (t (crear-cami l f c (esborra eleccio o) rs))))) ;;si s'acaba la branca
)
        
;; es crida opcions-sortida mentre no es trii una casella cami que pugui ser sortida
(defun opcions-sortida (l) ;; l:matriu
    (setq rs (make-random-state t))
    (setq fila (+ 2 (random (- (llarg l) 1) rs))) ;; + 2 per evitar que estigui a la primera fila
    (setq columna (+ 2 (random (- (llarg (car l)) 1) rs))) ;; ... a la primera columna
    (cond ((eq 'cami (get-valor l fila columna)) (set-valor l fila columna 'sortida))
    (t (opcions-sortida l)))
)



;;mirar posibilitats per no sortir del mapa. Torna una llista amb els possibles moviments per la casella actual:
(defun opcions (l f c) ;; l:matriu f:fila actual c:columna actual
    (setq limithoritzontal (llarg (car l)))
    (setq limitvertical (llarg l))
    ;; llista resultant= 1:abaix 2:adalt 3:dreta 4:esquerra
    (cond ((and (= f 2) (= c 2)) (list 1 3)) ;;cantó superior esquerra
    ((and (= f 2) (= c (- limithoritzontal 1))) (list 1 4)) ;;cantó superior dret
    ((= f 2) (list 1 3 4)) ;;vorera superior
    ((and (= f (- limitvertical 1)) (= c 2)) (list 2 3)) ;;cantó inferior esquerra
    ((and (= f (- limitvertical 1)) (= c (- limithoritzontal 1))) (list 2 4)) ;;cantó inferior dret
    ((= f (- limitvertical 1)) (list 2 3 4)) ;; vorera inferior
    ((= c (- limithoritzontal 1)) (list  1 2 4)) ;;vorera dreta
    ((= c 2) (list 1 2 3)) ;;vorera esquerra
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

;; torna t si a es paret
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

;; funció que tradueix una matriu a text a partir d'una funció auxiliar
(defun tradueix (l)
    (cond ((eq (car l) nil) nil)
    (t (cons (tradueix-llista (car l)) (tradueix (cdr l)))))
)

;; tradueix una llista als possibles caracters
(defun tradueix-llista (l)
    (cond ((eq (cdr l) nil) (list (get 'caracters (car l)) #\newline))
    (t (cons (get 'caracters (car l)) (tradueix-llista (cdr l))))))

;;calcula la llargaria de una llista
(defun llarg (l)
    (cond ((null l) 0)
    (t (+ 1 (llarg (cdr l))))))

;;esborra x de la llista l
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


(defun escriu-intern (fp contingut)
    (cond ((null contingut) nil)
        (t (write-char (car contingut) fp)
            (escriu-intern fp (cdr contingut)))))


;; FUNCIÓ EXPLORACIÓ INTERACTIVA DE LABERINTS
(defun explora (nom))