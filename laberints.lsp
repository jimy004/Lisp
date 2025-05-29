;;__________________________________________________________________________________________________
;; 3/05/2025, LLENGUATGES DE PROGRAMACIÓ , GRUP 1, CONVOCATÒRIA ORDINÀRIA
;; Professors: Antoni Oliver Tomàs, Francesc Xavier Gaya Morey
;; Alumnes: Jaume Calafat Cerdó, Enrique Grau Artigao
;;
;;INDICACIONS:
;; Generar el laberint: Per a la generació del laberint s'ha d'indicar el nom del fitxer a on s'ha
;; de guardar i el nombre de files i columnes. Per exemple:
;; (generar "laberint1.txt" 25 25)
;;
;; Exploració del laberint: Per a la exploració del laberint s'ha indicar el nom del fitxer que conté
;; el laberint i el nom del fitxer que conté la classificació. Per exemple:
;; (explora "laberint1.txt" "claberint1.txt")
;;
;; Aspectes opcionals:
;; 1.GESTIÓ DE LES DIFERENTS MIDES DELS LABERINTS QUE S'HAGUIN GENERAT: mitjançant llargfila i llargcolumna
;; dins la funcio explora.
;; 2.GESTIÓ DEL DIFERENTS TIPUS DE CASELLA QUE S'HAGUIN ESPECIFICAT: si es pitja la tecla space a la
;; casella de teleport es trasllada a l' altra casella.
;; 3.ALTRES TIPUS DE CASELLA: 't1 i 't2 que representen un teleport.
;; 4.GENERACIÓ DELS LABERINTS DE MIDES SUPERIORS: amb la funció generar es poden crear mides de 50*50
;; pero no es poden explorar.
;;
;; Explicació del disseny funcional:
;; La solució es divideix en dues parts principals: la generació del laberint i la seva exploració.
;; Es parteix d’una matriu plena de parets, generada amb inicialitzar-matriu. Es defineix aleatòriament
;; la posició d’entrada i es crea un camí mitjançant l'algorisme DFS  sense pila. Al final, es col·loca
;; aleatòriament una casella de sortida i dues caselles de teleport. El laberint es tradueix a caràcters
;; i s’escriu a un fitxer.
;; A partir del fitxer, es llegeix el laberint i es detecten les posicions especials (entrada, sortida, t1, t2).
;; Es dibuixa gràficament el laberint i es permet al jugador moure's amb tecles. Quan s’arriba a la sortida,
;; es mostra una puntuació basada en el nombre de moviments. La classificació de puntuacions es guarda 
;; en un fitxer i es mostra per pantalla, gestionant si aquest ja existeix o cal crear-lo.
;;__________________________________________________________________________________________________
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
    (putprop 'caracters #\1 't1)
    (putprop 'caracters #\2 't2)
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
    (posar-casella (posar-casella (posar-casella laberint 'sortida) 't1) 't2)
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

;; torna la opció indicada per n de la llista l
(defun agafa-opcio (l n)
    (cond ((= n 0) (car l)) 
    (t (agafa-opcio (cdr l) (- n 1))))
)

;; torna t si les caselles adjacent son totes paret
(defun mirar-veins (l f c e) ;; depenent de la elecció e mirarà unes caselles o unes altres
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

;; es crida posar casella mentre no es trii una casella cami que pugui ser sortida
(defun posar-casella (l c) ;; l:matriu
    (setq rs (make-random-state t))
    (setq fila (+ 2 (random (- (llarg l) 1) rs))) ;; + 2 per evitar que estigui a la primera fila
    (setq columna (+ 2 (random (- (llarg (car l)) 1) rs))) ;; ... a la primera columna
    (cond ((eq 'cami (get-valor l fila columna)) (set-valor l fila columna c))
    (t (posar-casella l c)))
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

;;__________________________________________________________________________________________________
;; FUNCIÓ EXPLORACIÓ INTERACTIVA DE LABERINTS
(defun explora (nom cnom)
    ;;establiment de variables
    (setq fitxer (llegeix nom))
    (setq pinici (cercar fitxer 1 1 'entrada)) ;;troba la posicio inicial
    (setq pfinal (cercar fitxer 1 1 'sortida)) ;;troba la meta
    (setq teleport1 (cercar fitxer 1 1 't1))
    (setq teleport2 (cercar fitxer 1 1 't2))
    (setq llargfila (- (cadr (cercar fitxer 1 1 'llarg)) 1))
    (setq llargcolumna(- (car (cercar fitxer 1 1 'llargc)) 1))
    (setq m (dividir 374 llargfila))
    (setq n (dividir 374 llargcolumna)) ;;matrius m*m
    ;;es defineixen els colors per els diferents elements del laberint
    (putprop 'colors '(0 0 0) #\#)
    (putprop 'colors '(255 255 255) #\.)
    (putprop 'colors '(0 255 0) #\e)
    (putprop 'colors '(0 0 255) #\s)
    (putprop 'colors '(255 0 0) #\1)
    (putprop 'colors '(255 0 0) #\2)

    (setq puntuacio (passa fitxer pinici 0)) ;;funció que obté la puntuació del jugador
    (setq valors (list jugador puntuacio)) ;;jugador es defineix dins la funció text cridada per passa a la instrucció anterior
    
    (princ nom)
    (goto-xy 0 5)
    (print "#######################################")
    (cond ((open cnom  :direction :probe)  ;; si el fitxer ja existeix la classificació s'ha de actualitzar i imprimir.
        (setq classificacio (actualitzar (llegeix-exp cnom) valors)) (escriu-exp cnom classificacio) (imprimeix (llegeix-exp cnom) 1 6))
        (t ;; sino existeix hem de crear el fitxer i imprimir.
        (escriu-exp cnom (list valors)) (imprimeix (llegeix-exp cnom) 1 6)))
)

;; pinta la llista i la seva posicio, comprova que es pulsi una tecla i torna a cridar passa amb la nova posició i una passa m més
(defun passa (l p m) ;; m : nombre de passes que es duen
    (cls)
    (move 0 (- 374 n))
    (pinta l p (- llargfila (cadr p)))
    (setq k (get-key))
    (cond ((compara p pfinal) (text m) m)
    (t (cond 
    ((and (or (equal k 65) (equal k 97) (equal k 331)) ;; tecla A a <-
        (no-paret l (list (- (car p) 1) (- (cadr p) 1)))) (passa l (list (car p) (- (cadr p) 1)) (+ 1 m)))
    ((and (or (equal k 68) (equal k 100) (equal k 333));; tecla D d -> 
        (no-paret  l (list (- (car p) 1) (+ (cadr p) 1)))) (passa l (list (car p) (+ (cadr p) 1)) (+ 1 m)))
    ((and (or (equal k 87) (equal k 119) (equal k 328)) ;; tecla W w ↑
        (no-paret l (list (- (car p) 2) (cadr p) ))) (passa l (list (- (car p) 1) (cadr p)) (+ 1 m)))
    ((and (or (equal k 83) (equal k 115) (equal k 336)) ;; tecla S s ↓ 
        (no-paret l (list (+ (car p) 0) (cadr p) ))) (passa l (list (+ (car p) 1) (cadr p)) (+ 1 m)))
    ;;teleports
    ((and (compara p teleport1) (equal k 32)) (passa l teleport2 (+ 1 m)))
    ((and (compara p teleport2) (equal k 32)) (passa l teleport1 (+ 1 m)))
    ;;ESC
    ((equal k 27) m)
    (t (passa l p m))
    )))
)
;; dibuixa per pantall el laberint l amb la posicio p del jugador
(defun pinta (l p x) ;; x representa la columna del jugador dins el laberint
    (cond ((null l) nil)
    ((compara p '(0 0)) 
        (color 255 0 0) (moverel (- (* x m)) n) (moure-rel) (moverel (* x m) (- n)) (pinta l '(-1 -1) x))
    ((eq (car l)  #\newline) 
        (moverel (- (* m llargfila)) (- (+ n 1))) (pinta (cdr l) (list (- (car p) 1) llargfila) x))
    ((eq (car l) #\#) 
        (apply 'color (get 'colors (car l))) (linees) (quadrat (- m 1)) (moverel m 0) (pinta (cdr l) (list (car p) (- (cadr p) 1)) x))
    (t 
        (apply 'color (get 'colors (car l))) (quadrat (- m 1)) (moverel m 0) (pinta (cdr l) (list (car p) (- (cadr p) 1)) x)))
    )

;; actualitzar consisteix en inserir un v a la llista l i ordenarla per a que n'hi hagui 10 elements com a molt
(defun actualitzar (l v)
    (ordenar (inserir v l))
)

;;inserir consisteix en ficar el valor v dins l en el seu lloc
(defun inserir (v l)
    (cond
        ((null l) (list v))
        ((< (cadr v) (cadr (car l))) (cons v l))
        (t (cons (car l) (inserir v (cdr l))))
    )
)

(defun ordenar (l)
    (agafa-primers-10 (inserir-ordenat l) 10)
)

(defun inserir-ordenat (l)
    (cond
    ((null l) nil)
    (t (inserir (car l) (inserir-ordenat (cdr l))))
    )
)

(defun agafa-primers-10 (l x)
    (cond
    ((or (null l) (= x 0)) nil)
    (t (cons (car l) (agafa-primers-10 (cdr l) (- x 1))))
    )
)

;; imprimeix una linea de la classificació
(defun imprimeix (l p y)
    (cond ((null l) nil)
    (t (princ p) (princ '.) (goto-xy 5 y) (princ 'Jugador=) (princ (car (car l))) (goto-xy 25 y) (princ 'Passes=)(print (cadr (car l))) 
    (imprimeix (cdr l) (+ p 1) (+ y 1))))
)

;; torna true si les dues posicions son iguals
(defun compara (p1 p2)
    (cond ((and (eq (car p1) (car p2)) (eq (cadr p1) (cadr p2))) t)
    (t nil))
)

;; mira que a la posició p no hi hagui paret (#\#)
(defun no-paret (l p)
    (cond ((null l) nil)
    ((= (car p) 0)
        (cond ((= (cadr p) 0) (not (eq (car l) #\#))) 
        (t (no-paret (cdr l) (list 0 (- (cadr p) 1))))))
    ((eq (car l) #\newline)
        (no-paret (cdr l) (list (- (car p) 1) (cadr p))))
    (t (no-paret (cdr l) (list (car p) (cadr p)))))
)

;; text que s'utilitza en guanyar una partida
(defun text (z)
    (cls)
    (print "INTRODUEIX EL TEU NOM:")
    (setq jugador (read))
    (goto-xy 0 1)
    (color 0 0 0)
    (princ jugador)
    (princ " ,HAS GUANYAT: amb ")
    (color 255 0 0)
    (princ z)
    (color 0 0 0)
    (princ " passes")
    (goto-xy 0 3)
    (print "#######################################")
    (princ "CLASSIFICACIO GLOBAL: ")
    )


(defun obtenir-caracter (l p)
    (obtenir-caracter-rec l (+ (* (car p) llargfila) (cadr p))))

(defun obtenir-caracter-rec (l n)
    (cond ((= n 0) (car l))
        (t (obtenir-caracter-rec (cdr l) (- n 1)))))

;; depenent del laberint el llugador tendra una mida un altre
(defun moure-rel ()
    (cond ((> llargfila 20)
    (moverel (dividir m 4) (dividir m 4)) (quadrat (- m 6)) (moverel (- (dividir m 4)) (- (dividir m 4))))
    (t (moverel (dividir m 10) (dividir m 10)) (quadrat (- m 6)) (moverel (- (dividir m 10)) (- (dividir m 10)))))
)

;; dibuixa les linees de les parets
(defun linees ()
    (drawrel m n) ;;DIAGONAL DE BAIX A DALT
    (moverel 0 (- n))

    (drawrel (- m) n) ;;DIAGONAL DE DALT A BAIX
    (moverel 0 (- n))

    (moverel (dividir m 2) 0)
    (drawrel 0 n) ;; RECTA VERTICAL
    (moverel (- (dividir m 2)) (- n))

    (moverel 0 (dividir n 2))
    (drawrel m 0) ;; RECTA HORITZONTAL
    (moverel (- m) (- (dividir n 2)))

)

;; torna la posició de l del que es demani a n 
(defun cercar (l f c n)
    (cond  ((and (eq n 'llargc) (null l)) (list f c))
    
    ((eq n 'llarg)
        (cond ((eq (car l) #\newline) (list f c))
        (t (cercar (cdr l) f (+ c 1) 'llarg))))
    
    (t 
        (cond ((eq (car l) (get 'caracters n)) (list f c))
            ((eq (car l) #\newline) (cercar (cdr l) (+ f 1) 0 n))
            (t (cercar (cdr l) f (+ c 1) n)))
    )
    )
)
;;__________________________________________________________________________________________________
;; FUNCIONS AGAFADES DE LA PRESENTACIÓ DEL TEMA5 DE LISP
(defun quadrat (m)
    (drawrel m 0)
    (drawrel 0 n)
    (drawrel (- m) 0)
    (drawrel 0 (- n)))

(defun dividir (m n)
    (cond ((< m n) 0)
    (t (+ 1 (dividir (- m n) n)))))

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

(defun llegeix (nom )
    (let* ((fp (open nom))
        (contingut (llegeix-intern fp)))
        (close fp)
        contingut))

(defun llegeix-intern (fp)
    (let ((c (read-char fp nil nil)))
        (cond ((null c) '())
            (t (cons c (llegeix-intern fp))))))

(defun llegeix-exp (nom)
    (let* ((fp (open nom))
    (e (read fp nil nil)))
    (close fp)
    e))

(defun escriu-exp (nom e)
    (let ((fp (open nom :direction :output :if-does-not-exist :create)))
    (print e fp)
    (close fp))) 

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

(defun escriu (nom contingut)
    (let ((fp (open nom :direction :output)))
        (escriu-intern fp contingut)
        (close fp)))

(defun escriu-intern (fp contingut)
    (cond ((null contingut) nil)
        (t (write-char (car contingut) fp)
            (escriu-intern fp (cdr contingut)))))   