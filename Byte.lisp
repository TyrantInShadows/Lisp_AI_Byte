(setq prom'(B(K1 K2)))

(defstruct gameState
            naPotezu 
            prviIgra
            tabla
            velicinaTable
            rezultatIgre
            krajIgre 
 )

(defun test (l)
    
)
(defun PocetneOpcije ()
    (setf state
    (make-gameState :naPotezu 'X
                    :rezultatIgre '(0 0))
    )
    (princ "Unesite 8 ili 10 u zavisnosti od velicine tabele na kojoj zelite da igrate: ")
    (setf (gameState-velicinaTable state) (read))
    (GenerisiTabelu (gameState-velicinaTable state) 0)
    (princ "Unesite P ili K u zavisnosti da li zelite igrac ili racunar da igra prvi, respektivno: ")
    (setf (gameState-prviIgra state) (read))
)
(defun GenerisiTabelu(velicina i)
    (cond
        ((equalp velicina i)'())
        (t(setf (gameState-tabla state) (cons (GenerisiRed i 0) (GenerisiTabelu velicina (1+ i)))))
    )
)

(defun GenerisiRed(i j)
    (cond
        ((equalp j 8)'())
        ((equalp 0 (mod (+ i j) 2)) (cons (list 'B (OdrediDisk i)) (GenerisiPolje i (1+ j))))
        (t (cons (list 'W '()) (GenerisiPolje i (1+ j))))
    )
)

(defun OdrediDisk (i)
    (cond
        ((OR (equalp i 0) (equal i 7))'())
        ((equalp 0 (mod i 2))(list 'X))
        (t (list 'O))
    )
)

(defun PronadjiElement (i j l)
    (ProndjiElement1 j (ProndjiElement1  i l 0) 0)
)

(defun ProndjiElement1 (i l index)
    (cond 
        ((null l)'())
        ((equalp i index)(car l))
        (t(ProndjiElement1 i (cdr l) (1+ index)))
    )
)

(defun VisinaSteka (i j l)
    (length (car (cdr (PronadjiElement i j l))))
)
;;Prebacivanje sa stacka na stack (s1 je prvi stack,a s2 drugi,n je nivo sa koga stack 1 prebacujemo na 2)
(defun StackOnStack (s1 s2 n)
    (if (< n (length s2))
        (if (FiguraPripadaIgracu s1 n 0)
            (if (<= (+ (length (VratiStackN s1 n 0)) (length s2)) 8 ) t 
            nil)
        nil)
    nil) 
)
;; vraca deo stacka od nivoa n (s-stack,n-nivo,i-iterator)
(defun VratiStackN (s n i)
    (cond
    ((equalp i n) s)
    (t (VratiStackN (cdr s) n (1+ i)))
    )
)
;;Da li figura na odredjenom nivou u stacku pripada igracu koji je trenutno na potezu (s-stack,n-nivo, i-iterator)
(defun FiguraPripadaIgracu (s n i)
    (cond 
        ((= n i)
            (if (equalp (car s) (gameState-naPotezu state)) t nil)
        )
        (t (FiguraPripadaIgracu (cdr s) n (1+ i)))
    )
)
(PocetneOpcije)
(setq stack1 '(X O X X O))
(setq stack2 '(X O X O))