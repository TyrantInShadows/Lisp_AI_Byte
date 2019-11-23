(defstruct gameState
            naPotezu 
            prviIgra
            tabla
            velicinaTable
            popunjenaPolja
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

    (setf (gameState-tabla state) (PronadjiElement+ 1 2 (gameState-tabla state) 0 t "dodaj" 'X))
    (setf (gameState-tabla state) (PronadjiElement+ 1 2 (gameState-tabla state) 0 t "dodaj" 'O))
    (setf (gameState-tabla state) (PronadjiElement+ 1 1 (gameState-tabla state) 0 t "izbaci" 'X))
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
        ((equalp 0 (mod (+ i j) 2)) (cons (list 'B (OdrediDisk i)) (GenerisiRed i (1+ j))))
        (t (cons (list 'W '()) (GenerisiRed i (1+ j))))
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
    (PronadjiElement1 j (PronadjiElement1  i l 0) 0)
)

(defun PronadjiElement+ (i j l index bool operacija el)
    (cond 
        ((null l)'())
        ((equalp i index)(if (equalp bool t)
                        (cons (PronadjiElement+ j i (car l) 0 '() operacija el) (cdr l))
                        (if (equalp operacija "dodaj") 
                            (cons (cons (car (car l)) (list (DodajElement (car (cdr (car l))) el))) (cdr l))
                            (cons (cons (car (car l))(list (IzbaciElement (car (cdr (car l))) (length (car (cdr (car l)))) 0))) (cdr l))  
                        )))
        (t(cons (car l) (PronadjiElement+ i j (cdr l) (1+ index) bool operacija el)))
    )
)
(defun PronadjiElement1 (i l index)
    (cond 
        ((null l)'())
        ((equalp i index)(car l))
        (t(PronadjiElement1 i (cdr l) (1+ index)))
    )
)

(defun VisinaSteka (l)
    (length (car (cdr l)))
)

(defun DodajElement (l el)
    (cond
        ((null l)(list el))
        (t(cons (car l) (DodajElement (cdr l) el)))
    )
)

(defun IzbaciElement (l duzina index)
    (cond
        ((null l)'())
        ((equalp (- 1 duzina) index)'())
        (t(cons (car l) (IzbaciElement (cdr l) duzina (1+ index))))
    )
)

(defun test(l)
    (car (cdr (car l)))
)

(defun LegitimnoPomeranjeNaPrazno (iDiska jDiska iPomeraj jPomeraj)
    (setf (OdrediNajblizi iDiska jDiska (gameState-velicinaTable state) (gameState-tabla state) 0) lista)
    (cond
        ((AND(< (- iPomeraj iDiska) 0)(< (- jPomeraj jDiska) 0)) (ProveriKavadrant iPomeraj jPomeraj 1 lista))
        ((AND(> (- iPomeraj iDiska) 0)(> (- jPomeraj jDiska) 0)) (ProveriKavadrant iPomeraj jPomeraj 2 lista))
        ((AND(< (- iPomeraj iDiska) 0)(> (- jPomeraj jDiska) 0)) (ProveriKavadrant iPomeraj jPomeraj 3 lista))
        ((AND(> (- iPomeraj iDiska) 0)(< (- jPomeraj jDiska) 0)) (ProveriKavadrant iPomeraj jPomeraj 4 lista))
    )
)

(defun ProveriKavadrant (iPomeraj jPomeraj kon l)
    (cond
        ((null l)'())
        ((equalp kon 1) (if (>= (+ iPomeraj jPomeraj) (+ (car (car (cdr (car l)))) (cdr (car (cdr (car l)))) ))
                            (t)
                            (ProveriKavadrant iPomeraj jPomeraj kon (cdr l))
                        ))
        ((equalp kon 2) (if (<= (+ iPomeraj jPomeraj) (+ (car (car (cdr (car l)))) (cdr (car (cdr (car l)))) ))
                            (t)
                            (ProveriKavadrant iPomeraj jPomeraj kon (cdr l))
                        ))
        ((equalp kon 3) (if (>= (- iPomeraj jPomeraj) (- (car (car (cdr (car l)))) (cdr (car (cdr (car l)))) ))
                            (t)
                            (ProveriKavadrant iPomeraj jPomeraj kon (cdr l))
                        ))
        ((equalp kon 4) (if (<= (- iPomeraj jPomeraj) (- (car (car (cdr (car l)))) (cdr (car (cdr (car l)))) ))
                            (t)
                            (ProveriKavadrant iPomeraj jPomeraj kon (cdr l))
                        ))
    )
)

(defun OdrediNajblizi (iDiska jDiska maxi l index)
    (append (gameState-popunjenaPolja state) (ParLista (- iDiska index) (+ iDiska index) l maxi iDiska jDiska))
    (setf (OdrediNajmanjiNivo index  (gameState-popunjenaPolja state)) lista)
    (cond
        ((null lista) (OdrediNajblizi iDiska jDiska maxi l (1+ index)))
        (t(lista))
    )
)

(defun ObradiListu (i j l iDiska jDiska)
    (cond
        ((null l)'())
        ((> (VisinaSteka (car l)) 0)(if (AND (equalp i iDiska) (equalp j jDiska))   
                                    (ObradiListu i (1+ j) (cdr l) iDiska jDiska)
                                    (if (> (abs (- i iDiska)) (abs (- j jDiska)))
                                        (cons (cons (- i iDiska) (list 'i 'j)) (ObradiListu i (1+ j) (cdr l) iDiska jDiska))
                                        (cons (cons (- j jDiska) (list 'i 'j))(ObradiListu i (1+ j) (cdr l) iDiska jDiska))
                                    )))
        (t(ObradiListu i (1+ j) (cdr l) iDiska jDiska))
    )
)

(defun OdrediNajmanjiNivo (index l)
    (cond
        ((null l)'())
        ((equalp (car (car l)) index)(cons (car l) (OdrediNajmanjiNivo index (cdr l))))
        (t(OdrediNajmanjiNivo index (cdr l)))
    )
)

(defun ParLista (i1 i2 l maxi iDiska jDiska)
    (case
    (equalp i1 i2) (ObradiListu i1 0 (PronadjiElement1 i1 l 0) iDiska jDiska)
    ((OR (< i1 0) (> i1 maxi))(ObradiListu i2 0 (PronadjiElement1 i2 l 0) iDiska jDiska))
    ((OR (< i2 0) (> i2 maxi))(ObradiListu i1 0 (PronadjiElement1 i1 l 0) iDiska jDiska))
    (t(append (ObradiListu i1 0 (PronadjiElement1 i1 l 0) iDiska jDiska) (ObradiListu i2 0 (PronadjiElement1 i2 l 0) iDiska jDiska)))
    )
)