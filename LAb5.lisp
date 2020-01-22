(defun skolemizacija(izraz x)
    (cond
        ((null izraz)'())
        (equalp (car (car izraz) farall)(cond (car izraz)(skolemizacija (cdr izraz) x)))
        (equalp (car (car izraz) exists)(skolemizacija (cdr izraz) (car (cdr (car izraz)))))
        (t(cond (car (car izraz)) (pomFunck (cdr (car izraz)) x)))
    )
    
)

(defun pomFunck (l x)
    (cond 
        ((null l)'())
        ((equalp (car l) x) )
    )
)
(setq izraz '((farall ?x) (farall ?y) (exists ?z) (P(?x ?y ?z)))

;; ∀x∀y∃z P(x, y, z)→∀x∀y P(x, y, f1(x, y))