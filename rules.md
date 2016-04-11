a, b, c ... is flow property variables
X, Y, Z ... is type variables
C is flow property constraints
D is type constraints

W(F, term) = case term of
    x:
        t = lookup x in F
        if t = \forall [X1, X2, ..., Xm] . k
            k1 = replace all [X1, X2, ..., Xm] with fresh type variables in k

        if k1 = \forall [a1, a2, ..., am] C => k2
            retC = replace [a1, a2, ..., am] with fresh flow property variables in C
            retk = replace [a1, a2, ..., am] with fresh flow property variables in k2

        return (retk, {}, retC)

    let x = e1 in e2:
        (t, D, C) = W(F, e1)
        sub = unify(D)
        t = apply(sub, t)
        C = apply(sub, C)
        t = close(F, close(C, t))
        return W(F \union x:t, e2)

    \x -> e:
        a = fresh type variables
        l = fresh flow label
        (t2, D, C) = W(F \union x:a, e)
        return (a ->{l} t2, D, C)
        
    e1 e2
        (t1, D1, C1) = W(F, e1)
        (t2, D2, C2) = W(F, e2)
        return (a, D1 \union D2 \union {t1 = t2 -> a}, C1 \union C2 \union constrain(t2 <= left(t1)))

    if e1 then e2 else e3
        t = fresh type variables
        (t1, D1, C1) = W(F, e1)
        (t2, D2, C2) = W(F, e2)
        (t3, D3, C3) = W(F, e3)
        return (t, D1 \union D2 \union D3 \union {t1 = Bool, t2 = t, t3 = t},
                C1 \union C2 \union C3 \union {t2 <= t, t3 <= t})

    fix e
        (t, D, C) = W(F, e)
        tv = fresh type variable
        return (tv, D \union , C \union {t = tv -> tv})

