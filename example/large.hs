-- Booleans
let T x y = x in
let F x y = y in

-- SKI combinators
let I x = x in
let K x y = x in
let S f g x = f x (g x) in

let skk = S K K in

let Mu f = f in

-- Other combinators
let B x y z = x (y z) in
let C x y z = x z y in
let W x y = x y y in

-- Integer arithmetic
let nsucc x = x + 1 in
let npred x = x - 1 in

-- Arithmetic
let succ n f x = f (n f x) in

let zero  f x = x  in
let one   f x = f x  in
let two   f x = f (f x)  in
let three f x = f (f (f x))  in
let four  f x = f (f (f (f x)))  in
let five  f x = f (f (f (f (f x))))  in
let six   f x = f (f (f (f (f (f x)))))  in
let seven f x = f (f (f (f (f (f (f x))))))  in
let eight f x = f (f (f (f (f (f (f (f x)))))))  in
let nine  f x = f (f (f (f (f (f (f (f (f x))))))))  in
let ten   f x = f (f (f (f (f (f (f (f (f (f x)))))))))  in

let iszero n = n (\x -> F) T in
let plus m n f x = n f (m f x) in
let mult m n f = m (n f) in
let pow m n = n m in
let pred n f x = n (\g h -> h (g f)) (\u -> x) I in
let ack = \m -> m (\f n -> n f (f one)) succ in
let sub m n = (n pred) m in

-- Conversions

let unbool n = n True False in
let unchurch n = n (\i -> i + 1) 0 in
let rec church n = 
  if (n == 0)
  then zero
  else \f x -> f (church (n-1) f x) in

-- Logic
let not p = p F T in
let and p q = p q F in
let or p q = p T q in
let cond p x y = p x y in
let xor p q = p (q F T) q in
let equ p q = not (xor p q) in
let nand x y = cond x (not y) T in
let nor x y = cond x F (not y) in

-- Tuples
let fst p = p T in
let snd p = p F in
let pair a b f = f a b in

-- Lists
let nil x = x in
let cons x y = pair F (pair x y) in
let null z = z T in
let head z = fst (snd z) in
let tail z = snd (snd z) in
let indx xs n = head (n tail xs) in

let fact = (\fact -> \n -> if (n == 0) then 1 else (n * (fact (n-1)))) in

-- Functions
let const x y = x in
let compose f g = \x -> f (g x) in
let twice f x = f (f x) in
let on g f = \x y -> g (f x) (f y) in
let ap f x = f (f x) in

-- Let Polymorphism
let poly = I (I I) (I 3) in
let self = (\x -> x) (\x -> x) in
let innerlet = \x -> (let y = \z -> z in y) in

-- Fresh variables
let wtf = \a b c d e e' f g h i j k l m n o o' o'' o''' p q r r' s t u u' v w x y z ->
    q u i c k b r o w n f o' x j u' m p s o'' v e r' t h e' l a z y d o''' g in

-- if-then-else
let notbool x = if x then False else True in
let eqzero x = if (x == 0) then True else False in

let const = \x -> (\y -> x) in
let shit = \y -> 
    (let f = \x -> 
        if x then True else False in const (f y) y) in

let test = \x ->
    let y = \z -> if True then x else z in y 1
    in 1
