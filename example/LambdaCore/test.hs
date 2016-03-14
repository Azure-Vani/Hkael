data H a b c = F a c | T b

f :: H Int String a -> a
f (F _ x) = x
