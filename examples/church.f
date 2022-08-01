-- church-encoded natural numbers
type Nat = t -> (t -> t) -> t;

Z : Nat = \z s. z;
S (n : Nat) : Nat = \z s. n z s;

add (a b : Nat) : Nat = a @Nat b S;

-- church-encoded lists
type List t = r -> (t -> r -> r) -> r;

Nil : List t = \nil cons. nil;
Cons (hd : t) (tl : List t) : List t = \nil cons. cons hd (tl nil cons);

map @a @b (f : a -> b) (l : List a) : List b =
  l @(List b) Nil (\hd tl. Cons (f hd) tl);

list = Cons @Nat Z Nil;
list2 = map (add (S Z)) list;
