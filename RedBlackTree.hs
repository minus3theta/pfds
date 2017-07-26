module RedBlackTree where

data Color = R | B
data Tree e = E | T Color (Tree e) e (Tree e)

member :: Ord e => e -> Tree e -> Bool
member _ E = False
member x (T _ l y r) =
  if x < y then
    member x l
  else if x > y then
    member x r
  else
    True

balance :: Color -> Tree e -> e -> Tree e -> Tree e
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance color l x r = T color l x r

insert :: Ord e => e -> Tree e -> Tree e
insert x t = T B l' y' r'
  where
    ins E = T R E x E
    ins s@(T color l y r) =
      if x < y then
        balance color (ins l) y r
      else if x > y then
        balance color l y (ins r)
      else
        s
    T _ l' y' r' = ins t
