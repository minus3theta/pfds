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

llbalance :: Color -> Tree e -> e -> Tree e -> Tree e
llbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
llbalance color l x r = T color l x r

lrbalance :: Color -> Tree e -> e -> Tree e -> Tree e
lrbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lrbalance color l x r = T color l x r

rlbalance :: Color -> Tree e -> e -> Tree e -> Tree e
rlbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rlbalance color l x r = T color l x r

rrbalance :: Color -> Tree e -> e -> Tree e -> Tree e
rrbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rrbalance color l x r = T color l x r

insert :: Ord e => e -> Tree e -> Tree e
insert x t = T B l' y' r'
  where
    ins E = (T R E x E, 0 :: Int)
    ins s@(T color l y r) =
      if x < y then
        let (u, dir) = ins l in
          ((case dir of
              -1 -> llbalance
              1  -> lrbalance
              _  -> T)
           color u y r, -1)
      else if x > y then
        let (u, dir) = ins r in
          ((case dir of
              -1 -> rlbalance
              1  -> rrbalance
              _  -> T)
           color l y u, 1)
      else
        (s, 0)
    (T _ l' y' r', _) = ins t
