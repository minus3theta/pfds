module Heap where

data Heap e = E | T Int e (Heap e) (Heap e) deriving Show

rank :: Heap e -> Int
rank E = 0
rank (T r _ _ _) = r

makeT :: e -> Heap e -> Heap e -> Heap e
makeT x a b =
  if rank a >= rank b then
    T (rank b + 1) x a b
  else
    T (rank a + 1) x b a

merge :: Ord e => Heap e -> Heap e -> Heap e
merge h E = h
merge E h = h
merge h1@(T _ x1 l1 r1) h2@(T _ x2 l2 r2) =
  if x1 <= x2 then
    makeT x1 l1 $ merge r1 h2
  else
    makeT x2 l2 $ merge h1 r2

insert :: Ord e => e -> Heap e -> Heap e
insert x h = merge (T 1 x E E) h

-- Ex. 3.2
insert' :: Ord e => e -> Heap e -> Heap e
insert' x E = T 1 x E E
insert' x h@(T _ y l r) =
  if x <= y then
    makeT x E h
  else
    makeT y l $ insert' x r
