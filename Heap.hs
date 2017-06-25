module Heap where

data Heap e = E | T Int e (Heap e) (Heap e)

rank E = 0
rank (T r _ _ _) = r

makeT x a b =
  if rank a >= rank b then
    T (rank b + 1) x a b
  else
    T (rank a + 1) x b a

merge :: Ord e => Heap e -> Heap e -> Heap e
merge h E = h
merge E h = h
merge (T _ x1 l1 r1@h1) (T _ x2 l2 r2@h2) =
  if x1 <= x2 then
    makeT x1 l1 $ merge r1 h2
  else
    makeT x2 l2 $ merge h1 r2
