module WHeap where

data WHeap e = E | T Int e (WHeap e) (WHeap e) deriving Show

size :: WHeap e -> Int
size E = 0
size (T s _ _ _) = s

makeT :: e -> WHeap e -> WHeap e -> WHeap e
makeT x a b =
  let s = size a + size b + 1 in
    if size a >= size b then
      T s x a b
    else
      T s x b a

merge :: Ord e => WHeap e -> WHeap e -> WHeap e
merge h E = h
merge E h = h
merge h1@(T s1 x1 l1 r1) h2@(T s2 x2 l2 r2) =
  if x1 <= x2 then
    if size l1 >= s2 then
      T (s1+s2) x1 l1 (merge r1 h2)
    else
      T (s1+s2) x1 (merge r1 h2) l1
  else
    if size l2 >= s1 then
      T (s1+s2) x2 l2 (merge r2 h1)
    else
      T (s1+s2) x2 (merge r2 h1) l2
