module BinomialHeap2 where

data Tree e = Node e [Tree e] deriving Show
type Heap e = [(Int, Tree e)]

link :: Ord e => Tree e -> Tree e -> Tree e
link t1@(Node x1 c1) t2@(Node x2 c2) =
  if x1 <= x2 then
    Node x1 $ t2:c1
  else
    Node x2 $ t1:c2

root :: Tree e -> e
root (Node x _) = x

insTree :: Ord e => Int -> Tree e -> Heap e -> Heap e
insTree r t [] = [(r,t)]
insTree r t ts@((r', t') : ts') =
  if r < r' then
    (r, t) : ts
  else
    insTree (r+1) (link t t') ts'

insert :: Ord e => e -> Heap e -> Heap e
insert x ts = insTree 0 (Node x []) ts

merge :: Ord e => Heap e -> Heap e -> Heap e
merge h [] = h
merge [] h = h
merge h1@((r1, t1) : ts1) h2@((r2, t2) : ts2) =
  if r1 < r2 then
    (r1, t1) : merge ts1 h2
  else if r2 > r1 then
    (r2, t2) : merge ts2 h1
  else
    insTree (r1+1) (link t1 t2) $ merge ts1 ts2

findMin :: Ord e => Heap e -> Maybe e
findMin [] = Nothing
findMin ((_, Node x _) : ts) =
  case findMin ts of
    Nothing -> Just x
    Just y  -> Just $ min x y

removeMinTree :: Ord e => Heap e -> (Maybe (Tree e), Heap e)
removeMinTree [] = (Nothing, [])
removeMinTree [(_, t)] = (Just t, [])
removeMinTree ((r, t) : ts) =
  if root t <= root t' then
    (Just t, ts)
  else
    (Just t', (r, t) : ts')
  where (Just t', ts') = removeMinTree ts
