module BinomialHeap where

data Tree e = Node Int e [Tree e] deriving Show
type Heap e = [Tree e]

link :: Ord e => Tree e -> Tree e -> Tree e
link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
  if x1 <= x2 then
    Node (r+1) x1 $ t2:c1
  else
    Node (r+1) x2 $ t1:c2

rank :: Tree e -> Int
rank (Node r _ _) = r

root :: Tree e -> e
root (Node _ x _) = x

insTree :: Ord e => Tree e -> Heap e -> Heap e
insTree t [] = [t]
insTree t ts@(t':ts') =
  if rank t < rank t' then
    t : ts
  else
    insTree (link t t') ts'

insert :: Ord e => e -> Heap e -> Heap e
insert x ts = insTree (Node 0 x []) ts

merge :: Ord e => Heap e -> Heap e -> Heap e
merge h [] = h
merge [] h = h
merge h1@(t1:ts1) h2@(t2:ts2) =
  if rank t1 < rank t2 then
    t1 : merge ts1 h2
  else if rank t2 > rank t1 then
    t2 : merge ts2 h1
  else
    insTree (link t1 t2) $ merge ts1 ts2

findMin :: Ord e => Heap e -> Maybe e
findMin [] = Nothing
findMin (Node _ x _ : ts) =
  case findMin ts of
    Nothing -> Just x
    Just y  -> Just $ min x y

removeMinTree :: Ord e => Heap e -> (Maybe (Tree e), Heap e)
removeMinTree [] = (Nothing, [])
removeMinTree [t] = (Just t, [])
removeMinTree (t:ts) =
  if root t <= root t' then
    (Just t, ts)
  else
    (Just t', t:ts')
  where (Just t', ts') = removeMinTree ts

deleteMin :: Ord e => Heap e -> Heap e
deleteMin ts =
  case mt of
    Nothing -> ts
    Just (Node _ _ ts1) -> merge (reverse ts1) ts2
  where (mt, ts2) = removeMinTree ts
