module Bst where

data Tree e = E | T (Tree e) e (Tree e)
  deriving Show

member :: Ord e => e -> Tree e -> Bool
member _ E = False
member x (T l y r) =
  if x < y then
    member x l
  else if x > y then
    member x r
  else
    True

member2 :: Ord e => e -> Tree e -> Bool
member2 x t = go t Nothing
  where go E Nothing = False
        go E (Just y) = x == y
        go (T l y r) c =
          if x < y then
            go l c
          else
            go r $ Just y

insert :: Ord e => e -> Tree e -> Tree e
insert x E = T E x E
insert x (T l y r @ s) =
  if x < y then
    T (insert x l) y r
  else if x > y then
    T l y (insert x r)
  else
    s

insert2 :: Ord e => e -> Tree e -> Tree e
insert2 x t = maybe t id $ go t Just
  where go E cont = cont $ T E x E
        go (T l y r) cont =
          if x < y then
            go l (\u -> cont $ T u y r)
          else if x > y then
            go r (\u -> cont $ T l y u)
          else
            Nothing

insert3 :: Ord e => e -> Tree e -> Tree e
insert3 x t = maybe t id $ go t Nothing Just
  where go E Nothing cont = cont $ T E x E
        go E (Just y) cont =
          if x == y then
            Nothing
          else
            cont $ T E x E
        go (T l y r) can cont =
          if x < y then
            go l can (\u -> cont $ T u y r)
          else
            go r (Just y) (\u -> cont $ T l y u)

complete :: e -> Int -> Tree e
complete _ 0 = E
complete x n = let t = complete x $ n-1 in
                 T t x t
