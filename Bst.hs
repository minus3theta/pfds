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
