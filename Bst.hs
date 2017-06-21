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
member2 x t = go x t Nothing
  where go _ E Nothing = False
        go y E (Just z) = y == z
        go y (T l z r) c =
          if y < z then
            go y l c
          else
            go y r (Just z)

insert :: Ord e => e -> Tree e -> Tree e
insert x E = T E x E
insert x (T l y r @ s) =
  if x < y then
    T (insert x l) y r
  else if x > y then
    T l y (insert x r)
  else
    s
