module Data.Queue

import Data.SnocList

%default total

export
record Queue (a : Type) where
  constructor MkQueue
  front : List a
  back  : SnocList a

export
empty : Queue a
empty = MkQueue [] [<]

export
push : a -> Queue a -> Queue a
push x = { back $= (:< x) }

export
pop : Queue a -> Maybe (a, Queue a)
pop (MkQueue [] sx) = case sx <>> [] of
  [] => Nothing
  (x :: xs) => Just (x, MkQueue xs [<])
pop (MkQueue (x :: xs) ys) = Just (x, MkQueue xs ys)

export
Functor Queue where
  map f = { front $= map f, back $= map f }

export
Foldable Queue where
  foldr c n (MkQueue xs ys) = foldr c (foldr c n ys) xs
