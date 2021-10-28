module NCurses

import NCurses.Core

%default total

||| An IO computation involving NCurses primitives.
||| This is not `public export` *on purpose* so that the only way to
||| run such a computation is via `runNCurses` which makes sure things
||| are properly instantiated.
export
record NCurses (io : Type -> Type)
               (noDelayIn, noDelayOut : Bool)
               (a : Type) where
  constructor MkNCurses
  getNCurses : io a

export
lift : io a -> NCurses io i i a
lift = MkNCurses

export
runNCurses : HasIO io => NCurses io False i a -> io a
runNCurses (MkNCurses act) = withNCurses act

export
map : Functor io => (a -> b) -> NCurses io i j a -> NCurses io i j b
map f (MkNCurses act) = MkNCurses (f <$> act)

export
(<$>) : Functor io => (a -> b) -> NCurses io i j a -> NCurses io i j b
(<$>) = map

export
pure : Applicative io => a -> NCurses io i i a
pure = MkNCurses . pure

export
(<*>) : Applicative io =>
        NCurses io i j (a -> b) -> NCurses io j k a ->
        NCurses io i k b
MkNCurses mf <*> MkNCurses mx = MkNCurses (mf <*> mx)

export
(>>=) : Monad io =>
        NCurses io i j a -> (a -> NCurses io j k b) ->
        NCurses io i k b
MkNCurses mx >>= f = MkNCurses (mx >>= getNCurses . f)

export
(>>) : Monad io =>
       NCurses io i j a -> NCurses io j k b ->
       NCurses io i k b
ma >> mb = ma >>= const mb

export
getCh : {i : Bool} -> HasIO io =>
        NCurses io i i (ifThenElse i Maybe Prelude.id $ Char)
getCh {i = True}  = MkNCurses $ do
   i <- Core.getChAsInt8
   pure $ cast i <$ guard (0 <= i)
getCh {i = False} = MkNCurses Core.getCh

export
noDelay : HasIO io => (b : Bool) -> NCurses io i b ()
noDelay b = MkNCurses (Core.noDelay b)
