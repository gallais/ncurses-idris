module NCurses

import NCurses.Core

%default total

||| An `io a` computation involving ncurses primitives.
||| @ noDelayIn  records whether `noDelay` is `True` before the action is run
||| @ noDelayOut records whether `noDelay` is `True` after the action was run
|||
||| This is not `public export` *on purpose* so that the only way to run such
||| a computation is via `runNCurses` which makes sure things are properly
||| initialised and cleaned up afterwards.
export
record NCursesT
  (io : Type -> Type)
  (noDelayIn, noDelayOut : Bool)
  (a : Type) where
  constructor MkNCurses
  getNCurses : io a

||| The constructor is not exported so we provide a `lift` function to embed an
||| `io a` computation into an `NCursesT` one.
export
lift : io a -> NCursesT io i i a
lift = MkNCurses

||| The function to use to turn a computation using ncurses primitives into an
||| IO computation that properly initialises & deinitialises the ncurses lib.
||| Note that we know that we always start with `noDelay` off and do not care
||| about the end state.
export
runNCursesT: HasIO io => NCursesT io False i a -> io a
runNCursesT(MkNCurses act) = withNCurses act

||| Convenient alias when the underlying IO monad is IO itself and the indices
||| can be inferred.
public export
NCurses : {noDelayIn, noDelayOut : Bool} -> Type -> Type
NCurses = NCursesT IO noDelayIn noDelayOut

--------------------------------------------------------------------------------
-- Indexed Functor, Applicative, Monad
-- These do not fit the usual hierarchy so unfortunately we cannot reuse the
-- existing interfaces. We could perhaps define `IFunctor` & friends.
--------------------------------------------------------------------------------

||| Mapping a function over the result of an ncurses computation.
||| If the `io` abides by the functor laws then so will this.
export
map : Functor io => (a -> b) -> NCursesT io i j a -> NCursesT io i j b
map f (MkNCurses act) = MkNCurses (f <$> act)


||| Alias for `map`
export
(<$>) : Functor io => (a -> b) -> NCursesT io i j a -> NCursesT io i j b
(<$>) = map

||| Returning a constant value after running the ncurses computation
export
(<$) : Functor io => b -> NCursesT io i j a -> NCursesT io i j b
(<$) = map . const

||| Ignoring the returned value of an ncurses computation. We cannot use the
||| standard libraries' `ignore` because it relies on `Functor`.
export
ignore : Functor io => NCursesT io i j a -> NCursesT io i j ()
ignore = (() <$)

||| Returning a pure value as a trivial ncurses computation.
||| If the `io` abides by the applicative laws then so will this.
export
pure : Applicative io => a -> NCursesT io i i a
pure = MkNCurses . pure

||| Combining an ncurses computation returning a function and one returning an
||| argument for it.
||| We are using a `Monad io` constraint so that we can guarantee that the the
||| computation returning the function is run first. This specific ordering is
||| crucial to ensure that the i-to-j-to-k ordering demanded by the indices is
||| respected.
export
(<*>) : Monad io =>
        NCursesT io i j (a -> b) -> NCursesT io j k a ->
        NCursesT io i k b
MkNCurses mf <*> MkNCurses mx = MkNCurses (mf >>= \ f => (f $) <$> mx)

||| Sequencing two ncurses computations.
export
(>>=) : Monad io =>
        NCursesT io i j a -> (a -> NCursesT io j k b) ->
        NCursesT io i k b
MkNCurses mx >>= f = MkNCurses (mx >>= getNCurses . f)

||| Sequencing two ncurses computations, the first of which only computes
||| a value of type Unit.
export
(>>) : Monad io =>
       NCursesT io i j a -> NCursesT io j k b ->
       NCursesT io i k b
ma >> mb = ma >>= const mb

--------------------------------------------------------------------------------
-- Ncurses primitives. This time with interesting indices!
--------------------------------------------------------------------------------

||| Get a character from the FIFO.
||| If the FIFO is empty then:
|||  * if `noDelay` is `True` we return `Nothing`
|||  * if `noDelay` is `False` we wait until a character arrives
||| The type of the function reflects that configuration status.
export
getCh : {i : Bool} -> HasIO io =>
        NCursesT io i i (ifThenElse i Maybe Prelude.id $ Char)
getCh {i = True}  = MkNCurses $ do
   i <- Core.getChAsInt8
   pure $ cast i <$ guard (0 <= i)
getCh {i = False} = MkNCurses Core.getCh

||| Set `noDelay` to the boolean value passed.
||| This will have an action on the type of any `getCh` action run afterwards.
export
noDelay : HasIO io => (b : Bool) -> NCursesT io i b ()
noDelay b = MkNCurses  (Core.noDelay b)
