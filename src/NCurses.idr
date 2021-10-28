module NCurses

import NCurses.Core
import public NCurses.Types

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
runNCurses : HasIO io => NCursesT io False i a -> io a
runNCurses (MkNCurses act) = withNCurses act

||| Convenient alias when the underlying IO monad is IO itself and the indices
||| can be inferred.
public export
NCurses : (noDelayIn, noDelayOut : Bool) -> Type -> Type
NCurses = NCursesT IO

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

||| Turn echoing off so that user-inputed characters do not show up in the
||| terminal.
export
noEcho : HasIO io => NCursesT io i i ()
noEcho = MkNCurses Core.noEcho

||| Switch keyboard input to cbreak mode. That is to say that the FIFO of input
||| characters is filled in as soon as they are typed rather than on every
||| newline character.
export
cBreak : HasIO io => NCursesT io i i ()
cBreak = MkNCurses Core.cBreak

||| Get the standard window's size
export
getSize : HasIO io => NCursesT io i i Size
getSize = MkNCurses $ do Core.getSize !stdWindow

||| Refresh the standard window.
export
refresh : HasIO io => NCursesT io i i ()
refresh = MkNCurses Core.refresh

||| Clear the standard window.
export
clear : HasIO io => NCursesT io i i ()
clear = MkNCurses Core.clear

||| Move the cursor in the standard window.
export
nMoveCursor : HasIO io => Position -> NCursesT io i i ()
nMoveCursor pos = MkNCurses (Core.nMoveCursor pos.row pos.col)

export
mvAddCh : HasIO io => Position -> Char -> NCursesT io i i ()
mvAddCh pos c = MkNCurses (Core.mvAddCh pos.row pos.col c)

export
nPutStrLn : HasIO io => (row : Nat) -> String -> NCursesT io i i ()
nPutStrLn row str = MkNCurses (Core.nPutStrLn row str)

--------------------------------------------------------------------------------
-- These are not the best because they're for computations that do not change
-- the  `noDelay` value. They are declared because they can be useful to call
-- existing functions such as `for_`.
--------------------------------------------------------------------------------

namespace Functor

  export
  [NCURSES] Functor io => Functor (NCursesT io i i) where
    map = NCurses.map

namespace Applicative

  export
  [NCURSES] Monad io => Applicative (NCursesT io i i)
    using Functor.NCURSES
    where
      pure = NCurses.pure
      (<*>) = NCurses.(<*>)

namespace Monad

  export
  [NCURSES] Monad io => Monad (NCursesT io i i)
    using Applicative.NCURSES
    where
      (>>=) = NCurses.(>>=)
      join mmx = NCurses.(>>=) mmx id

namespace HasIO

  export
  [NCURSES] HasIO io => HasIO (NCursesT io i i)
    using Monad.NCURSES
    where
      liftIO = MkNCurses . liftIO
