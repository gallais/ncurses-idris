module NCurses

import public Data.So
import NCurses.Core
import public NCurses.Types

%default total

||| The current state of the NCurses configuration
public export
record Config where
  constructor MkConfig
  noDelayEnabled : Bool
  keypadEnabled  : Bool
  colorEnabled   : Bool

public export
setNoDelay : (enable : Bool) -> Config -> Config
setNoDelay b (MkConfig _ ke ce) = MkConfig b ke ce

public export
setKeypad : (enable : Bool) -> Config -> Config
setKeypad b (MkConfig nd _ ce) = MkConfig nd b ce

public export
setColor : Config -> Config
setColor (MkConfig nd ke _) = MkConfig nd ke True

namespace Config

  ||| Initial configuration when you start running a freshly initialised
  ||| ncurses computation
  public export
  init : Config
  init = MkConfig
    { noDelayEnabled = False
    , keypadEnabled  = False
    , colorEnabled   = False
    }

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
  (inC, outC : Config)
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
runNCurses : HasIO io => NCursesT io Config.init i a -> io a
runNCurses (MkNCurses act) = withNCurses act

||| Convenient alias when the underlying IO monad is IO itself and the indices
||| can be inferred.
public export
NCurses : (inC, outC : Config) -> Type -> Type
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

||| Only run the ncurses computation if the boolean is True.
export
when : Applicative io => Bool -> Lazy (NCursesT io i i ()) -> NCursesT io i i ()
when True act = act
when False _ = pure ()

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

||| Return type of `getCh`.
||| 1. Return either a character or potentially a key if the keypad is enabled
||| 2. Block until an input arrives or return immediately if noDelay is enalbed
|||    In that second scenario, if the FIFO is empty then return `Nothing`.
public export
GetCh : Config -> Type
GetCh cfg = ifThenElse cfg.noDelayEnabled Maybe Prelude.id
          $ ifThenElse cfg.keypadEnabled  (Either Key Char) Char

asKey : HasIO io => (b : Bool) -> Char ->
        io (ifThenElse b (Either Key Char) Char)
asKey False c = pure c
asKey True c = do Nothing <- isKey c
                    | Just k => pure (Left k)
                  pure (Right c)

||| Get a character from the FIFO. The return type is heavily dependent on the
||| current configuration of the ncurses computation. See `GetCh` for more
||| information.
export
getCh : {i : Config} -> HasIO io => NCursesT io i i (GetCh i)
getCh {i = MkConfig True b _}  = MkNCurses $ do
   i <- Core.getChAsInt8
   let True = 0 <= i
       | False => pure Nothing
   Just <$> asKey b (cast i)
getCh {i = MkConfig False b _} = MkNCurses $ do
  c <- Core.getCh
  asKey b c

||| Set `noDelay` to the boolean value passed.
||| This will have an action on the type of any `getCh` action run afterwards.
export
noDelay : HasIO io => (b : Bool) -> NCursesT io i (setNoDelay b i) ()
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

||| Move the cursor and insert a char in the standard window
export
mvAddCh : HasIO io => Position -> Char -> NCursesT io i i ()
mvAddCh pos c = MkNCurses (Core.mvAddCh pos.row pos.col c)

||| Move the cursor and get the char under it in the standard window
export
mvInCh : HasIO io => Position -> NCursesT io i i Char
mvInCh pos = MkNCurses (Core.mvInCh pos.row pos.col)

||| Print a horizontal line in the standard window
||| @ c is the character the line is made of
||| @ n is the length of the line
export
hLine : HasIO io => (c : Char) -> (n : Nat) -> NCursesT io i i ()
hLine c n = MkNCurses (Core.hLine c n)

||| Print a vertical line in the standard window
||| @ c is the character the line is made of
||| @ n is the length of the line
export
vLine : HasIO io => (c : Char) -> (n : Nat) -> NCursesT io i i ()
vLine c n = MkNCurses (Core.vLine c n)

||| Move the cursor and print a horizontal line in the standard window
||| @ c is the character the line is made of
||| @ n is the length of the line
export
mvHLine : HasIO io => Position -> (c : Char) -> (n : Nat) -> NCursesT io i i ()
mvHLine pos c n = MkNCurses (Core.mvHLine pos.row pos.col c n)

||| Move the cursor and print a vertical line in the standard window
||| @ c is the character the line is made of
||| @ n is the length of the line
export
mvVLine : HasIO io => Position -> (c : Char) -> (n : Nat) -> NCursesT io i i ()
mvVLine pos c n = MkNCurses (Core.mvVLine pos.row pos.col c n)

||| Move the cursor
export
move : HasIO io => Position -> NCursesT io i i ()
move pos = MkNCurses (Core.nMoveCursor pos.row pos.col)

||| Move the cursor and print to the standard window.
export
mvPrint : HasIO io => Position -> String -> NCursesT io i i ()
mvPrint pos str = MkNCurses (Core.mvPrint pos.row pos.col str)

||| Set the visibility of the cursor.
export
setCursorVisibility : HasIO io => CursorVisibility -> NCursesT io i i ()
setCursorVisibility vis = MkNCurses (Core.setCursorVisibility vis)

||| Turn keypad mode on or off for the std window.
||| When on, function keys (F0, F1, ...) and arrow keys are
||| transformed into single chars that can be compared against
||| the result of passing a particular key to the fnKeyChar
||| function.
export
keypad : HasIO io => (b : Bool) -> NCursesT io i (setKeypad b i) ()
keypad enable = MkNCurses (Core.keypad enable)

export
ColorPair : Type
ColorPair = Core.ColorPair

%hide Core.ColorPair

export
startColor : HasIO io => NCursesT io i (setColor i) ()
startColor = MkNCurses Core.startColor

export
initColorPair : HasIO io => So i.colorEnabled =>
                Nat -> (fg, bg : Color) -> NCursesT io i i ColorPair
initColorPair n fg bg = MkNCurses (Core.initColorPair n fg bg)

public export
Attribute : Type
Attribute = Attribute' ColorPair

%hide Core.Attribute

||| Set an attribute to be applied in the standard window
||| until it is cleared or overwritten.
|||
||| See @nSetAttr'@ for a version that works on
||| any given window.
export
nSetAttr : HasIO io => Attribute -> NCursesT io i i ()
nSetAttr att = MkNCurses (Core.nSetAttr att)

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
