module NCurses.Core.Color

import NCurses.Core

%default total

%foreign libncurses "start_color"
prim__startColor : PrimIO ()

%foreign libncurses "init_pair"
prim__initColorPair : Int -> Int -> Int -> PrimIO ()

%foreign libhelper "black_color"
prim__blackColor : PrimIO Int

%foreign libhelper "red_color"
prim__redColor : PrimIO Int

%foreign libhelper "green_color"
prim__greenColor : PrimIO Int

%foreign libhelper "yellow_color"
prim__yellowColor : PrimIO Int

%foreign libhelper "blue_color"
prim__blueColor : PrimIO Int

%foreign libhelper "magenta_color"
prim__magentaColor : PrimIO Int

%foreign libhelper "cyan_color"
prim__cyanColor : PrimIO Int

%foreign libhelper "white_color"
prim__whiteColor : PrimIO Int

||| The default ncurses colors that can be used in constructing
||| color pairs.
public export
data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White

getColor : HasIO io => Color -> io Int
getColor color = case color of
                      Black   => primIO $ prim__blackColor
                      Red     => primIO $ prim__redColor
                      Green   => primIO $ prim__greenColor
                      Yellow  => primIO $ prim__yellowColor
                      Blue    => primIO $ prim__blueColor
                      Magenta => primIO $ prim__magentaColor
                      Cyan    => primIO $ prim__cyanColor
                      White   => primIO $ prim__whiteColor
               

export
data ColorPair = MkColorPair Nat

||| Get the index within ncurses where the given color
||| pair can be referenced. In almost all situations, this
||| can be left as an implementation detail.
export
(.idx) : ColorPair -> Nat
(.idx) (MkColorPair n) = n

export
defaultColorPair : ColorPair
defaultColorPair = MkColorPair 0

||| Create a new color pair. You must tell it the index to create
||| the color at, which should be a number starting at 0. Some
||| platforms allow you to redefine a color at a given index but this
||| is not universally supported.
|||
||| You might notice that ncurses expects color indices to start at 1 --
||| this function increments the index it is given so that passing 0 to
||| it will use the first available user color pair index of 1.
export
initColorPair : HasIO io => Nat -> (fg : Color) -> (bg : Color) -> io ColorPair
initColorPair idx fg bg = 
  do bgColor <- getColor bg
     fgColor <- getColor fg
     let actualIdx = (S idx)
     primIO $ prim__initColorPair (cast actualIdx) fgColor bgColor 
     pure (MkColorPair actualIdx)

||| Begin using color mode.
export
startColor : HasIO io => io ()
startColor = primIO $ prim__startColor

