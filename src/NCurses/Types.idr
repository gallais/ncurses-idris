module NCurses.Types

import Data.Nat

%default total

||| Window size.
public export
record Size where
  constructor MkSize
  height : Nat
  width  : Nat

||| Position
public export
record Position where
  constructor MkPosition
  row : Nat
  col : Nat

export
MkRow : Nat -> Position
MkRow n = MkPosition n 0

export
up : Position -> Position
up = { row $= pred }

export
down : Position -> Position
down = { row $= S }

export
left : Position -> Position
left = { col $= pred }

export
right : Position -> Position
right = { col $= S }

public export
data CursorVisibility = CInvisible | CNormal| CHighlyVisible

||| Keys that can be used when keypad is turned on.
public export
data Key = F0
         | F1
         | F2
         | F3
         | F4
         | F5
         | F6
         | F7
         | F8
         | F9
         | F10
         | F11
         | F12
         | Up
         | Down
         | Left
         | Right
         | Backspace

export
allKeys : List Key
allKeys = [F0, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12
          , Up, Down, Left, Right
          , Backspace
          ]

||| The default ncurses colors that can be used in constructing
||| color pairs.
public export
data Color
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

||| Attributes that can be given to text within an ncurses window.
||| @ cp is the representation of colour pairs
public export
data Attribute' cp
  = Normal
  | Underline
  | Standout
  | Reverse
  | Blink
  | Dim
  | Bold
  | Protected
  | Invisible
  | CP cp
