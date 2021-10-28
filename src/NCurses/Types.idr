module NCurses.Types

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
