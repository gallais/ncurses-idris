||| Drawing a pumpkin for Halloween

module Pumpkin

import Data.Nat
import Data.Vect
import NCurses

%default total

main : IO ()
main = runNCurses $ NCurses.do
  -- Initialise curses
  cBreak
  noEcho
  clear
  startColor
  setCursorVisibility CInvisible

  -- Initialise colours
  orange <- initColorPair 1 Black Yellow
  green  <- initColorPair 2 Green Green
  black  <- initColorPair 3 Black Black

  nSetAttr (CP green)
  mvHLine (MkPosition { row = 0, col = 18 }) ' ' 4
  mvHLine (MkPosition { row = 1, col = 20 }) ' ' 4
  mvHLine (MkPosition { row = 2, col = 20 }) ' ' 4
  mvHLine (MkPosition { row = 3, col = 20 }) ' ' 6
  mvHLine (MkPosition { row = 4, col = 18 }) ' ' 10
  mvHLine (MkPosition { row = 5, col = 18 }) ' ' 10

  nSetAttr (CP orange)
  mvHLine (MkPosition { row = 3,  col = 8  }) ' ' 8
  mvHLine (MkPosition { row = 3,  col = 28 }) ' ' 8
  mvHLine (MkPosition { row = 4,  col = 6  }) ' ' 12
  mvHLine (MkPosition { row = 4,  col = 28 }) ' ' 10
  mvHLine (MkPosition { row = 5,  col = 4  }) ' ' 14
  mvHLine (MkPosition { row = 5,  col = 20 }) ' ' 2
  mvHLine (MkPosition { row = 5,  col = 24 }) ' ' 2
  mvHLine (MkPosition { row = 5,  col = 28 }) ' ' 12
  mvHLine (MkPosition { row = 6,  col = 2  }) ' ' 40
  mvHLine (MkPosition { row = 7,  col = 2  }) ' ' 40
  mvHLine (MkPosition { row = 8,  col = 0  }) ' ' 44
  mvHLine (MkPosition { row = 9,  col = 0  }) ' ' 44
  mvHLine (MkPosition { row = 10, col = 0  }) ' ' 44
  mvHLine (MkPosition { row = 11, col = 0  }) ' ' 44
  mvHLine (MkPosition { row = 12, col = 0  }) ' ' 44
  mvHLine (MkPosition { row = 13, col = 0  }) ' ' 44
  mvHLine (MkPosition { row = 14, col = 2  }) ' ' 40
  mvHLine (MkPosition { row = 15, col = 2  }) ' ' 40
  mvHLine (MkPosition { row = 16, col = 4  }) ' ' 36
  mvHLine (MkPosition { row = 17, col = 6  }) ' ' 30
  mvHLine (MkPosition { row = 18, col = 10 }) ' ' 10
  mvHLine (MkPosition { row = 18, col = 24 }) ' ' 10

  nSetAttr (CP black)
  mvHLine (MkPosition { row = 7,  col = 14 }) ' ' 2
  mvHLine (MkPosition { row = 7,  col = 30 }) ' ' 2
  mvHLine (MkPosition { row = 8,  col = 12 }) ' ' 6
  mvHLine (MkPosition { row = 8,  col = 28 }) ' ' 6
  mvHLine (MkPosition { row = 9,  col = 10 }) ' ' 10
  mvHLine (MkPosition { row = 9,  col = 22 }) ' ' 2
  mvHLine (MkPosition { row = 9,  col = 26 }) ' ' 10
  mvHLine (MkPosition { row = 10, col = 20 }) ' ' 6
  mvHLine (MkPosition { row = 11, col = 18 }) ' ' 10
  mvHLine (MkPosition { row = 13, col = 10 }) ' ' 6
  mvHLine (MkPosition { row = 13, col = 20 }) ' ' 8
  mvHLine (MkPosition { row = 13, col = 32 }) ' ' 6
  mvHLine (MkPosition { row = 14, col = 12 }) ' ' 24
  mvHLine (MkPosition { row = 15, col = 14 }) ' ' 20
  mvHLine (MkPosition { row = 16, col = 16 }) ' ' 6
  mvHLine (MkPosition { row = 16, col = 26 }) ' ' 4

  ignore $ getCh
