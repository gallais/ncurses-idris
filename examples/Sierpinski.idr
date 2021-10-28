||| Drawing a Sierpinski triangle
||| Ported from https://www.linuxjournal.com/content/getting-started-ncurses

module Sierpinski

import Data.Fin
import Data.Nat
import Data.Vect
import Data.String
import NCurses
import System.Random

%default total

main : IO ()
main = runNCurses $ NCurses.do
  -- Initialise curses
  cBreak
  noEcho
  clear

  -- Initialise triangle
  size <- getSize
  let maxlines = pred size.height
  let maxcols = pred size.width

  let p0 = MkPosition { row = 0, col = 0 }
  let p1 = MkPosition { row = maxlines
                      , col = (maxcols `divNatNZ` 2) %search}
  let p2 = MkPosition { row = 0, col = maxcols }
  let ps : Vect 3 ? = [p0,p1,p2]
  for_ @{NCURSES} Fin.range $ \ k => do
    let char = assert_total (show k `strIndex` 0)
    mvAddCh (index k ps) char

  -- Initialise position with random values
  let p = MkPosition { row = !(lift $ randNat maxlines)
                     , col = !(lift $ randNat maxcols) }
  -- Iterate the triangle
  loop ps 200000 p
  -- Done
  nPutStrLn maxlines "Press any key to quit"
  refresh
  ignore $ getCh


  where

    -- The bound better be smaller than MAX_INT32
    randNat : Nat -> IO Nat
    randNat bnd = do let i : Int32 = !randomIO
                     pure $ cast (i `mod` cast bnd)

    randIndex : (n : Nat) -> {auto 0 _ : NonZero n} -> IO (Fin n)
    randIndex (S n) = do k <- randNat (S n)
                         pure (restrict n $ cast k)

    loop : Vect 3 Position -> Nat -> Position -> NCurses False False ()
    loop ps Z     pi = pure ()
    loop ps (S n) pi = do
      k <- lift $ randIndex 3
      let pk = index k ps
      let pi' = { row $= \ xi => ((xi + pk.row) `divNatNZ` 2) %search
                , col $= \ yi => ((yi + pk.col) `divNatNZ` 2) %search
                } pi
      mvAddCh pi' '*'
      refresh
      loop ps n pi'
