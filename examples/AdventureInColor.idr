||| Adding colours to the adventure game
||| Ported from https://www.linuxjournal.com/content/programming-color-ncurses

module AdventureInColor

import Data.Nat
import NCurses

%default covering

-- Define symbols for the map
grass    = ' '
empty    = '.'
water    = '~'
mountain = '^'
player   = '*'

isMoveOkay : Position -> NCurses i i Bool
isMoveOkay pos = do c <- mvInCh pos
                    pure (c == grass || c == empty)

CFG = MkConfig
  { noDelayEnabled = False
  , keypadEnabled  = True
  , colorEnabled   = True
  }

main : IO ()
main = runNCurses $ NCurses.do
  -- Initialise curses
  cBreak
  noEcho
  clear
  keypad True
  startColor

  -- Grab size
  size <- getSize

  -- Initialise the colors
  grassC    <- initColorPair 1 Yellow Green
  waterC    <- initColorPair 2 Cyan   Blue
  mountainC <- initColorPair 3 Black  White
  playerC   <- initColorPair 4 Red    Magenta

  -- Draw map
  -- background
  nSetAttr (CP grassC)
  for_ @{NCURSES} [0..size.height] $ \ row =>
    mvHLine (MkRow row) grass size.width

  -- mountain
  nSetAttr (CP mountainC)
  for_ @{NCURSES} [half size.width .. threequarter size.width] $ \ col =>
    mvVLine (MkPosition { row = 0, col }) mountain size.height

  -- mountain path
  nSetAttr (CP grassC)
  mvHLine (MkRow $ quarter size.height) grass size.width

  -- lake
  nSetAttr (CP waterC)
  for_ @{NCURSES} [1..half size.height] $ \ row =>
    mvHLine (MkPosition { row, col = 1 }) water (third size.width)

  -- wait for the user quitting
  exploring playerC grassC size (MkRow $ pred size.height)

  where

    quarter : Nat -> Nat
    quarter n = (n `divNatNZ` 4) %search

    third : Nat -> Nat
    third n = (n `divNatNZ` 3) %search

    half : Nat -> Nat
    half n = (n `divNatNZ` 2) %search

    threequarter : Nat -> Nat
    threequarter n = (3 * n `divNatNZ` 4) %search

    step : ColorPair -> (pos, pos' : Position) -> NCurses CFG CFG Position
    step emptyC pos pos'
      = do ok <- isMoveOkay pos'
           ifThenElse (not ok) (pure pos) $ do
             nSetAttr (CP emptyC)
             mvAddCh pos empty
             pure pos'

    exploring : (playerC, emptyC : ColorPair) ->
                Size -> Position -> NCurses CFG CFG ()
    exploring playerC emptyC sz pos@(MkPosition { row, col }) = NCurses.do
      -- display player at position
        nSetAttr (CP playerC)
        mvAddCh pos player
        move pos
        refresh
        -- wait for an input to move
        Left c <- getCh
          | Right 'q' => pure () -- quitting
          | Right _ => exploring playerC emptyC sz pos -- invalid input
        case c of
          Up    => do pos <- step emptyC pos (up pos)
                      exploring playerC emptyC sz pos
          Down  => do let pos' = down pos
                      let True = pos'.row < pred sz.height
                        | _ => exploring playerC emptyC sz pos
                      pos <- step emptyC pos pos'
                      exploring playerC emptyC sz pos
          Left  => do pos <- step emptyC pos (left pos)
                      exploring playerC emptyC sz pos
          Right => do let pos' = right pos
                      let True = pos'.col < pred sz.width
                        | _ => exploring playerC emptyC sz pos
                      pos <- step emptyC pos pos'
                      exploring playerC emptyC sz pos
          _ => exploring playerC emptyC sz pos -- invalid input
