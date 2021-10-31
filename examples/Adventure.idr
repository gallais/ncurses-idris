||| Creating an adventure game
||| Ported from https://www.linuxjournal.com/content/creating-adventure-game-terminal-ncurses

module Adventure

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

CFG = MkConfig False True False

exploring : Size -> Position -> NCurses CFG CFG ()
exploring sz pos@(MkPosition { row, col }) = NCurses.do
  -- display player at position
  mvAddCh pos player
  move pos
  refresh
  -- wait for an input to move
  Left c <- getCh
    | Right 'q' => pure () -- quitting
    | Right _ => exploring sz pos -- invalid input
  case c of
    Up    => do let pos' = up pos
                ok <- isMoveOkay pos'
                when ok $ mvAddCh pos empty
                exploring sz (ifThenElse ok pos' pos)
    Down  => do let pos' = down pos
                let True = pos'.row < pred sz.height
                  | _ => exploring sz pos
                ok <- isMoveOkay pos'
                when ok $ mvAddCh pos empty
                exploring sz (ifThenElse ok pos' pos)
    Left  => do let pos' = left pos
                ok <- isMoveOkay pos'
                when ok $ mvAddCh pos empty
                exploring sz (ifThenElse ok pos' pos)
    Right => do let pos' = right pos
                let True = pos'.col < pred sz.width
                  | _ => exploring sz pos
                ok <- isMoveOkay pos'
                when ok $ mvAddCh pos empty
                exploring sz (ifThenElse ok pos' pos)
    _ => exploring sz pos -- invalid input

main : IO ()
main = runNCurses $ NCurses.do
  -- Initialise curses
  cBreak
  noEcho
  clear
  keypad True

  -- Grab size
  size <- getSize

  -- Draw map
  -- background
  for_ @{NCURSES} [0..size.height] $ \ row =>
    mvHLine (MkRow row) grass size.width

  -- mountain
  for_ @{NCURSES} [half size.width .. threequarter size.width] $ \ col =>
    mvVLine (MkPosition { row = 0, col }) mountain size.height

  -- mountain path
  mvHLine (MkRow $ quarter size.height) grass size.width

  -- lake
  for_ @{NCURSES} [1..half size.height] $ \ row =>
    mvHLine (MkPosition { row, col = 1 }) water (third size.width)

  -- wait for the user quitting
  exploring size (MkRow $ pred size.height)

  where

    quarter : Nat -> Nat
    quarter n = (n `divNatNZ` 4) %search

    third : Nat -> Nat
    third n = (n `divNatNZ` 3) %search

    half : Nat -> Nat
    half n = (n `divNatNZ` 2) %search

    threequarter : Nat -> Nat
    threequarter n = (3 * n `divNatNZ` 4) %search
