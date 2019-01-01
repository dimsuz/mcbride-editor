{--------------------------------------------------------------}
{- CS410 Advanced Functional Programming                      -}
{- Practical 1: A Text Editor                                 -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- SUBMIT YOUR VERSION OF THIS FILE BY EMAIL TO CONOR         -}
{- DEADLINE: midnight, Thursday 11 October                    -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- IDENTIFY YOURSELF:                                         -}
{- Name:                                                      -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- THIS CODE WILL COMPILE UNDER UNIX BUT NOT WINDOWS          -}
{-                                                            -}
{- To compile the project, use shell command                  -}
{-   make                                                     -}
{- To run your editor, try                                    -}
{-   ./credit <filename>                                      -}
{- or                                                         -}
{-   ./credit                                                 -}
{- to start from blank.                                       -}
{- To quit the editor, press ESCAPE.                          -}
{--------------------------------------------------------------}

{--------------------------------------------------------------}
{- This practical makes use of a bunch of other files I've    -}
{- written, including the layout file from last time. This is -}
{- the only file you should modify. Don't rename this file!   -} 
{--------------------------------------------------------------}

module Prac1 where

import Block
import Overlay

{--------------------------------------------------------------}
{- This module starts with some equipment I've provided for   -}
{- you. To get going, you should not need to make any changes -}
{- but to try more sophisticated things, e.g. editing with    -}
{- selection, cut, and paste, you may want to make changes.   -}
{-                                                            -}
{- Your main mission is to implement handleKey, down below.   -}
{--------------------------------------------------------------}

{- From the lecture, here's that type of backward lists. -}

data Bwd x = B0 | Bwd x :< x deriving (Show, Eq)

instance Foldable Bwd where
  foldr f z B0 = z
  foldr f z (xs :< x) = foldr f (f x z) xs

toFwd :: Bwd x -> [x]
toFwd = foldr (:) []

{- A slight improvement on the lecture: this is a cursor with x things
   round the outside, and an m in the middle. The idea is that we keep
   everything in exactly the right order, so you can always see what's
   where. -}

type Cursor x m = (Bwd x, m, [x])

{- Here's something to put in the middle, to show where you are. -}

data Here = Here deriving Show

{- If you start working with selections, you may wish to modify the
   Here type to account for the current state, e.g. {no selection,
   selection left of cursor, or selection right of cursor}.      -}

{- For one line, we have characters either side, and Here in the middle. -}

type StringCursor = Cursor Char Here

{- For multiple lines, we have strings above and below, and a StringCursor
   in the middle. -}

type TextCursor = Cursor String StringCursor

{- Useful equipment: deactivate turns a cursor into a list by shuffling
   everything to the right, but it also tells you /numerically/ where the
   cursor was. This might help you implement up and down, for example. -}

deactivate :: Cursor x Here -> (Int, [x])
deactivate c = outward 0 c where
  outward i (B0, Here, xs)       = (i, xs)
  outward i (xz :< x, Here, xs)  = outward (i + 1) (xz, Here, x : xs)

{- Activate turns a list into a cursor open at the given position, or as
   near as it gets. -}

activate :: (Int, [x]) -> Cursor x Here
activate (i, xs) = inward i (B0, Here, xs) where
  inward _ c@(_, Here, [])     = c  -- we can go no further
  inward 0 c                   = c  -- we should go no further
  inward i (xz, Here, x : xs)  = inward (i - 1) (xz :< x, Here, xs)  -- and on!

{- Now, if you give me a TextCursor, I can compute the corresponding
   Layout Box, together with the coordinates of Here.
   This is how my code figures out what to display and where to put the
   cursor. -}

whatAndWhere :: TextCursor -> (Layout Box, Point)
whatAndWhere (czz, cur, css) = (foldr (joinV . layS) layZ strs, (x, y)) where
  (x, cs) = deactivate cur
  (y, strs) = deactivate (czz, Here, cs : css)

{- Next, you'll need some model of keystrokes. Here's a type describing
   some keystrokes. You may want more. -}

data ArrowDir = UpArrow | DownArrow | LeftArrow | RightArrow
data Modifier = Normal | Shift | Control

data Key
  = CharKey Char                -- an ordinary printable character
  | ArrowKey Modifier ArrowDir  -- an arrow key
  | Return
  | Backspace
  | Delete
  | Quit

{- Keys come in as standard ANSI escape sequences. You can look 'em up
   online. Feel free to extend escapeKeys so that more keystrokes get
   translated. -}

directions :: [(Char, ArrowDir)]
directions = [('A', UpArrow), ('B', DownArrow),
              ('C', RightArrow), ('D', LeftArrow)]

escapeKeys :: [(String, Key)]
escapeKeys =
  [([c], ArrowKey Normal d) | (c, d) <- directions] ++
  [("1;2" ++ [c], ArrowKey Shift d) | (c, d) <- directions] ++
  [("1;5" ++ [c], ArrowKey Control d) | (c, d) <- directions] ++
  [("3~", Delete)]

{- Last but not least, you get to tell my code how much damage you've done.
   This makes the redrawing more efficient: if you've done less damage to
   the file, my code needs to do less to update. If in doubt, overestimate
   the damage: a slow display is better than a broken display. -}

data Damage
  = NoChange       -- use this if nothing at all happened
  | PointChanged   -- use this if you moved the cursor but kept the text
  | LineChanged    -- use this if you changed text only on the current line
  | LotsChanged    -- use this if you changed text off the current line
  deriving (Show, Eq, Ord)

{--------------------------------------------------------------------------}
{- AT LAST, YOUR BIT!                                                     -}
{- Given a Key and an initial TextCursor, either reject the keystroke or  -}
{- return a modified cursor, with an overestimate of the damage you've    -}
{- you've done. To give you the idea, I've supplied a broken version of   -}
{- ordinary typing, which you get to fix.                                 -}
{-                                                                        -}
{- Be creative!                                                           -}
{--------------------------------------------------------------------------}

handleKey :: Key -> TextCursor -> Maybe (Damage, TextCursor)

handleKey (CharKey c) (sz, (cz, Here, cs), ss)
               = Just (LineChanged, (sz, (cz :< c, Here, cs), ss))

handleKey (ArrowKey Normal d) s@(sz, c, ss)
  = case moveLineCursorH d c of
      Just nc -> Just (PointChanged, (sz, nc, ss))
      Nothing -> case moveLineCursorV d s of
        Just ns -> Just (PointChanged, ns)
        Nothing -> Nothing

handleKey Return (sz, (cz, Here, cs), ss)
  = Just (LotsChanged, (sz :< toFwd cz, (B0, Here, cs), ss))

moveLineCursorH :: ArrowDir -> StringCursor -> Maybe StringCursor
moveLineCursorH LeftArrow (cz :< c, Here, cs) = Just (cz, Here, c : cs)
moveLineCursorH RightArrow (cz, Here, c : cs) = Just (cz :< c, Here, cs)
moveLineCursorH _ _ = Nothing

moveLineCursorV :: ArrowDir -> TextCursor -> Maybe TextCursor
moveLineCursorV UpArrow (sz :< s, c, ss)
  = let (nc, oldLine) = moveVertical c s
    in Just (sz, nc, oldLine : ss)
moveLineCursorV DownArrow (sz, c, s : ss)
  = let (nc, oldLine) = moveVertical c s
    in Just (sz :< oldLine, nc, ss)
moveLineCursorV _ _ = Nothing

moveVertical :: StringCursor -> String -> (StringCursor, String)
moveVertical c targetLine
  = let (oldPos, oldLine) = deactivate c
        nc = activate (oldPos, targetLine)
    in (nc, oldLine)

lineCursor :: StringCursor
lineCursor = (B0 :< 'h' :< 'e' :< 'l' :< 'l' :< 'o', Here, ", world!")

testTextCursor :: TextCursor
testTextCursor = (B0 :< "line0", lineCursor, ["line2"])

