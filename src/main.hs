-- == == == == == == == == ==    ~Chord Probe~    == == == == == == == == == ==
-- == == == == == == == == ==    ~Jack Kaloger~   == == == == == == == == == ==
-- == == == == == == == == ==    ~August 2017~    == == == == == == == == == ==

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.Char
import Data.List

-- -- -- -- -- -- -- -- -- --       ~Types~       -- -- -- -- -- -- -- -- -- --

type Note = Char
type Octave = Int
type Pitch = (Note, Octave)
type Chord = (Pitch, Pitch, Pitch)
type Feedback = (Int, Int, Int)

-- (previous guess and feedback), (Valid Pitches), (Known Pitches)
type GameState = ( (Chord, Feedback), [Pitch], [Pitch] )

-- -- -- -- -- -- -- -- -- --     ~Constants~     -- -- -- -- -- -- -- -- -- --

-- all possible notes and octaves
notes = ['A', 'B', 'C', 'D', 'E', 'F', 'G']
octaves = [1..3]

-- the empty pitch and chord used for empty guess history
emptyPitch = (' ', 0)
emptyChord = (emptyPitch, emptyPitch, emptyPitch)

-- -- -- -- -- -- -- -- -- --   ~Main Functions~  -- -- -- -- -- -- -- -- -- --

-- make the initial guess
initialGuess :: ([String], GameState)
initialGuess = ([], gameState (emptyChord, (-1,-1,-1)) pitches [] )
        where pitches = [(note, octave) | note <- notes, octave <- octaves]

-- the next guess is chosen from valid remaining guesses
nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String], GameState)
nextGuess (guess, gs) feedback = (guess, gs)


-- -- -- -- -- -- -- -- -- --      ~Refinery~     -- -- -- -- -- -- -- -- -- --


-- -- -- -- -- -- -- -- -- -- ~Utility Functions~ -- -- -- -- -- -- -- -- -- --
-- swap between internal and external chord representation
chord2string :: Chord -> [String]
chord2string ( (n1, o1), (n2, o2), (n3, o3) )
        = [[n1] ++ show o1, [n2] ++ show o2, [n3] ++ show o3]

-- swap between external and internal chord representation
string2chord :: [String] -> Chord
string2chord lst = (p !! 0, p !! 1, p !! 2)
        where p = map string2pitch lst

-- swap between external and internal pitch representation
string2pitch :: String -> Pitch
string2pitch (x:xs) = (x, digitToInt (head xs))

-- create a gamestate from params
gameState :: (Chord, Feedback) -> [Pitch] -> [Pitch] -> GameState
gameState (guess, feedback) valid known = ((guess, feedback), valid, known)