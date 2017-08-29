-- ChordProbe
-- Jack Kaloger, August 2017
module Proj1 (initialGuess, nextGuess, GameState) where

import Data.Char
import Data.List

type Pitch = (Char, Int)
type Chord = (Pitch, Pitch, Pitch)
type Feedback = (Int, Int, Int) -- (p,n,o) correct
type GameState = (Chord, Feedback, [Chord]) -- guess, feedback, chords left

notes = ['A','B','C','D','E','F','G']

initialGuess :: ([String], GameState)
initialGuess = 
        let chords = 
                let pitches = [(note, octave) | note <- notes, octave <- [1..3]]
                in [(p1, p2, p3) | p1 <- pitches, p2 <- pitches, p3 <- pitches, p1 /= p2, p2 /= p3, p1 /= p3]
            guess = ["A1", "A2", "A3"]
            empty = (' ', 0)
        in ( guess, createGameState (empty, empty, empty) (0, 0, 0) chords )

nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String], GameState)
nextGuess (guess, gs) (x, y, z)
        | x == 0 && y == 0 && z == 0 = let newGs = deleteAllPitches (guess2chord guess) (deleteAllOctaves (guess2chord guess) (deleteAllNotes (guess2chord guess) gs))
                                  in (gs2guess newGs, newGs) -- remove all guess data from gs
        | x == 0 && y == 0 = let newGs = deleteAllPitches (guess2chord guess) (deleteAllNotes (guess2chord guess) gs)
                                  in (gs2guess newGs, newGs) -- remove all pitches + notes from gs
        | x == 0 && z == 0 = let newGs = deleteAllPitches (guess2chord guess) (deleteAllOctaves (guess2chord guess) gs)
                                  in (gs2guess newGs, newGs) -- remove all pitches + octaves from gs
        | x == 0 = let newGs = deleteAllPitches (guess2chord guess) gs
                                  in (gs2guess newGs, newGs) -- remove all pitches from gs
        | y == 0 = let newGs = deleteAllNotes (guess2chord guess) gs
                                  in (gs2guess newGs, newGs) -- remove all notes from gs
        | z == 0 = let newGs = deleteAllOctaves (guess2chord guess) gs
                                  in (gs2guess newGs, newGs)  -- remove all octaves from gs
        | otherwise = let newgs = deleteChord (guess2chord guess) gs
                      in (gs2guess newgs, newgs)

generateGuess :: GameState -> Chord
generateGuess (chord, feedback, chords)

correctPitches :: GameState -> Int
correctPitches (_, (a,b,c), _) = a

invalidNotes :: GameState -> Int
invalidNotes (_, (a,b,c), _) = b

invalidOctaves :: GameState -> Int
invalidOctaves (_, (a,b,c), _) = c

deleteChord :: Chord -> GameState -> GameState
deleteChord c (chord, feedback, lst) = createGameState chord feedback (delete c lst)

deleteAllPitches :: Chord -> GameState -> GameState
deleteAllPitches (p1, p2, p3) gs = deletePitch p1 (deletePitch p2 (deletePitch p3 gs))

deletePitch :: Pitch -> GameState -> GameState
deletePitch pitch (chord, feedback, chords) = createGameState chord feedback (filter (\(x,y,z) -> x /= pitch || y /= pitch || z /= pitch) chords)

-- delete all octaves in a chord from the gamestate
deleteAllOctaves :: Chord -> GameState -> GameState
deleteAllOctaves (p1, p2, p3) gs = deleteOctave p1 (deleteOctave p2 (deleteOctave p3 gs))

-- delete an octave from the gamestate
deleteOctave :: Pitch -> GameState -> GameState
deleteOctave (note, octave) (chord, feedback, chords) = createGameState chord feedback (filter (\((x,a),(y,b),(z,c)) -> a /= octave || b /= octave || c /= octave) chords)

-- delete all notes in a chord from the gamestate
deleteAllNotes :: Chord -> GameState -> GameState
deleteAllNotes (p1, p2, p3) gs = deleteNote p1 (deleteNote p2 (deleteNote p3 gs))

-- delete a note from the gamestate
deleteNote :: Pitch -> GameState -> GameState
deleteNote (note, octave) (chord, feedback, chords) = createGameState chord feedback (filter (\((x,a),(y,b),(z,c)) -> x /= note || y /= note || z /= note) chords)

pitch2string :: Pitch -> String
pitch2string (note, octave) = [note] ++ show octave

string2pitch :: String -> Pitch
string2pitch (x:xs) = (x, digitToInt (head xs))

gs2guess :: GameState -> [String]
gs2guess (x, y, z) = head (map chord2guess z)

guess2chord :: [String] -> Chord
guess2chord lst = let p = map string2pitch lst
                  in (p !! 0, p !! 1, p !! 2)

chord2guess :: Chord -> [String]
chord2guess (p1,p2,p3) = [pitch2string p1, pitch2string p2, pitch2string p3]

createGameState :: Chord -> Feedback -> [Chord] -> GameState
createGameState chord feedback chords = (chord, feedback, chords)
