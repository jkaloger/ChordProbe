-- ChordProbe
-- Jack Kaloger, August 2017
--module ChordProbe (initialGuess, nextGuess, GameState) where
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
                in [(p1, p2, p3) | p1 <- pitches, p2 <- pitches, p3 <- pitches]
            guess = ["A1", "A2", "A3"]
            empty = (' ', 0)
        in ( guess, createGameState (empty, empty, empty) (0,0,0) chords )

nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String], GameState)
nextGuess (guess, (x:xs)) (a,b,c) = ([], [])

chord2string :: Pitch -> String
chord2string (pitch, octave) = [pitch] ++ (show octave)

string2chord :: String -> Pitch
string2chord (x:xs) = (x, digitToInt (head xs))

createGameState :: Chord -> Feedback -> [Chord] -> GameState
createGameState chord feedback chords = (chord, feedback, chords)