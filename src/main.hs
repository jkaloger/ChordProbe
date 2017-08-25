-- ChordProbe
-- Jack Kaloger, August 2017
module ChordProbe (initialGuess, nextGuess, chord2string, string2chord, Chord, GameState) where

import Data.Char

type Chord = (Char, Int)
type GameState = [Chord]

pitches = ['A','B','C','D','E','F','G']

initialGuess :: ([String], GameState)
initialGuess = (["A1", "B2", "C3"], [(pitch, octave) | pitch <- pitches, octave <- [1..3]])

nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String], GameState)
nextGuess (guess, (x:xs)) (a,b,c) = (guess, (x:xs))

chord2string :: Chord -> String
chord2string (pitch, octave) = [pitch] ++ (show octave)

string2chord :: String -> Chord
string2chord (x:xs) = (x, digitToInt (head xs))
