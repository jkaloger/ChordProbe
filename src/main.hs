-- ChordProbe
-- Jack Kaloger, August 2017
module ChordProbe (initialGuess, nextGuess, GameState) where

type GameState = ()

initialGuess :: ([String], GameState)

nextGuess :: ([String,GameState) → (Int,Int,Int) → ([String],GameState)

