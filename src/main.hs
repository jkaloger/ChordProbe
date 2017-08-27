-- ChordProbe
-- Jack Kaloger, August 2017
-- module ChordProbe (initialGuess, nextGuess, GameState) where
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
nextGuess (guess, gs) (0 ,0, 0) = let newGs = deleteAllPitches (convertGuess guess) (deleteAllOctaves (convertGuess guess) (deleteAllNotes (convertGuess guess) gs))
                                  in (makeGuess newGs, newGs) -- remove all guess data from gs
nextGuess (guess, gs) (0, 0, _) = let newGs = deleteAllPitches (convertGuess guess) (deleteAllNotes (convertGuess guess) gs)
                                  in (makeGuess newGs, newGs) -- remove all pitches + notes from gs
nextGuess (guess, gs) (0, _, 0) = let newGs = deleteAllPitches (convertGuess guess) (deleteAllOctaves (convertGuess guess) gs)
                                  in (makeGuess newGs, newGs) -- remove all pitches + octaves from gs
nextGuess (guess, gs) (0, _, _) = let newGs = deleteAllPitches (convertGuess guess) gs
                                  in (makeGuess newGs, newGs) -- remove all pitches from gs
nextGuess (guess, gs) (_, 0, _) = let newGs = deleteAllNotes (convertGuess guess) gs
                                  in (makeGuess newGs, newGs) -- remove all notes from gs
nextGuess (guess, gs) (_, _, 0) = let newGs = deleteAllOctaves (convertGuess guess) gs
                                  in (makeGuess newGs, newGs)  -- remove all octaves from gs

makeGuess :: GameState -> [String]
makeGuess chord feedback chords = map (chord2string chords)

convertGuess :: [String] -> Chord
convertGuess lst = map string2chord lst

deleteAllPitches :: Chord -> GameState -> GameState
deleteAllPitches (p1, p2, p3) gs = deletePitch p1 (deletePitch p2 (deletePitch p3 gs))

deletePitch :: Pitch -> GameState -> GameState
deletePitch pitch chord feedback chords = createGameState chord feedback (delete pitch chords)

-- delete all octaves in a chord from the gamestate
deleteAllOctaves :: Chord -> GameState -> GameState
deleteAllOctaves (p1, p2, p3) gs = deleteOctave p1 (deleteOctave p2 (deleteOctave p3 gs))

-- delete an octave from the gamestate
deleteOctave :: Pitch -> GameState -> GameState
deleteOctave (note, octave) chord feedback chords = createGameState chord feedback (chords \\ octave)

-- delete all notes in a chord from the gamestate
deleteAllNotes :: Chord -> GameState -> GameState
deleteAllNotes (p1, p2, p3) gs = deleteNote p1 (deleteNote p2 (deleteNote p3 gs))

-- delete a note from the gamestate
deleteNote :: Pitch -> GameState -> GameState
deleteNote (note, octave) chord feedback chords = createGameState chord feedback (chords \\ note)

chord2string :: Pitch -> String
chord2string (pitch, octave) = [pitch] ++ (show octave)

string2chord :: String -> Pitch
string2chord (x:xs) = (x, digitToInt (head xs))

createGameState :: Chord -> Feedback -> [Chord] -> GameState
createGameState chord feedback chords = (chord, feedback, chords)
