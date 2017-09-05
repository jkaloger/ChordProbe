-- == == == == == == == == ==    ~Chord Probe~    == == == == == == == == --
-- == == == == == == == == ==    ~Jack Kaloger~   == == == == == == == == --
-- == == == == == == == == ==    ~August 2017~    == == == == == == == == --

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import Data.Char

-- -- -- -- -- -- -- -- -- --       ~Types~       -- -- -- -- -- -- -- -- --

type Note = Char
type Octave = Int
type Pitch = (Note, Octave)
type Chord = (Pitch, Pitch, Pitch)
type Feedback = (Chord, (Int, Int, Int))
-- (previous guess and feedback),(Valid Chords)
data GameState = State ( Feedback,  [Chord]) | Start deriving (Show)

-- -- -- -- -- -- -- -- -- --     ~Constants~     -- -- -- -- -- -- -- -- --

-- all possible notes and octaves
notes :: [Char]
notes = ['A', 'B', 'C', 'D', 'E', 'F', 'G']

octaves :: [Int]
octaves = [1..3]

-- -- -- -- -- -- -- -- -- --   ~Main Functions~  -- -- -- -- -- -- -- -- --

-- make the initial guess
initialGuess :: ([String], GameState)
initialGuess = (["A1", "B1", "C1"], Start)

-- our next guess is based on a refined list of possible chords
nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String], GameState)
nextGuess ( guess, Start ) ( p, n, o ) -- Start state has to generate chords
    = (makeGuess chords, State ( (string2chord guess, (p,n,o)), chords))
        where chords = refine Start ( string2chord guess, (p,n+p,o+p) )
nextGuess ( guess, gs ) ( p, n, o ) -- otherwise we refine as usual
    = (makeGuess chords, State ( (string2chord guess, (p,n,o)), chords))
        where chords = refine gs ( string2chord guess, (p,n+p,o+p) )

-- -- -- -- -- -- -- -- -- -- Refining Functions~ -- -- -- -- -- -- -- -- --
-- grabs a guess from valid chords, making the highest entropy guess
makeGuess :: [Chord] -> [String]
makeGuess [] = []
makeGuess (c:cs) = chord2string (head (msort (c:cs)))

-- we want to refine the possible chords by enforcing pitches, notes
-- and octaves we know to be in the chord, or removing pitches, notes
-- and octaves we know are not in the list
refine :: GameState -> Feedback -> [Chord]
refine gs ((pp,nn,oo), (0,n,o)) -- none of the pitches are valid
    = removePitch pp (refine gs ((pp,nn,oo), (-1,n,o)))
refine gs ((pp,nn,oo), (p,0,o)) -- none of the notes are valid
    = removeNote nn (refine gs ((pp,nn,oo), (p,-1,o)))
refine gs ((pp,nn,oo), (p,n,0)) -- none of the octaves are valid
    = removeOctave oo (refine gs ((pp,nn,oo), (p,n,-1)))
refine gs ((pp,nn,oo), (p,n,o)) -- ie, we know at least p pitches in chord
    | p > 0 = enforcePitch p (pp,nn,oo) (refine gs ((pp,nn,oo), (-1,n,o)))
    | n > 0 = enforceNote n (pp,nn,oo) (refine gs ((pp,nn,oo), (p,-1,o)))
    | o > 0 = enforceOctave o (pp,nn,oo) (refine gs ((pp,nn,oo), (p,n,-1)))
refine (State (_, chords)) (chord, _) = delete chord chords
refine Start (chord, _) -- we need to generate our list of chords
    = delete chord generateChords

-- delete chords without specified num pitches from chord
enforcePitch :: Int -> Chord -> [Chord] -> [Chord]
enforcePitch 1 (p1,p2,p3) =
    -- return a list filtered by lambda exp
    filter (\x -> pitchInChord p1 x ||
                  pitchInChord p2 x ||
                  pitchInChord p3 x)
enforcePitch 2 (p1,p2,p3) =
    filter (\x -> pitchInChord p1 x && pitchInChord p2 x ||
                  pitchInChord p2 x && pitchInChord p3 x ||
                  pitchInChord p1 x && pitchInChord p3 x) 

-- delete chords without specified num notes from chord
enforceNote :: Int -> Chord -> [Chord] -> [Chord]
enforceNote 1 (p1,p2,p3) =
    filter (\x -> noteInChord p1 x || noteInChord p2 x || noteInChord p3 x)
enforceNote 2 (p1,p2,p3) =
    filter (\x -> (noteInChord p1 x && noteInChord p2 x) ||
                  (noteInChord p2 x && noteInChord p3 x) ||
                  (noteInChord p1 x && noteInChord p3 x))
enforceNote 3 (p1,p2,p3) =
    filter (\x -> noteInChord p1 x && noteInChord p2 x && noteInChord p3 x)

-- delete chords without specified num octaves from chord
enforceOctave :: Int -> Chord -> [Chord] -> [Chord]

enforceOctave 1 (p1,p2,p3) =
    filter (\x -> octaveInChord p1 x ||
                  octaveInChord p2 x ||
                  octaveInChord p3 x)
enforceOctave 2 (p1,p2,p3) =
    filter (\x -> octaveInChord p1 x && octaveInChord p2 x ||
                  octaveInChord p2 x && octaveInChord p3 x ||
                  octaveInChord p1 x && octaveInChord p3 x)
enforceOctave 3 (p1,p2,p3) =
    filter (\x -> octaveInChord p1 x &&
                  octaveInChord p2 x &&
                  octaveInChord p3 x)

-- delete chords with pitches known to be false
removePitch :: Pitch -> [Chord] -> [Chord]
removePitch p =
    filter (\(x,y,z) -> x /= p && y /= p && z /= p)

-- delete chords containing specified note
removeNote :: Pitch -> [Chord] -> [Chord]
removeNote (n,o) =
    filter (\((xn,_),(yn,_),(zn,_)) ->
        xn /= n && yn /= n && zn /= n)

-- delete chords containing specified octave
removeOctave :: Pitch -> [Chord] -> [Chord]
removeOctave (n,o) =
    filter (\((_,xo),(_,yo),(_,zo)) ->
        xo /= o && yo /= o && zo /= o)

-- -- -- -- -- -- -- -- -- -- ~Utility Functions~ -- -- -- -- -- -- -- -- --
-- generate every possible pitch combination (chord)
generateChords :: [Chord]
generateChords = checkDupes [(p1,p2,p3) | p1 <- pitches,
                                          p2 <- pitches,
                                          p3 <- pitches,
                                          p1 /= p2,
                                          p2 /= p3, 
                                          p1 /= p3]
    where pitches = [(note, octave) | note <- notes,
                                      octave <- octaves]

-- remove duplicate chords
checkDupes :: [Chord] -> [Chord]
checkDupes [] = []
checkDupes (c:cs) = c : checkDupes (filter (\x -> not(eqChord c x)) cs)

-- check if two chords are equivalent
eqChord :: Chord -> Chord -> Bool
eqChord (p11,p12,p13) c
    = pitchInChord p11 c &&
      pitchInChord p12 c &&
      pitchInChord p13 c

-- check if provided pitch is a part of the chord
pitchInChord :: Pitch -> Chord -> Bool
pitchInChord p (p1,p2,p3) =
    (p == p1) || (p == p2) || (p == p3)

-- check if provided note is a part of the chord
noteInChord :: Pitch -> Chord -> Bool
noteInChord (n,o) ((n1, o1), (n2,o2), (n3,o3)) =
    (n == n1) || (n == n2) || (n == n3)

-- check if provided octave is a part of the chord
octaveInChord :: Pitch -> Chord -> Bool
octaveInChord (n,o) ((n1, o1), (n2,o2), (n3,o3)) =
    (o == o1) || (o == o2) || (o == o3)

-- swap between internal and external chord representation
chord2string :: Chord -> [String]
chord2string ((n1, o1), (n2, o2), (n3, o3))
    = [[n1] ++ show o1, [n2] ++ show o2, [n3] ++ show o3]

-- swap between external and internal chord representation
string2chord :: [String] -> Chord
string2chord lst = (p !! 0, p !! 1, p !! 2)
    where p = map string2pitch lst

-- swap between external and internal pitch representation
string2pitch :: String -> Pitch
string2pitch (x:xs) = (x, digitToInt (head xs))

-- swap between internal and external pitch representation
pitch2string :: Pitch -> String
pitch2string (note, octave) = [note] ++ show octave

-- merge sort function
msort :: [Chord] -> [Chord]
msort [] = []
msort (x:xs)
    | length (x:xs) > 1 = merge (msort left) (msort right)
    | otherwise = (x:xs)
    where left = take (div (length (x:xs)) 2) (x:xs)
          right = drop (div (length (x:xs)) 2) (x:xs)

merge :: [Chord] -> [Chord] -> [Chord]
merge lst [] = lst
merge [] lst = lst
merge (x:xs) (y:ys)
    | entropy x > entropy y = x:merge xs (y:ys)
    | otherwise = y:merge ys (x:xs)

-- we want to preferentially select chords that are different..
entropy :: Chord -> Int
entropy ((n1,o1), (n2,o2), (n3,o3))
    | n1 == n2 && n1 == n3 && n1 == n2 = 3
    | o1 == o2 && o1 == o3 && o1 == o2 = 3
    | n1 == n2 && n1 == n3 ||
      n2 == n1 && n2 == n3 ||
      n3 == n1 && n3 == n2 = 2
    | o1 == o2 && o1 == o3 ||
      o2 == o1 && o2 == o3 ||
      o3 == o1 && o3 == o2 = 2
    | otherwise = 1


