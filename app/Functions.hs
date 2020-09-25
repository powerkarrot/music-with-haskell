module Functions where

import Structures


makeNote :: [Semitone] -> [Beats] -> [Volume] -> [Note]
makeNote line dur vol = [Note x y z |(x, y, z) <- zip3 line dur vol]

--  Fetches Key value by indice
getKey :: Int -> Key
getKey i = [A ..] !! (i `mod` 12)

-- Fetches Enum indice from Key
key :: Enum a => a -> Semitone
key k = fromIntegral $ fromEnum k 

--Function that shifts (root) note to another key 
makeKey :: [Semitone] -> Key -> Octave -> [Pulse]
makeKey line k octave = map (\x -> x + key(k) + (octave * 12)) line

--Creates Scale in Key in Octave
createScale :: Key -> [Interval] -> Octave -> Scale Semitone
createScale root intervals octave = Scale $ makeKey intervals root octave

makeScale :: Scale Semitone -> Semitone -> Scale Semitone
makeScale scale i = Scale $ map (\x -> x + i) (intervals scale)

-- Transposes scale to another key
transposeScale :: Scale Semitone -> Semitone -> Key-> Scale Semitone
transposeScale scale i k = Scale $ map (\x -> x + (i - key k )) (intervals scale)

transposeScale2 :: Scale Semitone -> Key-> Scale Semitone
transposeScale2 scale i = Scale $ map (\x -> x + (key i - (intervals scale !! 0))) (intervals scale)

-- Returns key from a chord progression indice
mapProgToKey :: Key -> Position -> Octave -> Semitone
mapProgToKey k indice octave =
    let scale = majorScale k octave
    in ((intervals scale) !! (indice -1)) -1

--Create some basic scales
majorScale root octave = createScale root majScale octave

minorScale root octave = createScale root minScale octave

majorPentagonic root octave = createScale root majPentagonic octave

minorPentagonic root octave = createScale root minPentagonic octave

diminishedScale root octave = createScale root dimScale octave

augmentedScale root octave = createScale root augScale octave

chordScale root octave = createScale root fifthsMScale octave

fifthsMinorScale root octave = createScale root fifthsmScale octave

