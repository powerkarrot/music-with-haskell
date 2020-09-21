module Synthesizer where

import Structures
import Settings
import Utils
import IOFunctions
import Functions


beatDuration :: Seconds
beatDuration =  60.0 / bpm

beat :: Float -> Beats
beat n = beatDuration * n

f :: Semitone -> Hz
f n = pitchStandard * (2 ** (1.0 / 12.0)) ** n

sineWave :: Note -> [Pulse]
sineWave n = map (* _vol n) $ envelope $ freq (f (_semitone n)) (_dur n * beatDuration)

sawTooth :: Note -> [Pulse]
sawTooth n = envelope $ map ( subtract $ _vol n ) ( map (/ _vol n) ( map (* pi) a))
          where a = freq (f (_semitone n)) (_dur n * beatDuration) 




sawTooth2 n = let a = freq (f (_semitone n)) (_dur n * beatDuration)
              in envelope $ [ ( _vol n `subtract`) (_vol n / (pi * x)) | x <- a]
              
   
--horrendo                 
squareWave2 n = let a = envelope $ freq (f (_semitone n)) (_dur n * beatDuration)
                    d = [2 * pi * (_vol n) * x | x <- a]
              
              in  d

squareWave n = let a = freq (f (_semitone n)) (_dur n * beatDuration)
                   
              in  [if (x < 0) then (_vol n ) else (negate $ _vol n) | x <- a]


 --nao se ouve nada                   
gibbsWave n = let a = envelope $ freq (f (_semitone n)) (_dur n * beatDuration)
                  d = [(4 / (pi * 1 )) * cos (2 * pi * (_vol n) * x)| x <- a]
              
              in  d

--testa
gibbsWaveAcc n = let a = envelope $ freq (f (_semitone n)) (_dur n * beatDuration)
                     b = [((negate 2) / (pi * t )) | t <- [ 1 .. 100]]
                     c = [sum $ map (* (2 * pi * (_vol n) * x)) b | x <- a]
                in  c


--lmao not a triagle
triangleWave n = let a = envelope $ freq (f (_semitone n)) (_dur n * beatDuration)

                     one = negate $ _vol n  + (( 2 * _vol n) / pi)
                     two = (3 * _vol n) - ((2 * _vol n) / pi)
                     d = [if (x < 0) then (x * one) else (x * two) | x <- a]
              in  d

--frequency with envelope
freq :: Hz -> Seconds -> [Pulse]
freq hz duration = output
    where
      step :: Hz
      step = (hz * 2 * pi) / sampleRate  --A440 pitch standard
      output :: [Pulse]
      output = map sin $ map (*step) [ 0.0 .. sampleRate * duration]

envelope line =  zipWith3 (\x y z -> x * y * z) release attack line
    where
      attack :: [Pulse]
      attack = map (min 1.0) [0.0, 0.001 ..]
      release :: [Pulse]
      release = reverse $ take (length line) attack


---------------------------------------------MAKE LINE --------------------------------------------

makeLine :: [Note] -> [Pulse]
makeLine notes  = concat [sineWave n | n <- notes]

-- whatever this kind of 3 times list comprehension is called
makeNote :: [Semitone] -> [Beats] -> [Volume] -> [Note]
makeNote line dur vol = [Note x y z |(x, y, z) <- zip3 line dur vol]




--test  waves
test = let 
        maj = makeNote (intervals (majorScale A $ negate 2) ) (replicate  7 1) (replicate 7 (0.5))
        b = makeLine maj
        in saveAsWav b "/home/karrot/test.wav"
        --in b

