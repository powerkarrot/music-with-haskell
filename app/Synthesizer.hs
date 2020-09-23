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

sine :: Note -> [Pulse]
sine n = amplify (_vol n) $ envelope $ sample n

huch n =  concat [map (* (1 - i)) $(sample n) |  i <- [1 .. 10]]


--distort :: [Pulse] -> [Pulse]
distort sinew p = map (\x -> signum x * abs x ** p) $ sinew

sawTooth :: Note -> [Pulse]
sawTooth n = envelope $ map ( subtract $ _vol n ) ( map (/ _vol n) ( map (* pi) a))
          where a = sample n


sawTooth2 n = let a = sample n
              in envelope $ [ ( _vol n `subtract`) (_vol n / (pi * x)) | x <- a]


sawTooth3 n = let a = freq (f (_semitone n)) (_dur n * beatDuration)
              in envelope $ amplify (_vol n) $ [ 1 - 2 * x | x <- a]


squareWave4 n = let a = envelope $ sample n
                   
              in  envelope $ amplify (_vol n) $ [ (signum x * 2) -1 | x <- a]

squareWave n = let a = envelope $ sample n
                   
              in  envelope $ amplify (_vol n) $ [ signum x| x <- a]

test2 n = let a = envelope $ sample n
              b = [1, 3 .. 103]
              c = amplify (_vol n) $ [sum $ map (* x) b | x <- a]
          in  c

tri n = let a = sample n
            in  envelope $ map (* _vol n) $ [if (2 - (4 * x)) < 1 then 4 * x else if (4 * x) > 3 then (4 * x) - 4 else 2 * 4 * x| x <- a]

--testa
gibbsWaveAcc n = let a = envelope $ sample n
                     b = [((negate 2) / (pi * t )) | t <- [ 1 .. 100]]
                     c = [sum $ map (* (2 * pi * (_vol n) * x)) b | x <- a]
                in  c


--lmao not a triagle. 
triangleWave2 n = let a = envelope $ sample n

                      one = negate $ _vol n  + (( 2 * _vol n) / pi)
                      two = (3 * _vol n) - ((2 * _vol n) / pi)
                      d = [if (x < 0) then (x * one) else (negate (x * one)) | x <- a]
              in  d

triangleWave n = let a = envelope $ sample n

                      one = negate $ (_vol n)  + (( 2 * _vol n) / pi)
                      d = [if (x < 0) then (x * one) else (negate (x * one)) | x <- a]
              in  d


amplify :: Num b => b -> [b] -> [b]
amplify v = map (v *)


sineWave :: Hz -> Samples
sineWave hz =  hz * 2 * pi

--frequency
freq :: Hz -> Seconds -> [Pulse]
freq hz duration = output
    where
      step :: Hz
      step = sineWave hz / sampleRate  --A440 pitch standard
      output :: [Pulse]
      output = map sin $ map (*step) [ 0.0 .. sampleRate * duration]


envelope :: [Pulse] -> [Pulse]
envelope line =  zipWith3 (\x y z -> x * y * z) release attack line
    where
      attack :: [Pulse]
      attack = map (min 1.0) [0.0, 0.001 ..]
      release :: [Pulse]
      release = reverse $ take (length line) attack


makeLine :: [Note] -> [Pulse]
makeLine notes  = concat [triangleWave  n | n <- notes]

-- whatever this kind of 3 times list comprehension is called
makeNote :: [Semitone] -> [Beats] -> [Volume] -> [Note]
makeNote line dur vol = [Note x y z |(x, y, z) <- zip3 line dur vol]


sample n = freq (f (_semitone n)) (_dur n * beatDuration)


--test  waves
test = let 
        maj = makeNote (intervals (majorScale A $ negate 2) ) (replicate  7 1) (replicate 7 (0.5))

        b = makeLine maj
        in saveAsWav b "/home/karrot/test.wav"
        --in b
 