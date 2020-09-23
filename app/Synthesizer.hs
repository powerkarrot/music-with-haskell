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


amplify :: Num b => b -> [b] -> [b]
amplify v = map (v *)


sine :: Hz -> Samples
sine hz =  hz * 2 * pi

--frequency
freq :: Hz -> Seconds -> [Pulse]
freq hz duration = output
    where
      step :: Hz
      step = sine hz / sampleRate  --A440 pitch standard
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
makeLine notes  = concat [triangleWave n| n <- notes]

-- whatever this kind of 3 times list comprehension is called
makeNote :: [Semitone] -> [Beats] -> [Volume] -> [Note]
makeNote line dur vol = [Note x y z |(x, y, z) <- zip3 line dur vol]

sample :: Note -> [Pulse]
sample n = freq (f (_semitone n)) (_dur n * beatDuration)


sineWave :: Note -> [Pulse]
sineWave n = amplify (_vol n) $ envelope $ sample n


distort :: Floating b => [b] -> b -> [b]
distort sinew p = map (\x -> signum x * abs x ** p) $ sinew


failedSaw :: Note -> [Pulse]
failedSaw n = let a = envelope $ sample n
              in  envelope $ amplify (_vol n) $ [ ((-2) / pi ) * x| x <- a]

squareWave :: Note -> [Pulse]
squareWave n = envelope $ amplify (_vol n) $ map signum (sample n)

triangleWave :: Note -> [Pulse]
triangleWave n = let a = envelope $ sample n

                     one = (_vol n)  + (( 2 * _vol n) / pi )

                     d = [if (x < 0) then negate (x * one) else  (x * one) | x <- a] 
              in  d

--test  waves
test = let 
        maj = makeNote (intervals (majorScale A $ negate 2) ) (replicate  7 1) (replicate 7 (0.5))

        b = makeLine maj
        in saveAsWav b "/home/karrot/test.wav"
        --in b
 

huch n =   [sum $ map (+ (i )) $(sample n) |  i <- [1 .. 10000]]
