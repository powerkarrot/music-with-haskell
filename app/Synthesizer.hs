module Synthesizer (module Synthesizer, module Structures, module Settings, module Utils, module IOFunctions, module Functions) where

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

isOdd :: Float -> Bool
isOdd x = (mod (round x) 2) /= 0

--meh
triangleWave2 :: Note -> [Pulse]
triangleWave2 n = let a = envelope $ sample n 
                      d = [sum $ map (* x) [1.91]  | x <- a] 
                      e = [sum $ map (* x) [0.0001, 0.0003 .. 0.0205]  | x <- a] 
                      f = [x * 1.020005 | x <- a] -- map lol
                  in amplify (_vol n ) d

makeWave :: (Note -> [Pulse]) -> [Note] -> [Pulse]
makeWave f notes  = concat [f n| n <- notes]


-----------------------------------------soundtest functions------------------------------------------------------------
test = let 
        maj = makeNote (intervals (majorScale A $ negate 2) ) (replicate  7 1) (replicate 7 (0.5))

        b = makeWave triangleWave2 maj
        in saveAsWav b "/home/karrot/test.wav"
 
test2 n =   [sum $ map (+ (i )) $(sample n) |  i <- [1 .. 10000]]
