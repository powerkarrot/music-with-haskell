{-# LANGUAGE ParallelListComp #-}
module Music (module Music, module Synthesizer) where 

import Synthesizer

import Control.Monad.Random
import Control.Monad.Par
import Data.List 
import Control.Concurrent.Async
import Control.Concurrent
import Control.DeepSeq
import Control.Applicative
import Data.Maybe 

arpeggio:: Semitone -> Scale Semitone -> [Beats] -> [Volume] -> [Pulse]
arpeggio key tscale dur vol = 
    let scale = makeScale tscale key
        arpeg = chord ++ [0]
        chordDone = [(intervals scale) !! round x | x <- arpeg]
        notes = makeNote chordDone dur vol
    in makeWave squareWave notes

triad :: Semitone -> Scale Semitone -> Beats -> Volume -> [Pulse]
triad key tscale dur vol = 
    let scale = makeScale tscale key
        v = take (length chord) $ repeat vol
        d = take (length chord) $ repeat dur
        chordDone = [(intervals scale) !! round x | x <- chord]
        notes = makeNote chordDone d v
    in concat [zipWith3 (\x y z -> x + y + z) 
        (makeWave squareWave ([notes !! 0])) 
        (makeWave squareWave  ([notes !! 1])) 
        (makeWave squareWave  ([notes !! 2]))]

--rules for minor progression
chordInMinorProg :: Position -> Key -> Octave -> Beats-> [Pulse]
chordInMinorProg index k octave t
    | index == 5 || index == 6 = triad i (majorScale A octave) t volume
    | index == 1 || index == 4 = triad i (minorScale A octave) t volume 
    | index == 2 || index == 7 = triad i (diminishedScale A octave) t volume
    | index == 3 = triad i (augmentedScale A octave) qn volume
    | otherwise =  error "Invalid chord progression"
    where 
     i :: Semitone 
     i = mapProgToKey k index octave

--rules for major progression
chordInMajorProgression :: Position -> Key -> Octave -> Beats -> [Pulse]
chordInMajorProgression index k octave t
    | index == 1 || index == 4 || index == 5 = triad i (majorScale A octave) t volume
    | index == 2 || index == 3 || index == 6 = triad i (minorScale A octave) t volume 
    | index == 7 = triad i (diminishedScale A octave) t volume
    | otherwise =  error "Invalid chord progression"
    where
     i :: Semitone
     i = mapProgToKey k index octave
  
arpeggioInMajorProgression :: Position -> Key -> Octave -> [Pulse]
arpeggioInMajorProgression index k octave
    | index == 1 || index == 4 || index == 5 = arpeggio i (majorScale A octave) (replicate 4 (qn/4)) (replicate 4 (qn/4))
    | index == 2 || index == 3 || index == 6 = arpeggio i (minorScale A octave) (replicate 4 (qn/4))(replicate 4 (qn/4))
    | index == 7 = arpeggio i (diminishedScale A octave) (take 4 (repeat (qn/4))) (replicate 4 (qn/4))
    | otherwise =  error "Invalid chord progression"
    where
     i :: Semitone
     i = mapProgToKey k index octave 

--alternative melody creation that plays in root key over all bars
randomMelody :: MonadRandom m => Scale Semitone -> NumNotes -> p -> m [Pulse]
randomMelody scale num_notes voices = do
    line <- pickNRandom (intervals scale) num_notes
    vols <- pickNRandom [0.0, 0.05 .. 0.5] num_notes
    durs <- if num_notes <= npm 
            then pickNRandom [0.5, 1 .. 1] num_notes 
            else pickNRandom [0.25, 0.5 .. 1] num_notes
    let notes = makeNote line durs vols
    return $ makeWave sineWave $ notes ++ notes ++ reverse notes

--Melody that follows chord progression
progressiveMelody :: MonadRandom m => Scale Semitone -> Octave -> NumNotes -> Voice -> m [Note]
progressiveMelody scale octave num_notes voices = do    
    line <- pickNRandom (intervals scale) num_notes
    vols <- pickNRandom [0.0, 0.025 .. 0.2] num_notes
    durs <- pickNSum [0.25, 0.5 .. 1] num_notes 
    let v = filterReplace ( < ceiling 0.1) vols  0
    return $ makeNote line durs vols

--Soulless walking bass 
walkingBass :: MonadRandom m => Scale Semitone -> Octave -> Int -> Voice -> m [Note]
walkingBass scale octave num_notes voices = do
    l <- pickNRandom (intervals scale) 2
    let line = [(intervals scale !! 0)] ++ l ++ [intervals scale !! 0]
    return $ makeNote line (replicate npm 1)   (replicate npm 0.1) 

-- Function that creates a progression 
createProgressionBar :: (Int -> Key ->  Octave -> Beats ->  [Pulse]) -> Progression -> Key  -> Octave-> [Pulse]
createProgressionBar f prog key octave = 
    let rythm = [1,0.5, 1, 0.5, 0.5, 0.5] --dont let sum go over npm (4 usually)....
    --let rythm = [1,2,1]
    in join [f i key octave x| i <- prog, x <- rythm] 

--Let a voice follow over a chord Progression
--aqui: estructura para Bars
voiceProgression :: MonadRandom m => [Int] -> Key -> Octave -> MajMin -> Int -> Voice -> m [Pulse]
voiceProgression chordProg tk octave majMin num_notes voices 
    | voices == Melody = do 
        let scale = if majMin == Major then chordScale tk octave else  minorPentagonic tk octave
        seq <- sequence[progressiveMelody (transposeScale scale k tk) octave num_notes voices | k <- chords ]
        return  $ makeWave squareWave $ join seq
    | voices == Bassline = do
        let bassScale = if majMin == Major then chordScale tk (octave - 2) else minorScale tk (octave - 2)
        seq <- sequence[walkingBass (transposeScale bassScale k tk) octave num_notes voices | k <- chords ]
        return  $ makeWave squareWave $ join seq
    | voices == ChordProgression = do
        let f = if majMin == Major then chordInMajorProgression else chordInMinorProg  
        return $ createProgressionBar f chordProg tk octave
    where
     chords :: [Semitone]
     chords =   [ mapProgToKey tk index octave | index <- chordProg]

-- creates different voices
createBars :: Progression -> Key  -> Octave -> MajMin -> Int -> NumNotes -> IO [Pulse]
createBars chordSeq k octave majMin num_bars num_notes = do

    --melodyA <- randomMelody scale num_notes Melody 
    --let melAlt = join $ replicate (length chordSeq * num_bars) melodyA

    let melody = voiceProgression chordSeq  k octave majMin num_notes Melody
    let bass = voiceProgression chordSeq k octave majMin num_notes Bassline  
    let chords = voiceProgression chordSeq k octave majMin num_notes ChordProgression

    c1 <- concurrently melody melody
    c2 <- concurrently melody melody
    c3 <- concurrently bass chords

    let b = join $ replicate (num_bars) $ fst c3
        chordpattern = join $ replicate (num_bars) $ snd c3
        mel = fst c1 ++ snd c1 ++ fst c2 ++ snd c2
        mels = join $ replicate (num_bars) $ fst c1 ++ snd c1 ++ fst c2 ++ snd c2

    return $ chordpattern |:| b |:|  mels


createSheet:: FilePath -> Progression -> Key -> Octave -> MajMin -> Int -> NumNotes -> IO ()
createSheet filePath chordSeq key octave majMin numBars num_notes
    | any (< 0) chordSeq || any (>7) chordSeq    = do error "Invalid chord progression"
    | otherwise = do
        bars <-  createBars chordSeq key octave majMin numBars num_notes
        let sheet = Sheet {_chordProg = chordSeq, _key = key, _majMin = majMin, _numBars = numBars, _barSeq = bars} 
        saveAsWav (_barSeq sheet) filePath


