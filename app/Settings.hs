module Settings where 
import Structures

volume :: Volume
volume = 0.07

octave :: Octave
octave = 0.0

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

--beats per minute
bpm :: Beats
bpm = 120.0

--Quarter note
qn :: Beats
qn = 1

-- Notes per meter
npm :: Int
npm = 4