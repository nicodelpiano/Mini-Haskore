module Instrumentos.Percusion where

import Notas.Musica as Musica
import Notas.Tiempos as Tiempos
import Notas.Funciones as Funciones
import Notas.Composicion as Composicion

import MIDI.ToMidi as ToMidi

import Instrumentos.InstrCuerda as InstrCuerda
import Instrumentos.Guitarra as Guitarra
import Instrumentos.Bajo as Bajo


data Percusion =
        AcousticBassDrum  -- Numero MIDI 35
     |  BassDrum1         -- Numero MIDI 36
     |  SideStick         -- ...
     |  AcousticSnare  | HandClap      | ElectricSnare  | LowFloorTom
     |  ClosedHiHat    | HighFloorTom  | PedalHiHat     | LowTom
     |  OpenHiHat      | LowMidTom     | HiMidTom       | CrashCymbal1
     |  HighTom        | RideCymbal1   | ChineseCymbal  | RideBell
     |  Tambourine     | SplashCymbal  | Cowbell        | CrashCymbal2
     |  Vibraslap      | RideCymbal2   | HiBongo        | LowBongo
     |  MuteHiConga    | OpenHiConga   | LowConga       | HighTimbale
     |  LowTimbale     | HighAgogo     | LowAgogo       | Cabasa
     |  Maracas        | ShortWhistle  | LongWhistle    | ShortGuiro
     |  LongGuiro      | Claves        | HiWoodBlock    | LowWoodBlock
     |  MuteCuica      | OpenCuica     | MuteTriangle
     |  OpenTriangle      -- Numero MIDI 82

   deriving (Show,Eq,Ord,Enum)

perc :: Percusion -> Dur -> Melodia
perc p d = nota d (aTono (fromEnum p + 35))

ritmo1
  = let  p1 = perc LowTom nn
         p2 = perc ClosedHiHat nn
         p3 = perc LowTom ncp
         p4 = perc OpenHiHat ncp
         p5 = perc ClosedHiHat sc
    in Mod (Instr Percussion) (loop 3 (p1 :+: crs :+: p2 :+: p1 :+: p3 :+: p5 :+: p4))

ritmo2
  = let p1 = perc ClosedHiHat co
        p2 = perc AcousticSnare co
        p3 = perc LowTom co
    in Mod (Instr Percussion) (loop 7 ((p1 :=: p3) :+: p1 :+: (p1 :=: p2) :+: p1))

ritmo3
  = let p1 = perc ClosedHiHat co
        p2 = perc AcousticSnare co
        p3 = perc LowTom co
    in  Mod (Instr Percussion) (loop 7 ((p1 :=: p3) :+: p1 :+: (p1 :=: p2) :+: (p1 :=: p2) :+: (p1 :=: p3) :+: p1 :+: (p1 :=: p2) :+: p1))
