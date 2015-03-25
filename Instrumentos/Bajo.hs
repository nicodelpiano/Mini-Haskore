module Instrumentos.Bajo where

import Notas.Musica as Musica
import Notas.Tiempos as Tiempos
import Notas.Funciones as Funciones
import Notas.Composicion as Composicion

import MIDI.ToMidi as ToMidi

import Instrumentos.InstrCuerda as InstrCuerda

type Bajo = InstrCuerda

-- Bajo default
defBajo :: Bajo
defBajo = InstrCuerda af440 4 21 Dedo


