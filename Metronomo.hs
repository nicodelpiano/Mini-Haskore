import Notas.Musica as Musica
import Notas.Tiempos as Tiempos
import Notas.Escalas as Escalas
import Notas.Funciones as Funciones
import Notas.Composicion as Composicion

import Instrumentos.InstrCuerda as InstrCuerda
import Instrumentos.Guitarra as Guitarra
import Instrumentos.Bajo as Bajo
import Instrumentos.Percusion as Percusion

import MIDI.ToMidi as ToMidi
import MIDI.GeneralMidi as GeneralMidi
import MIDI.MidiIO as MidiIO

metronomo :: Dur -> IO ()
metronomo d = do play (nota d (La,5))
                 metronomo d
