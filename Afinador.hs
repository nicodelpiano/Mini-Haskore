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

ins = defGuitar

afinador :: IO ()
afinador = do sonarCuerda 6 0 4 ins
              sonarCuerda 5 0 4 ins
              sonarCuerda 4 0 4 ins
              sonarCuerda 3 0 4 ins
              sonarCuerda 2 0 4 ins
              sonarCuerda 1 0 4 ins
              
