module MIDI.ToMidi where

import Notas.Musica as Musica
import Notas.Tiempos as Tiempos
import Notas.Composicion as Composicion
import Notas.Funciones as Funciones

import MIDI.GeneralMidi as GeneralMidi
--import MIDI.MidiIO as MidiIO 
import Sound.PortMidi

import Data.List(partition)
import Data.Char(toLower,toUpper)
import Codec.Midi

type ProgNum     = Int
type Configuracion = [(NombreInstr, Channel)]

{-

El objetivo es pasar de una composicion (algo del tipo Musica) a un objeto del tipo Midi.
Luego convertir ese objeto Midi a un archivo real .mid, usando las funcion exporfile de MidiIO.hs .

-}

-- Creamos una configuracion a partir de una lista de instrumentos.
crearConf :: [NombreInstr] -> Configuracion
crearConf ins = crearConf' 0 ins                                   

crearConf' :: Int -> [NombreInstr] -> Configuracion
crearConf' _ []     = []
crearConf' n (x:xs) = if n>=15 then error "crearConf: Demasiados Instrumentos!"
                               else case x of
                                       Percussion -> (Percussion,9):crearConf' (n+1) xs
                                       ins        -> (ins,canal!!n):crearConf' (n+1) xs
                                               where canal = [0..8]++[10..15]

-- Dada una configuracion y un instrumento, devolvemos el canal y el numero del instrumento asociado.
-- La funcion toGM se encuentra en el archivo GeneralMidi.hs.
confLookup :: Configuracion  -> NombreInstr 
                           -> (Channel, ProgNum)
confLookup conf ins = (chan, toGM ins)
  where chan = maybe  (error (  "El Instrumento " ++ show ins ++ 
                                " no esta en la configuracion.")  )
                      id (lookup ins conf)

-- pasamos de una composicion y una configuracion a un Midi
-- Simplemente, creamos un objeto Midi, haciendo un split de una composicion
-- y luego pasamos la configuracion a eventos Midi.
toMidi :: Composicion -> Configuracion -> Midi
toMidi comp conf =
  let split     = splitByInst comp
      insts     = map fst split
      rightMap  =  if (allValid conf insts) then conf 
                   else (crearConf insts)
  in Midi  (if length split == 1  then SingleTrack 
                                  else MultiTrack)
           (TicksPerBeat division)
           (map (fromAbsTime . performToMEvs rightMap) split)

division = 96 :: Int
-- Chequea si los instrumentos estan en la configuracion
allValid :: Configuracion -> [NombreInstr] -> Bool
allValid conf = and . map (lookupB conf)

-- Se fija si el instrumento esta en la configuracion
lookupB :: Configuracion -> NombreInstr -> Bool
lookupB conf x = or (map ((== x) . fst) conf)


-- Separamos una composicion, en una lista de Instrumentos y sus composiciones asociadas.
splitByInst :: Composicion ->  [(NombreInstr,Composicion)]
splitByInst []   = []
splitByInst comp = (i, comp1) : splitByInst comp2
                      where i              = nInstr (head comp)
                            (comp1, comp2) = partition (\e -> nInstr e == i) comp
{-
Midi	
fileType :: FileType	
timeDiv :: TimeDiv	
tracks :: [Track Ticks]

data FileType
Constructors
 SingleTrack	
 MultiTrack	
 MultiPattern

data TimeDiv 
Constructors
 TicksPerBeat Int	
 TicksPerSecond Int Int

Track a = (a,Message)
-}
type MEvent = (Ticks, Message)

defST = 500000

-- Creamos una lista de MEvents a partir de un ejecutor.
-- Mapeamos la configuracion para darle el canal a cada instrumento,
-- y usamos mkMEvents para componer las notas en eventos Midi (la ordenamos segun los ticks).
performToMEvs ::  Configuracion
                  -> (NombreInstr, Composicion) 
                  -> [MEvent]
performToMEvs upm (ins, comp) =
  let  (chan,progNum)   = confLookup upm ins
       setupInst        = (0, ProgramChange chan progNum)
       setTempo         = (0, TempoChange defST)
       loop []      =  []
       loop (e:es)  =  let (mev1,mev2) = mkMEvents chan e
                       in mev1 : insertMEvent mev2 (loop es)
  in setupInst : setTempo : loop comp


-- Los mensajes que vamos a utilizar son NoteOn y NoteOff.
-- Estos mensajes le dicen a MIDI cuando tocar una nota y cuando dejar de tocarla.
{-
Los tipos Channel, Key y Velocity son sinonimos de Int.

NoteOff	
channel :: !Channel	
key :: !Key	
velocity :: !Velocity	

NoteOn	
channel :: !Channel	
key :: !Key	
velocity :: !Velocity	
-}
-- La funcion mkMEvents, crea una tupla de eventos midi, que 
-- dicen cuando empezar a tocar una nota (primer elemento de la tupla)
-- y cuando dejar de tocarla.
mkMEvents :: Channel -> Notas -> (MEvent,MEvent)
mkMEvents  mChan (Notas {  nTiempo = t, nTono = p, 
                           nDur = d, nVol = v})
                  = (  (toDelta t, NoteOn  mChan p v'),
                       (toDelta (t+d), NoteOff mChan p v') )
           where v' = max 0 (min 127 (fromIntegral v))

-- toDelta t: Redondea los ticks por golpe
toDelta t = round (t * 2.0 * fromIntegral division)

-- Inserta un Evento Midi en una lista de eventos Midi.
-- De acuerdo a los ticks, si son menores van antes.
insertMEvent :: MEvent -> [MEvent] -> [MEvent]
insertMEvent mev1  []         = [mev1]
insertMEvent mev1@(t1,_) mevs@(mev2@(t2,_):mevs') = 
      if t1 <= t2 then mev1 : mevs
                  else mev2 : insertMEvent mev1 mevs'

-- Configuracion de Instrumentos y canales asociados.
defConf :: Configuracion
defConf = [(DistortionGuitar,1),
          (AcousticGuitarNylon,2),
          (AcousticGuitarSteel,3),
          (FretlessBass,4),
          (ElectricBassPicked,5),
          (ElectricBassFingered,6),
          (ElectricGuitarClean,7),
          (AcousticBass,8),
          (RockOrgan,9)]  
            

testMidi :: Melodia -> Midi
testMidi m = toMidi (componer m) defConf

test :: Melodia -> IO ()
test     m = exportFile "test.mid" (testMidi m)

grabar :: Melodia -> String -> IO ()
grabar m s = exportFile s (testMidi m)

play :: Melodia -> IO ()
play = playM . testMidi 
 
playM :: Midi -> IO ()
playM midi = do
  initialize
  (defaultOutput playMidi) midi 
  terminate
  return ()

-- probar Banda
testBanda :: Banda -> IO ()
testBanda banda = let comp = componerBanda banda
                  in exportFile "banda.mid" (toMidi comp defConf)

escucharBanda :: Banda -> IO ()
escucharBanda = play . joinBanda  
 

