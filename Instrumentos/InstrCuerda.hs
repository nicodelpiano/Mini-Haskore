-- Modulo InstrCuerda
-- Esta encargado de modelar la estructura
-- de un instrumento de cuerda.

module Instrumentos.InstrCuerda where

import Notas.Musica as Musica
import Notas.Tiempos as Tiempos
import Notas.Funciones as Funciones
import Notas.Composicion as Composicion

import MIDI.ToMidi as ToMidi

data InstrCuerda = InstrCuerda {
                                cAfin :: Afinacion,
                                cNCue :: Cuerda,
                                cNTra :: Traste,
                                cTipo :: TipoInstr
                               }

type Cuerda = Int
type Traste = Int
type Afinacion = [ClaseTono]

type Tablatura = [(Cuerda,Traste,Dur)]

af440 :: Afinacion
af440 = [Mi,Si,Sol,Re,La,Mi]

data TipoInstr = Electrica | ElectricaLimpio | Acustica | Criolla  -- Para Guitarras
                 | Dedo | Pua | Fretless | Acustico                -- Para Bajo 

mapC :: TipoInstr -> NombreInstr
mapC Electrica       = DistortionGuitar
mapC Acustica        = AcousticGuitarSteel
mapC ElectricaLimpio = ElectricGuitarClean
mapC Criolla         = AcousticGuitarNylon
mapC Dedo            = ElectricBassFingered
mapC Pua             = ElectricBassPicked
mapC Fretless        = FretlessBass
mapC Acustico        = AcousticBass

octavGui :: Int -> Int
octavGui 6 = 3
octavGui 5 = 3
octavGui 4 = 4
octavGui 3 = 4
octavGui 2 = 4
octavGui 1 = 5

octavBaj :: Int -> Int
octavBaj _ = 4

mapCGui :: Afinacion -> Cuerda -> Traste -> Dur -> TipoInstr -> Melodia
mapCGui af c t d tipo = let ct = tonoAInt (af !! (c-1)) + t
                            (tono, oct) = aTono ct
                        in Mod (Instr (mapC tipo)) (nota d (tono, oct + octavGui c))

mapCBaj :: Afinacion -> Cuerda -> Traste -> Dur -> TipoInstr -> Melodia
mapCBaj af c t d tipo = let ct = tonoAInt (af !! (c+1)) + t
                            (tono, oct) = aTono ct
                        in Mod (Instr (mapC tipo)) (nota d (tono, oct + octavBaj c))

sonarCuerda :: Cuerda -> Traste -> Dur -> InstrCuerda -> IO ()
sonarCuerda c t d i = play $ tocarCuerda c t d i

tocarCuerda :: Cuerda -> Traste -> Dur -> InstrCuerda -> Melodia
tocarCuerda c t d (InstrCuerda af nc ntras tipo) = 
                    case tipo of
                          Dedo     -> mapCBaj af c t d tipo 
                          Pua      -> mapCBaj af c t d tipo
                          Fretless -> mapCBaj af c t d tipo
                          Acustico -> mapCBaj af c t d tipo
                          x        -> mapCGui af c t d tipo

tocarAcorde :: [(Cuerda,Traste,Dur)] -> InstrCuerda -> IO ()
tocarAcorde xs g = play (acordeC xs g)

lineaC :: [(Cuerda,Traste,Dur)] -> InstrCuerda -> Melodia
lineaC []           g = silen 0
lineaC ((c,t,d):xs) g = tocarCuerda c t d g :+: lineaC xs g

acordeC :: [(Cuerda,Traste,Dur)] -> InstrCuerda -> Melodia
acordeC []           g = silen 0
acordeC ((c,t,d):xs) g = tocarCuerda c t d g :=: acordeC xs g

