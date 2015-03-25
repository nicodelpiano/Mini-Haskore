module Instrumentos.Guitarra where

import Notas.Musica as Musica
import Notas.Tiempos as Tiempos
import Notas.Funciones as Funciones
import Notas.Composicion as Composicion

import MIDI.ToMidi as ToMidi

import Instrumentos.InstrCuerda as InstrCuerda

type Guitarra = InstrCuerda 

-- Guitarra default
defGuitar :: Guitarra
defGuitar = InstrCuerda af440 6 24 Electrica

-- Definiciones utiles

-- de tablatura a melodia
tabtac :: Tablatura -> Melodia
tabtac t = acordeC t defGuitar

-- Acordes 5ta
do5,re5,mi5,fa5,sol5,la5,si5 :: Dur -> Tablatura
dob5  d  = [(6,7,d),(5,9,d),(4,9,d)]
do5   d  = [(6,8,d),(5,10,d),(4,10,d)]
dos5  d  = [(6,9,d),(5,11,d),(4,11,d)]
reb5  d  = [(6,9,d),(5,11,d),(4,11,d)]
re5   d  = [(6,10,d),(5,12,d),(4,12,d)]
res5  d  = [(6,11,d),(5,13,d),(4,13,d)]
mib5  d  = [(6,11,d),(5,13,d),(4,13,d)]
mi5   d  = [(6,0,d),(5,2,d),(4,2,d)]
mis5  d  = [(6,1,d),(5,3,d),(4,3,d)]
fab5  d  = [(6,0,d),(5,2,d),(4,2,d)]
fa5   d  = [(6,1,d),(5,3,d),(4,3,d)]
fas5  d  = [(6,2,d),(5,4,d),(4,4,d)]
solb5 d  = [(6,2,d),(5,4,d),(4,4,d)]
sol5  d  = [(6,3,d),(5,5,d),(4,5,d)]
sols5 d  = [(6,4,d),(5,6,d),(4,6,d)]
lab5  d  = [(6,4,d),(5,6,d),(4,6,d)]
la5   d  = [(6,5,d),(5,7,d),(4,7,d)]
las5  d  = [(6,6,d),(5,8,d),(4,8,d)]
sib5  d  = [(6,6,d),(5,8,d),(4,8,d)]
si5   d  = [(6,7,d),(5,9,d),(4,9,d)]
sis5  d  = [(6,8,d),(5,10,d),(4,10,d)]

-- Acordes Mayores

-- funcion aux
may ::  (Dur -> Tablatura) -> Dur -> Tablatura
may t d = let ys       = (t d)
              (_,tr,_) = head ys
          in (1,tr,d):((2,tr,d):((3,tr+1,d):ys))

doM,reM,miM,faM,solM,laM,siM :: Dur -> Tablatura
doM = may do5
reM = may re5
miM = may mi5
faM = may fa5
solM = may sol5
laM = may la5
siM = may si5

-- Acordes Menores

-- funcion aux
men ::  (Dur -> Tablatura) -> Dur -> Tablatura
men t d = let ys       = (t d)
              (_,tr,_) = head ys
          in (1,tr,d):((2,tr,d):((3,tr,d):ys))

dom,rem,mim,fam,solm,lam,sim :: Dur -> Tablatura
dom = men do5
rem = men re5
mim = men mi5
fam = men fa5
solm = men sol5
lam = men la5
sim = men si5

-- Solo

solo :: Tablatura -> Melodia
solo = foldr (\(c,t,d) xs -> tocarCuerda c t d defGuitar :+: xs) (silen 0)
