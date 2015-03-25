{-

Modulo Composicion

Defino la estructura de una composicion musical como un conjunto (lista) de notas.
Una nota es una tupla de 5 objetos, un tiempo (de comienzo), el instrumento con el que se toca,
el tono, la duracion y volumen.
Tambien damos el concepto de Melodia como un objeto musical de tonos.

En este modulo, lo que trato de hacer es transfomar una melodia a una composicion, para 
luego pasarlo al protocolo MIDI.

-}

module Notas.Composicion where

import Notas.Musica as Musica
import Notas.Tiempos as Tiempos
import Notas.Funciones as Funciones

-- Como se dijo antes, una composicion es un conjunto de notas
type Composicion = [Notas]

-- Una melodia musical es una estructura musical de tonos
type Melodia = Musica Tono

-- Notas: Tratamos de especificar mejor cada nota en particular.  
-- Para eso definimos el tipo de datos notas, el cual representa una estructura
-- que es por lo menos hasta ahora bastante aceptable.
-- Podriamos agregar mas cosas.
data Notas = Notas { 
                      nTiempo :: Tiempo, -- el tiempo de inicio, cuando empezo a tocarse la nota
                      nInstr  :: NombreInstr, -- el nombre del instrumento con el que se toca
                      nTono   :: TonoAbs, -- el tono
                      nDur    :: Tiempo, -- la duracion (en Tiempo que escencialmente es igual que Dur)
                      nVol    :: Volumen -- volumen, por ahora no agrega nada
                   }
         deriving (Show,Eq,Ord)

-- Tiempo, lo defino igual que duracion, como un racional
type Tiempo  = Rational

-- Volumen, un entero que en teoria va de 0 a 127
-- Tengo que modificar el tipo de datos y la monada para manejar un posible error
type Volumen = Int -- de 0 a 127 en MIDI

-- Entorno: expresamos con este tipo de datos un entorno para las notas.
-- Es exactamente el mismo tipo de datos solo que lo usamos para definir un posible entorno.
data Entorno = Entorno {
                         eTiempo :: Tiempo,
                         eInstr  :: NombreInstr,
                         eTono   :: TonoAbs,
                         eDur    :: Tiempo,
                         eVol    :: Volumen
                       }
         deriving Show

-- Monada State
-- Necesitamos hacer una funcion componer, que dada una melodia, la transformamos en una composicion,
-- o sea, un conjunto de notas.
-- Para ello, usamos la monada State llevando como estado el entorno que ira modificandose a medida
-- que evaluamos la funcion en cuestion.

newtype State s a = St {state :: s -> (a,s)}

runState :: State s a -> s -> (a,s)
runState (St x) = x

-- instancia Monad
instance Monad (State s) where
          return x = St (\s -> (x,s))
          St h >>= f = St (\s -> let (x,s') = h s
                                 in runState (f x) s')      

-- set, seteamos el nuevo estado
set :: s -> State s ()
set st = St (\s -> ((),st))

-- get, obtenemos el estado actual para usar su informacion
get :: State s s
get = St (\s -> (s,s))

------------------
------------------
--volumen maximo
maxvol = 127
-- entorno por default
defEnt :: Entorno
defEnt = Entorno 0 AcousticGuitarSteel 0 (tap 120 nn) maxvol

-- tap, funcion que sirve como metronomo.
-- dados un entero (velocidad del metronomo) y una duracion de nota,
-- obtenemos el tiempo de duracion de una nota en un minuto (en segundos).
-- uso fromIntegral para transformar s de la clase Integral a Num.
tap :: Int -> Dur -> Tiempo
tap s d = 60 / (fromIntegral s * d)

-- Funcion tocarNota
-- Dados un entorno, duracion y tono, construimos la nota.
tocarNota :: Entorno -> Dur -> Tono -> Composicion
tocarNota (Entorno eT eI eK eD eV) d (t,o) = 
                    let nota1 = Notas eT eI (tonoAbs (t,o) + eK) (d*eD) eV
                    in [nota1]

-- Funcion componer
-- A partir de una Melodia creamos una composicion
componer :: Melodia -> Composicion
componer m = let (a,s) = (runState (componer' m) defEnt)
             in fst a

-- Componer'
-- Toma una melodia y crea la composicion asociada, 
-- junto con el tiempo total de duracion la misma.
-- La duracion de la nota primitiva en total es, la duracion de la misma multiplicado la duracion
-- que lleva el entorno ya que es la de la melodia. 
componer' :: Melodia -> State Entorno (Composicion, Tiempo)
componer' (Prim (Silencio d)) = do ent <- get
                                   return ([],d*(eDur ent))
componer' (Prim (Nota d t))   = do ent <- get
                                   return (tocarNota ent d t,d*(eDur ent))
componer' (m1 :+: m2)         = do ent <- get
                                   (cm1,t1) <- componer' m1
                                   set (ent {eTiempo = eTiempo ent + t1})
                                   (cm2,t2) <- componer' m2
                                   return (cm1++cm2, t1 + t2)
componer' (m1 :=: m2)         = do ent <- get
                                   (cm1,t1) <- componer' m1
                                   set ent
                                   (cm2,t2) <- componer' m2
                                   return (merge cm1 cm2, max t1 t2)
componer' (Mod (Instr i) m)   = do ent <- get
                                   set (ent {eInstr = i})
                                   cm <- componer' m
                                   return cm
componer' (Mod (Tempo t) m)   = do ent <- get
                                   set (ent {eDur = (eDur ent)/t})
                                   cm <- componer' m
                                   return cm

-- merge: de dos composiciones, y las mezclamos de acuerdo a los tiempos de inicio de cada
-- nota. Si una nota arranca antes entonces esta antes en la composicion y asi sucesivamente.
merge :: Composicion -> Composicion -> Composicion
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = if nTiempo x < nTiempo y then x : merge xs (y:ys)
                                               else y : merge (x:xs) ys                


-- Banda (Esta parte todavia en construccion)
-- Una banda es un conjunto de Instrumentos con sus respectivas melodias.
type Banda = [(NombreInstr,Melodia)] 

-- joinBanda: A partir de una banda, creamos la melodia conjunta. 
joinBanda :: Banda -> Melodia
joinBanda []          = silen 0
joinBanda ((i,ms):xs) = (Mod (Instr i) ms) :=: joinBanda xs

-- ComponerBanda: dada una banda, creamos la melodia conjunta y componemos.
componerBanda :: Banda -> Composicion
componerBanda  = componer . joinBanda


