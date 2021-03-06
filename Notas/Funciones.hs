{-

Modulo Funciones

El objetivo de este modulo es agregar algunas funciones
importantes que nos sirvan para mejorar y facilitar algunas cuestiones.

-}

module Notas.Funciones where

import Notas.Musica as Musica
import Notas.Tiempos as Tiempos

-- data Musica a = Primitiva a
--               | Musica a :+: Musica a
--               | Musica a :=: Musica a
--               | Mod Cambiar (Musica a)

-- Musica es un Functor
-- Nota: Hay que hacer la prueba
instance Functor Musica where
           --fmap :: (a -> b) -> Musica a -> Musica b
           fmap f (m1 :+: m2)         = fmap f m1 :+: fmap f m2
           fmap f (m1 :=: m2)         = fmap f m1 :=: fmap f m2
           fmap f (Mod (Instr i) m)   = Mod (Instr i) (fmap f m)
           fmap f (Mod (Tempo t) m)   = Mod (Tempo t) (fmap f m)
           fmap f (Prim n)            = Prim (fmap f n)

instance Functor Primitiva where
           fmap f (Silencio d) = Silencio d
           fmap f (Nota d t)   = Nota d (f t)

-- Defino substitucion para el tipo de datos Musica
(>>:) :: Musica a -> (a -> Musica b) -> Musica b
(Prim (Silencio d)) >>: v = silen d
(Prim (Nota d t))   >>: v = v t
(m1 :+: m2)         >>: v = (m1 >>: v) :+: (m2 >>: v)
(m1 :=: m2)         >>: v = (m1 >>: v) :=: (m2 >>: v)
(Mod (Tempo t) m)   >>: v = (Mod (Tempo t)) (m >>: v)
(Mod (Instr i) m)   >>: v = (Mod (Instr i)) (m >>: v)

-- Musica es una Monada.
-- Nota: Hay que hacer la prueba
instance Monad Musica where
           return = (\x -> silen 0)
           (>>=)  = (>>:)

-- foldM: fold para monadas.
-- Nota: practica 5 de Monadas
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM op e []     = return e
foldM op e (x:xs) = do e' <- op e x
                       foldM op e' xs

-- linea: de una lista de piezas musicales, las concatena en serie
-- acorde: de una lista de piezas musicales, las concatena en paralelo
linea, acorde :: [Musica a] -> Musica a
linea   = foldr (:+:) (silen 0)
acorde  = foldr (:=:) (silen 0)

linea1, acorde1 :: [Musica a] -> Musica a
linea1  = foldr1 (:+:)
acorde1 = foldr1 (:=:)

-- delay: podemos crear un efecto de delay (retraso), que seria como la misma melodia
-- pero esperando un tiempo. Es un efecto interesante si se aplica junto al sonido original.
delay :: Dur -> Musica a -> Musica a
delay d m = silen d :+: m

-- loop: damos un entero n, y le decimos que repita n veces la melodia dada.
loop :: Int -> Musica a -> Musica a
loop 0 m = silen 0
loop n m = m :+: loop (n-1) m

-- defini loopInfinito, pero no creo que tenga un uso frecuente 
-- ya que al no terminar nunca, no podemos escucharla.
loopInfinito :: Musica a -> Musica a
loopInfinito m = m :+: loopInfinito m

-- reversa: funcion que invierte una composicion musical.
reversa :: Musica a -> Musica a
reversa (Mod (Instr i) m) = Mod (Instr i) (reversa m)
reversa (Mod (Tempo t) m) = Mod (Tempo t) (reversa m)
reversa (m1 :+: m2)       = reversa m2 :+: reversa m1
reversa (m1 :=: m2)       = reversa m2 :=: reversa m1
reversa m                 = m

-- cambiarOct: cambiamos la octava de todas las notas de una pieza musical.
-- notar el uso de fmap.
cambiarOct :: Octava -> Musica Tono -> Musica Tono
cambiarOct o = fmap (\(t,o') -> (t,o'+o)) 

-- octavador: este efecto logra sonidos interesantes ya que suena la melodia y la misma
-- melodia pero en otra octava. 
octavador :: Octava -> Musica Tono -> Musica Tono
octavador o m = m :=: (cambiarOct o m)

-- cambiarNotas: cambiamos las hojas del AST por otro tipo de musica.
cambiarNotas :: Musica b -> Musica a -> Musica b
cambiarNotas n m = m >>= (\t -> n)

-- silenciar: cambiamos todas las hojas del AST por silencio.
silenciar :: Musica a -> Musica a
silenciar = cambiarNotas (silen 0)

-- cambiar instrumentos
switchIns :: NombreInstr -> Musica a -> Musica a
switchIns ins (Mod (Instr i) m) = Mod (Instr ins) (switchIns ins m)
switchIns ins (Mod (Tempo t) m) = Mod (Tempo t) (switchIns ins m)
switchIns ins (m1 :+: m2)       = switchIns ins m1 :+: switchIns ins m2
switchIns ins (m1 :=: m2)       = switchIns ins m1 :=: switchIns ins m2
switchIns ins prim              = prim

-- completar con mas funciones

