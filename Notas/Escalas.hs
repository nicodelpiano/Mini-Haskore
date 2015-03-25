module Notas.Escalas where

import Notas.Musica as Musica
import Notas.Funciones as Funciones
import Notas.Composicion as Composicion

-- Info extraida de http://bloguitar.es/teoria/escalas-teoria/
--                  http://es.wikipedia.org/wiki/Escala_musical
--                  http://desafinados.es/la-escala-menor-guitarra-para-principiantes/

type Escala = [Tono]

-- El patron 
type Patron = [TonoAbs]

crearEscala :: [TonoAbs] -> ClaseTono -> Escala
crearEscala e ct = map (aTono . ((+) (tonoAInt ct))) e


-- Un patron esta compuesto por tonos y semitonos
-- Tono    Semitono
-- (3/2)T = 3, T = 2 , st = 1
pmayor, pmenor, ppentatonica, pblues, pdorica, pfrigia,
          plidia, pmixolidia, peolica, plocria, pcromatica :: Patron
pmayor       = [2,2,1,2,2,2,1]
pmenor       = [2,1,2,2,1,2,2]
ppentatonica = [2,2,3,2,3]
pblues       = [2,2,3,1,2,3]
pdorica      = [2,1,2,2,2,1,2]
pfrigia      = [1,2,2,2,1,2,2]
plidia       = [2,2,2,1,2,2,1]
pmixolidia   = [2,2,1,2,2,1,2]
peolica      = [2,1,2,2,1,2,2]
plocria      = [1,2,2,1,2,2,2]
pcromatica   = [1,1,1,1,1,1,1,1,1,1,1,1]

escala :: Patron -> ClaseTono -> Escala
escala p = crearEscala (scanl (+) 0 p)

mayor, menor, pentatonica, blues, dorica, frigia,
          lidia, mixolidia, eolica, locria, cromatica :: ClaseTono -> Escala
mayor       = escala pmayor
menor       = escala pmenor
pentatonica = escala ppentatonica
blues       = escala pblues
dorica      = escala pdorica
frigia      = escala pfrigia
lidia       = escala plidia
mixolidia   = escala pmixolidia
eolica      = escala peolica
locria      = escala plocria
cromatica   = escala pcromatica

escAMel :: Dur -> Escala -> Melodia
escAMel d = linea . (map (nota d))
