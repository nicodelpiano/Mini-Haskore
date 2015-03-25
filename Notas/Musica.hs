{-
 
Modulo Musica

En este modulo se encuentran los tipos de datos
mas importantes del DSL y algunas funciones que son de gran ayuda.

La principal idea es manejar la musica con dos operadores :+: y :=:,
componiendo objetos musicales de forma secuencial y paralela respectivamente.
Cada composicion u objeto musical esta compuesto por notas o silencios, los operadores anteriormente
mencionados y modificadores de estado.
Podemos ver cada pieza musical como un AST:
             :+:
            /   \
           /     \
          :=:    Prim Nota 2 (Si,4) 
         /    \
        /      \
       /        \
      /          \
     /            \
Prim Nota 1 (La,4) Prim Nota 1 (La,5) 
-}

module Notas.Musica where

-- Precedencia de los operadores con asociatividad a derecha
-- :+: computa dos musicas en serie
-- :=: computa dos musicas en paralelo
infixr 5 :+:, :=:

-- Algunos conceptos y sinonimos (tipos) importantes

-- Octava: una octava es el intervalo que separa dos sonidos.
-- Lo representamos como un entero.
-- En forma directa, una octava mayor tiene un sonido mas agudo, por ejemplo
-- un La en 4 octava tiene un sonido mas grave que un La en una 5 octava.
-- http://es.wikipedia.org/wiki/Octava
type Octava = Int

-- Tono: En la teoria musical, un tono esta definido como un par
-- de un Tono y una octava
type Tono = (ClaseTono,Octava)

-- Dur: Duracion de una nota o silencio.
-- Lo representamos con un Racional, porque podemos representar a los numeros de la forma
-- k = n/d (generalmente las duraciones son de esta forma). Los tiempos irracionales no se utilizan mucho.
type Dur = Rational

-- ClaseTono : *
-- Representan las notas musicales por sus nombres.
-- Algoritmicamente nos van a interesar que esten numeradas, pero asi las vemos de una forma
-- mas elegante. 
data ClaseTono = 
              Dob | Do | DoS | Reb | Re | ReS 
            | Mib | Mi | MiS | Fab | Fa | FaS 
            |Solb | Sol | SolS | Lab | La | LaS
            | Sib | Si | SiS
                        deriving (Show,Eq,Ord)

-- Primitiva :: * -> *
-- Este tipo de datos se usa para construir una nota.
-- Una nota tiene una duracion y un tipo (a, puede ser un Tono, etc).
-- Tambien puede ser un silencio, que solo tiene duracion.
data Primitiva a = Nota Dur a | Silencio Dur
			deriving Show

-- Musica : * -> *
-- Este tipo de datos es fundamental en el DSL.
-- Representa la forma que queremos que tenga nuestra musica.
-- De forma muy simple, podemos componer musica en forma paralela y en serie.
data Musica a = Prim (Primitiva a)     -- primitiva
              | Musica a :=: Musica a  -- paralela
              | Musica a :+: Musica a  -- serie
              | Mod Cambiar (Musica a) -- cambio
                        deriving Show

-- Este tipo de datos esta hecho para representar un cambio o una caracteristica en la musica.
-- Por ejemplo, podemos especificar que queremos tocar con una guitarra la nota La en la 5 octava 
-- de la siguiente forma: Mod (Instr DistortionGuitar) (Prim (Nota 1 (La,5)))
data Cambiar = Instr NombreInstr | Tempo Dur
         deriving Show

-- NombreInstr : *
-- Simplemente especifica los nombres de los instrumentos que usa MIDI.
data NombreInstr =
     AcousticGrandPiano     | BrightAcousticPiano    | ElectricGrandPiano
  |  HonkyTonkPiano         | RhodesPiano            | ChorusedPiano
  |  Harpsichord            | Clavinet               | Celesta 
  |  Glockenspiel           | MusicBox               | Vibraphone  
  |  Marimba                | Xylophone              | TubularBells
  |  Dulcimer               | HammondOrgan           | PercussiveOrgan 
  |  RockOrgan              | ChurchOrgan            | ReedOrgan
  |  Accordion              | Harmonica              | TangoAccordion
  |  AcousticGuitarNylon    | AcousticGuitarSteel    | ElectricGuitarJazz
  |  ElectricGuitarClean    | ElectricGuitarMuted    | OverdrivenGuitar
  |  DistortionGuitar       | GuitarHarmonics        | AcousticBass
  |  ElectricBassFingered   | ElectricBassPicked     | FretlessBass
  |  SlapBass1              | SlapBass2              | SynthBass1   
  |  SynthBass2             | Violin                 | Viola  
  |  Cello                  | Contrabass             | TremoloStrings
  |  PizzicatoStrings       | OrchestralHarp         | Timpani
  |  StringEnsemble1        | StringEnsemble2        | SynthStrings1
  |  SynthStrings2          | ChoirAahs              | VoiceOohs
  |  SynthVoice             | OrchestraHit           | Trumpet
  |  Trombone               | Tuba                   | MutedTrumpet
  |  FrenchHorn             | BrassSection           | SynthBrass1
  |  SynthBrass2            | SopranoSax             | AltoSax 
  |  TenorSax               | BaritoneSax            | Oboe  
  |  Bassoon                | EnglishHorn            | Clarinet
  |  Piccolo                | Flute                  | Recorder
  |  PanFlute               | BlownBottle            | Shakuhachi
  |  Whistle                | Ocarina                | Lead1Square
  |  Lead2Sawtooth          | Lead3Calliope          | Lead4Chiff
  |  Lead5Charang           | Lead6Voice             | Lead7Fifths
  |  Lead8BassLead          | Pad1NewAge             | Pad2Warm
  |  Pad3Polysynth          | Pad4Choir              | Pad5Bowed
  |  Pad6Metallic           | Pad7Halo               | Pad8Sweep
  |  FX1Train               | FX2Soundtrack          | FX3Crystal
  |  FX4Atmosphere          | FX5Brightness          | FX6Goblins
  |  FX7Echoes              | FX8SciFi               | Sitar
  |  Banjo                  | Shamisen               | Koto
  |  Kalimba                | Bagpipe                | Fiddle 
  |  Shanai                 | TinkleBell             | Agogo  
  |  SteelDrums             | Woodblock              | TaikoDrum
  |  MelodicDrum            | SynthDrum              | ReverseCymbal
  |  GuitarFretNoise        | BreathNoise            | Seashore
  |  BirdTweet              | TelephoneRing          | Helicopter
  |  Applause               | Gunshot                | Percussion
  |  Custom String

  deriving (Show, Eq, Ord)


-- Algunas funciones importantes

-- para crear una nota, damos una duracion, un elemento de tipo a
-- y creamos una primitiva
nota :: Dur -> a -> Musica a
nota d p = Prim (Nota d p)

-- silen, crea un silencio a partir de una duracion
silen :: Dur -> Musica a
silen d = Prim (Silencio d)

-- tiempo, damos una duracion y modificamos el Tempo
tiempo :: Dur -> Musica a -> Musica a
tiempo d m = Mod (Tempo d) m

-- cambiamos el instrumento de la musica
instrumento :: NombreInstr -> Musica a -> Musica a
instrumento i m  = Mod (Instr i) m

-- usando la funcion nota, creamos una funcion para cada nombre de nota
-- dando una octava y su duracion. 
-- Nota: a causa de la privatizacion de la palabra "do" :P, esta nota se crea con doo.
dob,doo,dos,reb,re,res,mib,mi,mis,fab,fa,fas,solb,sol,sols,
	lab,la,las,sib,si,sis :: Octava -> Dur -> Musica Tono

dob  o d = nota d (Dob,  o)  
doo  o d = nota d (Do,   o)
dos  o d = nota d (DoS,  o)  
reb  o d = nota d (Reb,  o)
re   o d = nota d (Re,   o)  
res  o d = nota d (ReS,  o)
mib  o d = nota d (Mib,  o)  
mi   o d = nota d (Mi,   o)
mis  o d = nota d (MiS,  o)  
fab  o d = nota d (Fab,  o)
fa   o d = nota d (Fa,   o)  
fas  o d = nota d (FaS,  o)
solb o d = nota d (Solb, o)  
sol  o d = nota d (Sol,  o)
sols o d = nota d (SolS, o)  
lab  o d = nota d (Lab,  o)
la   o d = nota d (La,   o) 
las  o d = nota d (LaS,  o)
sib  o d = nota d (Sib,  o)  
si   o d = nota d (Si,   o)
sis  o d = nota d (SiS,  o)

-- Manejo de los tonos como enteros
-- Para trabajar mejor algoritmicamente, podemos representar a los tonos como
-- enteros (tono absoluto).
type TonoAbs = Int

-- Para pasar de un tono absoluto a un tono
-- usamos el metodo de modulo 12, que consiste en usar el cociente
-- y el resto de la division del tono absoluto por 12.
-- Como el sistema de enteros tiene 12 notas, podemos indexar el cociente que nos va a dar un
-- numero entre 0 y 11 y la octava el resto.
aTono     :: TonoAbs -> Tono
aTono ta  = 
    let (o, n) = divMod ta 12
    in  ([Do,DoS,Re,ReS,Mi,Fa,FaS,Sol,SolS,La,LaS,Si] !! n, o)

-- En forma contraria, para obtener el tono absoluto a partir del tono
-- lo que hacemos es multiplicar la octava por 12 y sumar el numero respectivo del tono.
-- Con esto obtenemos el dividendo que es lo que buscamos.
tonoAbs           :: Tono -> TonoAbs
tonoAbs (t,o)  = 12*o + tonoAInt t

-- Esta funcion simplemente asigna un numero a cada nota musical.
-- Empezando del Do natural (0) hasta el Si Natural (11).
-- Los bemoles y sostenidos pueden repetirse ya que sabemos por ejemplo, que
-- Mi sostenido coincide con Fa, Re bemol es Do sostenido, etc.
tonoAInt     :: ClaseTono -> Int
tonoAInt ct  = case ct of
  Dob  -> -1;  Do  -> 0;   DoS  -> 1;
  Reb -> 1;    Re  -> 2;   ReS  -> 3;
  Mib  -> 3;   Mi  -> 4;   MiS -> 5;
  Fab  -> 4;   Fa  -> 5;   FaS  -> 6; 
  Solb  -> 6;  Sol  -> 7;  SolS  -> 8;
  Lab  -> 8;   La  -> 9;   LaS -> 10;
  Sib  -> 10;  Si  -> 11;  SiS  -> 12;

-- funcion inversa de tonoAInt
intAct :: Int -> ClaseTono
intAct i = case i of
               (-1)  -> Dob;
                0  -> Do;
                1  -> DoS;
                2  -> Re;
                3  -> ReS;
                4  -> Mi;
                5  -> Fa;
                6  -> FaS;
                7  -> Sol;
                8  -> SolS;
                9  -> La;
                10 -> LaS;
                11 -> Si;
                12 -> SiS 
-- trans (transpose), funcion encargada de correr i lugares un tono
trans      :: Int -> Tono -> Tono
trans i p  = aTono (tonoAbs p + i)


