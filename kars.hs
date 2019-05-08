module Kars where

import Text.Show.Functions -- esto permite que las funciones sean mostrables
import Data.List -- esto me da la funcion genericLength, usar esta en vez de length

-- Poner aqui la solucion:

type Nombre = String
type Velocidad = Float 
type NivelNafta = Float 
type NombreEnamorade = String
type Truco = Auto -> Auto
type TamanioTanque = Float
type CantidadVueltas = Float
type LongitudPista = Float
type Publico = [NombresPublico]
type Trampa = Carrera -> Carrera
type Participantes = [Auto]

data NombresPublico =  Ronco | Tinch | Dodain deriving(Show)

data Carrera = CrearCarrera {
    cantidadVueltas :: CantidadVueltas,
    longitudPista :: LongitudPista,
    publico :: Publico,
    trampa :: Trampa,
    participantes :: Participantes
} deriving (Show)

data Auto = UnAuto {
    nombre :: Nombre, 
    nivelNafta :: NivelNafta,
    velocidad :: Velocidad,
    nombreEnamorade :: NombreEnamorade,
    trucoFavorito :: Truco,
    tamanioTanque :: TamanioTanque
} deriving (Show)

aumentarPorNombre :: Nombre -> Velocidad -> Velocidad
aumentarPorNombre nombre velocidad = velocidad + genericLength nombre

impresionar :: Auto -> Auto
impresionar auto = auto {velocidad = (aumentarPorNombre (nombre auto) (velocidad auto)) }

nitro :: Auto -> Auto
nitro auto = auto {velocidad = velocidad auto + 15}
    
fingirAmor :: String -> Auto -> Auto
fingirAmor nombreNuevoEnamorade auto =  auto {nombreEnamorade = nombreNuevoEnamorade}



deReversa :: Auto -> Auto
deReversa auto = auto { nivelNafta = nivelNafta auto + (velocidad auto) / 5}

rochaMcQueen :: Auto
rochaMcQueen = UnAuto "rochaMcQueen" 300 0 "Ronco"  deReversa 1000
biankerr :: Auto
biankerr = UnAuto  "biankerr" 500 20 "Tinch"  impresionar 1000
gushtav :: Auto
gushtav = UnAuto "gushtav"  200  130 "PetiLaLinda"  nitro 300
rodra :: Auto
rodra = UnAuto "rodra" 0 50 "Taisa" (fingirAmor "gushtav") 300 

-- Para que un auto haga su truco favorito debemos poner en consola:
--   (trucoFavorito auto) auto
-- Por ejemplo para que rodra haga su truco favoito:
--   (trucoFavorito rodra) rodra

nombrePalindromo :: String -> Bool
nombrePalindromo nombre = nombre == reverse nombre

aumentarVelocidadSegunEnamorade :: String -> Float -> Float
aumentarVelocidadSegunEnamorade nombre velocidad | nombrePalindromo nombre = velocidad + 50
                                                 | genericLength nombre <= 2 = velocidad + 15
                                                 | genericLength nombre <= 4 = velocidad + 20
                                                 | otherwise = velocidad + 30
                                   
incrementarVelocidad :: Auto -> Auto
incrementarVelocidad auto = auto {velocidad = aumentarVelocidadSegunEnamorade (nombreEnamorade auto) (velocidad auto)}

hayNafta = (>0)
velocidadMenor100 = (<100)

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco  autoDespuesDeTruco = (hayNafta.nivelNafta) autoDespuesDeTruco && (velocidadMenor100.velocidad) autoDespuesDeTruco   

aumentaVelocidadSegunNafta :: Auto -> Auto                        
aumentaVelocidadSegunNafta auto = auto {velocidad = (nivelNafta auto) *10}

llevaNaftaA1 :: Auto -> Auto
llevaNaftaA1 auto = auto {nivelNafta = 1 }

comboLoco :: Auto -> Auto
comboLoco = deReversa . nitro

queTrucazo :: Auto -> Auto 
queTrucazo = incrementarVelocidad . (fingirAmor "ana")

turbo :: Auto -> Auto
turbo = llevaNaftaA1 . aumentaVelocidadSegunNafta      

potreroFunes :: Carrera
potreroFunes = CrearCarrera 3 5 [Ronco, Tinch, Dodain] sacarUno [rochaMcQueen, biankerr, rodra] 

sacarUno :: Carrera -> Carrera
sacarUno carrera = carrera { participantes = tail (participantes carrera)}

lluvia :: Carrera -> Carrera 
lluvia carrera = carrera  { participantes = cambiarVelocidadEnLista (-10) (participantes carrera)}

cambiarVelocidadEnLista :: Velocidad -> Participantes -> Participantes 
cambiarVelocidadEnLista  velocidadACambiar participantes = map (cambiarVelocidad velocidadACambiar) participantes

cambiarVelocidad :: Velocidad -> Auto -> Auto
cambiarVelocidad velocidadACambiar auto = auto { velocidad = velocidad auto + velocidadACambiar}





                     



