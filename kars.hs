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
type Publico = [String]
type Trampa = Carrera -> Carrera
type Participantes = [Auto]

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
} deriving Show

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

realizaTrucoFavorito auto =  (trucoFavorito auto) auto

cambiarVelocidad :: Velocidad -> Auto -> Auto
cambiarVelocidad velocidadACambiar auto = auto { velocidad = velocidad auto + velocidadACambiar}

nombrePalindromo :: String -> Bool
nombrePalindromo nombre = nombre == reverse nombre

velocidadSegunEnamorade :: String  -> Float
velocidadSegunEnamorade nombre | nombrePalindromo nombre = 50
                               | genericLength nombre <= 2 =  15
                               | genericLength nombre <= 4 =   20
                               | otherwise =   30

incrementarVelocidadEnamorade :: Auto -> Auto
incrementarVelocidadEnamorade auto = cambiarVelocidad (velocidadSegunEnamorade (nombre auto)) auto

hayNafta = (>0)
velocidadMenor100 = (<100)

puedeRealizarTruco :: Auto -> Truco -> Bool
puedeRealizarTruco  auto truco = (hayNafta.nivelNafta) (truco auto) && (velocidadMenor100.velocidad) (truco auto)   

aumentaVelocidadSegunNafta :: Auto -> Auto                        
aumentaVelocidadSegunNafta auto = cambiarVelocidad (nivelNafta auto * 10) auto

llevaNaftaA1 :: Auto -> Auto
llevaNaftaA1 auto = auto {nivelNafta = 1 }

-- Trucos

comboLoco :: Auto -> Auto
comboLoco = deReversa . nitro

queTrucazo :: Auto -> Auto 
queTrucazo = incrementarVelocidadEnamorade . (fingirAmor "ana")

turbo :: Auto -> Auto
turbo = llevaNaftaA1 . aumentaVelocidadSegunNafta  

inutilidad :: Auto -> Auto
inutilidad auto = auto

llenarTanque :: Auto -> Auto
llenarTanque auto = auto { nivelNafta = tamanioTanque auto }

elGranTruco :: [Truco] -> Auto -> Auto
elGranTruco [] auto = auto
elGranTruco (x:xs) auto = elGranTruco xs ( x auto )    

multiNitro :: Float -> Auto -> Auto
multiNitro 0 auto = auto
multiNitro cantidad auto = multiNitro (cantidad - 1) (nitro auto)
 
-- Carreras

potreroFunes :: Carrera
potreroFunes = CrearCarrera 3 5 ["Ronco", "Tinch", "Dodain"] sacarUno [rochaMcQueen, biankerr,gushtav, rodra] 

--Trampas

sacarUno :: Carrera -> Carrera
sacarUno carrera = carrera { participantes = tail (participantes carrera)}

lluvia :: Carrera -> Carrera 
lluvia carrera = carrera  { participantes = cambiarVelocidadEnLista (-10) (participantes carrera)}

cambiarVelocidadEnLista :: Velocidad -> Participantes -> Participantes 
cambiarVelocidadEnLista  velocidadACambiar  = map (cambiarVelocidad velocidadACambiar) 

neutralizarTrucos :: Carrera -> Carrera 
neutralizarTrucos carrera = carrera { participantes = map neutralizar (participantes carrera)}

neutralizar :: Auto -> Auto
neutralizar auto = auto { trucoFavorito = inutilidad}

pocaReserva :: Carrera -> Carrera
pocaReserva carrera = carrera { participantes = tieneNaftaLista 30 (participantes carrera)}

tieneNaftaLista :: NivelNafta -> Participantes -> Participantes 
tieneNaftaLista nivelNaftaNecesario  = filter (tieneNafta nivelNaftaNecesario) 

tieneNafta :: NivelNafta -> Auto -> Bool
tieneNafta nivelNaftaNecesario  auto = nivelNafta auto >= nivelNaftaNecesario

-- Punto 4

darVuelta :: Carrera -> Carrera
darVuelta  carrera = (restar1Vuelta . (trampa carrera) . estaEnamorade . autosRestanCombustible) carrera
 
autosDanVuelta :: LongitudPista -> Participantes -> Participantes
autosDanVuelta longitud participantes = map (restarCombustible longitud) participantes

restarCombustible ::LongitudPista -> Auto -> Auto
restarCombustible longitud auto = auto {nivelNafta = max 0 (nivelNafta auto - longitud / 10 *velocidad auto)} 

autosRestanCombustible :: Carrera -> Carrera
autosRestanCombustible carrera = 
    carrera { participantes = autosDanVuelta (longitudPista carrera) (participantes carrera)}

estaEnamorade :: Carrera -> Carrera
estaEnamorade carrera =
     carrera { participantes = hacerTrucosSiEnamoradesEsta (publico carrera) (participantes carrera)}

hacerTrucosSiEnamoradesEsta :: Publico -> Participantes -> Participantes
hacerTrucosSiEnamoradesEsta enamorades  = map (cambiarSiEstaEnamorade enamorades) 

cambiarSiEstaEnamorade :: Publico -> Auto -> Auto
cambiarSiEstaEnamorade publico auto | any (==nombreEnamorade auto) publico = (trucoFavorito auto) auto
                                    | otherwise = auto

correrCarrera :: Carrera -> Carrera
correrCarrera carrera | cantidadVueltas carrera > 0 = correrCarrera (darVuelta carrera)
                      | otherwise = carrera

restar1Vuelta :: Carrera -> Carrera
restar1Vuelta carrera = carrera { cantidadVueltas = cantidadVueltas carrera - 1}

-- Punto 5

quienGana :: Carrera -> Auto
quienGana  = autoGanador . correrCarrera

autoGanador :: Carrera -> Auto
autoGanador carrera = participanteConMasVelocidad (participantes carrera)

participanteConMasVelocidad :: Participantes -> Auto
participanteConMasVelocidad (x:xs) = foldl autoMasRapido x xs

autoMasRapido :: Auto -> Auto -> Auto 
autoMasRapido auto1 auto2 | velocidad auto1 > velocidad auto2 = auto1
                          | otherwise = auto2


 {- Punto 6 no se puede porque 
 estariamos comparando infinitamente -}


 
-- Algunas funciones para testeo de Casos A Prueba
autoParticipa :: Auto -> Carrera -> Bool                          
autoParticipa auto carrera = any (autoEsta auto) (participantes carrera)
autoEsta :: Auto -> Auto -> Bool
autoEsta auto1 auto2 = nombre auto1 == nombre auto2
soloRodra participantes = length participantes == 1 && nombre ( participantes !! 0 ) == "rodra"





                    

                    





                     



