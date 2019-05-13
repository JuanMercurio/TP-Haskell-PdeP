import Kars
import Test.Hspec



probar = hspec $ do 
    describe "Punto 1" $ do 
        it "Nafta RochaMcQueen despues de trucoFavorito es correcta" $ do
           (nivelNafta . (trucoFavorito rochaMcQueen)) rochaMcQueen  `shouldBe` 300
        it  "Nafta RochaMcQueen despues de comboLoco es correcta" $ do
           (nivelNafta . comboLoco ) rochaMcQueen `shouldBe` 303
    describe "Punto 2" $ do
        it "Cantidad de participantes luego de sacarUno en potreroFunes es correcta" $ do 
            (length . participantes . sacarUno) potreroFunes `shouldBe` 3
        it "RochaMcQueen no participa en potreroFunes tras sacarUno es correcto" $ do
             (not . (autoParticipa rochaMcQueen) ) (sacarUno potreroFunes) `shouldBe` True --segun enunciado deberia dar false?   
        it "Cantidad de participantes luego de pocaReserva en potreroFunes es correcta" $ do
            (length . participantes . pocaReserva) potreroFunes `shouldBe` 3
        it "Rodra no participa mas en potreroFunes despues de pocaReseva es correcto" $ do
            (not . (autoParticipa rodra)) (pocaReserva potreroFunes) `shouldBe` True --segun enunciado deberia dar false?
    describe "Punto 3" $ do     
        it "Nafta de rodra tras llenarTanque es correcta" $ do 
            (nivelNafta . llenarTanque) rodra `shouldBe` 300
        it "Velocidad de rodra despues de elGranTruco con nitro, deReversa e impresionar es correcta" $ do
            (velocidad . (elGranTruco [nitro, deReversa, impresionar])) rodra `shouldBe` 70
        it "Nafta de rodra despues de elGranTruco con nitro, deReversa e impresionar es correcta " $ do 
            (nivelNafta . (elGranTruco [nitro, deReversa, impresionar])) rodra `shouldBe` 13
        it "Velocidad de rodra despues de realizar multiNitro 5 veces es correcta" $ do
            (velocidad . (multiNitro 5)) rodra `shouldBe` 125 
    describe "Punto 4" $ do
        it "Nafta del primer participante luego de dar una vuelta en potreroFunes es correcta" $ do
            (nivelNafta . (\participantes -> participantes !! 0) . participantes . darVuelta ) potreroFunes `shouldBe` 490
        it "Velocidad del primer participante luego de dar una vuelta en potreroFunes es correcta" $ do 
            (velocidad . ( !! 0) . participantes . darVuelta ) potreroFunes `shouldBe` 28
        it "Cantidad de participantes luego de dar 2 vueltas en potrerofunes es correcta" $ do 
            (length . participantes . darVuelta. darVuelta) potreroFunes `shouldBe` 2
        it "Nafta del primer participante en potreroFunes despues de 2 vueltas es correcta" $ do 
            (nivelNafta . ( !! 0 ) . participantes . darVuelta . darVuelta ) potreroFunes `shouldBe` 70
        it "El unico participantes luego de correr potreroFunes es Rodra" $ do 
            (soloRodra . participantes . correrCarrera ) potreroFunes `shouldBe` True 
        it "El ganador de correr la carrera potreroFunes es correcto" $ do 
            (nombre . quienGana) potreroFunes `shouldBe` "rodra" 
            

  
    



