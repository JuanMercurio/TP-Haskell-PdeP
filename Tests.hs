import Kars
import Test.Hspec

runTests = hspec $ do
  describe "Al realizar su truco favorito:" $ do
    it "la nafta de RochaMcQueen es correcta" $ do
      (nivelNafta . (trucoFavorito rochaMcQueen)) rochaMcQueen  `shouldBe` 500
    it "la velocidad de biankerr es correcta" $ do
      (velocidad . (trucoFavorito biankerr)) biankerr `shouldBe` 28
    it "la velocidad de gushtav es correcta" $ do
      (velocidad . (trucoFavorito gushtav)) gushtav `shouldBe` 145
    it "La enamorada de rodra es correcta" $ do 
      (nombreEnamorade . (trucoFavorito gushtav)) gushtav `shouldBe` "gushtav"

  describe "Incrementos de velocidad" $ do
    it "RochaMcQueen incrementa su velocidad correctamente" $ do
      (velocidad . incrementarVelocidad) rochaMcQueen `shouldBe` 30
    it "Biankerr incrementa su velocidad correctamente" $ do
      (velocidad . incrementarVelocidad) biankerr `shouldBe` 50
    it "Gushtav incrementa su velocidad correctamente" $ do
      (velocidad . incrementarVelocidad) gushtav `shouldBe` 160
    it "Rodra incrementa su velocidad correctamente" $ do
      (velocidad . incrementarVelocidad) rodra `shouldBe` 80

  describe "Otros trucos" $ do
    it "comboLoco modifica nafta correctamente" $ do
      (nivelNafta . comboLoco) rochaMcQueen `shouldBe` 500
    it "comboLoco modifica velocidad correctamente" $ do
      (velocidad . comboLoco) rochaMcQueen `shouldBe` 15
    it "queTrucazo modifica velocidad correctamente" $ do
      (velocidad . queTrucazo) rodra `shouldBe` 100
    it "turbo modifica velocidad correctamente en Gushtav" $ do
      (velocidad . turbo) gushtav `shouldBe` 2130
    it "turbo modifica nafta correctamente en Gushtav" $ do
      (nivelNafta . turbo) gushtav `shouldBe` 1
    it "turbo modifica velocidad correctamente en Rodra" $ do
      (velocidad . turbo) rodra `shouldBe` 50
    it "turbo modifica nafta correctamente en Rodra" $ do
      (nivelNafta . turbo) rodra `shouldBe` 1