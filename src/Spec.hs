module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "Simulacro de Parcial" $ do
        testsPunto1
        testsPunto2

testsPunto1 = do
        describe "actividades diarias" $ do
            describe "enfrentar a un esqueleto" $ do
                it "si el tripulante no esta en las ultimas, pierde 10 de energia" $ do
                    enfrentarEsqueleto (Tripulante 100) `shouldBe` Tripulante 90
                it "si el tripulante esta en las ultimas, pierde 20 de energia" $ do
                    enfrentarEsqueleto (Tripulante 40) `shouldBe` Tripulante 20
            describe "transportar una carga" $ do
                it "disminuye la energia del tripulante en el peso de la carga a transportar" $ do
                    transportarCarga 15 (Tripulante 35) `shouldBe` Tripulante 20
            describe "beberGrog" $ do
                it "aumenta la energia del tripulante en 20" $ do
                    beberGrog (Tripulante 10) `shouldBe` Tripulante 30
        describe "energia del tripulante" $ do
            it "no puede bajar a menos de 0" $ do
                enfrentarEsqueleto (Tripulante 1) `shouldBe` Tripulante 0
            it "un tripulante esta muerto si su energia es 0" $ do
                estaMuerto (Tripulante 0) `shouldBe` True
            it "un tripulante no esta muerto si su energia es mayor a 0" $ do
                estaMuerto (Tripulante 1) `shouldBe` False

barcoConTripulantes :: [Tripulante] -> Barco
barcoConTripulantes unosTripulantes =
    Barco unosTripulantes Bergatin 0 0

barcoDeTipo :: TipoDeBarco -> Barco
barcoDeTipo unTipo =
    Barco [] unTipo 0 0

testsPunto2 = do
    describe "barcos" $ do
        describe "barco fantasma" $ do
            it "un barco es barco fantasma si no tiene tripulacion" $ do
                esBarcoFantasma (barcoConTripulantes []) `shouldBe` True
            it "un barco NO es barco fantasma si tiene algun tripulante con vida" $ do
                esBarcoFantasma (barcoConTripulantes [Tripulante 10]) `shouldBe` False
            it "un barco es barco fantasma si toda su tripulacion murio" $ do
                esBarcoFantasma (barcoConTripulantes [Tripulante 0, Tripulante 0]) `shouldBe` True
        describe "llenar un barco" $ do
            it "al llenarlo, queda con 7kg de oro por m2 y 3 balas x tripulante" $ do
                let barcoLleno = llenar (Barco [Tripulante 10, Tripulante 0] Balandro 0 0)
                balas barcoLleno `shouldBe` 6
                oro barcoLleno `shouldBe` 350
        describe "m2 del barco" $ do
            it "si es un galeon es 150" $ do
                metrosCuadrados (barcoDeTipo Galeon) `shouldBe` 150
            it "si es un bergatin es 100" $ do
                metrosCuadrados (barcoDeTipo Bergatin) `shouldBe` 100
            it "si es un balandro es 50" $ do
                metrosCuadrados (barcoDeTipo Balandro) `shouldBe` 50
