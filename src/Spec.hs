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
        testsPunto3
        testsPunto4
        testsPunto5

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

barcoConOroYTripulantes :: [Tripulante] -> Number -> Barco
barcoConOroYTripulantes unosTripulantes cantidadOro =
    Barco unosTripulantes Bergatin cantidadOro 0 0

barcoConTripulantes :: [Tripulante] -> Barco
barcoConTripulantes unosTripulantes =
    Barco unosTripulantes Bergatin 0 0 0

barcoDeTipo :: TipoDeBarco -> Barco
barcoDeTipo unTipo =
    Barco [] unTipo 0 0 0

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
                let barcoLleno = llenar (Barco [Tripulante 10, Tripulante 0] Balandro 0 0 0)
                balas barcoLleno `shouldBe` 6
                oro barcoLleno `shouldBe` 350
        describe "m2 del barco" $ do
            it "si es un galeon es 150" $ do
                metrosCuadrados (barcoDeTipo Galeon) `shouldBe` 150
            it "si es un bergatin es 100" $ do
                metrosCuadrados (barcoDeTipo Bergatin) `shouldBe` 100
            it "si es un balandro es 50" $ do
                metrosCuadrados (barcoDeTipo Balandro) `shouldBe` 50

testsPunto3 = do
    describe "enfrentamientos" $ do
        it "si un barco pierde contra un enemigo, se queda sin recursos" $ do
            let barcoDerrotado = enfrentarseContra (Barco [Tripulante 1] Galeon 10 3 7) (Barco [Tripulante 1] Bergatin 0 5 0)
            oro barcoDerrotado `shouldBe` 0
            balas barcoDerrotado `shouldBe` 0
            madera barcoDerrotado `shouldBe` 0
        it "si un barco gana contra un enemigo, se lleva los recursos del otro barco" $ do
            let barcoGanador = enfrentarseContra (Barco [Tripulante 1] Bergatin 2 5 1) (Barco [Tripulante 1] Galeon 10 3 7)
            oro barcoGanador `shouldBe` 12
            balas barcoGanador `shouldBe` 8
            madera barcoGanador `shouldBe` 8
        describe "ganaContra" $ do
            describe "si es mas grande que su enemigo" $ do
                let barcoGrandeConBalas cantidadBalas = Barco [] Galeon 0 cantidadBalas 0
                    barcoChicoConBalas cantidadBalas = Barco [] Balandro 0 cantidadBalas 0
                it "pierde si tiene menos balas" $ do
                    ganaContra (barcoGrandeConBalas 3)
                               (barcoChicoConBalas 5) `shouldBe` False
                it "gana si tiene igual cantidad de balas" $ do
                    ganaContra (barcoGrandeConBalas 5)
                               (barcoChicoConBalas 5) `shouldBe` True
                it "gana si tiene mas balas" $ do
                    ganaContra (barcoGrandeConBalas 7)
                               (barcoChicoConBalas 5) `shouldBe` True
            describe "si tienen el mismo tamaño" $ do
                let barcoMedianoConMadera cantidadMadera = Barco [] Bergatin 0 0 cantidadMadera
                it "gana si tiene mas madera" $ do
                    ganaContra (barcoMedianoConMadera 6)
                               (barcoMedianoConMadera 5) `shouldBe` True
                it "gana si tiene igual madera" $ do
                    ganaContra (barcoMedianoConMadera 5)
                               (barcoMedianoConMadera 5) `shouldBe` True
                it "pierde si tiene menos madera" $ do
                    ganaContra (barcoMedianoConMadera 5)
                               (barcoMedianoConMadera 6) `shouldBe` False
            describe "si es mas chico que su enemigo" $ do
                let barcoChicoConTripulantesVivos cantidadTripulantes = Barco (replicate cantidadTripulantes tripulanteVivo) Balandro 0 0 0
                    barcoGrandeConTripulantesVivos cantidadTripulantes = Barco (replicate cantidadTripulantes tripulanteVivo) Galeon 0 0 0
                    tripulanteVivo = Tripulante 1
                it "le gana si tiene igual tripulantes con vida" $ do
                    ganaContra (barcoChicoConTripulantesVivos 3)
                                (barcoGrandeConTripulantesVivos 3) `shouldBe` True
                it "le gana si tiene mas tripulantes con vida" $ do
                    ganaContra (barcoChicoConTripulantesVivos 4)
                                (barcoGrandeConTripulantesVivos 3) `shouldBe` True
                it "pierde si tiene menos tripulantes con vida" $ do
                    ganaContra (barcoChicoConTripulantesVivos 2)
                                (barcoGrandeConTripulantesVivos 3) `shouldBe` False

testsPunto4 = do
    describe "Sucesos" $ do
        describe "embarcar un tesoro" $ do
            it "cada tripulante con vida carga una parte equivalente del peso en oro del tesoro, y se agrega al oro del barco el oro del tesoro" $ do
                let barco = barcoConOroYTripulantes [Tripulante 10, Tripulante 6, Tripulante 0] 5
                    barcoTrasSuceso = embarcarUnTesoro 10 barco
                oro barcoTrasSuceso `shouldBe` 15
                tripulantes barcoTrasSuceso `shouldBe` [Tripulante 5, Tripulante 1, Tripulante 0]
        describe "encontrar cargamento de grogs" $ do
            it "cada tripulante se toma 5 grogs" $ do
                let barcoTrasSuceso = encontrarCargamentoGrog (barcoConTripulantes [Tripulante 0, Tripulante 10, Tripulante 215])
                tripulantes barcoTrasSuceso `shouldBe` [Tripulante 100, Tripulante 110, Tripulante 315]
        describe "enfrentar ejercito de esqueletos" $ do
            it "el primer tripulante con vida del barco se enfrenta a todos los esqueletos uno tras otro" $ do
                let barco = barcoConTripulantes [Tripulante 0, Tripulante 50]
                    barcoTrasSuceso = enfrentarEjercitoDeEsqueletos 2 barco
                tripulantes barcoTrasSuceso `shouldBe` [Tripulante 20, Tripulante 0]
        describe "pasar por tienda de grog" $ do
            it "si no tiene oro no pasa nada" $ do
                let barco = barcoConOroYTripulantes [Tripulante 0, Tripulante 10] 0
                pasarPorTiendaDeGrog barco `shouldBe` barco
            it "si toda la tripulacion esta viva no pasa nada" $ do
                let barco = barcoConOroYTripulantes [Tripulante 10] 10
                pasarPorTiendaDeGrog barco `shouldBe` barco
            it "si tiene algun tripulante muerto y tiene oro, le da de beber grog a su primer tripulante muerto y paga el costo de la bebida (30 oro)" $ do
                let barco = barcoConOroYTripulantes [Tripulante 0, Tripulante 10] 40
                    barcoTrasSuceso = pasarPorTiendaDeGrog barco
                oro barcoTrasSuceso `shouldBe` 10
                tripulantes barcoTrasSuceso `shouldBe` [Tripulante 20, Tripulante 10]
        it "un barco fantasma no le puede ocurrir un suceso" $ do
            let barco = barcoConOroYTripulantes [Tripulante 0] 100
            suceder barco pasarPorTiendaDeGrog `shouldBe` barco

testsPunto5 = do
    describe "Travesias" $ do
        it "si un barco supera la travesia (pasa los sucesos de la misma uno tras otro) sin convertirse en un barco fantasma, cobra el oro de la misma" $ do
            let barco = barcoConOroYTripulantes [Tripulante 0, Tripulante 2000] 30
                barcoTrasTravesia = realizarTravesia fuerteDeLosCondenados barco
            tripulantes barcoTrasTravesia `shouldBe` [Tripulante 5, Tripulante 985]
            oro barcoTrasTravesia `shouldBe` 80
        it "si un barco queda fantasma tras realizar una travesia, no cobra la recompensa" $ do
            let barco = barcoConOroYTripulantes [Tripulante 500] 25
                barcoTrasTravesia = realizarTravesia fuerteDeLosCondenados barco
            tripulantes barcoTrasTravesia `shouldBe` [Tripulante 0]
            oro barcoTrasTravesia `shouldBe` 25