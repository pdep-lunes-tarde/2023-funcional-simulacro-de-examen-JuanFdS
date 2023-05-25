module Library where
import PdePreludat

data Tripulante = Tripulante Number deriving (Eq, Show)

type ActividadDiaria = Tripulante -> Tripulante

energia :: Tripulante -> Number
energia (Tripulante unaEnergia) = unaEnergia

cambiarEnergia :: Number -> Tripulante -> Tripulante
cambiarEnergia cantidad (Tripulante energia) =
    Tripulante (max 0 (energia + cantidad))

disminuirEnergia :: Number -> Tripulante -> Tripulante
disminuirEnergia cantidad = cambiarEnergia (-cantidad)

aumentarEnergia :: Number -> Tripulante -> Tripulante
aumentarEnergia cantidad = cambiarEnergia cantidad

enfrentarEsqueleto :: ActividadDiaria
enfrentarEsqueleto tripulante
    | estaEnLasUltimas tripulante = disminuirEnergia 20 tripulante
    | otherwise = disminuirEnergia 10 tripulante

estaEnLasUltimas :: Tripulante -> Bool
estaEnLasUltimas tripulante = energia tripulante < 50

transportarCarga :: Number -> ActividadDiaria
transportarCarga peso = disminuirEnergia peso

beberGrog :: ActividadDiaria
beberGrog = aumentarEnergia 20

estaMuerto :: Tripulante -> Bool
estaMuerto tripulante = energia tripulante == 0

data Barco = Barco {
    tripulantes :: [Tripulante],
    tipoDeBarco :: TipoDeBarco,
    oro :: Number,
    balas :: Number
 } deriving (Eq, Show)

data TipoDeBarco = Galeon | Bergatin | Balandro deriving (Eq, Show)

esBarcoFantasma :: Barco -> Bool
esBarcoFantasma = all estaMuerto . tripulantes

cantidadTripulantes :: Barco -> Number
cantidadTripulantes = length . tripulantes

modificarCantidadBalas :: Number -> Barco -> Barco
modificarCantidadBalas cantidadBalas barco = barco { balas = cantidadBalas }

modificarCantidadOro :: Number -> Barco -> Barco
modificarCantidadOro cantidadOro barco = barco { oro = cantidadOro }

llenar :: Barco -> Barco
llenar barco = (
    modificarCantidadOro (metrosCuadrados barco * 7) . modificarCantidadBalas (cantidadTripulantes barco * 3)
    ) barco

metrosCuadrados :: Barco -> Number
metrosCuadrados = metrosCuadradosSegunTipoDeEmbarcacion . tipoDeBarco

metrosCuadradosSegunTipoDeEmbarcacion :: TipoDeBarco -> Number
metrosCuadradosSegunTipoDeEmbarcacion Galeon = 150
metrosCuadradosSegunTipoDeEmbarcacion Bergatin = 100
metrosCuadradosSegunTipoDeEmbarcacion Balandro = 50
