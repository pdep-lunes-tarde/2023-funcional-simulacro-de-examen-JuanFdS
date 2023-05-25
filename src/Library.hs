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
    balas :: Number,
    madera :: Number
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

modificarCantidadMadera :: Number -> Barco -> Barco
modificarCantidadMadera cantidadMadera barco = barco { madera = cantidadMadera }

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

enfrentarseContra :: Barco -> Barco -> Barco
enfrentarseContra barco barcoEnemigo
    | ganaContra barco barcoEnemigo = robarRecursosDe barco barcoEnemigo
    | otherwise = vaciarRecursos barco

ganaContra :: Barco -> Barco -> Bool
ganaContra barco barcoEnemigo =
    (barco `esMasGrandeQue` barcoEnemigo && barco `tieneIgualOMasBalasQue` barcoEnemigo) ||
    (barco `esDeMismoTamanioQue` barcoEnemigo && barco `tieneIgualOMasMaderaQue` barcoEnemigo) ||
    (barco `esMasChicoQue` barcoEnemigo && barco `tieneIgualOMasTripulantesVivosQue` barcoEnemigo)

-- ganaContra :: Barco -> Barco -> Bool
-- ganaContra barco barcoEnemigo = condicionDeVictoriaSegunTamanio (tamanioRelativo barco barcoEnemigo) barco barcoEnemigo

-- tamanioRelativo :: Barco -> Barco -> TamanioRelativo
-- tamanioRelativo unBarco otroBarco
--     | metrosCuadrados unBarco > metrosCuadrados otroBarco = MasGrande
--     | metrosCuadrados unBarco == metrosCuadrados otroBarco = Igual
--     | otherwise = MasChico

-- condicionDeVictoriaSegunTamanio :: TamanioRelativo -> Barco -> Barco -> Bool
-- condicionDeVictoriaSegunTamanio MasGrande = tieneIgualOMasBalasQue
-- condicionDeVictoriaSegunTamanio Igual = tieneIgualOMasMaderaQue
-- condicionDeVictoriaSegunTamanio MasChico = tieneIgualOMasTripulantesVivosQue

-- data TamanioRelativo = MasGrande | Igual | MasChico

tieneMasOIgualQue :: Ord a => (Barco -> a) -> Barco -> Barco -> Bool
tieneMasOIgualQue criterio unBarco otroBarco =
    criterio unBarco >= criterio otroBarco

esMasChicoQue :: Barco -> Barco -> Bool
unBarco `esMasChicoQue` otroBarco =
    metrosCuadrados unBarco < metrosCuadrados otroBarco

esMasGrandeQue :: Barco -> Barco -> Bool
unBarco `esMasGrandeQue` otroBarco =
    metrosCuadrados unBarco > metrosCuadrados otroBarco

esDeMismoTamanioQue :: Barco -> Barco -> Bool
unBarco `esDeMismoTamanioQue` otroBarco =
    metrosCuadrados unBarco == metrosCuadrados otroBarco

tieneIgualOMasMaderaQue :: Barco -> Barco -> Bool
tieneIgualOMasMaderaQue = tieneMasOIgualQue madera

tieneIgualOMasTripulantesVivosQue :: Barco -> Barco -> Bool
tieneIgualOMasTripulantesVivosQue = tieneMasOIgualQue cantidadTripulantesVivos

tieneIgualOMasBalasQue :: Barco -> Barco -> Bool
tieneIgualOMasBalasQue = tieneMasOIgualQue balas

contar :: (a -> Bool) -> [a] -> Number
contar condicion = length . filter condicion

cantidadTripulantesVivos :: Barco -> Number
cantidadTripulantesVivos = contar (not . estaMuerto) . tripulantes

vaciarRecursos :: Barco -> Barco
vaciarRecursos =
    modificarCantidadBalas 0 . modificarCantidadOro 0 . modificarCantidadMadera 0

robarRecursosDe :: Barco -> Barco -> Barco
robarRecursosDe barcoLadron barcoRobado = (
    modificarCantidadBalas (balas barcoLadron + balas barcoRobado) .
    modificarCantidadOro (oro barcoLadron + oro barcoRobado) .
    modificarCantidadMadera (madera barcoLadron + madera barcoRobado)
    ) barcoLadron

-------------------------

cambiarTripulantes :: [Tripulante] -> Barco -> Barco
cambiarTripulantes nuevosTripulantes barco =
    barco { tripulantes = nuevosTripulantes }

afectarTripulantes :: (Tripulante -> Tripulante) -> Barco -> Barco
afectarTripulantes efecto barco =
    cambiarTripulantes (map efecto (tripulantes barco)) barco

type Suceso = Barco -> Barco

embarcarUnTesoro :: Number -> Suceso
embarcarUnTesoro pesoEnOro barco = (
    afectarTripulantes (disminuirEnergia (pesoEnOro / cantidadTripulantesVivos barco)) .
    modificarCantidadOro (pesoEnOro + oro barco)
    ) barco

enfrentarUnBarco :: Barco -> Suceso
enfrentarUnBarco = flip enfrentarseContra

afectarVariasVeces :: Number -> (Tripulante -> Tripulante) -> Tripulante -> Tripulante
afectarVariasVeces cantidad efecto tripulante = foldr ($) tripulante (replicate cantidad efecto)

beberVariasVecesGrog :: Number -> Tripulante -> Tripulante
beberVariasVecesGrog cantidad = afectarVariasVeces 5 beberGrog

-- beberVariasVecesGrog 0 tripulante = tripulante
-- beberVariasVecesGrog n tripulante = beberVariasVecesGrog (n-1) (beberGrog tripulante)

-- beberVariasVecesGrog cantidad = foldr (.) id (replicate cantidad beberGrog)

encontrarCargamentoGrog :: Suceso
encontrarCargamentoGrog = afectarTripulantes (beberVariasVecesGrog 5)

estaVivo :: Tripulante -> Bool
estaVivo = not . estaMuerto

enfrentarEjercitoDeEsqueletos :: Number -> Suceso
enfrentarEjercitoDeEsqueletos cantidad =
    afectarPrimerTripulanteQueCumple estaVivo (enfrentarEsqueletos cantidad)

enfrentarEsqueletos :: Number -> Tripulante -> Tripulante
enfrentarEsqueletos cantidad =  afectarVariasVeces cantidad enfrentarEsqueleto

afectarPrimerTripulanteQueCumple :: (Tripulante -> Bool) -> (Tripulante -> Tripulante) -> Barco -> Barco
afectarPrimerTripulanteQueCumple condicion efecto barco = (
        agregarTripulante ((efecto . encontrarPrimero condicion) barco) .
        sacarTripulante (encontrarPrimero condicion barco)
    ) barco

agregarTripulante :: Tripulante -> Barco -> Barco
agregarTripulante tripulante barco = cambiarTripulantes (tripulante : tripulantes barco) barco

encontrarPrimero :: (Tripulante -> Bool) -> Barco -> Tripulante
encontrarPrimero condicion = head . filter condicion . tripulantes

primerTripulanteConVida :: Barco -> Tripulante
primerTripulanteConVida = head . filter (not . estaMuerto) . tripulantes

sacarTripulante :: Tripulante -> Barco -> Barco
sacarTripulante tripulante barco = cambiarTripulantes (sacar tripulante (tripulantes barco)) barco

sacar :: Eq a => a -> [a] -> [a]
sacar elemento lista = takeWhile (/= elemento) lista ++ tail (dropWhile (/= elemento) lista)

precioGrog = 30

estanTodosVivos :: Barco -> Bool
estanTodosVivos = all (not . estaMuerto) . tripulantes

primerTripulanteMuerto :: Barco -> Tripulante
primerTripulanteMuerto = head . filter estaMuerto . tripulantes

pasarPorTiendaDeGrog :: Barco -> Barco
pasarPorTiendaDeGrog barco
    | oro barco < precioGrog || estanTodosVivos barco = barco
    | otherwise = (afectarPrimerTripulanteQueCumple estaMuerto beberGrog . modificarCantidadOro (oro barco - precioGrog)) barco

suceder :: Barco -> (Barco -> Barco) -> Barco
suceder barco suceso
    | esBarcoFantasma barco = barco
    | otherwise = suceso barco

data Travesia = Travesia { recompensaEnOro :: (Barco -> Number), sucesos :: [Suceso] }

fuerteDeLosCondenados :: Travesia
fuerteDeLosCondenados = Travesia (\_ -> 50) [enfrentarEjercitoDeEsqueletos 100, pasarPorTiendaDeGrog, embarcarUnTesoro 30]

travesiaDelFlameHeart :: Travesia
travesiaDelFlameHeart = Travesia ((*200) . cantidadTripulantesVivos) [
    enfrentarUnBarco (Barco (replicate 4 (Tripulante 30)) Galeon 0 50 50),
    enfrentarUnBarco (Barco (replicate 3 (Tripulante 10)) Bergatin 0 30 30),
    encontrarCargamentoGrog,
    embarcarUnTesoro 150
    ]

laGirita :: Travesia
laGirita = Travesia (\barco -> oro barco) (replicate 4 pasarPorTiendaDeGrog ++ [enfrentarEjercitoDeEsqueletos 10])

realizarTravesia :: Travesia -> Barco -> Barco
realizarTravesia travesia barco =
    (cobrarRecompensaSiSobrevivio (recompensaEnOro travesia) . foldl suceder barco) (sucesos travesia)

cobrarRecompensaSiSobrevivio :: (Barco -> Number) -> Barco -> Barco
cobrarRecompensaSiSobrevivio cuantoOro barco
    | esBarcoFantasma barco = barco
    | otherwise = modificarCantidadOro (oro barco + cuantoOro barco) barco