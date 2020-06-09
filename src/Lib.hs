module Lib where
import Text.Show.Functions

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

--1)
type Palo= Habilidad->Tiro

putter :: Palo
putter unaHabilidad = UnTiro{
  velocidad=10,
  precision=(*2).precisionJugador $unaHabilidad,
  altura=0
}

madera :: Palo
madera unaHabilidad = UnTiro{
  velocidad=100,
  precision=(`div` 2).precisionJugador $unaHabilidad,
  altura=5
}

hierro :: Int->Palo
hierro unNumero unaHabilidad = UnTiro{
  velocidad=(*unNumero).fuerzaJugador $unaHabilidad,
  precision=(`div` unNumero).precisionJugador $unaHabilidad,
  altura= max (unNumero-3) 0
}

palos = [putter, madera] ++ map hierro [1..10]

--2)

golpe :: Palo->Jugador->Tiro
golpe unPalo= unPalo.habilidad 

--3)
modificarVelocidadTiro :: (Int->Int)->Tiro->Tiro
modificarVelocidadTiro unaFuncion unTiro = UnTiro {velocidad = unaFuncion.velocidad $unTiro}

modificarPrecisionTiro :: (Int->Int)->Tiro->Tiro
modificarPrecisionTiro unaFuncion unTiro = UnTiro {precision = unaFuncion.precision $unTiro}

modificarAlturaTiro :: (Int->Int)->Tiro->Tiro
modificarAlturaTiro unaFuncion unTiro =  UnTiro {altura = unaFuncion.altura $unTiro}

tiroQuedaEnCero :: Efecto
tiroQuedaEnCero = modificarVelocidadTiro (\velocidad->0).modificarPrecisionTiro (\presicion->0).modificarAlturaTiro (\altura->0)

superaTunel :: PuedeSuperar
superaTunel  =(>90).precision

superaLaguna :: PuedeSuperar
superaLaguna unTiro = ((>80).velocidad $unTiro) && (between (altura unTiro) 1 5)

superaHoyo :: PuedeSuperar
superaHoyo unTiro= (between (velocidad unTiro) 5 20) && ((>95).precision $unTiro)



efectoTunelConRampa :: Efecto
efectoTunelConRampa unTiro
 | superaTunel unTiro = modificarVelocidadTiro (*2).modificarPrecisionTiro (\presicion->100).modificarAlturaTiro (\altura->0) $unTiro
 | otherwise          =tiroQuedaEnCero unTiro

efectoLaguna :: Int->Efecto
efectoLaguna largoLaguna unTiro
 | superaLaguna unTiro = modificarAlturaTiro (\altura->div altura largoLaguna) unTiro
 | otherwise           = tiroQuedaEnCero unTiro


efectoHoyo :: Efecto
efectoHoyo unTiro 
 |superaHoyo unTiro = tiroQuedaEnCero unTiro
 |otherwise         = tiroQuedaEnCero unTiro

--4)

type Obstaculo = (PuedeSuperar,Efecto)
type PuedeSuperar = Tiro->Bool
type Efecto = Tiro->Tiro

tunelConRampa = (superaTunel,efectoTunelConRampa)
laguna = (superaLaguna,efectoLaguna)
hoyo = (superaHoyo,efectoHoyo)

puedeSuperar :: Obstaculo->PuedeSuperar
puedeSuperar =fst

efecto :: Obstaculo->Efecto
efecto = snd

superaObstaculo :: Obstaculo->Jugador->Palo->Bool
superaObstaculo obstaculo unJugador unPalo = (puedeSuperar obstaculo) (golpe unPalo unJugador)

palosUtiles :: Jugador->Obstaculo->[Palo]
palosUtiles unJugador unObstaculo = filter (superaObstaculo unObstaculo unJugador) palos

seSuperan :: [Obstaculo]->Tiro->[Obstaculo]
seSuperan obstaculos unTiro = takeWhile (\obstaculo->(puedeSuperar obstaculo) $unTiro ) obstaculos

cantidadQueSupera :: [Obstaculo]->Tiro->Int
cantidadQueSupera obstaculos =length.(seSuperan obstaculos)

paloMasUtil :: Jugador->[Obstaculo]->Palo
paloMasUtil unJugador obstaculos = maximoSegun (cantidadQueSupera obstaculos.(flip golpe unJugador)) palos



