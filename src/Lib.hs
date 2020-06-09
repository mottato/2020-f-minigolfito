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



