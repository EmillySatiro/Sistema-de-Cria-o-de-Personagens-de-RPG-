
-- | Módulo e funções para gerar um personagem aléatorio.


module Aleatorio (gerarPersonagemAleatorio) where

import Personagem

todasClasses :: [Classe]
todasClasses = [Barbaro, Bardo, Bruxo, Clerigo, Druida, Feiticeiro, Guerreiro, Ladino, Mago, Monge, Paladino, Patrulheiro]

todasRacas :: [Raca]
todasRacas = [Humano, Elfo, Anão, Halfling, MeioOrc, MeioElfo, Gnomo, Draconato, Tiefling]

prefixos :: [String]
prefixos = ["Ar", "Ly", "Dro", "Tha", "Mer", "Ka", "Ser", "Ny", "To", "Ela"]

sufixos :: [String]
sufixos = ["than", "ra", "gan", "lia", "ric", "el", "aph", "m", "rak", "ith"]

lcg :: Int -> (Int, Int)
lcg seed = (next, next)
  where
    a = 1664525
    c = 1013904223
    m = 2 ^ 32
    next = (a * seed + c) `mod` m

gerarRandoms :: Int -> [Int]
gerarRandoms seed = let (r, seed') = lcg seed in r : gerarRandoms seed'

range :: Int -> Int -> Int -> Int
range low high n = low + (n `mod` (high - low + 1))

gerarNome :: Int -> Int -> String
gerarNome i1 i2 = prefixos !! (i1 `mod` length prefixos) ++ sufixos !! (i2 `mod` length sufixos)

gerarPersonagemComContador :: Int -> Int -> Personagem
gerarPersonagemComContador seed contador = gerarPersonagemAleatorio (seed + contador)

gerarPersonagemAleatorio :: Int -> Personagem
gerarPersonagemAleatorio seed =
  let randoms = gerarRandoms seed
      nomeAleatorio = gerarNome (randoms !! 0) (randoms !! 1)
      classeIdx = range 0 (length todasClasses - 1) (randoms !! 2)
      racaIdx = range 0 (length todasRacas - 1) (randoms !! 3)
      f = range 1 20 (randoms !! 4)
      i = range 1 20 (randoms !! 5)
      d = range 1 20 (randoms !! 6)
      classeAleatoria = todasClasses !! classeIdx
      racaAleatoria = todasRacas !! racaIdx
      atributos = Atributos f i d
  in criarPersonagem nomeAleatorio classeAleatoria racaAleatoria atributos
