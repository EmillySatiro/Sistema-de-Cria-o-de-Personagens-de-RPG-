-- | Módulo de Tipos e Funções de Personagem para o sistema de RPG
module Personagem (
    Classe(..), Raca(..), Atributos(..), Item(..), Personagem,
    criarPersonagem, encontrarPersonagem, adicionarItem, removerItem,
    listarPersonagens, removerPersonagem, temItem, descricaoCompleta
) where

import Nomeavel

-- Tipos Algébricos

data Classe
  = Barbaro
  | Bardo
  | Bruxo
  | Clerigo
  | Druida
  | Feiticeiro
  | Guerreiro
  | Ladino
  | Mago
  | Monge
  | Paladino
  | Patrulheiro  -- (Ranger)
  deriving (Show, Eq, Read)

data Raca
  = Humano
  | Elfo
  | Anão
  | Halfling
  | MeioOrc
  | MeioElfo
  | Gnomo
  | Draconato
  | Tiefling
  deriving (Show, Eq, Read)

data Atributos = Atributos {
  forca :: Int,
  inteligencia :: Int,
  destreza :: Int
} deriving (Show, Eq, Read)

data Item = Item {
  nomeItem :: String,
  descricaoItem :: String
} deriving (Show, Eq, Read)

data Personagem = Personagem {
  nome :: String,
  classe :: Classe,
  raca :: Raca,
  atributos :: Atributos,
  inventario :: [Item]
} deriving (Show, Eq, Read)

instance Nomeavel Personagem where
    obterNome = nome

instance Nomeavel Item where
    obterNome = nomeItem

-- Funções de manipulação
criarPersonagem :: String -> Classe -> Raca -> Atributos -> Personagem
criarPersonagem n c r a = Personagem n c r a []

encontrarPersonagem :: String -> [Personagem] -> Maybe Personagem
encontrarPersonagem n = foldr (\p acc -> if nome p == n then Just p else acc) Nothing

adicionarItem :: Item -> Personagem -> Personagem
adicionarItem item p = p { inventario = item : inventario p }

removerItem :: String -> Personagem -> Personagem
removerItem nomeIt p = p { inventario = filter ((/= nomeIt) . nomeItem) (inventario p) }

listarPersonagens :: [Personagem] -> [String]
listarPersonagens = map nome

removerPersonagem :: String -> [Personagem] -> [Personagem]
removerPersonagem n = filter ((/= n) . nome)

temItem :: String -> Personagem -> Bool
temItem nomeIt p = any ((== nomeIt) . nomeItem) (inventario p)

descricaoCompleta :: Personagem -> String
descricaoCompleta p = nome p ++ " (" ++ show (classe p) ++ ", " ++ show (raca p) ++ ")\nAtributos: " ++ show (atributos p) ++ "\nItens: " ++ show (map nomeItem (inventario p))
