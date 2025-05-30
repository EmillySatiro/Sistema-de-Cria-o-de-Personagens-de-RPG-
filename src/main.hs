-- Sistema de Criação de Personagens de RPG
-- Tema: Criação de personagens de RPG
-- Justificativa: Permite aplicar conceitos de programação funcional, como tipos algébricos, listas, abstrações, ADT, modularização e IO, de forma criativa e divertida.

module Main where

import Personagem
import Nomeavel
import System.IO
import Data.Char (toUpper)

-- Função para mostrar todos os personagens com descrição completa
mostrarPersonagens :: [Personagem] -> IO ()
mostrarPersonagens [] = putStrLn "\nNenhum personagem cadastrado ainda."
mostrarPersonagens ps = do
    putStrLn "\nPersonagens Cadastrados:"
    mapM_ (putStrLn . descricaoCompleta) ps

-- Função para adicionar item ao personagem
adicionarItemPersonagem :: String -> String -> String -> [Personagem] -> [Personagem]
adicionarItemPersonagem nomeItem nomePersonagem descricao ps =
  let (antes, resto) = break (\p -> obterNome p == nomePersonagem) ps
  in case resto of
       [] -> ps  -- Personagem não encontrado
       (p:depois) ->
         let pAtualizado = adicionarItem (Item nomeItem descricao) p
         in antes ++ [pAtualizado] ++ depois

-- Função para adicionar personagem
adicionarPersonagem :: Personagem -> [Personagem] -> [Personagem]
adicionarPersonagem p ps = p : ps

-- Função para remover personagem (renomeada para evitar ambiguidade)
removerPersonagemMain :: String -> [Personagem] -> [Personagem]
removerPersonagemMain nome ps = filter (\p -> obterNome p /= nome) ps

-- Função para interação simples via terminal
menu :: [Personagem] -> IO ()
menu ps = do
    putStrLn "\n==============================="
    putStrLn "    Menu do Sistema de RPG"
    putStrLn "==============================="
    putStrLn "1. Listar personagens"
    putStrLn "2. Adicionar personagem"
    putStrLn "3. Remover personagem"
    putStrLn "4. Adicionar item a personagem"
    putStrLn "5. Remover item de personagem"
    putStrLn "6. Salvar personagens em arquivo"
    putStrLn "7. Carregar personagens do arquivo"
    putStrLn "0. Sair"
    putStr "Escolha uma opção: "
    hFlush stdout
    op <- getLine
    case op of
      "1" -> do
        mostrarPersonagens ps
        menu ps
      "2" -> do
        putStr "Nome: "; hFlush stdout; n <- getLine
        putStrLn "Escolha a Classe:"
        putStrLn "0. Guerreiro"
        putStrLn "1. Mago"
        putStrLn "2. Ladino"
        putStrLn "3. Clérigo"
        putStrLn "4. Arqueiro"
        putStr "Digite o número da classe: "; hFlush stdout; cIdx <- readLn
        let classe' = case cIdx of
                        0 -> Guerreiro
                        1 -> Mago
                        2 -> Ladino
                        3 -> Clerigo
                        4 -> Arqueiro
                        _ -> Guerreiro -- valor padrão
        putStrLn "Escolha a Raça:"
        putStrLn "0. Humano"
        putStrLn "1. Elfo"
        putStrLn "2. Anão"
        putStrLn "3. Orc"
        putStrLn "4. Goblin"
        putStr "Digite o número da raça: "; hFlush stdout; rIdx <- readLn
        let raca' = case rIdx of
                      0 -> Humano
                      1 -> Elfo
                      2 -> Anao
                      3 -> Orc
                      4 -> Goblin
                      _ -> Humano -- valor padrão
        putStr "Força: "; hFlush stdout; f <- readLn
        putStr "Inteligência: "; hFlush stdout; i <- readLn
        putStr "Destreza: "; hFlush stdout; d <- readLn
        let novo = criarPersonagem n classe' raca' (Atributos f i d)
        menu (adicionarPersonagem novo ps)
      "3" -> do
        putStr "Nome do personagem a remover: "; hFlush stdout; n <- getLine
        menu (removerPersonagemMain n ps)  -- Usando a função renomeada
      "4" -> do
        putStr "Nome do personagem: "; hFlush stdout; n <- getLine
        putStr "Nome do item: "; hFlush stdout; ni <- getLine
        putStr "Descrição do item: "; hFlush stdout; di <- getLine
        if any (\p -> obterNome p == n) ps
          then do
            let ps' = adicionarItemPersonagem ni n di ps
            putStrLn "Item adicionado com sucesso!"
            menu ps'
          else do
            putStrLn "Erro: Personagem não encontrado!"
            menu ps
      "5" -> do
        putStr "Nome do personagem: "; hFlush stdout; n <- getLine
        putStr "Nome do item a remover: "; hFlush stdout; ni <- getLine
        let ps' = [if obterNome p == n then removerItem ni p else p | p <- ps]
        menu ps'
      "6" -> do
        writeFile "personagens.txt" (show ps)
        putStrLn "\nPersonagens salvos com sucesso!"
        menu ps
      "7" -> do
        conteudo <- readFile "personagens.txt"
        let ps' = read conteudo :: [Personagem]
        putStrLn "\nPersonagens carregados com sucesso!"
        menu ps'
      "0" -> putStrLn "Saindo... Até logo!"
      _   -> menu ps

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Criação de Personagens de RPG!"
  menu []
