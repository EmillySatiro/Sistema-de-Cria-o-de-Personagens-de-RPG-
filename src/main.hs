-- Sistema de Criação de Personagens de RPG
-- Tema: Criação de personagens de RPG
-- Justificativa: Permite aplicar conceitos de programação funcional, como tipos algébricos, listas, abstrações, ADT, modularização e IO, de forma criativa e divertida.

module Main where

import Personagem
import Nomeavel
import System.IO
import Data.Char (toUpper, toLower, isSpace)

-- Função auxiliar para remover espaços no início e fim
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

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
        putStrLn "Caso você escreva um valor invalido nas opções abaixo, seu personagem receberá as configurações padrão (Guerreiro/Humano)"
        putStr "Nome: "; hFlush stdout; n <- getLine
        putStrLn "Escolha a Classe:"
        putStrLn "0. Barbaro"
        putStrLn "1. Bardo"
        putStrLn "2. Bruxo"
        putStrLn "3. Clerigo"
        putStrLn "4. Druida"
        putStrLn "5. Feiticeiro"
        putStrLn "6. Guerreiro"
        putStrLn "7. Ladino"
        putStrLn "8. Mago"
        putStrLn "9. Monge"
        putStrLn "10. Paladino"
        putStrLn "11. Patrulheiro"
        putStr "Digite o número da classe: "; hFlush stdout; cIdx <- readLn
        let classe' = case cIdx of
                        0 -> Barbaro
                        1 -> Bardo
                        2 -> Bruxo
                        3 -> Clerigo
                        4 -> Druida
                        5 -> Feiticeiro
                        6 -> Guerreiro
                        7 -> Ladino
                        8 -> Mago
                        9 -> Monge
                        10 -> Paladino
                        11 -> Patrulheiro
                        _ -> Guerreiro -- valor padrão

        putStrLn "Escolha a Raça:"
        putStrLn "0. Humano"
        putStrLn "1. Elfo"
        putStrLn "2. Anão"
        putStrLn "3. Halfling"
        putStrLn "4. Meio-Orc"
        putStrLn "5. Meio-Elfo"
        putStrLn "6. Gnomo"
        putStrLn "7. Draconato"
        putStrLn "8. Tiefling"
        putStr "Digite o número da raça: "; hFlush stdout; rIdx <- readLn
        let raca' = case rIdx of
                      0 -> Humano
                      1 -> Elfo
                      2 -> Anão
                      3 -> Halfling
                      4 -> MeioOrc
                      5 -> MeioElfo
                      6 -> Gnomo
                      7 -> Draconato
                      8 -> Tiefling
                      _ -> Humano -- valor padrão

        putStr "Força: "; hFlush stdout; f <- readLn
        putStr "Inteligência: "; hFlush stdout; i <- readLn
        putStr "Destreza: "; hFlush stdout; d <- readLn
        let novo = criarPersonagem n classe' raca' (Atributos f i d)
        menu (adicionarPersonagem novo ps)
      "3" -> do
        putStr "Nome do personagem a remover: "; hFlush stdout; n <- getLine
        let nomeNormalizado = map toLower (trim n)
        if any (\p -> map toLower (trim (obterNome p)) == nomeNormalizado) ps
          then do
            let ps' = removerPersonagemMain n ps
            putStrLn "Personagem removido com sucesso!"
            menu ps'
          else do
            putStrLn "Erro: Personagem não encontrado!"
            menu ps
      "4" -> do
        putStr "Nome do personagem: "; hFlush stdout; n <- getLine
        putStr "Nome do item: "; hFlush stdout; ni <- getLine
        putStr "Descrição do item: "; hFlush stdout; di <- getLine
        let nomeNormalizado = map toLower (trim n)
        if any (\p -> map toLower (trim (obterNome p)) == nomeNormalizado) ps
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
        let nomeNormalizado = map toLower (trim n)

        case filter (\p -> map toLower (trim (obterNome p)) == nomeNormalizado) ps of
          [] -> do
            putStrLn "Erro: Personagem não encontrado!"
            menu ps
          (pEncontrado:_) ->
            let personagemAtualizado = removerItem ni pEncontrado
            in if personagemAtualizado == pEncontrado
                then do
                  putStrLn "Erro: Item não encontrado no personagem!"
                  menu ps
                else do
                  let ps' = map (\p -> if map toLower (trim (obterNome p)) == nomeNormalizado
                                        then personagemAtualizado
                                        else p) ps
                  putStrLn "Item removido com sucesso!"
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
