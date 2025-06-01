# Sistema de Criação de Ficha para personagens de RPG em Haskell


Este projeto apresenta um sistema robusto desenvolvido em Haskell para a criação e gerenciamento de personagens de Role-Playing Games (RPG). Utilizando conceitos fundamentais da programação funcional, como tipos algébricos de dados (ADTs), funções de ordem superior, classes de tipo e modularização, o sistema oferece uma interface de linha de comando para interagir com os dados dos personagens, incluindo persistência em arquivo.

## Visão Geral do Projeto

O objetivo principal deste sistema é fornecer uma ferramenta prática e funcional para jogadores e mestres de RPG que desejam criar e organizar informações de personagens de forma eficiente. O código-fonte está estruturado de maneira modular para facilitar a compreensão e futuras expansões. A escolha da linguagem Haskell permite explorar paradigmas de programação funcional em um contexto lúdico e desafiador.

## Estrutura do Projeto

O código está organizado da seguinte forma:

*   `/src`: Contém os módulos Haskell principais do sistema.
    *   `Personagem.hs`: Define as estruturas de dados centrais, como `Classe`, `Raça`, `Atributos`, `Item` e `Personagem`, juntamente com funções para manipular essas estruturas.
    *   `Nomeavel.hs`: Implementa uma classe de tipo (`typeclass`) personalizada para entidades que possuem um nome.
    *   `Aleatorio.hs`: Provavelmente contém funcionalidades relacionadas à geração de elementos aleatórios (embora não detalhado no README original, o nome sugere isso).
    *   `main.hs`: Ponto de entrada da aplicação, responsável pela interface com o usuário via terminal, orquestração das operações e gerenciamento da persistência dos dados em arquivos.
*   `docker-compose.yml`: Arquivo de configuração para o Docker Compose, definindo o ambiente de execução conteinerizado com a imagem `haskell:9.2`.
*   `makefile`: Simplifica os processos de compilação (build) e execução do projeto utilizando Docker Compose.
*   `README.md`: Este arquivo, fornecendo informações sobre o projeto e instruções de uso.

## Pré-requisitos

Para executar este projeto utilizando o método recomendado (via Docker), você precisará ter instalados em seu sistema:

*   **Docker:** Uma plataforma de software para criar, implantar e gerenciar contêineres de aplicativos.
*   **Docker Compose:** Uma ferramenta para definir e executar aplicativos Docker multi-contêineres. O `makefile` detecta automaticamente qual comando usar (`docker compose` ou `docker-compose`).

Nenhuma instalação local do compilador Haskell (GHC) é necessária se você optar por usar o Docker, pois o ambiente já está configurado na imagem `haskell:9.2` especificada no `docker-compose.yml`.

## Como Executar o Projeto

A maneira mais simples e recomendada para executar o sistema é utilizando Docker e o `makefile` fornecido. Siga os passos abaixo no terminal, a partir do diretório raiz do projeto (onde o `makefile` e `docker-compose.yml` estão localizados):

1.  **Construir a Imagem Docker:** Execute o comando `make build`. Este comando utilizará o `docker-compose.yml` para baixar a imagem `haskell:9.2` (se ainda não estiver presente localmente) e configurar o ambiente necessário dentro de um contêiner Docker. Este passo só precisa ser executado uma vez ou sempre que houver alterações nas dependências ou configuração do Docker.

    ```bash
    docker compose up --build
    ```

2.  **Executar o Sistema:** Após a construção bem-sucedida da imagem, execute o comando `make `. Este comando iniciará um contêiner Docker baseado na imagem construída e executará o script principal `src/main.hs` usando `runhaskell`. A interface do sistema será apresentada no seu terminal, permitindo que você interaja com as funcionalidades de criação e gerenciamento de personagens.

    ```bash
    make 
    ```

O sistema guiará você através das opções disponíveis, como adicionar novos personagens, visualizar personagens existentes, gerenciar inventários e salvar/carregar dados.

## Funcionalidades Principais

O sistema oferece as seguintes funcionalidades básicas através de sua interface de terminal:

*   **Adição de Personagens:** Permite criar novos personagens fornecendo detalhes como nome, classe, raça e atributos.
*   **Gerenciamento de Inventário:** Possibilita adicionar ou remover itens do inventário de um personagem específico.
*   **Listagem e Busca:** Oferece opções para listar todos os personagens cadastrados ou buscar por personagens específicos.
*   **Persistência:** Salva o estado atual dos personagens em um arquivo, permitindo carregar os dados em sessões futuras.

Este conjunto de funcionalidades demonstra a aplicação prática dos conceitos de programação funcional em Haskell para resolver um problema concreto, com potencial para futuras expansões como sistemas de combate, magias, ou integração com outras ferramentas de RPG.
"""
