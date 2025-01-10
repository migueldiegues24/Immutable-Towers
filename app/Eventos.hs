module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import LI12425
import Tarefa1 (validaTorres)

-- | Função para reagir a eventos de jogador
reageEventos :: Event -> Jogo -> Jogo
reageEventos evento jogo = case evento of
    EventKey (MouseButton LeftButton) Down _ pos -> colocaTorre pos jogo
    EventKey (SpecialKey KeySpace) Down _ _      -> pausaJogo jogo
    _                                           -> jogo

-- | Coloca uma torre no mapa ao clicar com o botão esquerdo
colocaTorre :: (Float, Float) -> Jogo -> Jogo
colocaTorre (x, y) jogo = 
    let novaTorre = Torre { posicaoTorre = coordsParaPosicao (x, y)
                          , danoTorre = 10
                          , alcanceTorre = 60
                          , rajadaTorre = 1
                          , cicloTorre = 2
                          , tempoTorre = 0
                          , projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Infinita }
                          }
        novoJogo = jogo { torresJogo = novaTorre : torresJogo jogo }
    in if validaTorres novoJogo
       then novoJogo
       else jogo

-- | Pausa o jogo ao pressionar a barra de espaço
pausaJogo :: Jogo -> Jogo
pausaJogo jogo = jogo -- Placeholder, pode incluir lógica de pausa futuramente

-- | Conversão de coordenadas da tela para posições lógicas
coordsParaPosicao :: (Float, Float) -> Posicao
coordsParaPosicao (x, y) = (x / 40, y / 40)
