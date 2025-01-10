module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game()

import LI12425
import Desenhar
import Eventos
import Tempo

-- | Configuração inicial da janela
tamanhoJanela :: (Int, Int)
tamanhoJanela = (800, 600)

tituloJanela :: String
tituloJanela = "Immutable Towers"

-- | Estado inicial do jogo (placeholder: criar um exemplo para testar)
estadoInicial :: Jogo
estadoInicial = Jogo {
    baseJogo = Base { posicaoBase = (5, 2), vidaBase = 100, creditosBase = 50 },
    portaisJogo = [Portal { posicaoPortal = (0, 0), ondasPortal = [] }],
    torresJogo = [],
    mapaJogo = [[Terra, Terra, Relva, Agua, Agua, Agua],
                [Relva, Terra, Relva, Agua, Relva, Relva],
                [Relva, Terra, Relva, Agua, Relva, Terra],
                [Relva, Terra, Relva, Agua, Relva, Terra],
                [Relva, Terra, Terra, Terra, Terra, Terra],
                [Agua, Agua, Agua, Agua, Relva, Relva]],
    inimigosJogo = [],
    lojaJogo = []
  }  

-- | Main: configuração inicial e loop do jogo
main :: IO ()
main = play
    (InWindow tituloJanela tamanhoJanela (100, 100)) -- Janela
    white                                           -- Cor do fundo
    60                                              -- Frames por segundo
    estadoInicial                                   -- Estado inicial do jogo
    desenhaJogo                                     -- Função de desenho
    reageEventos                                    -- Função para reagir a eventos
    reageTempo                                      -- Função para atualizar o jogo com o tempo
