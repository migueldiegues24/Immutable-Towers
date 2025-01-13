module Main where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game()

import ImmutableTowers
import Desenhar
import Eventos
import Tempo

-- | Configuração inicial da janela
tamanhoJanela :: (Int, Int)
tamanhoJanela = (800, 600)

tituloJanela :: String
tituloJanela = "Immutable Towers"


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