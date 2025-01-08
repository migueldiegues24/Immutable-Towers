module Main where

import Desenhar
import Eventos
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import Tempo
import LI12425

-- Definição da janela
tamanhoJanela :: (Int, Int)
tamanhoJanela = (1400, 1000)

janela :: Display
janela = InWindow "Immutable Towers" tamanhoJanela (250, 0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

-- Estado inicial do jogo
estadoInicial :: Jogo
estadoInicial = Jogo {
    baseJogo = Base { posicaoBase = (-300, 340), vidaBase = 100, creditosBase = 50 },
    portaisJogo = [Portal { posicaoPortal = (-700, 500), ondasPortal = [] }],
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

-- Função principal
main :: IO ()
main = play janela fundo fr estadoInicial desenha reageEventos reageTempo

