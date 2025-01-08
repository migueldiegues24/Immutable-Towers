module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425

-- Função para reagir a eventos do jogador
reageEventos :: Event -> Jogo -> Jogo
reageEventos (EventKey (MouseButton LeftButton) Down _ (mx, my)) jogo =
  let novaTorre = Torre {
        posicaoTorre = (mx, my),
        danoTorre = 10,
        alcanceTorre = 100,
        rajadaTorre = 1,
        cicloTorre = 1,
        tempoTorre = 0,
        projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Infinita }
      }
  in if creditosBase (baseJogo jogo) >= 10
       then jogo { torresJogo = novaTorre : torresJogo jogo, baseJogo = (baseJogo jogo) { creditosBase = creditosBase (baseJogo jogo) - 10 } }
       else jogo
reageEventos _ jogo = jogo
