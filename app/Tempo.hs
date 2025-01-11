module Tempo where

import LI12425
import Tarefa3 (atualizaJogo)

-- | Reage à passagem do tempo, atualizando o estado do jogo
reageTempo :: Float -> Jogo -> Jogo
reageTempo tempo jogo = atualizaJogo tempo jogo
