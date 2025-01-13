module ImmutableTowers where

import LI12425

data ImmutableTowers = ImmutableTowers {}

-- | Estado inicial do jogo 
estadoInicial :: Jogo
estadoInicial = Jogo {
    baseJogo = Base { posicaoBase = (5, 2), vidaBase = 100, creditosBase = 50 },
    portaisJogo = [Portal {
        posicaoPortal = (0, 0),
        ondasPortal = [
            Onda {
                inimigosOnda = [
                    Inimigo 
                    {posicaoInimigo = (0, 0), 
                    vidaInimigo = 100, 
                    velocidadeInimigo = 1, 
                    direcaoInimigo = Este, 
                    projeteisInimigo = [], 
                    ataqueInimigo = 20, 
                    butimInimigo = 20}],
                entradaOnda = 1,
                tempoOnda = 1,
                cicloOnda = 5
            }
        ]
    }],
    torresJogo = [],
    mapaJogo = [[Terra, Terra, Relva, Agua, Agua, Agua],
                [Relva, Terra, Relva, Agua, Relva, Relva],
                [Relva, Terra, Relva, Agua, Relva, Terra],
                [Relva, Terra, Relva, Agua, Relva, Terra],
                [Relva, Terra, Terra, Terra, Terra, Terra],
                [Agua, Agua, Agua, Agua, Relva, Relva]],
    inimigosJogo = [],
    lojaJogo = [
        (10, Torre (0, 0) 10 60 1 2 0 (Projetil Fogo Infinita)),
        (15, Torre (0, 0) 15 80 2 3 0 (Projetil Gelo (Finita 5))),
        (20, Torre (0, 0) 20 100 3 4 0 (Projetil Resina (Finita 10)))
        ]
  }
