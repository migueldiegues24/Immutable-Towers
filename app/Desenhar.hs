module Desenhar where

import Graphics.Gloss
import LI12425



-- Tamanho das células do mapa
tamanhoCelula :: Float
tamanhoCelula = 80

-- Função para obter a cor de cada terreno
corTerreno :: Terreno -> Color
corTerreno Relva = green
corTerreno Terra = orange
corTerreno Agua = blue

-- Desenhar uma célula do mapa
desenhaCelula :: Terreno -> (Int, Int) -> Picture
desenhaCelula terreno (x, y) = 
  Translate (fromIntegral x * tamanhoCelula) (fromIntegral (-y) * tamanhoCelula) $ 
    Color (corTerreno terreno) $ rectangleSolid tamanhoCelula tamanhoCelula

-- Desenhar o mapa completo
desenhaMapa :: Mapa -> Picture
desenhaMapa mapa = Translate (-700) 500 $ Pictures [
    desenhaCelula terreno (x, y)
    | (linha, y) <- zip mapa [0..]
    , (terreno, x) <- zip linha [0..]
  ]

-- Desenhar a base
desenhaBase :: Base -> Picture
desenhaBase base = Translate x y $ Color red $ circleSolid 40
  where (x, y) = posicaoBase base

-- Desenhar um portal
desenhaPortal :: Portal -> Picture
desenhaPortal portal = Translate x y $ Color violet $ rectangleSolid 40 40
  where (x, y) = posicaoPortal portal

-- Função principal para desenhar o estado do jogo
desenha :: Jogo -> Picture
desenha jogo = Pictures [
    desenhaMapa (mapaJogo jogo),
    desenhaBase (baseJogo jogo),
    Pictures (map desenhaPortal (portaisJogo jogo))
  ]
