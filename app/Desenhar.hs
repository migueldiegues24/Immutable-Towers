module Desenhar (desenhaJogo) where

import Graphics.Gloss
import LI12425

-- | Função para desenhar o estado do jogo
desenhaJogo :: Jogo -> Picture
desenhaJogo jogo = 
  pictures [desenhaMapa (mapaJogo jogo), 
            desenhaBase (baseJogo jogo), 
            desenhaTorres (torresJogo jogo), 
            desenhaInimigos (inimigosJogo jogo),
            desenhaPortais (portaisJogo jogo)]

-- | Desenhar o mapa
desenhaMapa :: Mapa -> Picture
desenhaMapa mapa = pictures $ concat $ zipWith desenhaLinha [0..] mapa

    
desenhaLinha :: Integral a => a -> [Terreno] -> [Picture]
desenhaLinha y linha = zipWith (desenhaCelula y) [0..] linha

desenhaCelula :: (Integral a1, Integral a2) => a2 -> a1 -> Terreno -> Picture
desenhaCelula y x terreno = translate (fromIntegral x * 40) (fromIntegral y * 40) (desenhaTerreno terreno)

-- | Representação gráfica de cada tipo de terreno
desenhaTerreno :: Terreno -> Picture
desenhaTerreno Relva = color green (rectangleSolid 40 40)
desenhaTerreno Agua = color blue (rectangleSolid 40 40)
desenhaTerreno Terra = color orange (rectangleSolid 40 40)

-- | Desenhar a base
desenhaBase :: Base -> Picture
desenhaBase base = translate x y $ color red (circleSolid 20)
  where
    (x, y) = posicaoParaCoords (posicaoBase base)

-- | Desenhar as torres
desenhaTorres :: [Torre] -> Picture
desenhaTorres torres = pictures $ map desenhaTorre torres

-- | Representação gráfica de uma torre
desenhaTorre :: Torre -> Picture
desenhaTorre torre = translate x y $ color yellow (rectangleSolid 30 30)
  where
    (x, y) = posicaoParaCoords (posicaoTorre torre)

-- | Desenhar os portais

desenhaPortais :: [Portal] -> Picture
desenhaPortais portais = pictures $ map desenhaPortal portais

desenhaPortal :: Portal -> Picture
desenhaPortal portal = translate x y $ color violet (rectangleSolid 30 30)
  where
    (x, y) = posicaoParaCoords (posicaoPortal portal)

-- | Desenhar os inimigos
desenhaInimigos :: [Inimigo] -> Picture
desenhaInimigos inimigos = pictures $ map desenhaInimigo inimigos

-- | Representação gráfica de um inimigo
desenhaInimigo :: Inimigo -> Picture
desenhaInimigo inimigo = translate x y $ color black (circleSolid 10)
  where
    (x, y) = posicaoParaCoords (posicaoInimigo inimigo)

-- | Converter posição lógica para coordenadas na tela
posicaoParaCoords :: Posicao -> (Float, Float)
posicaoParaCoords (cx, cy) = (cx * 40, cy * 40)
