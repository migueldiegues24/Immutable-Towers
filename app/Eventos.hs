module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import LI12425
import Tarefa1 (validaTorres)

-- | Função para reagir a eventos de jogador
reageEventos :: Event -> Jogo -> Jogo
reageEventos evento jogo = case evento of
    EventKey (MouseButton LeftButton) Down _ pos -> interagirComClique pos jogo
    EventKey (SpecialKey KeySpace) Down _ _      -> pausaJogo jogo
    _                                           -> jogo

-- | Identifica se o clique foi na loja ou no mapa
interagirComClique :: (Float, Float) -> Jogo -> Jogo
interagirComClique pos jogo
    | clicouNaLoja pos = comprarTorre pos jogo
    | otherwise         = colocaTorre pos jogo

-- | Verifica se o clique foi na loja
clicouNaLoja :: (Float, Float) -> Bool
clicouNaLoja (x, y) = x >= -300 && x <= -100 && y >= 150

-- | Lógica de compra de torre
comprarTorre :: (Float, Float) -> Jogo -> Jogo
comprarTorre (_, y) jogo =
    let 
        indiceTorre = floor ((200 - y) / 50)
        loja = lojaJogo jogo
    in 
        if indiceTorre >= 0 && indiceTorre < length loja
       then 
        let (preco, torre) = loja !! indiceTorre
            in 
                if creditosBase (baseJogo jogo) >= preco
               then jogo { baseJogo = (baseJogo jogo) { creditosBase = creditosBase (baseJogo jogo) - preco }
                         , torresJogo = torre { posicaoTorre = (-1, -1) } : torresJogo jogo } -- Torre temporariamente sem posição
               else jogo -- Sem créditos suficientes
       else jogo


-- | Coloca uma torre no mapa ao clicar com o botão esquerdo
colocaTorre :: (Float, Float) -> Jogo -> Jogo
colocaTorre (x, y) jogo = 
    let 
        (torres, restantes) = span (\t -> posicaoTorre t == (-1, -1)) (torresJogo jogo)
    in 
        case torres of
        (t:_) -> 
            let 
                novaTorre = t { posicaoTorre = coordsParaPosicao (x, y) }
                novoJogo = jogo { torresJogo = novaTorre : restantes }
            in 
                if validaTorres novoJogo
               then novoJogo
               else jogo -- Posição inválida, torre não é colocada
        [] -> jogo -- Sem torres para colocar, não faz nada


-- | Pausa o jogo ao pressionar a barra de espaço
pausaJogo :: Jogo -> Jogo
pausaJogo jogo = jogo

-- | Conversão de coordenadas da tela para posições lógicas
coordsParaPosicao :: (Float, Float) -> Posicao
coordsParaPosicao (x, y) = ( x / 50, y / 50)
