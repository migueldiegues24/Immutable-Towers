    {-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Miguel Rocha Diegues <a107361@alunos.uminho.pt>
              César António Fernandes Lopes <a109512@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
-}
module Tarefa3 where

import LI12425

atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo@(Jogo base portbaseais torres inimigos loja) = 
    let (torresAtualizadas, inimigosAtualizados) = atualizaTeI
        movInimigos = atualizaInimigos tempo inimigosAtualizados mapa
    in jogo { torresJogo = torresAtualizadas, inimigosJogo = movInimigos}
    
atualizaTeI :: Tempo -> [Torre] -> [Inimigo] -> ([Torre],[Inimigo])
atualizaTeI tempo torres inimigos = foldr (atualizaTorre tempo) ([],inimigos) torres

atualizaTorre :: Tempo -> Torre -> ([Torre],[Inimigo]) -> ([Torre],[Inimigo])
atualizaTorre tempo torre (torresAtualizadas, inimigosVivos) | tempoTorre torre > 0 = 
                                                                let torrePronta = torre { tempoTorre = max 0 (tempoTorre torre - tempo)}
                                                                in (torrePronta : torresAtualizadas, inimigosVivos)
                                                             | otherwise = 
                                                                let inimigosAlvo = detetaInimigos torre inimigosVivos
                                                                    (inimigosAtingidos, projetil) = dispara torre inimigosAlvo
                                                                    inimigosAtualizados = atualizaVidaInimigos inimigosVivos inimigosAtingidos projetil
                                                                    torreAtualizada = torre { tempoTorre = cicloTorre torre }
                                                                in (torreAtualizada : torresAtualizadas, inimigosAtualizados)
detetaInimigos :: Torre -> [Inimigo] -> [Inimigo]
detetaInimigos torre = filter (\inimigo -> distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre)

distancia :: Posicao -> Posicao -> Distancia
distancia (x1,y1) (x2,y2) = sqrt $ (x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)

dispara :: Torre -> [Inimigo] -> ([Inimigo],[Projetil])
dispara torre inimigos = let alvo = take (rajadaTorre torre) inimigos
                             dano = replicate (length alvo) (projetilTorre torre)
                         in (alvo,dano)
            
atualizaVidaInimigos :: [Inimigo] -> [Inimigo] -> [Projetil] -> [Inimigo]
atualizaVidaInimigos inimigos alvo dano = map (atualizaInimigo alvo dano) inimigos

atualizaInimigo :: [Inimigo] -> [Projetil] -> Inimigo -> Inimigo
atualizaInimigo alvo dano inimigo = case lookup (posicaoInimigo inimigo) (zip (map posicaoInimigo alvo) dano) 
                                      of Just danoSofrido -> inimigo {vidaInimigo = max 0 (vidaInimigo inimigo - danoSofrido)}
                                         Nothing -> inimigo

atualizaInimigos :: Tempo -> [Inimigo] -> Mapa -> [Inimigo]
atualizaInimigos tempo inimigos mapa = map (moveInimigo tempo mapa) inimigos

moveInimigo :: Tempo -> Mapa -> Inimigo -> Inimigo
moveInimigo tempo mapa inimigo = let (x,y) = posicaoInimigo inimigo 
                                         v = velocidadeInimigo inimigo * tempo
                                         novaPosicao = case direcaoInimigo inimigo of
                                                       Norte -> (x,y-v)
                                                       Sul   -> (x,y+v)
                                                       Este  -> (x+v,y)
                                                       Oeste -> (x-v,y)
                                 in if posicaoValida mapa novaPosicao then inimigo  {posicaoInimigo = novaPosicao} else inimigo 

posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida mapa (x,y) = let posicaox = floor x
                               posicaoy = floor y
                           in posicaox >= 0 && posicaoy >= 0 && posicaox < length mapa && posicaoy < length (head mapa) && mapa !! posicaoy !! posicaox == 'T'