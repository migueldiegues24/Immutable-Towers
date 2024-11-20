{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Miguel Rocha Diegues <a107361@alunos.uminho.pt>
              César António Fernandes Lopes <a109512@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425


-- Função que calcula a lista de inimigos no alcance da torre

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance _ [] = []
inimigosNoAlcance torre (h:t) =
    let
        posTorre = posicaoTorre torre
        posInimigo = posicaoInimigo h
        rangeTorre = alcanceTorre torre
    in
        if distanciaEntreDoisPontos posTorre posInimigo <= rangeTorre then h : inimigosNoAlcance torre t else inimigosNoAlcance torre t

-- Função auxiliar para calcular a distância entre dois pontos

distanciaEntreDoisPontos :: Posicao -> Posicao -> Distancia
distanciaEntreDoisPontos (x1,y1) (x2,y2) = sqrt $ (x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)


-- Função que atualiza o inimigo depois do mesmo ser atingido por um projétil

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo = undefined 

ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo = undefined

terminouJogo :: Jogo -> Bool
terminouJogo = undefined

ganhouJogo :: Jogo -> Bool
ganhouJogo = undefined

perdeuJogo :: Jogo -> Bool
perdeuJogo = undefined
