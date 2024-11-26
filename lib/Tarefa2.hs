{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Miguel Rocha Diegues <a107361@alunos.uminho.pt>
              César António Fernandes Lopes <a109512@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425


-- 1
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


-- 2
-- Função que atualiza o inimigo depois deste ser atingido pela torre

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo = undefined


-- Função auxiliar que atualiza a vida do Inimigo depois do mesmo ser atingido pela torre

atualizaVidaInimigo :: Torre -> Inimigo -> Inimigo
atualizaVidaInimigo torre inimigo =
    let
        dano = danoTorre torre
        novaVida = max 0 (vidaInimigo inimigo - dano)
    in
        inimigo { vidaInimigo = novaVida }


-- 3
-- Função que ativa o inimigo, isto é sai do portal e passa a estar ativo no jogo

ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigosAtivos =
    case ondasPortal portal of
        [] -> (portal, inimigosAtivos) -- Não há ondas no portal
        (onda:restoOndas) ->
                if entradaOnda onda > 0 || tempoOnda onda > 0 || null (inimigosOnda onda)
                then (portal, inimigosAtivos) -- Onda inativa ou sem inimigos
                else
                    case inimigosOnda onda of
                        [] -> (portal, inimigosAtivos)
                        (proximoInimigo:restoInimigos) ->
                            let
                                novaOnda = onda { inimigosOnda = restoInimigos, tempoOnda = cicloOnda onda }
                                novoPortal = portal { ondasPortal = novaOnda : restoOndas }
                                novosInimigosAtivos = (proximoInimigo : inimigosAtivos)
                            in
                                (novoPortal, novosInimigosAtivos)


-- 4
-- Função que verifica se o jogo terminou e por consequente se ganhou ou perdeu

terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

ganhouJogo :: Jogo -> Bool
ganhouJogo jogo =
    let
        inimigosAtivos = inimigosJogo jogo
        vidadabase = vidaBase (baseJogo jogo)
    in
        null inimigosAtivos && vidadabase > 0

perdeuJogo :: Jogo -> Bool
perdeuJogo jogo =
    let
        vidadabase = vidaBase (baseJogo jogo)
    in
        vidadabase <= 0
