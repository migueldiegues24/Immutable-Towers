{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-|
Module      : Tarefa2
Description : Auxiliares do Jogo
Copyright   : Miguel Rocha Diegues <a107361@alunos.uminho.pt>
              César António Fernandes Lopes <a109512@alunos.uminho.pt>


Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
-}
module Tarefa2 where

import LI12425
import Data.List


-- 1
-- Função que calcula a lista de inimigos no alcance da torre

inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre inimigos = filter (\inimigo -> distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre) inimigos
  where
    distancia (x1,y1) (x2,y2) = sqrt $ (x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)


-- 2
-- Função que atualiza o inimigo depois deste ser atingido pela torre

atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo = inimigo { 
  vidaInimigo = max 0 (vidaInimigo inimigo - danoTorre torre),
  projeteisInimigo = atualizaProjeteis (projetilTorre torre) (projeteisInimigo inimigo)
}



-- Funções auxiliares

atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
atualizaProjeteis novoProjetil projeteisAtuais =
  case tipoProjetil novoProjetil of
    Fogo   -> atualizaFogo novoProjetil projeteisAtuais
    Gelo   -> atualizaGelo novoProjetil projeteisAtuais
    Resina -> atualizaResina novoProjetil projeteisAtuais


atualizaFogo :: Projetil -> [Projetil] -> [Projetil]
atualizaFogo novo projeteis =
  let
    projeteisSemGelo = filter (\p -> tipoProjetil p /= Gelo) projeteis
  in
    case find (\p -> tipoProjetil p == Resina) projeteisSemGelo of
      Just _  -> somaOuDobra Fogo novo projeteisSemGelo True -- Dobra duração do Fogo
      Nothing -> somaOuDobra Fogo novo projeteisSemGelo False


atualizaGelo :: Projetil -> [Projetil] -> [Projetil]
atualizaGelo novo projeteis =
  let
    projeteisSemFogo = filter (\p -> tipoProjetil p /= Fogo) projeteis
  in
    somaOuDobra Gelo novo projeteisSemFogo False


atualizaResina :: Projetil -> [Projetil] -> [Projetil]
atualizaResina novo projeteis =
  somaOuDobra Resina novo projeteis False


somaOuDobra :: TipoProjetil -> Projetil -> [Projetil] -> Bool -> [Projetil]
somaOuDobra tipo novo projeteis dobra =
  let
    duracaoNovo = case duracaoProjetil novo of
                    Finita t -> t
                    Infinita -> 0  -- Projéteis infinitos não somam duração
    (restantes, existentes) = partition (\p -> tipoProjetil p == tipo) projeteis
    duracaoExistente = sum [t | Projetil _ (Finita t) <- existentes]
    novaDuracao = if dobra then 2 * duracaoNovo else duracaoNovo + duracaoExistente
  in
    Projetil tipo (Finita novaDuracao) : restantes


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
