{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Miguel Rocha Diegues <a107361@alunos.uminho.pt>
              César António Fernandes Lopes <a109512@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

-- Função que verifica se as 4 Validações são válidas, em caso afirmativo o jogo é válido
validaJogo :: Jogo -> Bool
validaJogo jogo =
    let portais = portaisJogo jogo
    in possuiPortais portais -- Temporário, depois mudar para função validaPortais



-- 1 (Portais)
-- Função que verifica se todas as alíneas forem válidas, esta também o é

-- Implementar função validaPortais


-- a)
-- Verifica a existencia de portais
possuiPortais :: [Portal] -> Bool
possuiPortais [] = False
possuiPortais _ = True



-- b)
-- Verifica se os portais estão sobre terra
portaisSobreTerra :: Jogo -> Bool
portaisSobreTerra jogo = all (posicaoTerra mapaJ) (map posicaoPortal (portaisJogo jogo))
    where
        mapaJ = mapaJogo jogo

-- Função auxiliar que verifica se uma certa posição é Terra
posicaoTerra :: Mapa -> Posicao -> Bool
posicaoTerra mapa (x,y)
    | (mapa !! (round y)) !! (round x) == Terra = True
    | otherwise = False


-- c)
-- Verifica se existe pelo menos um caminho de Terra ligando um portal à Base


-- d)
-- Verifica se os portais não se sobrepõe a Torres ou à Base
portalNaoSobrepoe :: Portal -> [Torre] -> Base -> Bool
portalNaoSobrepoe portal torres base =
    let posPortal = posicaoPortal portal
    -- Função auxiliar lambda que verifica se o Portal está sobreposto a alguma torre (percorre a lista de torres)
    in not (any (\t -> posicaoTorre t == posPortal) torres || posicaoBase base == posPortal)


-- e)
-- Verifica que só há no máximo uma onda ativa por portal




-- 2 (Inimigos)
-- Função que verifica se todas as alíneas forem válidas, esta também o é

-- Implementar função validaInimigos


-- a)
-- Verifica se Todos os inimigos por lançar têm a posição do respetivo portal, nível de vida positivo, e lista de projéteis ativos vazia



-- b)
-- Verifica se os inimigos em jogo encontram-se sobre Terra

inimigosSobreTerra :: Mapa -> [Inimigo] -> Bool
inimigosSobreTerra mapa inimigos = all (verificaInimigoSobreTerra mapa) inimigos
  where
    -- Função auxiliar que verifica se o inimigo está sobre terra
    verificaInimigoSobreTerra :: Mapa -> Inimigo -> Bool
    verificaInimigoSobreTerra m inimigo = posicoesAoRedorSaoTerra m (posicaoInimigo inimigo)


-- Função auxiliar que verifica se os blocos ao redor são Terra
-- Se ele tiver num bloco (x.5,y) significa que o bloco (x-0.5,y) e o bloco (x+0.5,y) têm que ser terra, o mesmo funciona para y

posicoesAoRedorSaoTerra :: Mapa -> Posicao -> Bool
posicoesAoRedorSaoTerra mapa (x,y)
    | temVirgula5 x && temVirgula5 y = if posicaoTerra mapa (x-0.5,y-0.5) && posicaoTerra mapa (x+0.5,y+0.5) then True else False
    | temVirgula5 x = if posicaoTerra mapa (x-0.5,y) && posicaoTerra mapa (x+0.5,y) then True else False
    | temVirgula5 y = if posicaoTerra mapa (x,y-0.5) && posicaoTerra mapa (x,y + 0.5) then True else False
    | otherwise = if posicaoTerra mapa (x,y) == True then True else False

-- Função auxiliar que verifica se a posicao é entre dois ou quatro blocos, ou apenas num

temVirgula5 :: Float -> Bool
temVirgula5 x = (x - fromIntegral (round x :: Int)) == 0.5


-- c)
-- Verifica se os inimigos não estão sobrepostos a Torres

inimigosNaoSobrepoemTorres :: [Inimigo] -> [Torre] -> Bool
inimigosNaoSobrepoemTorres inimigos torres =
    all (\inimigo -> inimigoNaoSobrepoeTorres inimigo torres) inimigos


inimigoNaoSobrepoeTorres :: Inimigo -> [Torre] -> Bool
inimigoNaoSobrepoeTorres inimigo torres =
    let posInimigo = posicaoInimigo inimigo
    -- Função auxiliar lambda que verifica se o Inimigo está sobreposto a alguma torre (percorre a lista de torres)
    in not (any (\t -> posicaoTorre t == posInimigo) torres)

-- d)
-- Verifica se a velocidade não é negativa

velocidadeDoInimigoNãoNegativa :: [Inimigo] -> Bool
velocidadeDoInimigoNãoNegativa inimigos =
    all (\inimigo -> velocidadeInimigo inimigo >= 0) inimigos

-- e)
-- Verifica se a lista de projéteis ativas, não contêm mais do que um projétil do mesmo tipo
-- Nem pode conter simultaneamente, projéteis do tipo Fogo e Resina, nem Fogo e Gelo