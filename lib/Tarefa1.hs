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
    validaInimigos jogo &&
    validaTorres jogo &&
    validaBase jogo


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
portaisSobreTerra :: Mapa -> [Portal] -> Bool
portaisSobreTerra mapa portais = 
    all (\portal -> posicaoTerra mapa (posicaoPortal portal)) portais


-- c)
-- Verifica se existe pelo menos um caminho de Terra ligando um portal à Base





-- d)
-- Verifica se os portais não se sobrepõe a Torres ou à Base

nenhumPortalSobrepoe :: [Portal] -> [Torre] -> Base -> Bool
nenhumPortalSobrepoe [] _ _ = True
nenhumPortalSobrepoe (h:t) torres base = if portalNaoSobrepoe h torres base then nenhumPortalSobrepoe t torres base else False

-- Verifica se um portal não se sobrepõe a Torres ou à Base

portalNaoSobrepoe :: Portal -> [Torre] -> Base -> Bool
portalNaoSobrepoe portal torres base =
    let posPortal = posicaoPortal portal
    -- Função auxiliar lambda que verifica se o Portal está sobreposto a alguma torre (percorre a lista de torres)
    in not (any (\t -> posicaoTorre t == posPortal) torres || posicaoBase base == posPortal)


-- e)
-- Verifica que só há no máximo uma onda ativa por portal

verificaOnda :: [Onda] -> Bool
verificaOnda lista = not (length (filter entradaOndaIguala0 lista) > 1)

entradaOndaIguala0 :: Onda -> Bool
entradaOndaIguala0 (Onda _ _ _ entrada) = entrada == 0




-- 2 (Inimigos)
-- Função que verifica se todas as alíneas forem válidas, esta também o é

validaInimigos :: Jogo -> Bool
validaInimigos jogo =
    let
        portais = portaisJogo jogo
        inimigos = inimigosJogo jogo
        mapa = mapaJogo jogo
        torres = torresJogo jogo
    in
        verificaInimigosPorLancarPortais portais &&
        inimigosSobreTerra mapa inimigos &&
        inimigosNaoSobrepoemTorres inimigos torres &&
        velocidadeDoInimigoNãoNegativa inimigos &&
        verificaProjeteisInimigos inimigos





-- a)
-- Verifica se Todos os inimigos por lançar têm a posição do respetivo portal, nível de vida positivo, e lista de projéteis ativos vazia

-- Função que verifica para todos os portais
verificaInimigosPorLancarPortais :: [Portal] -> Bool
verificaInimigosPorLancarPortais portais = all verificaPortal portais
  where
    verificaPortal :: Portal -> Bool
    verificaPortal portal = 
        verificaInimigosPorLancar (concatMap inimigosOnda (ondasPortal portal)) portal


verificaInimigosPorLancar :: [Inimigo] -> Portal -> Bool
verificaInimigosPorLancar inimigos portal =
    let posPortal = posicaoPortal portal
    in all (verificaInimigo posPortal) inimigos
  where
    -- Verifica se um inimigo satisfaz as condições
    verificaInimigo :: Posicao -> Inimigo -> Bool
    verificaInimigo posPortal inimigo =
        posicaoInimigo inimigo == posPortal &&  -- Coincide com o portal
        vidaInimigo inimigo > 0 &&             -- Vida positiva
        null (projeteisInimigo inimigo)        -- Sem projéteis ativos




-- b)
-- Verifica se os inimigos em jogo encontram-se sobre Terra

inimigosSobreTerra :: Mapa -> [Inimigo] -> Bool
inimigosSobreTerra mapa inimigos = all (verificaInimigoSobreTerra mapa) inimigos
  where
    -- Função auxiliar que verifica se o inimigo está sobre terra
    verificaInimigoSobreTerra :: Mapa -> Inimigo -> Bool
    verificaInimigoSobreTerra m inimigo = posicaoTerra m (posicaoInimigo inimigo)



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

verificaProjeteisInimigos :: [Inimigo] -> Bool
verificaProjeteisInimigos inimigos =
 all (\inimigo -> verificaprojeteisNormalizados (projeteisInimigo inimigo)) inimigos

-- Função auxiliar para verificar se um inimigo é valido
verificaprojeteisNormalizados :: [Projetil] -> Bool
verificaprojeteisNormalizados projeteis =
  let tipos = map tipoProjetil projeteis
  in noDuplicates tipos && not (nemFogoeResinaNemFogoeGelo tipos)

-- Função auxiliar para verificar se não há duplicados na lista de tipos de projéteis
noDuplicates :: Eq a => [a] -> Bool
noDuplicates [] = True
noDuplicates (x:xs) = notElem x xs && noDuplicates xs

-- Função auxiliar para verificar se há tipos combinações de fogo e resina ou fogo e gelo na lista
nemFogoeResinaNemFogoeGelo :: [TipoProjetil] -> Bool
nemFogoeResinaNemFogoeGelo tipos =
  (elem Fogo tipos && elem Resina tipos) || 
  (elem Fogo tipos && elem Gelo tipos)



-- 3 (Torres)
-- Função que verifica se todas as alíneas forem válidas, esta também o é

validaTorres :: Jogo -> Bool
validaTorres jogo =
    let
        mapa = mapaJogo jogo
        torres = torresJogo jogo
    in
        torresSobreRelva mapa torres &&
        verificaAlcanceTorres torres &&
        verificaRajadaTorres torres &&
        verificaCicloPositivo torres &&
        verificaTorresSobrepostas torres


-- a)
-- Verifica se as Torres estão posicionadas na relva

torresSobreRelva :: Mapa -> [Torre] -> Bool
torresSobreRelva _ [] = True
torresSobreRelva mapa (t:ts) = if posicaoRelva mapa (posicaoTorre t) then torresSobreRelva mapa ts else False


-- b)
-- Verifica se o alcance das torres é um valor positivo

verificaAlcanceTorres :: [Torre] -> Bool
verificaAlcanceTorres torres =
    all (\torre -> alcanceTorre torre > 0) torres


-- c)
-- Verifica se a rajada é superior a 0

verificaRajadaTorres :: [Torre] -> Bool
verificaRajadaTorres torres =
    all (\torre -> rajadaTorre torre > 0) torres

-- d)
-- Verifica se o ciclo das torres é finito e superior a 0


verificaCicloPositivo :: [Torre] -> Bool
verificaCicloPositivo torres =
    all (\x -> cicloTorre x > 0) torres

-- FALTA IMPLEMENTAR FUNÇÃO PARA VERIFICAR SE É FINITO


-- e)
-- Verifica se torres não estão sobrepostas

verificaTorresSobrepostas :: [Torre] -> Bool
verificaTorresSobrepostas [] = True
verificaTorresSobrepostas (t:ts) =
    -- Verifica se a primeira torre da lista não tem posição igual a mais nenhuma, depois itera pelas próximas
    all (\torre -> posicaoTorre t /= posicaoTorre torre) ts && verificaTorresSobrepostas ts


-- 4 (Base)
-- Função que verifica se todas as alíneas forem válidas, esta também o é

validaBase :: Jogo -> Bool
validaBase jogo =
    let base = baseJogo jogo
        mapa = mapaJogo jogo
        torres = torresJogo jogo
        portais = portaisJogo jogo
    in verificaBase mapa (posicaoBase base) &&
       verificaCreditosBase base &&
       baseNaoSobreposta base torres portais



-- a)
-- Verifica se a base está sobre Terra

verificaBase :: Mapa -> Posicao -> Bool
verificaBase mapa (x,y) = posicaoTerra mapa (x,y)


-- b)
-- Verifica se a base não tem crédito negativo

verificaCreditosBase :: Base -> Bool
verificaCreditosBase base = creditosBase base >= 0

-- c)
-- Verifica se a base não está sobreposta a uma torre ou a um portal

baseNaoSobreposta :: Base -> [Torre] -> [Portal] -> Bool
baseNaoSobreposta base torres portais =
    let posBase = posicaoBase base
        baseNaoSobreTorres = not (any (\t -> posicaoTorre t == posBase) torres)
        baseNaoSobrePortais = not (any (\p -> posicaoPortal p == posBase) portais)
    in baseNaoSobreTorres && baseNaoSobrePortais

-- Funções Auxiliares 

-- Função auxiliar que verifica se uma certa posição é Terra
posicaoTerra :: Mapa -> Posicao -> Bool
posicaoTerra mapa (x,y)
    | (mapa !! floor y) !! floor x == Terra = True
    | otherwise = False


-- Função auxiliar para verificar se uma certa posição é Relva

posicaoRelva :: Mapa -> Posicao -> Bool
posicaoRelva mapa (x,y)
    | (mapa !! floor y) !! floor x == Relva = True
    | otherwise = False


-- Função auxiliar para igualar a posição ao seu indice na matriz (passar de posição no mapa para a posição na matriz)

ajustaPosicao :: Posicao -> (Float, Float) 
ajustaPosicao (x, y) = (fromIntegral (floor x), fromIntegral (floor y))


