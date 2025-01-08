{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Miguel Rocha Diegues <a107361@alunos.uminho.pt>
              César António Fernandes Lopes <a109512@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25
-}
module Tarefa3 where

import LI12425

{-
atualizaJogo :: Tempo   -> Jogo -> Jogo
atualizaJogo tempo jogo@(Jogo base portais torres mapa inimigos loja) = 
    let (torresAtualizadas, inimigosAtualizados) = atualizaTeI
        movInimigos = inimigosAtualizados tempo inimigosAtualizados mapa
    in jogo { torresJogo = torresAtualizadas, inimigosJogo = movInimigos}

-}
{-}
atualizaTeI :: Tempo -> [Torre] -> [Inimigo] -> ([Torre],[Inimigo])
atualizaTeI tempo torres inimigos = foldr (atualizaTorre tempo) ([],inimigos) torres

atualizaTorre :: Tempo -> Torre -> ([Torre],[Inimigo]) -> ([Torre],[Inimigo])
atualizaTorre tempo torre (torresAtualizadas, inimigosVivos) | tempoTorre torre > 0 = 
                                                                let torrePronta = torre { tempoTorre = max 0 (tempoTorre torre - tempo)}
                                                                in (torrePronta : torresAtualizadas, inimigosVivos)
                                                             | otherwise = 
                                                                let inimigosAlvo = detetaInimigos torre inimigosVivos
                                                                    (inimigosAtingidos, projetil) = disparaProjeteis torre inimigosAlvo
                                                                    inimigosAtualizados = atualizaVidaInimigos inimigosVivos inimigosAtingidos projetil
                                                                    torreAtualizada = torre { tempoTorre = cicloTorre torre }
                                                                in (torreAtualizada : torresAtualizadas, inimigosAtualizados)
-}

-- 3.3.1 Comportamento das Torres


-- 1. Detetar inimigos dentro do seu alcance

detetaInimigos :: Torre -> [Inimigo] -> [Inimigo]
detetaInimigos torre = filter (\inimigo -> distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre)

distancia :: Posicao -> Posicao -> Distancia
distancia (x1,y1) (x2,y2) = sqrt $ (x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)

-- 2. Escolher e disparar automaticamente projéteis contra os inimigos detetados

disparaProjeteis :: Torre -> [Inimigo] -> ([Inimigo],[Projetil])
disparaProjeteis torre inimigos = 
  let alvo = take (rajadaTorre torre) inimigos
      dano = replicate (length alvo) (projetilTorre torre)
  in (alvo,dano)

{-
atualizaVidaInimigos :: [Inimigo] -> [Inimigo] -> [Projetil] -> [Inimigo]
atualizaVidaInimigos inimigos alvo dano = map (atualizaVidaInimigo alvo dano) inimigos

atualizaVidaInimigo :: [Inimigo] -> [Projetil] -> Inimigo -> Inimigo
atualizaVidaInimigo alvo dano inimigo = 
  case lookup (posicaoInimigo inimigo) (zip (map posicaoInimigo alvo) dano) of 
    Just danoSofrido -> inimigo {vidaInimigo = max 0 (vidaInimigo inimigo - danoSofrido)}
    Nothing -> inimigo
-}

--3.3.2

-- Atualiza todos os inimigos que estao no jogo
{-
inimigosAtualizados :: Tempo -> Jogo -> Jogo
inimigosAtualizados tempo jogo =
    let 
        inimigosMovidos = moveInimigos tempo (inimigosJogo jogo) (mapaJogo jogo)
        (baseAtualizada, outrosInimigos) = verificaInimigosBase (baseJogo jogo) inimigosMovidos
        inimigosComEfeitos = efeitosInimigos tempo outrosInimigos
        inimigosFinal = removeInimigosMortos inimigosComEfeitos
        baseComCreditos = creditos baseAtualizada outrosInimigos inimigosFinal
    in jogo { baseJogo = baseComCreditos, inimigosJogo = inimigosFinal }
-}

-- Movimenta todos os inimigos do mapa
moveInimigos :: Tempo -> [Inimigo] -> Mapa -> [Inimigo]
moveInimigos tempo inimigos mapa = map (\inimigo -> moveInimigo tempo inimigo mapa) inimigos

-- Movimenta apenas um inimigo
moveInimigo :: Tempo -> Inimigo -> Mapa -> Inimigo
moveInimigo tempo inimigo mapa | efeitoGelo inimigo = inimigo -- Não se move se estiver congelado
                               | otherwise =
      let velocidade = velocidadeResina inimigo
          distancia2 = velocidade * tempo
          novaPosicao = posicaoNova (posicaoInimigo inimigo) distancia2 (direcaoInimigo inimigo)
      in if posicaoValida mapa novaPosicao
         then inimigo { posicaoInimigo = novaPosicao }
         else inimigo -- Não move se a nova posição for inválida

-- 
posicaoNova :: Posicao -> Float -> Direcao -> Posicao
posicaoNova (x, y) d Norte = (x, y - d)
posicaoNova (x, y) d Sul   = (x, y + d)
posicaoNova (x, y) d Este  = (x + d, y)
posicaoNova (x, y) d Oeste = (x - d, y)

-- Verifica se a posição é válida no mapa considreando apenas o terreno "Terra"
posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida mapa (x, y) | posy < 0 || posy < 0 = False 
                          | posx >= length mapa || posy >= length (head mapa) = False 
                          | otherwise = mapa !! posy !! posx == Terra 
                            where
                              posx = floor x
                              posy = floor y

-- Verifica se o inimigo está sobre o efeito de gelo 
efeitoGelo :: Inimigo -> Bool
efeitoGelo inimigo = verificaGelo (projeteisInimigo inimigo)
   where verificaGelo[] = False
         verificaGelo (x:xs) | tipoProjetil x == Gelo = True
                             | otherwise = verificaGelo xs

-- Verifica se há algum projétil do tipo Resina
torreResina :: [Projetil] -> Bool
torreResina [] = False
torreResina (x:xs) | tipoProjetil x == Resina = True
                   | otherwise = torreResina xs

-- Calcula a velocidade do inimigo quando afetado pelo projetil de resina
velocidadeResina :: Inimigo -> Float
velocidadeResina inimigo | torreResina (projeteisInimigo inimigo) = 0.7 
                         | otherwise = 1.0 

-- Aplica os efeitos dos projetei sobre os inimigos
efeitosInimigos :: Tempo -> [Inimigo] -> [Inimigo]
efeitosInimigos _ [] = [] -- Não há inimigos para processar
efeitosInimigos tempo (inimigo:r) =
    let inimigoAtualizado = efeitosInimigo tempo inimigo
        outrosInimigos = efeitosInimigos tempo r
    in inimigoAtualizado : outrosInimigos

-- Aplica os efeitos a um unico inimigo (Fogo)
efeitosInimigo :: Tempo -> Inimigo -> Inimigo
efeitosInimigo tempo inimigo =
    let 
        fogo = danoFogo tempo (projeteisInimigo inimigo)
        vidaAtualizada = max 0 (vidaInimigo inimigo - fogo)
        projeteisAtualizados = atualizaDuracaoProjetil tempo (projeteisInimigo inimigo)
    in 
        inimigo { vidaInimigo = vidaAtualizada, projeteisInimigo = projeteisAtualizados }

-- Calcula o dano contínuo de Fogo
danoFogo :: Tempo -> [Projetil] -> Float
danoFogo _ [] = 0 
danoFogo tempo (x:xs) | tipoProjetil x == Fogo = 5.0 * tempo + danoFogo tempo xs
                         | otherwise = danoFogo tempo xs

-- Atualiza a duração dos projéteis nos inimigos

atualizaDuracaoProjetil :: Tempo -> [Projetil] -> [Projetil]
atualizaDuracaoProjetil _ [] = [] 
atualizaDuracaoProjetil tempo (x:xs) =
    let duracaoAtualizada = case duracaoProjetil x of
                              Finita d -> if d - tempo > 0 then Finita (d - tempo) else Finita 0
                              Infinita -> Infinita
        restoProjeteis = atualizaDuracaoProjetil tempo xs
    in if duracaoAtualizada == Finita 0
       then restoProjeteis 
       else x { duracaoProjetil = duracaoAtualizada } : restoProjeteis

-- Verifica se um inimigo conseguiu chegar a base
baseAlcancada :: Inimigo -> Base -> Bool
baseAlcancada inimigo base = posicaoInimigo inimigo == posicaoBase base

-- Processa inimigos que atingiram a base
{-
verificaInimigosBase :: Base -> [Inimigo] -> (Base, [Inimigo])
verificaInimigosBase base [] = (base, [])
verificaInimigosBase base (inimigos:r)
  | atingiuBase i base = let baseAtualizada = danoBase inimigos base
                         in verificaInimigosBase baseAtualizada r
  | otherwise = let (baseAtualizada, outrosInimigos) = verificaInimigosBase base r
                in (baseAtualizada, inimigos : outrosInimigos)
-}

danoBase :: Inimigo -> Base -> Base
danoBase inimigo base = base { vidaBase = vidaBase base - ataqueInimigo inimigo }

-- Adiciona créditos à base quando os inimigos sao eliminados
{-
creditos :: Base -> [Inimigo] -> [Inimigo] -> Base
creditos base inimigosI inimigosF =
    let eliminados = filter (\x -> vidaInimigo x <= 0) inimigosI
        total = sum (map butimInimigo eliminados)
    in base { creditosBase = creditos base + total }
-}
-- Remove os inimigos eliminados
removeInimigos :: [Inimigo] -> [Inimigo]
removeInimigos inimigos = filter (\inimigo -> vidaInimigo inimigo > 0) inimigos


-- 3.3.3 Comportamento dos Portais


-- Função principal que atualiza os portais do Jogo

atualizaPortais :: Tempo -> Jogo -> Jogo
atualizaPortais tempo jogo = jogo { 
    portaisJogo = novosPortais,
    inimigosJogo = inimigosAtuais ++ concat novosInimigos
  }
  where
    (novosPortais, novosInimigos) = unzip $ map (atualizaPortal tempo) (portaisJogo jogo)
    inimigosAtuais = inimigosJogo jogo

-- Função auxiliar que atualiza um portal

atualizaPortal :: Tempo -> Portal -> (Portal, [Inimigo])
atualizaPortal tempo portal = 
    let ondasAtualizadas = atualizaOndas tempo (ondasPortal portal)
        (novasOndas, novosInimigos) = processaOndas ondasAtualizadas
    in (portal { ondasPortal = novasOndas }, novosInimigos)


-- Funções auxiliares

-- Atualiza o tempo das ondas

atualizaOndas :: Tempo -> [Onda] -> [Onda]
atualizaOndas tempo = map atualizaOnda
  where
    atualizaOnda onda
      | entradaOnda onda > 0 = onda { entradaOnda = entradaOnda onda - tempo }
      | tempoOnda onda > 0 = onda { tempoOnda = max 0 (tempoOnda onda - tempo) }
      | otherwise = onda

-- Processa as ondas para lançar inimigos

processaOndas :: [Onda] -> ([Onda], [Inimigo])
processaOndas [] = ([], [])
processaOndas (onda:resto)
  | entradaOnda onda > 0 || null (inimigosOnda onda) = 
      let (restoOndas, novosInimigos) = processaOndas resto
      in (onda : restoOndas, novosInimigos)
  | tempoOnda onda <= 0 = 
      let inimigoLançado = head (inimigosOnda onda)
          ondaAtualizada = onda { inimigosOnda = tail (inimigosOnda onda), tempoOnda = cicloOnda onda }
          (restoOndas, novosInimigos) = processaOndas resto
      in (ondaAtualizada : restoOndas, inimigoLançado : novosInimigos)
  | otherwise = 
      let (restoOndas, novosInimigos) = processaOndas resto
      in (onda : restoOndas, novosInimigos)
