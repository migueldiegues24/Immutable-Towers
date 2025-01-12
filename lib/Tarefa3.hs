{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Miguel Rocha Diegues <a107361@alunos.uminho.pt>
              César António Fernandes Lopes <a109512@alunos.uminho.pt>


Módulo para a realização da Tarefa 3 de LI1 em 2024/25
-}
module Tarefa3 where

import LI12425


atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo tempo jogo =
  let
      jogoComPortaisAtualizados = atualizaPortais tempo jogo
      jogoComInimigosAtualizados = atualizaInimigos tempo jogoComPortaisAtualizados
      (torresAtualizadas, inimigosAtualizados) =
        atualizaTorres tempo (torresJogo jogoComInimigosAtualizados) (inimigosJogo jogoComInimigosAtualizados)
  in
      jogoComInimigosAtualizados
        { torresJogo = torresAtualizadas,
          inimigosJogo = inimigosAtualizados }




-- 3.3.1 Comportamento das Torres

atualizaTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
atualizaTorres tempo torres inimigos = foldr (atualizaTorre tempo) ([], inimigos) torres

atualizaTorre :: Tempo -> Torre -> ([Torre], [Inimigo]) -> ([Torre], [Inimigo])
atualizaTorre tempo torre (torresAtualizadas, inimigosVivos)
  | tempoTorre torre > 0 = 
      let torrePronta = torre { tempoTorre = max 0 (tempoTorre torre - tempo) }
      in (torrePronta : torresAtualizadas, inimigosVivos)
  | otherwise =
      let projeteis = disparaProjeteis torre inimigosVivos
          inimigosAtualizados = atualizaVidaInimigos inimigosVivos projeteis
          torreAtualizada = torre { tempoTorre = cicloTorre torre }
      in (torreAtualizada : torresAtualizadas, inimigosAtualizados)



-- 1. Detetar inimigos dentro do seu alcance

detetaInimigos :: Torre -> [Inimigo] -> [Inimigo]
detetaInimigos torre = filter (\inimigo -> distancia (posicaoTorre torre) (posicaoInimigo inimigo) <= alcanceTorre torre)

distancia :: Posicao -> Posicao -> Distancia
distancia (x1,y1) (x2,y2) = sqrt $ (x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)

-- 2. Escolher e disparar automaticamente projéteis contra os inimigos detetados

-- Dispara projéteis e retorna a lista de posições e danos
disparaProjeteis :: Torre -> [Inimigo] -> [(Posicao, Float)]
disparaProjeteis torre inimigos =
  let alvo = take (rajadaTorre torre) inimigos
      dano = danoTorre torre
  in map (\inimigo -> (posicaoInimigo inimigo, dano)) alvo


-- Atualiza a vida dos inimigos com base nos projéteis disparados
atualizaVidaInimigos :: [Inimigo] -> [(Posicao, Float)] -> [Inimigo]
atualizaVidaInimigos inimigos projeteis =
  map (atualizaVidaInimigo projeteis) inimigos

-- Atualiza a vida de um único inimigo
atualizaVidaInimigo :: [(Posicao, Float)] -> Inimigo -> Inimigo
atualizaVidaInimigo projeteis inimigo =
  case lookup (posicaoInimigo inimigo) projeteis of
    Just dano -> inimigo { vidaInimigo = max 0 (vidaInimigo inimigo - dano) }
    Nothing -> inimigo

--3.3.2

-- Atualiza todos os inimigos que estao no jogo

atualizaInimigos :: Tempo -> Jogo -> Jogo
atualizaInimigos tempo jogo =
    let 
        inimigosMovidos = moveInimigos tempo (inimigosJogo jogo) (mapaJogo jogo)
        (baseAtualizada, outrosInimigos) = verificaInimigosBase (baseJogo jogo) inimigosMovidos
        inimigosComEfeitos = efeitosInimigos tempo outrosInimigos
        inimigosFinal = removeInimigosMortos inimigosComEfeitos
        baseComCreditos = creditos baseAtualizada outrosInimigos
    in jogo { baseJogo = baseComCreditos, inimigosJogo = inimigosFinal }


-- Movimenta todos os inimigos do mapa
moveInimigos :: Tempo -> [Inimigo] -> Mapa -> [Inimigo]
moveInimigos tempo inimigos mapa = map (\inimigo -> moveInimigo tempo inimigo mapa) inimigos

moveInimigo :: Tempo -> Inimigo -> Mapa -> Inimigo
moveInimigo tempo inimigo mapa
    | efeitoGelo inimigo = inimigo -- Não se move se estiver congelado
    | otherwise =
        let velocidade = velocidadeResina inimigo
            distancia2 = velocidade * tempo
            posAtual = posicaoInimigo inimigo
            direcaoAtual = direcaoInimigo inimigo
            novaDirecao = calculaNovaDirecao posAtual direcaoAtual mapa
            novaPosicao = posicaoNova posAtual distancia2 novaDirecao
        in if posicaoValida mapa novaPosicao
           then inimigo { posicaoInimigo = novaPosicao, direcaoInimigo = novaDirecao }
           else inimigo -- Não altera nada se não puder se mover


calculaNovaDirecao :: Posicao -> Direcao -> Mapa -> Direcao
calculaNovaDirecao (x, y) direcaoAtual mapa =
    let 
        adjacentes = [(Norte, (x, y + 1)), (Sul, (x, y - 1)), (Este, (x + 1, y)), (Oeste, (x - 1, y))]
        validos = filter (\(_, (nx, ny)) -> posicaoValida mapa (nx, ny)) adjacentes
        progressivos = filter (\(dir, _) -> dir /= direcaoOposta direcaoAtual) validos
    in 
        case progressivos of
         ((novaDirecao, _):_) -> novaDirecao -- Prioriza a primeira direção válida e progressiva
         [] -> direcaoAtual -- Caso não haja direções progressivas, mantém a direção atual
  where
    direcaoOposta Norte = Sul
    direcaoOposta Sul   = Norte
    direcaoOposta Este  = Oeste
    direcaoOposta Oeste = Este


posicaoNova :: Posicao -> Float -> Direcao -> Posicao
posicaoNova (x, y) d Norte = (x, y + d)
posicaoNova (x, y) d Sul   = (x, y - d)
posicaoNova (x, y) d Este  = (x + d, y)
posicaoNova (x, y) d Oeste = (x - d, y)

-- Verifica se a posição é válida no mapa considerando apenas o terreno Terra
posicaoValida :: Mapa -> Posicao -> Bool
posicaoValida mapa (x, y)
    | posx < 0 || posy < 0 = False 
    | posx >= length (head mapa) || posy >= length mapa = False 
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


-- Processa inimigos que atingiram a base

verificaInimigosBase :: Base -> [Inimigo] -> (Base, [Inimigo])
verificaInimigosBase base [] = (base, [])
verificaInimigosBase base (inimigos:r)
  | atingiuBase inimigos base = let baseAtualizada = danoBase inimigos base
                         in verificaInimigosBase baseAtualizada r
  | otherwise = let (baseAtualizada, outrosInimigos) = verificaInimigosBase base r
                in (baseAtualizada, inimigos : outrosInimigos)


atingiuBase :: Inimigo -> Base -> Bool
atingiuBase inimigo base = posicaoInimigo inimigo == posicaoBase base


danoBase :: Inimigo -> Base -> Base
danoBase inimigo base = base { vidaBase = vidaBase base - ataqueInimigo inimigo }

-- Adiciona créditos à base quando os inimigos sao eliminados

creditos :: Base -> [Inimigo] -> Base
creditos base inimigosI =
    let eliminados = filter (\x -> vidaInimigo x <= 0) inimigosI
        total = sum (map butimInimigo eliminados)
    in base { creditosBase = creditosBase base + total }


-- Remove os inimigos eliminados
removeInimigosMortos :: [Inimigo] -> [Inimigo]
removeInimigosMortos inimigos = filter (\inimigo -> vidaInimigo inimigo > 0) inimigos


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