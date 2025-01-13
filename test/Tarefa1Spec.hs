
module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Tarefa1
import LI12425

torre1 = Torre {posicaoTorre = (0,0), danoTorre = 10, alcanceTorre = 5, rajadaTorre = 10, cicloTorre = 5, tempoTorre = 10, projetilTorre = Projetil { tipoProjetil = Fogo , duracaoProjetil = Infinita}}
torre2 = Torre {posicaoTorre = (1,1), danoTorre = 10, alcanceTorre = 5, rajadaTorre = 10, cicloTorre = 5, tempoTorre = 10, projetilTorre = Projetil { tipoProjetil = Fogo , duracaoProjetil = Infinita}}
inimigo1 = Inimigo {posicaoInimigo = (0,0), direcaoInimigo = Norte, vidaInimigo = 100, velocidadeInimigo = 10, ataqueInimigo = 10, butimInimigo = 20, projeteisInimigo = []}
inimigo2 = Inimigo {posicaoInimigo = (1,1), direcaoInimigo = Norte, vidaInimigo = 100, velocidadeInimigo = 10, ataqueInimigo = 10, butimInimigo = 20, projeteisInimigo = []}

testesTarefa1 :: Test
testesTarefa1 =
  TestLabel "Testes Tarefa 1" $
    test
      [ "basic example test" ~: (2 :: Int) ~=? 1 + 1,
        "another basic example" ~: True ~=? not False
      ]

-- Testes para a função 'possuiPortais'
testesPossuiPortais :: Test
testesPossuiPortais = 
    test
      [ "Portais presentes" ~: True ~=? possuiPortais [Portal (1, 1) []],
        "Sem portais" ~: False ~=? possuiPortais []
      ]

-- Testes para a função 'portaisSobreTerra'
testesPortaisSobreTerra :: Test
testesPortaisSobreTerra = 
    test
      [ "Portal em Terra" ~: True ~=? portaisSobreTerra [[Terra]] [Portal (0, 0) []],
        "Portal fora da Terra" ~: False ~=? portaisSobreTerra [[Agua]] [Portal (0, 0) []]
      ]

-- Testes para a função 'nenhumPortalSobrepoe'
testesNenhumPortalSobrepoe :: Test
testesNenhumPortalSobrepoe = 
    test
      [ "Portal separado de torre e base" ~: True ~=? nenhumPortalSobrepoe [Portal (1, 1) []] [torre1] (Base 100 (3, 3) 10),
        "Portal sobreposto à torre" ~: False ~=? nenhumPortalSobrepoe [Portal (1, 1) []] [torre2] (Base 100 (3, 3) 10)
      ]

-- Testes para a função 'verificaOndaPorPortal'
testesVerificaOndaPorPortal :: Test
testesVerificaOndaPorPortal = 
    test
      [ "Uma onda ativa por portal" ~: True ~=? verificaOndaPorPortal [Portal (1, 1) [Onda [] 0 0 0]],
        "Mais de uma onda ativa" ~: False ~=? verificaOndaPorPortal [Portal (1, 1) [Onda [] 0 0 0, Onda [] 0 0 0]]
      ]

-- Testes para a função 'verificaInimigosPorLancarPortais'
testesVerificaInimigosPorLancarPortais :: Test
testesVerificaInimigosPorLancarPortais =  
    test
      [ "Inimigos válidos" ~: True ~=? verificaInimigosPorLancarPortais [Portal (0, 0) [Onda [inimigo1] 0 0 0]],
        "Inimigo inválido" ~: False ~=? verificaInimigosPorLancarPortais [Portal (0, 0) [Onda [inimigo2] 0 0 0]]
      ]

-- Testes para a função 'inimigosSobreTerra'
testesInimigosSobreTerra :: Test
testesInimigosSobreTerra =  
    test
      [ "Inimigos em Terra" ~: True ~=? inimigosSobreTerra [[Terra]] [inimigo1],
        "Inimigos fora da Terra" ~: False ~=? inimigosSobreTerra [[Agua]] [inimigo1]
      ]

-- Testes para inimigosNaoSobrepoemTorres
testesInimigosNaoSobrepoemTorres :: Test
testesInimigosNaoSobrepoemTorres =
    test
      [ "inimigos e torres não sobrepostos" ~: True ~=? inimigosNaoSobrepoemTorres [inimigo1] [torre2],
        "inimigos e torres sobrepostos" ~: False ~=? inimigosNaoSobrepoemTorres [inimigo2] [torre2]
      ]

-- Testes para velocidadeDoInimigoNãoNegativa
testesVelocidadeDoInimigoNaoNegativa :: Test
testesVelocidadeDoInimigoNaoNegativa =
    test
      [ "velocidades não negativas" ~: True ~=? velocidadeDoInimigoNãoNegativa [inimigo1 , inimigo2],
        "velocidade negativa" ~: False ~=? velocidadeDoInimigoNãoNegativa  [ inimigo1, inimigo2] --erro 
      ]
{-
-- Testes para verificaProjeteisInimigos
testesVerificaProjeteisInimigos :: Test
testesVerificaProjeteisInimigos =
    test
      [ "sem projéteis conflitantes" ~: True ~=? verificaProjeteisInimigos [inimigo1]
      ]
-}

testesTorresSobreRelva :: Test
testesTorresSobreRelva =
    test
      [ "Todas as torres na relva" ~: True ~=? torresSobreRelva exemploMapa [torre1 , Torre (2, 2) 1 1 1 2 0 (Projetil Resina (Finita 5))],
        "Alguma torre fora da relva" ~: True ~=? torresSobreRelva exemploMapa [Torre (0, 1) 20 2 2 3 4 (Projetil Fogo (Finita 5)) , Torre (2, 1) 1 1 1 2 0 (Projetil Resina (Finita 5))]
      ]
  where
    exemploMapa =
      [ [Relva, Terra, Relva],
        [Relva, Terra, Relva],
        [Relva, Relva, Relva]
      ]
    
testesVerificaAlcanceTorres :: Test
testesVerificaAlcanceTorres =
    test
      [ "Todos os alcances positivos" ~: True ~=? verificaAlcanceTorres [torre1,torre2],
        "Algum alcance zero" ~: False ~=? verificaAlcanceTorres [torre1,torre2] -- failure
      ]

testesVerificaRajadaTorres :: Test
testesVerificaRajadaTorres =
    test
      [ "Todas as rajadas positivas" ~: True ~=? verificaRajadaTorres [torre1, torre2],
        "Alguma rajada negativa" ~: False ~=? verificaRajadaTorres [torre1] -- failure
      ]

testesVerificaCicloPositivo :: Test
testesVerificaCicloPositivo =
    test
      [ "Todos os ciclos positivos" ~: True ~=? verificaCicloPositivo [torre1,torre2],
        "Algum ciclo zero" ~: False ~=? verificaCicloPositivo [torre1,torre2] -- failure
      ]

testesVerificaTorresSobrepostas :: Test
testesVerificaTorresSobrepostas =
    test
      [ "Nenhuma torre sobreposta" ~: True ~=? verificaTorresSobrepostas [torre1,torre2],
        "Torres sobrepostas" ~: False ~=? verificaTorresSobrepostas [torre1,torre2] -- failure
      ]

testesVerificaBase :: Test
testesVerificaBase =
    test
      [ "Base sobre terra" ~: True ~=? verificaBase exemploMapa (1, 1),
        "Base fora da terra" ~: False ~=? verificaBase exemploMapa (0, 0)
      ]
  where
    exemploMapa =
      [ [Relva, Terra, Relva],
        [Relva, Terra, Relva],
        [Relva, Relva, Relva]
      ]
  
-- Testes para a função 'verificaCreditosBase'
testesVerificaCreditosBase :: Test
testesVerificaCreditosBase = 
    test
      [ "Base com créditos positivos" ~: True ~=? verificaCreditosBase (Base 100 (0, 0) 10),
        "Base sem créditos" ~: True ~=? verificaCreditosBase (Base 100 (0, 0) 0 )
      ]

testesBaseNaoSobreposta :: Test
testesBaseNaoSobreposta =
    test
      [ "Base não sobreposta a torres nem portais" ~: True ~=? baseNaoSobreposta exemploBase [] [],
        "Base sobreposta a uma torre" ~: False ~=? baseNaoSobreposta exemploBase [torre2] [],
        "Base sobreposta a um portal" ~: False ~=? baseNaoSobreposta exemploBase [] [Portal (0, 0) []] -- erro
      ]
  where
    exemploBase = Base 100 (1, 1) 10

todosOsTestes :: Test
todosOsTestes = TestList
  [testesPossuiPortais, 
  testesPortaisSobreTerra,
  testesNenhumPortalSobrepoe,
  testesVerificaOndaPorPortal, 
  testesVerificaInimigosPorLancarPortais,
  testesInimigosSobreTerra,
  testesInimigosNaoSobrepoemTorres,
  testesVelocidadeDoInimigoNaoNegativa,
  testesTorresSobreRelva,
  testesVerificaAlcanceTorres,
  testesVerificaRajadaTorres,
  testesVerificaCicloPositivo,
  testesVerificaTorresSobrepostas,
  testesVerificaBase,
  testesVerificaCreditosBase,
  testesBaseNaoSobreposta
 ]

