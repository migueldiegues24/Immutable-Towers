module Tarefa2Spec (testesTarefa2) where

import Test.HUnit
import Tarefa2
import LI12425

testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ "basic example test" ~: (2 :: Int) ~=? 1 + 1,
        "another basic example" ~: True ~=? not False
      ]

base1 = Base { posicaoBase = (10, 10), vidaBase = 50, creditosBase = 100 }
jogo1 = Jogo { baseJogo = base1, portaisJogo = [], torresJogo = [], mapaJogo = [], inimigosJogo = [], lojaJogo = []}

testesTerminouJogo :: Test
testesTerminouJogo =
    test
      [ "Jogo em andamento (inimigos ativos, vida positiva)" ~: False ~=? terminouJogo jogo1
        --"Jogo ganho (sem inimigos, vida positiva)" ~: True ~=? terminouJogo jogo2,
        --"Jogo perdido (vida base <= 0)" ~: True ~=? terminouJogo jogo3
      ]
