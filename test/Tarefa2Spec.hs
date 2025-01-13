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
base2 = Base { posicaoBase = (10, 10), vidaBase = 0, creditosBase = 100 }

jogo1 = Jogo { baseJogo = base1, portaisJogo = [], torresJogo = [], mapaJogo = [], inimigosJogo = [inimigo1], lojaJogo = []}
jogo2 = Jogo { baseJogo = base1, portaisJogo = [], torresJogo = [], mapaJogo = [], inimigosJogo = [], lojaJogo = []}
jogo3 = Jogo { baseJogo = base1, portaisJogo = [], torresJogo = [], mapaJogo = [], inimigosJogo = [], lojaJogo = []}

inimigo1 = Inimigo {posicaoInimigo = (0,0), direcaoInimigo = Norte, vidaInimigo = 100, velocidadeInimigo = 10, ataqueInimigo = 10, butimInimigo = 20, projeteisInimigo = []}

testesTerminouJogo :: Test
testesTerminouJogo =
    test
      [ "Jogo em andamento (inimigos ativos, vida positiva)" ~: False ~=? terminouJogo jogo1,
        "Jogo ganho (sem inimigos, vida positiva)" ~: True ~=? terminouJogo jogo2,
        "Jogo perdido (vida base <= 0)" ~: True ~=? terminouJogo jogo3
      ]



testesAtivaInimigo :: Test
testesAtivaInimigo =
    test
      [ "Sem ondas no portal" ~: (portal1, []) ~=? ativaInimigo portal1 [] 
      ]

portal1 = Portal { posicaoPortal = (3, 3),ondasPortal = [] }