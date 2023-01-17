{- |
Module      : Tarefa6_2021li1g023
Description : Resolução de um puzzle
Copyright   : Eduardo José Gonçalves dos Reis <a100819@alunos.uminho.pt>;
            : Inês Gonzalez Perdigão Marques <a100606@alunos.uminho.pt>;

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2021/22.
-}

module Tarefa6_2021li1g023 where

import LI12122
import Tarefa4_2021li1g023


data Tree a = Leaf a | Node [Tree a]


resolveJogo :: Int -> Jogo -> Maybe [Movimento]
resolveJogo mov jogo  = undefined

{- I want this to simply run the "test"-}
bot :: Jogo -> [Movimento] -> Jogo
bot map jogador =undefined


{-Doing it with Trees-}

--correr todos os movimentos possiveis--

dataTree ::Tree Movimento   -> Tree [Movimento]   
dataTree _ = undefined
























{-podiamos usar o correr movimentos ou algo parecido...mas primeiro, identifciamos os obstaculos e vamos diretamente
a procura da sua soluçao...parece-me ser a melhor ideia-
divide tudo em mini casos. pode ser mais facil tratar de um caso de cada vez-}
{-
entao (daı́ o Maybe no resultado) resolver um jogo num número máximo
de movimentos. Resolver um jogo consiste em encontrar uma sequência de
movimentos que o jogador pode realizar para chegar à porta. Sempre que
não for possı́vel encontrar uma sequência de movimentos vencedora den-
tro do limite de movimentos máximo imposto, a função deverá avaliar em
Nothing.

temos que encontrar o numero maximo de movimentos possiveis
 com as guardas podes fazer um bot que tente primeiro andar para a frente. neles todos, caso se acuse que o 
 rapaz encontra uma impossibilidade ele deve voltar a estaca zero. Estaca zero sera as coordendas iniciais do jogador


1.fazer uma funcao que localiza os objetos no mapa (primeiro localiza a porta e o jogador)
2.fazer funcao que ve se existem impossibilidades pelo caminho (ou seja, paredes com 2 blocos precisam de uma caixa)
3.correr todos os movimentos possiveis  
4.BASICAMENTE VÊ TODOS OS CASOS
-}
