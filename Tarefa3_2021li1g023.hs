{- |
Module      : Tarefa3_2021li1g023
Description : Representação textual do jogo
Copyright   : Eduardo José Gonçalves dos Reis <a100819@alunos.uminho.pt>;
            : Inês Gonzalez Perdigão Marques <a100606@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g023 where

import LI12122

{-| Utilizando o "Show" conseguimos mostar a lista de pecas que, ao lhe darem um Mapa, torna todos os seus membros 
em string, mostrando-o graficamente. -}
instance Show Jogo where
  show (Jogo mapa jogador) = cortaLista jogador (graphic mapa)


{-| tamanhoString calcula o tamanho de uma string-}
tamanhoString :: String -> Int
tamanhoString [] = 0
tamanhoString (h:t)
  |h == '\n' = 0
  |otherwise  = 1 + tamanhoString t

{- visualizer é a funcao que percorre uma lista de pecas e transforma as pecas em string, sendo assim possivel
representar graficame o Mapa.-}
visualizer :: [Peca] -> String
visualizer [] = ""
visualizer (h:t) = case h of Vazio -> " " ++ visualizer t
                             Caixa -> "C" ++ visualizer t
                             Porta -> "P" ++ visualizer t
                             Bloco -> "X" ++ visualizer t

{-| graphic vai  separar a lista de string anteriorente calculada e vai partindo , sendo possivel uma melhor visualizacao
do mapa-}
graphic :: Mapa -> String
graphic [] = ""
graphic [mapa] = visualizer mapa
graphic (h:t) = visualizer h ++ "\n" ++ graphic t


{-| descobreJogador vai nos dar uma ideia de onde se encontra o Jogador dentro do mapa, calculando-o atraves da 
indicação das coordenadas iniciais.
(Dado o tamanh ode uma string, conseguimos chegar ao valor pretendido somando mais um valor (que representa o '\n' de cada
lista)-}


descobreJogador :: Coordenadas -> String -> Int
descobreJogador (x,y) [] = 0
descobreJogador (x,y) map = (tamanhoString map+1)*y + x


{-cortaLista vai "Cortar a lista", depois por consequente vai impor o Jogador dentro dessa mesma lista e finalmente
devolve o resto dessa mesma lista. Esta reparticao vai possibilitar encontrarmos um Jogador dentro do Mapa.
O Jogador vai poder estar a olhar tanto para Este como para Oeste, dando-lhes a sua "imagem" olhando para o lado proferido
NOTA : Depois da Tarefa5 ficamos a saber que não tinhamos conseguido desenhar a caixa em cima do jogador...dessa forma
conseguimos corrigir o problema-}

cortaLista :: Jogador -> String ->  String
cortaLista (Jogador (x,y) direction False )  map          -- tivemos de arranjar um map para inserir o jogador--
        |h /= ' '  = " Wrong position , try another one "    -- se a posicao do jogador for difrente da um vazio...o jogador nao pode estar la--
        |otherwise = take redundante map ++ player ++ drop (redundante+1)  map

  where (h:t) = drop redundante map
        redundante = descobreJogador (x,y) map  --head drop da listas do drop  redundante
        player = if direction == Oeste then "<" else ">"

cortaLista (Jogador (x,y) direction True) map  -- desenhar a Caixa na cabeça do Jogador
            |h /= ' ' || head /= ' ' = " Wrong position , try another one " 
            |otherwise = take top map ++ "C" ++ take (tamanhoString map) (drop (top+1) map) ++ player ++ drop (redundante+1) map
  
  
  where (h:t) = drop redundante map
        redundante = descobreJogador (x,y) map
        (head:tails) = drop top map --lista anterior a chegarmos à caixa
        top = redundante - ((tamanhoString map)+1) -- linha superior, onde a caixa se consegue encontrar
        player = if direction == Este then ">" else "<"
        --usamos o mesmo método de abrir o mapa a meio e colocar dentro dessa "brecha" a caixa