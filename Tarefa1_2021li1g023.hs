{- |
Module      : Tarefa5_2021li1g023
Description : Validação de um potencial mapa
Copyright   : Eduardo José Gonçalves dos Reis <a100819@alunos.uminho.pt>;
            : Inês Gonzalez Perdigão Marques <a100606@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}
module Tarefa5_2021li1g023 where

import LI12122
{-| validaPotencialMapa vai validar todos os fatores que englobam um mapa,tentando provar que este existe
-}
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa l@((piece,(x,y)):t) = contaPorta l == 1  && 
                                        (x,y) `notElem` listaCoords t  &&
                                        Vazio /= piece || maximum (devolveXs l) * maximum (devolveYs l) == length l &&  caixaFlutua l 
                                         
    
{-|Função lista coordenadas para  serem comparardos -} 
listaCoords :: [(Peca,Coordenadas)] -> [Coordenadas]
listaCoords [] = []                                                     
listaCoords ((piece,(x,y)): t) = (x,y) : listaCoords t

{-|Vê se uma caixa tem um bloco em baixo -}
caixaFlutua :: [(Peca, Coordenadas)] -> Bool
caixaFlutua [] = False
caixaFlutua l@((piece,(x,y)): t) = (Caixa, (x,y)) `elem` l && (Bloco, (x,y+1)) `elem` l    


{-| contaPorta é uma funcao que recebe uma lista de pecas e coordenadas e devolve o numero de portas existentes nessa mesma lista.-}
contaPorta :: [(Peca,Coordenadas)] -> Int
contaPorta [] = 0
contaPorta ((piece,(x,y)):t)                        
    |piece == Porta = 1 + contaPorta t
    |otherwise = contaPorta t

{-|devolveXs devolve todas as coordenadas x existentes numa lista com pecas e coordenadas (vai ser utilizada para contar os vazios de uma mapa) -}
devolveXs :: [(Peca,Coordenadas)] -> [Int] 
devolveXs [] = []                                                 
devolveXs ((piece,(x,y)): t) = x : devolveXs t
{-|devolveYs devolve todas as coordenadas y existentes numa lista com pecas e coordenadas (também vai ser utilizada na contabilização dos vazios) -}
devolveYs :: [(Peca,Coordenadas)] -> [Int]
devolveYs [] = []                                                           
devolveYs ((piece,(x,y)): t) = y : devolveYs t

