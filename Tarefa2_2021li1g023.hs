{- |
Module      : Tarefa2_2021li1g023
Description : Construção/Desconstrução do mapa
Copyright   : Eduardo José Gonçalves dos Reis <a100819@alunos.uminho.pt>;
            : Inês Gonzalez Perdigão Marques <a100606@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g023 where

import LI12122

{- |constroiMapa vai ser uma funcao que  cria um mapa, tentado mostrar todas as peças numa lista de Pecas,
utilizando cutter que vai formar o Mapa pegando numa lista de Pecas e Coordendas e, ao ordenar consegue formar um mapa
, ou seja, forma uma lista de uma Lista de pecas (um Mapa). -}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa lista = cutter (maximoX (listX lista)+1) (verPecas coordenadas (ordenaPecas [] lista)) where
    coordenadas = ordenaLista (criarTotalCoords (ordenaPecas [] lista)) 
                            

{-| Parte a lista de Pecas em subconjuntos, formando assim um Mapa-}
cutter :: Int -> [Peca] -> Mapa     
cutter n [] = []                                                                
cutter n list = take n list : cutter n (drop n list)


{- | 
"verPecas" compara a lista de Coordenadas com a lista de pecas e coordenadas. A sua função auxiliar "verificaElem" vai verificar 
se as coordenadas sao iguais a lista de Pecas e Coordenadas. Caso se verificar vamos colocar a peca da respetiva coordenada. Caso
a coordenada nao consta ter uma peca associada, esta vai dar um Vazio à lista que será criada-}  

verPecas ::[Coordenadas] -> [(Peca,Coordenadas)] -> [Peca]
verPecas l1 [] = [] 
verPecas [] l2 = []
verPecas (head:tail)  ((piece,(x,y)):t)
    |verificaElem head ((piece,(x,y)):t)  = piece : verPecas tail t
    |otherwise = Vazio : verPecas tail ((piece,(x,y)):t) where
        
   verificaElem :: Coordenadas -> [(Peca,Coordenadas)] -> Bool
   verificaElem _ [] = False
   verificaElem head ((piece,coord):t)
       |head == coord = True
       |otherwise = verificaElem head t
 

{- | 
Lista por compreensao que cria a lista de todas as coordenadas possiveis
-}

criarTotalCoords :: [(Peca,Coordenadas)]  -> [Coordenadas]
criarTotalCoords [] = []
criarTotalCoords ((piece,(x,y)):t) = [ (x,y) | x <- [0..list1] , y <- [0..list2]] where
        (list1,list2) = (maximum (listX ((piece,(x,y)): t)),maximum (listY ((piece,(x,y)): t)))


{- | ordenaPecas é uma funcção peculiar que, em suma, recebe uma lista com pecas e coordenadas e outra vazia (que funcionará como um acumulador)
Este "acumulador" receberá todas as ordenadas com uma peca e, devido a sua funcao auxiliar, "inserPecas" , receberá uma lista ordenada com as suas 
respetivas peças 
-}

ordenaPecas :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
ordenaPecas acc [] = acc 
ordenaPecas acc (h:t) =  insertPecas h (ordenaPecas acc t)  where          
    insertPecas :: (Peca,Coordenadas) -> [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
    insertPecas x [] = [x]
    insertPecas l1@(piece,(x,y)) (l2@(piece1,(x1,y1)):t)
        | y > y1 = l2 : insertPecas l1 t
        | y < y1 = l1 : l2 : t
        | y == y1 =if x < x1 then l1 : insertPecas l2 t else l2 : insertPecas l1 t 
        |otherwise = []


{- | Esta funcção vai ordenar coordenadas pelos seus Y's, utilizando a funcao, ordenarPorY-}
ordenaLista :: [Coordenadas] -> [Coordenadas]
ordenaLista lista = ordenarPorY lista

    where
        ordenarPorY :: [Coordenadas] -> [Coordenadas]
        ordenarPorY [] = []                                            {-Inspirado na funcao pre-definida ISort -}
        ordenarPorY ((x,y):t) = insert (x,y) (ordenarPorY t)

        insert :: Coordenadas -> [Coordenadas] -> [Coordenadas]
        insert x [] = [x]
        insert (x,y) ((x1,y1):t)
            | y > y1 = (x1,y1) : insert (x,y) t
            | otherwise = (x,y) : (x1,y1) : t




{- | listX lista todas as coordenadas x dentro da lista de pecas e coordenadas-}
listX :: [(Peca,Coordenadas)] -> [Int]
listX [] = []
listX ((piece,(x,y)): t) = x : listX t




{- | listY lista todas as coordenadas y dentro da lista de pecas e coordenadas-}
listY :: [(Peca,Coordenadas)] -> [Int]
listY [] = []
listY ((piece,(x,y)): t) = y : listY t

{- | Calcula o valor maximo de uma lista de Ints-}
maximoX :: [Int] ->  Int
maximoX [] = undefined
maximoX list = maximum list

{- |Uma funcao que trata de determinar o numero maximo de uma lista de numeros-}
maximoY :: [Int] -> Int
maximoY [] = undefined
maximoY list = maximum list


desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa [] = []
desconstroiMapa (x:y) =  tirarVazios (criaPecaCoord (x:y))
    

    

{- |tirarVazios é uma função que elimina todos os vazios do Mapa-}

tirarVazios :: [(Peca,Coordenadas)] -> [(Peca,Coordenadas)]
tirarVazios [] = []
tirarVazios ((piece,(x,y)):t) 
    |piece == Vazio = tirarVazios t
    |otherwise = (piece,(x,y)) : tirarVazios t

{- |criarInverso é uma função que cria uma lista de coordenadas todas ordenadas pelo Y*-}
 
criarInverso :: Mapa -> [Coordenadas]               
criarInverso [] = []
criarInverso m@(h:t) = [ (x,y) |  y <- [0..list2] , x <- [0..list1]] where
        (list1,list2) = (length h -1 , length m-1  )                        

{-|criaPecaCoord é uma funcao que junta as Pecas do mapa à sua respetiva coordenada (calculada anterioremente), 
tornando-se assim numa lista de Pecas e Coordenadas-}
criaPecaCoord :: Mapa -> [(Peca,Coordenadas)]
criaPecaCoord [] = []
criaPecaCoord (h:t) = zip (concat (h:t)) (criarInverso (h:t))



