{- |
Module      : Tarefa4_2021li1g023
Description : Movimentação do personagem
Copyright   : Eduardo José Gonçalves dos Reis <a100819@alunos.uminho.pt>;
            : Inês Gonzalez Perdigão Marques <a100606@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g023 where

import LI12122

{-| Move Jogador vai correr todos os movimentos possiveis-}
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo mapa jogador)  movimento = case movimento of
                                AndarEsquerda -> esquerda mapa jogador
                                AndarDireita -> direita mapa jogador
                                Trepar -> trepar mapa jogador
                                InterageCaixa -> casoCaixa mapa jogador

                                
{-| CorrerMovimentos vai correr todos os movimentos possiveis de uma lista de Movimentos ,criando assim movimento para
a personagem/boneco. -}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos jogo [] = jogo
correrMovimentos jogo (movimento:t) = correrMovimentos (moveJogador jogo movimento) t


{- |Trepar vai fazer com que o Boneco consiga trepar/subir certos blocos. Utilizando a funcao pre-definida (!!), vamos 
procurar a Peca que existe na coordenada dada.-}
trepar :: Mapa -> Jogador -> Jogo
trepar mapa (Jogador (x,y) direcao bool)
    |(direcao == Este) && (right == Caixa) && (emCimaR == Vazio) && (bool == True) && (emCimaR2X == Bloco) = Jogo mapa (Jogador (x,y) direcao bool)
    |(direcao == Oeste) && (left == Caixa) && (emCimaL == Vazio) && (bool == True) && (emCimaL2X == Bloco) = Jogo mapa (Jogador (x,y) direcao bool)
    |(direcao == Este) && (right == Caixa) && (emCimaR == Bloco) = Jogo mapa (Jogador (x,y) direcao bool)
    |(direcao == Oeste) && (left == Caixa) && (emCimaL == Bloco)=Jogo mapa (Jogador (x,y) direcao bool)   
    |(direcao == Este) && (right == Caixa) && (emCimaR == Caixa) = Jogo mapa (Jogador (x,y) direcao bool) 
    |(direcao == Oeste) && (left == Caixa) && (emCimaL == Caixa)=Jogo mapa (Jogador (x,y) direcao bool) 
    |(direcao == Este) && (bool == False) && (right == Bloco) && (emCimaR == Caixa) = Jogo mapa (Jogador (x,y) direcao bool) 
    |(direcao == Oeste) && (bool == False) && (left == Bloco) && (emCimaL == Caixa) = Jogo mapa (Jogador (x,y) direcao bool) 
    |(direcao == Oeste) && (bool == True) && (left == Vazio) && (emCimaL == Vazio) = Jogo mapa (Jogador (x,y) direcao bool)
    |(direcao == Este) && (bool == True) && (right == Vazio) && (emCimaR == Vazio) = Jogo mapa (Jogador (x,y) direcao bool)
    |(direcao == Oeste) && (bool == False) && (left == Bloco) && (emCimaL == Bloco) = Jogo mapa (Jogador (x,y) direcao bool)
    |(direcao == Este) && (bool == False) && (right == Bloco) && (emCimaR == Bloco) = Jogo mapa (Jogador (x,y) direcao bool)
    |(direcao == Oeste) && (bool == False) && (left == Bloco) = Jogo mapa (Jogador (x-1,y-1) direcao bool)            --se o jogador estiver virado para Oeste, nao tem uma caixa na sua cabeca e tem um bloco a sua frente, entao deve conseguir saltar--
    |(direcao == Oeste) && (bool == True) && (left == Bloco) && (emCimaL == Vazio) =  Jogo mapa (Jogador (x-1,y-1) direcao bool)
    |(direcao == Este) && (bool == False) && (right == Bloco) = Jogo mapa (Jogador (x+1,y-1) direcao bool)
    |(direcao == Este) && (bool == True) && (right == Bloco) && (emCimaR == Vazio) = Jogo mapa (Jogador (x+1,y-1) direcao bool)
    |(direcao == Oeste) && (bool == False) && (left == Caixa) = Jogo mapa (Jogador (x-1,y-1) direcao bool)
    |(direcao == Este) && (bool == False) && (right == Caixa) = Jogo mapa (Jogador (x+1,y-1) direcao bool)
    |(direcao == Oeste) && (bool == True) && (left == Caixa) && (emCimaL == Vazio) = Jogo mapa (Jogador (x-1,y-1) direcao bool)
    |(direcao == Este) && (bool == True) && (right == Caixa) && (emCimaR == Vazio) = Jogo mapa (Jogador (x+1,y-1) direcao bool)
    |(direcao == Oeste) &&(bool == True) && (emCimaL == Vazio) = Jogo mapa (Jogador (x-1,y-1) direcao bool)
    |(direcao == Este) && (bool == True) && (emCimaR == Vazio) = Jogo mapa (Jogador (x+1,y+1) direcao bool)
    |otherwise  = Jogo mapa (Jogador (x,y) direcao bool)
        where
            left = (mapa !! y)!! (x-1)    --vê a Peca à esquerda--
            right = (mapa !! y)!! (x+1)    --vê a Peca à direita--
            emCimaL = (mapa !! (y-1))!!(x-1) --vê se em cima dele sera possivel encontrar um bloco na sua diagonal(caso seja vdd ele nao consegue passar)
            emCimaR = (mapa !! (y-1))!!(x+1) --same so que para a direita
            emCimaR2X = (mapa !! (y-2)!!(x+1)) -- em cima da diagonal (para verificar um caso especificio)
            emCimaL2X = (mapa !! (y-2)!!(x-1)) -- em cima da diagonal  (para verificar um caso especifico) 
{- |casoCaixa é a funcao que vai nos dar os movimentos possiveis quando o jogador interage com uma Caixa utilizando a funcao pre-definida (!!), vamos 
procurar a Peca que existe na coordenada dada. -}
casoCaixa :: Mapa -> Jogador -> Jogo
casoCaixa mapa (Jogador (x,y) direcao bool)
    |y == 0 = Jogo mapa (Jogador (x,y) direcao bool) 
    |(direcao == Este) && (bool == True) && (right == Vazio) && (emBaixoR == Caixa) = Jogo (boxInserter mapa (x+1,y)) (Jogador (x,y) direcao False)  
    |(direcao == Oeste) && (bool == True) && (left == Vazio) && (emBaixoL == Caixa) = Jogo (boxInserter mapa (x-1,y)) (Jogador (x,y) direcao False)    
    |(direcao == Este) && (bool == True) && (right == Vazio) && (emBaixoR == Bloco) = Jogo (boxInserter mapa (x+1,y)) (Jogador (x,y) direcao False) --meter à frente
    |(direcao == Oeste) && (bool == True) && (left == Vazio) && (emBaixoL == Bloco) = Jogo (boxInserter mapa (x-1,y)) (Jogador (x,y) direcao False) --meter à frente
    |(direcao == Este) && (bool == False) && (right == Caixa) && (emCimaR == Vazio) = Jogo (boxRemover mapa (x+1,y)) (Jogador (x,y) direcao True) --pegar
    |(direcao == Oeste) && (bool == False) && (left == Caixa) && (emCimaL == Vazio) = Jogo (boxRemover mapa (x-1,y)) (Jogador (x,y) direcao True) --pegar 
    |(direcao == Este) && (bool == True) && (right == Caixa) && (emCimaR == Vazio) = Jogo (boxInserter mapa (x+1,y-1)) (Jogador (x,y) direcao False)--meter Caixa em cima de Caixa
    |(direcao == Oeste) && (bool == True) && (left == Caixa) && (emCimaL == Vazio) = Jogo (boxInserter mapa (x-1,y-1)) (Jogador (x,y) direcao False) --meter Caixa em cima de Caixa
    |(direcao == Este) && (bool == True) && (right == Bloco) && (emCimaR == Vazio) = Jogo (boxInserter mapa (x+1,y-1)) (Jogador (x,y) direcao False)
    |(direcao == Oeste) && (bool == True) && (left == Bloco) && (emCimaL == Vazio) = Jogo (boxInserter mapa (x-1,y-1)) (Jogador (x,y) direcao False)
    |(direcao == Oeste) && (left == Vazio) && (emBaixoL == Vazio) && (bool == True) = Jogo (boxInserter mapa (dropCaixa mapa (x-1,y))) (Jogador (x,y) direcao False)
    |(direcao == Este) && (right == Vazio) && (emBaixoR == Vazio) && (bool == True) = Jogo (boxInserter mapa (dropCaixa mapa (x+1,y))) (Jogador (x,y) direcao False) 
    |(direcao == Este) && (top == Bloco) && (bool == False) && (right == Caixa) = Jogo mapa (Jogador (x,y) direcao bool) 
    |otherwise = Jogo mapa (Jogador (x,y) direcao bool)

    


        where
            left = (mapa !! y)!! (x-1) --vê a Peca à esquerda--
            right = (mapa !! y)!! (x+1) --vê a Peca à sua Direita--
            top = (mapa !! (y-1))!! x --vê a Peca autmomaticamente em cima
            bottom = (mapa !! (y+1)) !! x --vê a peca automaticamente em baixo
            emCimaR = (mapa !! (y-1))!!(x+1)--Vê a diagonal direita baixo--
            emBaixoR = (mapa !! (y+1))!!(x+1) --Vê a diagonal baixo--
            emCimaL = (mapa !! (y-1))!!(x-1)  --Vê diagonal esquerda--
            emBaixoL = (mapa !! (y+1))!!(x-1) -- Vê a diagonal inferiro-- 

{- |Funcao que faz com que a caixa lentamente caia-}
dropCaixa :: Mapa -> Coordenadas -> Coordenadas
dropCaixa mapa (x,y) 
    | (mapa !! y)!! x /= Vazio = (x,y-1)
    | otherwise = dropCaixa mapa (x,y+1)

{- |casoCaixa é a funcao que nos vai inserir uma caixa no mapa, trocando uma peca vazia por caixa -}
boxInserter :: Mapa -> Coordenadas  -> Mapa
boxInserter [] _ = []
boxInserter (h:t) (x,0) = (take x h ++ [Caixa] ++ drop (x+1) h) : t
boxInserter (h:t) (x,y) = h : boxInserter t (x,y-1)
{- |boxRemover é a funcao que nos vai remover uma certa caixa do Mapa, trocando-o por um vazio-}
boxRemover :: Mapa -> Coordenadas  -> Mapa
boxRemover [] _ = []
boxRemover (h:t) (x,0) =(take x h ++ [Vazio] ++ drop (x+1) h) : t
boxRemover (h:t) (x,y) = h : boxRemover t (x,y-1)
{-| Esquerda é a funcao que vai ditar todos os movimentos possiveis para a Esquerda , utilizando a funcao pre-definida (!!), vamos 
procurar a Peca que existe na coordenada dada.-}

esquerda :: Mapa -> Jogador -> Jogo
esquerda  mapa (Jogador (x,y) direcao bool)
    |(x == 0) = Jogo mapa (Jogador (x,y) Este bool)
    |(bool == True) && ((left == Vazio) || (left == Porta)) && (emCimaL == Bloco) = Jogo mapa (Jogador (x,y) Oeste bool)
    |(bool == True) && ((left == Vazio) || (left == Porta)) && (emCimaL == Vazio) && (emBaixoL == Bloco)= Jogo mapa (Jogador (x-1,y) Oeste True)
    |((left == Vazio) || (left == Porta)) && (emBaixoL  == Bloco) = Jogo mapa (Jogador (x-1,y) Oeste bool)           
    |(bool == True)  &&  ((left == Vazio) || (left == Porta)) && (emCimaL == Vazio) && (emBaixoL == Caixa)= Jogo mapa (Jogador (x-1,y) Oeste bool)
    |((left == Vazio) || (left == Porta)) && (emBaixoL == Bloco) = Jogo mapa (Jogador (x-1,y) Oeste bool) 
    |(left == Vazio) && (emBaixoL == Caixa) = Jogo mapa (Jogador (x-1,y) Oeste bool)
    |((left == Vazio) || (left == Porta)) && (emBaixoL == Vazio) = esquerda mapa (Jogador (x,y+1) Oeste bool)
    |otherwise = Jogo mapa (Jogador (x,y) Oeste bool)

    where
            left = (mapa !! y)!! (x-1)    --vê a Peca à esquerda--
            emCimaL = (mapa !! (y-1))!!(x-1) --vê se em cima dele sera possivel encontrar um bloco na sua diagonal(caso seja vdd ele nao consegue passar)
            emBaixoL = (mapa !! (y+1))!!(x-1)
            bottom = (mapa !! (y+1)) !! x
            maximoX = maxX mapa
                {- (!!) :: [a] -> Int -> a-}

{- |Direita é a funcao que vai ditar todos os movimentos possiveis para a Esquerda , utilizando a funcao pre-definida (!!), vamos 
procurar a Peca que existe na coordenada dada.-}


direita :: Mapa -> Jogador -> Jogo
direita  mapa (Jogador (x,y) direcao bool) 
    |(x==maximoX) = Jogo mapa (Jogador (x,y) Este bool)
    |(bool == False) && ((right == Vazio) || (right == Porta)) && (emCimaR == Vazio) && (emBaixoR == Bloco) = Jogo mapa (Jogador (x+1,y) Este bool)
    |(bool == False) && ((right == Vazio) || (right == Porta)) && (emCimaR  == Bloco) && (emBaixoR == Bloco) = Jogo mapa (Jogador (x+1,y) Este bool)
    |(bool == False) && ((right == Vazio) || (right == Porta)) && (emCimaR == Vazio) && (emBaixoR == Caixa) = Jogo mapa (Jogador (x+1,y) Este bool)
    |(bool == False) && ((right == Vazio) || (right == Porta)) && (emCimaR  == Bloco) && (emBaixoR == Caixa) = Jogo mapa (Jogador (x+1,y) Este bool)
    |(bool == True) && ((right == Vazio) || (right == Porta)) && (emCimaR == Vazio) && (emBaixoR == Bloco)= Jogo mapa (Jogador (x+1,y) Este bool)
    |(bool == True) && ((right == Vazio) || (right == Porta))&& (emCimaR == Vazio) && (emBaixoR == Caixa) = Jogo mapa (Jogador (x+1,y) Este bool)
    |(bool == True) && ((right == Vazio) || (right == Porta)) && (emCimaR == Vazio) && (emBaixoR == Caixa) = Jogo mapa (Jogador (x+1,y) Este bool)
    |(bool == True) && ((right == Vazio) || (right == Porta)) && (emCimaR == Vazio) && (emBaixoR == Bloco) = Jogo mapa (Jogador (x+1,y) Este bool)
    |(right == Vazio) && (emBaixoR == Caixa) = Jogo mapa (Jogador (x+1,y) Este bool)
    |((right == Vazio) || (right == Porta))&& (emBaixoR == Vazio) = direita mapa (Jogador (x,y+1) Este bool)
    | otherwise =Jogo  mapa (Jogador (x,y) Este bool)
          where
            right = (mapa !! y)!! (x+1)                               --vê a Peca à direita--
            emCimaR = (mapa !! (y-1))!!(x+1)
            emBaixoR = (mapa !! (y+1))!!(x+1)
            maximoX = maxX mapa
 
{-| maxX calcula o x maximo navegável no Mapa-}
maxX :: Mapa -> Int
maxX [] = 0
maxX (h:t) = length h -1




