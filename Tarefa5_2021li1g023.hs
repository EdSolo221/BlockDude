{- |
Module      : Tarefa5_2021li1g023
Description : Representação textual do Jogo
Copyright   : Eduardo José Gonçalves dos Reis <a100819@alunos.uminho.pt>;
            : Inês Gonzalez Perdigão Marques <a100606@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.
-}

module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12122
import Tarefa4_2021li1g023
import Tarefa2_2021li1g023
import Tarefa3_2021li1g023
import Data.Maybe
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Juicy (loadJuicy, loadJuicyJPG, loadJuicyPNG)
import Data.List.Split


type Mundo = (Menu,  Jogo, [Picture],Bool)


data Opcoes = Jogar 
            | Creditos 
            | Sair 

data Menu = ModoJogo Opcoes 
          |  Niveis Jogo Int
          | ConcluiMapa Int
          | VenceuJogo Int
          | Credito
          | Exit





{- |loadIMG é uma função que vai carregar todas as imagens do jogo e vai meter essas numa lista-}
loadIMG :: IO[Picture]
loadIMG = do
        blocopri <- loadJuicyJPG  "../personagens/NeoBloco.jpeg"
        portapri <- loadJuicyPNG  "../personagens/Porta.png"
        caixapri <- loadJuicyPNG  "../personagens/Caixa.png"
        jogadorR <- loadJuicyPNG "../personagens/CasperRight.png"
        jogadorL <- loadJuicyPNG "../personagens/CasperLeft.png"
        fundo <- loadJuicyJPG "../personagens/Fundo.jpeg"
        playgameOn <- loadJuicyPNG "../personagens/play.png"
        creditsOn <- loadJuicyPNG "../personagens/Credits.png"
        exitOn <- loadJuicyPNG "../personagens/exit.png"
        win <- loadJuicyPNG "../personagens/Win.png"
        next <- loadJuicyPNG "../personagens/next.png"
        return (map fromJust [blocopri,portapri,caixapri,jogadorR,jogadorL,fundo,playgameOn,creditsOn,exitOn,win,next])

{- |Tomando a Tarefa3 como exemplo ,pecaImagem vai trocar cada Char pela Imagem correspondente-}
pecaImagem :: (Char,[Picture]) -> Picture
pecaImagem (' ',pics) = Blank
pecaImagem ('X',pics) = pics !! 0
pecaImagem ('P',pics) = pics !! 1
pecaImagem ('C',pics) = pics !! 2
pecaImagem ('>',pics) = pics !! 3
pecaImagem ('<',pics) = pics !! 4
pecaImagem _ = Blank


{-|desenhaMapa é uma função que vai, utilizando a já concluida Tarefa3, desenhar o mapa passando pelas linhas (desenhado pela funcao desenhaLinha) passando pela lista inteira de string, ou seja, vai ler a lista inteira de String recursivamente e vai trocando Character pela sua imagem
respetiva utilizando a desenhaLinha-}
desenhaMapa :: ([String],[Picture]) -> (Int,Int) -> [Picture]
desenhaMapa ([],_)_ = []
desenhaMapa ((h:t),pic) (x,y) = Translate 0 (fromIntegral(y*(-16))) (Pictures (desenhaLinha (h,pic) x)):desenhaMapa (t,pic) (x,y+1)


{- desenhaLinha é uam funcao que ao receber uma string, via lê-la e vai trocando cada Char pela sua respetiva imagem-}
desenhaLinha :: (String,[Picture]) -> Int -> [Picture]
desenhaLinha ([],_) _ = []
desenhaLinha (h:t,pics) x = Translate (fromIntegral(x*16)) 0 (pecaImagem (h,pics)) : desenhaLinha (t,pics) (x+1)


{-drawMap é uma funcao que vai desenhar o Mapa, utilizando a funcao desenhaMapa, que tem todas as Chars trocadas pelas imagens que se encontram numa lista.
Esta funcao vai ler essa lista da Picture e recebendo o mapa respetivo, corre o desenhaMapa e vai separando cada linha da lista por "\n" utilizando splitOn
, uma função que separa lista em sublistas, fazendo com que seja possivel ver o jogo linha por linha-}
drawMap0 :: String -> [Picture] -> Picture
drawMap0 mapa pic = translate (-700) 505 (scale 4.5 4.5 (Pictures (desenhaMapa (splitOn "\n" mapa,pic) (0,0))))

drawMap2 :: String -> [Picture] -> Picture
drawMap2 mapa pic = translate (-750) 505 (scale 4.5 4.5 (Pictures (desenhaMapa (splitOn "\n" mapa,pic) (0,0)))) --change peerspective

drawMap1 :: String -> [Picture] -> Picture
drawMap1 mapa pic = translate (-700) 505 (scale 4.5 4.5 (Pictures (desenhaMapa (splitOn "\n" mapa,pic) (0,0))))


{- | A função evento vai detalhar todos os eventos que acontecem dentro do jogo, desde o movimento da personagem até à movimentação no Menu...passando também
como meio de uniao entre mapas e mesmo com cada opcao do menu ao seu resultado esperado.-}
evento :: Event -> Mundo -> Mundo

--dentor do menu
evento (EventKey (SpecialKey KeyDown) Down _ _)    (ModoJogo Jogar , jogo@(Jogo mapa jogador), imagens,bool) = (ModoJogo Creditos, jogo, imagens,bool) --zapping no menu
evento (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo Creditos,jogo@(Jogo mapa jogador),imagens,bool) = (ModoJogo Jogar,jogo,imagens,bool) --zapping no menu
evento (EventKey (SpecialKey KeyUp) Down _ _)    (ModoJogo Jogar, jogo@(Jogo mapa jogador), imagens,bool) = (ModoJogo Sair, jogo, imagens,bool) -- zapping no menu
evento (EventKey (SpecialKey KeyUp) Down _ _)    (ModoJogo Sair, jogo@(Jogo mapa jogador), imagens,bool)  = (ModoJogo Creditos, jogo, imagens,bool) --zapping no menu
evento (EventKey (SpecialKey KeyDown) Down _ _)  (ModoJogo Sair, jogo@(Jogo mapa jogador), imagens,bool)  = (ModoJogo Jogar, jogo, imagens,bool) --zapping no menu
evento (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo Creditos,jogo@(Jogo mapa jogador), imagens,bool) = (ModoJogo Sair, jogo ,imagens,bool) --zapping no menu
evento (EventKey (SpecialKey KeyEnter)Down _ _) (ModoJogo Sair,jogo,imagens,bool) = (Exit,jogo,imagens,bool)--zapping no menu
evento (EventKey (SpecialKey KeyF1) Down _ _) (Credito,jogo,imagens,bool) = (ModoJogo Jogar,jogo,imagens,bool)--ir de creditos para menu
evento (EventKey (SpecialKey KeyEnter) Down _ _) (ModoJogo Creditos, jogo, imagens,bool) = (Credito , jogo, imagens,bool)--zapping no menu


--começo dos niveis
evento (EventKey (SpecialKey KeyEnter) Down _ _) (ModoJogo Jogar, jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m0 0, jogo, imagens,bool)--começar o Jogo no Menu
--nivel 1
evento (EventKey (SpecialKey KeyEnter) Down _ _) (Niveis m0 0,Jogo mapa jogador,imagens,bool) = (Niveis m0 0, m0 ,imagens,bool) -- Fazer Reset
evento (EventKey (SpecialKey KeyLeft) Down _ _) (Niveis m0 0, jogo@(Jogo mapa jogador), imagens,False) = (Niveis m0 0, moveJogador jogo AndarEsquerda, imagens,ganhaste0 jogador) -- Esquerda
evento (EventKey (SpecialKey KeyRight) Down _ _) (Niveis m0 0, jogo@(Jogo mapa jogador), imagens,False) = (Niveis m0 0, moveJogador jogo AndarDireita, imagens,ganhaste0 jogador) -- Direita
evento (EventKey (SpecialKey KeyUp) Down _ _) (Niveis m0 0,jogo@(Jogo mapa jogador), imagens,False) = (Niveis m0 0, moveJogador jogo Trepar, imagens,ganhaste0 jogador) --Trepar
evento (EventKey (SpecialKey KeyDown) Down _ _) (Niveis m0 0, jogo@(Jogo mapa jogador), imagens,False) = (Niveis m0 0, moveJogador jogo InterageCaixa, imagens,ganhaste0 jogador) --Caixa
evento _ (Niveis m0 0,  jogo, imagens,True) = (ConcluiMapa 0, m0, imagens ,False) -- Concluir Mapa 
evento _ (ConcluiMapa 0, m0,imagem,bool) = (Niveis m1 1, m1,imagem,bool) -- Passar para o proximo nivel


--nivel 2
evento (EventKey (SpecialKey KeyEnter) Down _ _) (Niveis m1 1,Jogo mapa jogador,imagens,bool) = (Niveis m1 1 ,m1 ,imagens,bool) --reset
evento (EventKey (SpecialKey KeyUp) Down _ _) (Niveis m1 1,jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m1 1, moveJogador jogo Trepar, imagens,ganhaste1 jogador)--Trepar
evento (EventKey (SpecialKey KeyDown) Down _ _) (Niveis m1 1, jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m1 1, moveJogador jogo InterageCaixa, imagens,ganhaste1 jogador)--Caixa
evento (EventKey (SpecialKey KeyLeft) Down _ _) (Niveis m1 1, jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m1 1, moveJogador jogo AndarEsquerda, imagens,ganhaste1 jogador)--Esquerda
evento (EventKey (SpecialKey KeyRight) Down _ _) (Niveis m1 1, jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m1 1, moveJogador jogo AndarDireita, imagens,ganhaste1 jogador)--Direta
evento _ (Niveis m1 1,  jogo, imagens,True) = (ConcluiMapa 1, m1, imagens ,False) -- Concluir Mapa 
evento _ (ConcluiMapa 1,m1 ,imagem,bool) = (Niveis m2 2, m2,imagem,bool) -- Passar para o proximo nivel 
--Nivel 3
evento (EventKey (SpecialKey KeyEnter) Down _ _) (Niveis m2 2,Jogo mapa jogador,imagens,bool) = (Niveis m2 2 ,m2 ,imagens,bool) --reset
evento (EventKey (SpecialKey KeyUp) Down _ _) (Niveis m2 2,jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m2 2, moveJogador jogo Trepar, imagens,ganhaste2 jogador)--Trepa
evento (EventKey (SpecialKey KeyDown) Down _ _) (Niveis m2 2, jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m2 2, moveJogador jogo InterageCaixa, imagens,ganhaste2 jogador)--Caixa
evento (EventKey (SpecialKey KeyLeft) Down _ _) (Niveis m2 2, jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m2 2, moveJogador jogo AndarEsquerda, imagens,ganhaste2 jogador)--Esquerda
evento (EventKey (SpecialKey KeyRight) Down _ _) (Niveis m2 2, jogo@(Jogo mapa jogador), imagens,bool) = (Niveis m2 2, moveJogador jogo AndarDireita, imagens,ganhaste2 jogador) --Direita
evento _ (Niveis m2 2, jogo, imagens,True) = (VenceuJogo 0, jogo, imagens ,False) --Concluir Jogo
--acabamos niveis
evento (EventKey (SpecialKey KeyF1) Down  _ _) ( VenceuJogo 0, jogo,imagem ,bool) = (ModoJogo Jogar , jogo ,imagem, bool) -- ir para o Menu
evento (EventKey (SpecialKey KeyF3) Down  _ _) ( VenceuJogo 0, jogo,imagem ,bool) = (ModoJogo Sair , jogo ,imagem, bool)  -- Sair do Jogo
evento _ w = w --skip all events

{- fr vai ser o numero de frames por segundo que o nosso jogo vai ter, para adicionar ao main-}
fr :: Int
fr = 60

{- dis vai ser o nosso Display defenido-}
dis :: Display
dis = FullScreen

{- banksy (inspirado no artista) é uma função que vai pegar na lista de Pictures que temos e vai colocar Pictures como Backgorund para o nosso Jogo-}
banksy :: (Menu,[Picture]) -> Picture
banksy (ModoJogo Jogar ,pics) = pics !! 5
banksy (ModoJogo Creditos,pics) = pics !! 5
banksy (ModoJogo Sair,pics) = pics !! 5
banksy (Niveis _ _,pics) = pics !! 5
banksy  (VenceuJogo 0,pics) = pics !! 9
banksy (ConcluiMapa _,pics) = pics !! 10

{- |drawOptions vai desenhar no Menu as oçcoes com imagens da lista-}
drawOptions :: (Menu,[Picture]) -> Picture
drawOptions (ModoJogo Jogar ,pics) = pics !! 6
drawOptions (ModoJogo Sair,pics) = pics !! 8
drawOptions (ModoJogo Creditos,pics) = pics !! 7



{- desenha é a funcao que se encarrega de desenhar estados, como por exemplo, desenha o menu, o Jogo ou mesmo o que este 
 nos avisa quando ganhamos-}            
desenha :: Mundo -> Picture
--o que acontece dentro do Menu Principal
desenha (ModoJogo Jogar, jogo, imagens,bool) = Pictures [scale 24.0 18.0 (banksy (ModoJogo Jogar,imagens)),Translate 600 225 circulo,scale 3.0 3.0  (Translate 245 70 (drawOptions (ModoJogo Jogar,imagens))),scale 3.0 3.0  (Translate 245 0 (drawOptions (ModoJogo Creditos,imagens))),scale 3.0 3.0  (Translate 245 (-70) (drawOptions (ModoJogo Sair,imagens)))]
desenha (ModoJogo Creditos, jogo, imagens,bool) = Pictures [scale 24.0 18.0 (banksy (ModoJogo Creditos,imagens)),scale 3.0 3.0  (Translate 245 70 (drawOptions (ModoJogo Jogar,imagens))),scale 3.0 3.0  (Translate 245 0 (drawOptions (ModoJogo Creditos,imagens))),Translate 600 20 circulo,scale 3.0 3.0  (Translate 245 (-70) (drawOptions (ModoJogo Sair,imagens)))]
desenha (ModoJogo Sair, jogo,imagens,bool) = Pictures [scale 24.0 18.0 (banksy (ModoJogo Sair,imagens)),scale 3.0 3.0  (Translate 245 70 (drawOptions (ModoJogo Jogar,imagens))), scale 3.0 3.0 (Translate 245 0 (drawOptions (ModoJogo Creditos,imagens))),Translate 600 (-190) circulo,scale 3.0 3.0  (Translate 245 (-70) (drawOptions (ModoJogo Sair,imagens)))]
-- o que acontece em Exit e dentro dos creditos
desenha (Exit,jogo,imagens,bool) = Pictures[scale 0.5 0.5 $Translate (-700) (-200) $ Color red $ Text "Thank you for playing our game!",scale 1.0 1.0 $Translate (-800) (-400)$ Color red $ Text "Press Esc to leave"]
desenha (Credito,jogo,imagens,bool) = Pictures[scale 1.0 1.0 $Translate (-700) 0 $ Color red $ Text "Jogo feito por:",scale 1.0 1.0 $Translate (-700) (-200) $Color red $ Text "Eduardo e Ines  :)", scale 0.3 0.3 $ Translate (-700) (-1000) $ Color red $ Text " press Esc to leave or F1 to go to Menu"]
--o que acontece dentro dos nives 
desenha (Niveis m0 0, jogo@(Jogo mapa (Jogador coord dir bool)), imagens,_) = Pictures[scale 24.0 18.0(banksy (Niveis m0 0,imagens)), drawMap0 (show jogo) imagens]
desenha (Niveis m1 1, jogo@(Jogo mapa (Jogador coord dir bool)), imagens,_) =Pictures[scale 24.0 18.0(banksy (Niveis m1 1,imagens)), drawMap1 (show jogo) imagens]
desenha (Niveis m2 2, jogo@(Jogo mapa (Jogador coord dir bool)), imagens,_) = Pictures[scale 24.0 18.0(banksy (Niveis m2 2,imagens)), drawMap2 (show jogo) imagens]
desenha (ConcluiMapa 0, jogo, imagens,bool) = Pictures [scale 30.0 30.0(banksy (ConcluiMapa 0,imagens)), scale 0.5 0.5 (Translate (-780) (-600) $ Color black $ Text "Pressiona qualquer tecla")]
desenha (ConcluiMapa 1, jogo, imagens,bool) = Pictures [scale 30.0 30.0(banksy (ConcluiMapa 1,imagens)), scale 0.5 0.5 (Translate (-780) (-600) $ Color black $ Text "Pressiona qualquer tecla")]
desenha (VenceuJogo 0, jogo, imagens,bool) =  Pictures [scale 30.0 30.0(banksy (VenceuJogo 0,imagens)), scale 0.5 0.5 (Translate (-780) (-600) $ Color black $ Text "Pressiona Esc para sair")]




{- circulo é um circulo que serve de guia para o mapa-}
circulo0 :: Picture
circulo0 =  circleSolid 20

circulo :: Picture
circulo = Color blue circulo0
 
{-time é uma funcao que resumidamente cria um estado de tempo no jogo-}
time :: Float -> Mundo -> Mundo
time _ w = w


{- |Main é a função que vai "rodar" o jogo, tendo assim uma forma de conseguirmos realmente joga-lo.
Mesmo assim Main utiliza as imagens da funcao loadIMG que se encontra numa lista de IO[Picture] e vai dar load para dentro
de uma outra funcao que criamos chamada loadedeIMG, para assim termos as imagens numa lista só.
Após isso conseguimos defenir o estado incial, tendo como esta o Modo de Jogo aberto no que vai ser o primeiro mapa, o mapa 4.
Implementamos a nossa loadedIMG para carregar as imagens de cada Peca e mesmo do Jogador, e dizemos que o Bool é falso, sendo
que o jogador ainda nao chegou à Porta. Depois implementamos a funcao play do gloss para representar todas as funcoes do gloss precisas
para rodar o jogo. -}
main :: IO()
main = do

    loadedIMG <- loadIMG
    let estadoInicial = (ModoJogo Jogar, m0 , loadedIMG,False)
    play
        dis     --display
        white   --cor
        fr      --framerate
        estadoInicial --estadoInicial
        desenha --desenha
        evento
        time




{- | mapa0 é o primeiro mapa do nosso jogo-}

mapa0 :: Mapa
mapa0 =
        [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
        [Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco],
        [Bloco, Vazio, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco],
        [Bloco, Porta, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco],
        [Bloco, Bloco, Vazio, Vazio,Vazio, Vazio, Vazio, Bloco, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
        [Bloco, Bloco, Vazio, Vazio,Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco,Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Caixa, Vazio, Caixa, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco,Bloco],
        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco,Bloco]]

player0 :: Jogador --Jogador e o seu "estdo incial" dentro do Mapa
player0 = Jogador (17,2) Oeste True

m0 :: Jogo --O Jogo me si
m0 = Jogo mapa0 player0


ganhaste0 :: Jogador -> Bool --Garantia que o jogador chegou às coordenadas da Porta
ganhaste0 (Jogador (x,y) _ _)  = (x,y) == coords
    where coords = returnCoord (getPorta mapa0)

{- | mapa1 é o segundo mapa do nosso jogo-}
mapa1 :: Mapa
mapa1 =
    [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Porta, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Caixa, Vazio, Vazio, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Caixa, Caixa, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
    [Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Caixa, Caixa, Vazio, Vazio, Caixa, Bloco],
    [Bloco, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
    [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]

player1 :: Jogador --Jogador e o seu "estdo incial" dentro do Mapa
player1 = Jogador (1,2) Este False 

m1 :: Jogo --Jogo em si
m1 = Jogo mapa1 player1

ganhaste1 :: Jogador -> Bool --Garantia que o jogador chegou às coordenadas da Porta
ganhaste1 (Jogador (x,y) _ _)  = (x,y) == coords
    where coords = returnCoord (getPorta mapa1)

{- | mapa2 é o terceiro mapa do nosso jogo-}

mapa2 :: Mapa
mapa2 = [[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco,Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa,Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Caixa, Caixa,Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco,Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Vazio, Vazio, Vazio,Bloco],
        [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio,Bloco],
        [Bloco, Porta, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Caixa, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco,Bloco],
        [Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Caixa, Vazio, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio],
        [Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio],
        [Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio],
        [Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio],
        [Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio,Vazio]]
                     
player2 :: Jogador--Jogador e o seu "estado incial" dentro do Mapa
player2 = Jogador (17,3) Este False

m2 :: Jogo -- O Jogo em si
m2 = Jogo mapa2 player2

ganhaste2 :: Jogador -> Bool --Garantia que o jogador chegou às coordenadas da Porta
ganhaste2 (Jogador (x,y) _ _)  = (x,y) == coords
    where coords = returnCoord (getPorta mapa2)





{- |mapMaker é uma função que torna um Mapa em String, retirada da Tarefa3-}
mapMaker :: Mapa -> Jogador -> String
mapMaker mapa jogador = cortaLista jogador (graphic mapa)

{- |getPorta é uma função que transforma uma Mapa em (Peca,Coordenada) retirada da Tarefa2-}
getPorta :: Mapa -> [(Peca,Coordenadas)]
getPorta  = desconstroiMapa

{- |returnCoord é uma função que vai buscar a coordenada da Porta pegando na função anterior-}
returnCoord :: [(Peca,Coordenadas)] -> Coordenadas
returnCoord [] = undefined
returnCoord ((piece,coord):t)
    |piece == Porta = coord
    |otherwise = returnCoord t

