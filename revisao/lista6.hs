import System.Console.Terminfo (Color(Yellow))
import System.Posix (TerminalMode(BackgroundWriteInterrupt))
{--
Questão 1
Defina um tipo Triple a que represente um trio de valores do mesmo tipo. 
Crie uma função first :: Triple a -> a que retorna o primeiro elemento do trio.
--}

data Triple a = Triple a a a
first :: Triple a -> a
first (Triple a b c) = a    

{--
Questão 2
Defina um tipo Color que pode ser Red | Green, Blue, Yellow. 
Implemente uma função isPrimaryColor :: Color -> Bool que verifica se a cor é primária (Red, Green ou Blue).
--}

data Colorr = Red | Green | Blue | Yelloww

isPrimaryColor :: Colorr -> Bool
isPrimaryColor Red = True
isPrimaryColor Green = True
isPrimaryColor Blue = True
isPrimaryColor Yelloww = False

{--
Questão 3
Crie um tipo Weekday que pode ser Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday. 
Implemente uma função isWeekend :: Weekday -> Bool que verifica se o dia é final de semana.
--}
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
isWeekend :: Weekday -> Bool
isWeekend Monday = False
isWeekend Tuesday = False
isWeekend Wednesday = False
isWeekend Thursday = False
isWeekend Friday = False
isWeekend Saturday = True
isWeekend Sunday = True

{--
Questão 4
Defina um tipo MaybePair a que pode ser NothingPair ou JustPair (a, a). 
Implemente uma função sumPair :: Num a => MaybePair a -> a que retorna a soma dos elementos do par, ou 0 se for NothingPair.
--}

data MaybePair a = NothingPair | JustPair (a, a)

sumPair :: Num a => MaybePair a -> a 
sumPair NothingPair = 0
sumPair (JustPair (a, b)) = a + b

{--
Questão 5
Crie um tipo Position que representa uma posição 2D com coordenadas x e y. 
Implemente uma função distance :: Position -> Position -> Float que calcula a distância euclidiana entre duas posições.
--}

data Position = Position Float Float

distance :: Position -> Position -> Float
distance (Position a b) (Position d c) = sqrt (((a - d) * (a - d)) + ((b - c) * (b - c)))

{--
Questão 6
Defina um tipo Vehicle que pode ser Car com um número de portas ou Bike com um booleano indicando se tem cesta.
Implemente uma função isCar :: Vehicle -> Bool que verifica se o veículo é um carro.
--}

data Vehicle a = Car a | Bike a

isCar :: Vehicle a -> Bool 
isCar (Car a) = True
isCar (Bike a) = False

{--
Questão 7
Crie um tipo Option a que pode ser None ou Some a. 
Implemente uma função fromOption :: a -> Option a -> a que retorna o valor contido em Some, ou o valor padrão fornecido se for None.
--}

data Option a = None | Some a

fromOption :: a -> Option a -> a
fromOption a None = a
fromOption a (Some b) = b

{--
Questão 8
Defina um tipo Shape para representar formas geométricas que podem ser Square com um lado ou Triangle com base e altura. 
Implemente uma função perimeter :: Shape -> Float que calcula o perímetro de uma forma geométrica.
--}

data Shape = Square Float | Triangle Float Float  

perimeter :: Shape -> Float
perimeter (Square a) = a * a 
perimeter (Triangle a b) = a * b

{--
Questão 9
Crie um tipo Season que pode ser Spring, Summer, Autumn, Winter. 
Implemente uma função nextSeason :: Season -> Season que retorna a próxima estação.
--}

data Season = Spring | Summer | Autumn | Winter

nextSeason :: Season -> Season
nextSeason Spring = Summer
nextSeason Summer = Autumn
nextSeason Autumn = Winter
nextSeason Winter = Spring