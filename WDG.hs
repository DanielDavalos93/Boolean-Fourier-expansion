module WDG where 
--import BooleanFourier
import Graph 

type Weigth = Float

type WDG = ([Vertex], [(Edge,Weigth)])


--Convertir un grafo con pesos dinámicos a un grafo simple
wdgToGraph :: WDG -> Graph 
wdgToGraph d = (fst d, fst $ unzip $ snd d)


s :: Edge -> [Int] -> Int
s e x = (x !! (fst e - 1)) * (x !! (snd e - 1))

--Polinomio
gD :: WDG -> [Int] -> Float
gD wdg x = sum [ w * (fromIntegral (s e x)) | (e,w)<- (snd wdg) ]

comb 0 _ = [[]]
comb n r = [i:s | i <- r, s <- comb (n-1) r]

allgD wdg = [(x,gD wdg x) | x <- (comb (length $ fst wdg) [-1,1])]

--Delta -> diferencia mínima
delta :: WDG -> Float
delta wdg = maximum ws - minimum ws
  where ws = snd $ unzip (allgD wdg)

-- Norma Ln 
norm :: WDG -> Float
norm d = sum [abs w | w <- snd $ unzip (snd d)]

isNorm_n :: WDG -> Float -> Bool
isNorm_n d n = norm d == n

graphToWDG :: Graph -> [Weigth] -> WDG 
graphToWDG g ws = (fst g, [(e,w/(sum [ abs w | w <- ws])) | (e,w) <- (zip (snd g) ws)])
