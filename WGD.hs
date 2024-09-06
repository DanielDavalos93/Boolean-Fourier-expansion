module WGD where 
--import BooleanFourier
import Graph 

type Weigth = Float

type WDG = ([Vertex], [(Edge,Weigth)])

graphToWDG :: Graph -> [Weigth] -> WDG 
graphToWDG g ws = (fst g, [z | z <- (zip (snd g) ws)])

--example
d1 = graphToWDG graph1 [0,1,0.5,2,1,-1,-2,0,0,1] 
-- 

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

