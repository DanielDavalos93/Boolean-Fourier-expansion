module ProblemaCota where
import WDG
import Graph 
import Polynomial
{- Este archivo es para probar casos del problema de la cota superior 
 - -}

-- Grafo triangular con pesos (construcción)
gtr :: Graph
gtr = ([1,2,3],[(1,2),(2,3),(3,1)])

wtr1 :: WDG
wtr1 = graphToWDG gtr [0.5,-0.3,0.2]

wtr2 :: WDG 
wtr2 = graphToWDG gtr [1/3,-1/3,1/3]

wtr3 :: WDG 
wtr3 = graphToWDG gtr [0.1,-0.5,-0.4]

{-Considerando para cada v in Vertex
 - suma_e in E(v) |w(e)| < epsilon 
 - busquemos el máximo epsilon-}

edgeAdj :: Graph -> Vertex -> [Edge]
edgeAdj g v = [ e | e <- snd g, (v == fst e || v == snd e)]

--vepsilon :: WDG -> Vertex -> Float
vepsilon d v = snd $ unzip $ filter (\(e,w) -> e `elem` (edgeAdj (wdgToGraph d) v)) (snd d)

epsilon :: WDG -> Float
epsilon d = maximum $ [sum $ map abs (vepsilon d v) | v <- (fst d)]

-- Grafo cuadrangular con pesos 
gSq :: Graph
gSq = ([1,2,3,4],[(1,2),(2,3),(3,4),(2,4)])

--wgSq = ([1,2,3,4],[((1,2),1),((2,3),-2),((3,4),4),((2,4),2),((3,1),-1),((1,4),2)])

wgSq = graphToWDG gSq [-1,5,-4,2,1]


--Conjetura 2 
--D=(G,w)

d = graphToWDG ([1,2,3,4],[(1,2),(1,4),(3,4),(2,4)]) [-2,2,-1,2]

d1 = graphToWDG gSq [1,1,2,-1]

d1' = (fst d, (map (\(e,w) -> (e, (1-epsilon d)*w)) (snd d)) ++ [((1,4), epsilon d)])

d' = (fst d, (map (\(e,w) -> (e, (1-epsilon d)*w)) (snd d)) ++ [((1,3), epsilon d)])
