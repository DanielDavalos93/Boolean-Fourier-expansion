module ProblemaCota where
import WDG
import Graph 
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

epsilon :: WDG -> Float
epsilon d = maximum [sum [abs w | e <- (edgeAdj (wdgToGraph d) v), (e,w) <- (snd d) ] | v <- (fst d)]

-- Grafo cuadrangular con pesos 
gSq :: Graph
gSq = ([1,2,3,4],[(1,2),(2,3),(3,4),(2,4),(3,1)])

wgSq1 = graphToWDG gSq [1,-2,4,2,-1]
