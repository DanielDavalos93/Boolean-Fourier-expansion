module ProblemaCota where
import WDG
import Graph 
{- Este archivo es para probar casos del problema de la cota superior 
 - -}

-- Grafo triangular con pesos (construcción)
gtr :: Graph
gtr = ([1,2,3],[(1,2),(2,3),(3,1)])

wgtr :: WDG
wgtr = graphToWDG gtr [0.5,-0.3,0.2]

{-Considerando para cada v in Vertex
 - suma_e in E(v) |w(e)| < epsilon 
 - busquemos el máximo epsilon-}

--epsilon :: WDG -> Float
--epsilon d = maximum [sum [abs w | w <- ] | (e,w) <- (snd d)]
