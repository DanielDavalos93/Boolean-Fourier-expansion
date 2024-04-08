module BooleanFourier where
import Polynomial

intToChar :: Int -> Char
intToChar i | i>=0 && 255<i = (['a'..] !! i)

--Indicator polynomial
ind :: [Double] -> [String] -> Polynomial
ind a x = multipSevPoly [multipSevPoly [Polynomial [(0.5,"")], Polynomial [(1,""), (1 * (a !! i), x !! i)]] | i<-[0..(length a -1)]]

--Test
-- ind (-1,1) (x1,x2) = 1 - x1 + x2 -x1*x2
-- ind (-1,1) (1,1) = 1 -(-1) + 1 - (-1)*1 = 4
test1 = ind [-1,1] ["a","b"] -- = Polynomial [(1.0,""), (-1.0,"a"), (1.0,"b"), (-1.0,"ab")]
test2 = eval ([(ind [-1,1] ["a","b"], [1,1])]) -- = 4

{-
Polynomial representation
 f x = sum_{a in [-1,1]^n} (f a x) * (ind a x)
-}

polrep :: ([Double] -> Double) -> [String] -> Polynomial 
polrep f x = sumListPoly [multiplyPoly (Polynomial [(f a,"")]) (ind a x) | a<-(comb (length x) [-1,1])]

--Examples 
min' x = (minimum x) * (1.0)
min1 = polrep (min') -- write min1 ["x"]; or min1 ["x","y"]; ...
