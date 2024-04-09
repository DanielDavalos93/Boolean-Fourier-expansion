module BooleanFourier where
import Polynomial

--intToChar :: Int -> Char
--intToChar i | i>=0 && 255<i = (['a'..] !! i)

--Indicator polynomial
ind :: [Double] -> [String] -> Polynomial
ind a x = multipSevPoly [multipSevPoly [Polynomial [(0.5,"")], Polynomial [(1,""), (a !! i, x !! i)]] | i<-[0..(length a -1)]]

--Test
{- ind (-1,1) (x1,x2) = 1 - x1 + x2 -x1*x2
   ind (-1,1) (1,1) = 1 -(-1) + 1 - (-1)*1 = 4
-}
test1 = ind [-1,1] ["a","b"] -- = Polynomial [(1.0,""), (-1.0,"a"), (1.0,"b"), (-1.0,"ab")]
test2 = eval ([(ind [-1,1] ["a","b"], [1,1])]) -- = 4

{-
Polynomial representation
 f x = sum_{a in [-1,1]^n} (f a x) * (ind a x)
-}

polrep :: ([Double] -> Double) -> [String] -> Polynomial 
polrep f x = sumListPoly [multiplyPoly (Polynomial [(f a,"")]) (ind a x) | a<-(comb (length x) [-1,1])]

-- Examples --

--Minimum
min' x = (minimum x) * (1.0)
min2 = polrep (min') ["x","y"] -- write min1 ["x"]; or min2 ["x","y"]; ...
emin2 = eval [(min2, [1,-1])]

min3 = polrep (min') ["x","y","z"]
emin3 = eval [(min3, [1,1,1])]

--Maximum
max' x = (maximum x) * (1.0)
max2 = polrep (max') ["x","y"] 
emax2 = eval [(max2, [1,-1])]

--Or
or' [-1] = -1.0
or' (x:xs) = if x == 1 then 1 else or' xs
orPol = polrep or' ["x","y"]
eor = eval [(orPol, [1,-1])]

--And
and' [1] = 1.0
and' (x:xs) = if x == -1 then -1 else and' xs
andPol = polrep and' ["x","y"]
eand = eval [(andPol, [1,-1])]

--Equ_n
eq :: [Double] -> Double
eq [x] = 1.0 
eq (x:(y:xs)) = if x /= y then 0.0 else eq (y:xs) 
eqPol = polrep eq ["x","y"]

--Indicator function
--indf :: (Foldable t, Eq a, Double) => a -> t a -> Double
indf x a = if elem x a then 1.0 else 0.0
