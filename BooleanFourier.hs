module BooleanFourier where
import Polynomial

intToChar :: Int -> Char
intToChar i = (['a'..] !! i)

--Indicator polynomial
ind :: [Double] -> [String] -> Polynomial
ind a x = multipSevPoly [multipSevPoly [Polynomial [(0.5,"")], Polynomial [(1,""), (a !! i, x !! i)]] | i<-[0..(length a -1)]]

--Test
{- ind (-1,1) (x1,x2) = 1 - x1 + x2 -x1*x2
   ind (-1,1) (1,1) = 1 -(-1) + 1 - (-1)*1 = 4
-}
test1 = ind [-1,1] ["a","b"] -- Polynomial [(0.25,""), (-0.25,"a"), (0.25,"b"), (-0.25,"ab")]
test2 = eval ([(ind [-1,1] ["a","b"], [1,1])]) -- 0.0

{-
Polynomial representation
 f x = sum_{a in [-1,1]^n} (f a x) * (ind a x)
-}

--polrep :: ([Double] -> Double) -> [String] -> Polynomial 
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
or2 = polrep or' ["x","y"]
eor = eval [(or2, [1,-1])]

--And
and' [1] = 1.0
and' (x:xs) = if x == -1.0 then -1.0 else and' xs
and2 = polrep and' ["x","y"]
eand = eval [(and2, [1,-1])]

--Equ_n
eq :: [Double] -> Double
eq [x] = 1.0 
eq (x:(y:xs)) = if x /= y then 0.0 else eq (y:xs) 
eq2 = polrep eq ["x","y"]

eq' x = eq x + 1
eq2' = polrep eq' ["x","y"]

--Majority
maj :: [Double] -> Double
maj (xs) = 
  let s1 = filter (\x -> x == 1.0) xs; sm1 = filter (\x -> x == -1.0) xs
  in if length s1 > length sm1 then 1.0 else -1 
maj2 = polrep maj ["x","y"]
maj5 = polrep maj ["a","b","c","d","e"]
