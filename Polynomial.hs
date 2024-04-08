module Polynomial where

comb 0 _ = [[]]
comb n r = [i:s | i <- r, s <- comb (n-1) r]

set [] = []
set (x:xs) = if elem x xs then (set xs) else x:(set xs)

-- Definimos un tipo para representar un término del polinomio
type Term = (Double, String)

newtype Polynomial = Polynomial [Term] deriving (Show)

-- Función para crear un término
monom :: Double -> String -> Term
monom coef variable = (coef, variable)

-- Función para crear un polinomio
poly :: [(Double, String)] -> Polynomial
poly = Polynomial

sumMonom :: Term -> Term -> Term 
sumMonom (coef1, var1) (coef2, var2) = (coef1 + coef2, var1)

--Función para sumar polinomios 
sumPoly :: Polynomial -> Polynomial -> Polynomial
sumPoly (Polynomial []) (Polynomial terms2) = Polynomial terms2
sumPoly (Polynomial terms1) (Polynomial []) = Polynomial terms1 
sumPoly (Polynomial terms1) (Polynomial terms2) = 
  let sumTerms = [sumMonom term1 term2 | term1 <- terms1, term2 <- terms2, (snd term1) == (snd term2)]
  in poly sumTerms

--Función para fusionar polinomios
addPoly :: Polynomial -> Polynomial -> Polynomial
addPoly (Polynomial xs) (Polynomial ys) = Polynomial (xs ++ ys)

sepPoly :: Polynomial -> [Polynomial]
sepPoly (Polynomial []) = []
sepPoly (Polynomial (x:xs)) = [Polynomial [x]] ++ (sepPoly (Polynomial xs))

notZero :: Polynomial -> Polynomial
notZero (Polynomial xs) = Polynomial (filter (\x -> fst x /= 0.0) xs)

diff xs ys = [x | x<- xs, not (elem x ys)]

ordening :: Ord a => [a] -> [a]
ordening [] = []
ordening xs = (minimum xs):(ordening $ diff xs [minimum xs]) 

--Extraer variables
extVar (Polynomial terms) = map (\x -> snd x) terms

--Sumar términos de igual variable
sumEqVarPoly :: Polynomial -> [String] -> Polynomial
sumEqVarPoly (Polynomial []) _ = Polynomial []
sumEqVarPoly (Polynomial terms) [] = Polynomial terms
sumEqVarPoly (Polynomial terms) (v:vars) = 
  let terms_v = filter (\x -> (ordening $ snd x) == v) terms
  in addPoly (Polynomial [(sum $ fst $ unzip $ terms_v, v)]) (sumEqVarPoly (Polynomial (diff terms terms_v)) vars)

sumAllPoly :: Polynomial -> Polynomial
sumAllPoly p = notZero $ sumEqVarPoly p (set $ map (ordening) $ extVar p)

sumListPoly :: [Polynomial] -> Polynomial
sumListPoly [p] = p
sumListPoly (p:ps) = sumAllPoly $ addPoly p (sumListPoly ps)

-- Función para multiplicar dos términos
multipMonom :: Term -> Term -> Term
multipMonom (coef1, var1) (coef2, var2) = (coef1 * coef2, var1 ++ var2)

-- Función para multiplicar dos polinomios
multiplyPoly :: Polynomial -> Polynomial -> Polynomial
multiplyPoly (Polynomial []) _ = Polynomial []
multiplyPoly _ (Polynomial []) = Polynomial []
multiplyPoly (Polynomial terms1) (Polynomial terms2) =
    let multipliedTerms = [multipMonom term1 term2 | term1 <- terms1, term2 <- terms2]
    in poly multipliedTerms

--Multiplicación de varios polinomios 
multipSevPoly :: [Polynomial] -> Polynomial
multipSevPoly [p] = p
multipSevPoly (p:ps) = notZero $ multiplyPoly p (multipSevPoly ps)

--Evaluar un polinomio 
evalVars :: Polynomial -> String -> [Double] -> Double 
evalVars (Polynomial [(coef, "")]) _ _ = coef
evalVars (Polynomial [(coef, _)]) "" [] = coef
evalVars (Polynomial [(coef,var)]) (s:ss) (v:vs) = 
  let var_v = length $ filter (\x -> x == s) var
  in ((v^var_v) * (evalVars (Polynomial [(coef,var)]) ss vs))
evalVars (Polynomial []) _ _ = 1
evalVars (Polynomial (l:ls)) s v = evalVars (Polynomial [l]) s v + evalVars (Polynomial ls) s v

eval :: [(Polynomial,[Double])] -> Double
eval [] = 0
eval ((p,d):ls) = (evalVars p (ordening $ set $ foldr (++) "" (extVar p)) d) + (eval ls)
 

-- Ejemplo de uso:
main :: IO ()
main = do
    let term1 = monom 2.0 "x"
        term2 = monom (-1.5) "y"
        polynomial1 = poly [term1, term2]

        term3 = monom 1.0 "x"
        term4 = monom 2.0 "y"
        polynomial2 = poly [term3, term4]

        product = multiplyPoly polynomial1 polynomial2
        sum = sumPoly polynomial1 polynomial2

    print product
    print sum

