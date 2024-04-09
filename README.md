# Boolean-Fourier-expansion

## Haskell

Is a functional programming language so fast, his compilator is `ghc` (https://www.haskell.org/ghc/).

## BooleanFourier.hs file
`ind a x` is the representation of the **indicator polynomial**: $$1_{\{a\}}(x) = \prod_{i=1}^n \left(\cfrac{1+a_ix_i}{2}\right)$$
where `a` has type `[Int]` (List Int) and `x` type `[String]` (List String), and we only use `-1,1`, for example `a = [1,-1,1]` and `x = ["x","y","z"]`. 
! Obs.:
To consider the variable `x` not use a string with length greater than one (`x1`, `x2`, `var`, `y0`, etc.), because the next step is separate every element of a string as single variable (if `x = "ab1"` then the variables are `a`, `b` and `1`).

The **boolean Fourier expansion** `polrep f x`, is the polynomial:
$$f(x) = \displaystyle\sum_{a\in [-1,1]^n}f(a) 1_{\{a\}}(x)$$

## Polynomial.hs file
A polynomial has the form `Polynomial (List (coef,var))`, where `coef :: Double` and `var :: String`. For example, the polynomial $x^2 + xy +2$ should be represented by `Polynomial [(1.0,"xx"),(1.0,"xy"),(2.0,"")]`.

To evaluate a polynomial, use the `eval` function. You'll need a list of polynomial and list of values pairs: `eval :: [(Polynomial, [Double])] -> Double`. Generally we use a only pair (Polynomial,[Double]) in the list, but is useful to have this form to evaluate several polynomials $p_{x_1=a_1}(x_1)+\cdots + p_{x_n=a_n}(x_n)$. Such as the polynomial $p(x)+q(x,y) = (x+2) + (3xy)$ can be evaluate by $p_{x=1}(x) + q_{(x,y) = (-1,2)}(x,y) = -3$ using `eval [(Polynomial [(1.0,"x"), (2.0,"")],[1]),(Polynomial [(3.0,"xy")],[-1,2])]`.
