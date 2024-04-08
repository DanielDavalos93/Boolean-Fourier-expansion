# Boolean-Fourier-expansion

`ind a x` is the representation of the **indicator polynomial**: $$1_{\{a\}}(x) = \prod_{i=1}^n \left(\cfrac{1+a_ix_i}{2}\right)$$
where `a` has type of `List [Int]`, and we only use `-1,1`, for example `a = [1,-1,1]`.

The **boolean Fourier expansion** `polrep f x`, is the polynomial:
$$f(x) = \displaystyle\sum_{a\in \{-1,1\}^n}f(a) 1_{\{a\}}(x)$$

A polynomial has the form `Polynomial (List (coef,var))`, where `coef :: Double` and `var :: String`. For example, the polynomial $x^2 + xy +2$ should be represented by `Polynomial [(1.0,"xx"),(1.0,"xy"),(2.0,"")]`.
