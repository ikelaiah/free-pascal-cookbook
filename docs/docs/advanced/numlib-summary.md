# Numerical with NumLib | Summary

The notes on this page is based on the [Free Pascal NumLib official doc](https://wiki.freepascal.org/NumLib_Documentation).

!!! Tip "Credits"

    Thanks to Kees van Ginneken, Wil Kortsmit and Loek van Reij of the Computational centre of the Eindhoven University of Technology for making it available for the Free Pascal project.

    Thanks to Marco van de Voort ([marco@freepascal.org](mailto:marco@freepascal.org)) and Michael van Canneyt ([michael@freepascal.org](mailto:michael@freepascal.org)) for porting to FPC and documenting NumLib.


**Routines in NumLib**

- Basic operations with matrices and vectors - unit `omv`
- Calculation of determinants - unit `det`
- Matrix inversion - unit `inv`
- Solution of systems of linear equations - unit `sle`
- Calculation of eigenvalues - unit `eig`
- Finding roots of equations - unit `roo`
- Calculates integrals - unit `int`
- Oridnary differential equations - unit `ode`
- Fitting routines - unit `ipf`
- Calculation of special functions - unit `spe`

## Unit `omv`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Dot product <br><br> `function omvinp(var a, b: ArbFloat; n: ArbInt): ArbFloat;`     | $\begin{align}\mathbf{a} \cdot \mathbf{b} &= \sum_{i=1}^n a_i b_i \\ &= a_1 b_1 + a_2 b_2 + \dots + a_n b_n\end{align}$ |
| Product of two matrices <br><br> `procedure omvmmm(var a: ArbFloat; m, n, rwa: ArbInt; var b: ArbFloat; p, rwb: ArbInt; var c: ArbFloat; rwc: ArbInt);`   | $C_{ij} = \sum_{k=0}^n A_{ik} B_{kj}$       |
| Product of a matrix and a vector <br><br> `procedure omvmmv(var a: ArbFloat; m, n, rwidth: ArbInt; var b, c: ArbFloat);`  | $\begin{align}\mathbf{c} &= A\ \mathbf{b} \\ &= \left[ \begin{array}{cccc} a_{11} & a_{12} & a_{13} & \ldots & a_{1n} \\ a_{21} & a_{22} & a_{23} & \ldots & a_{2n} \\ \vdots & \vdots & \vdots & \ddots & \vdots \\ a_{m1} & a_{m2} & a_{m3} & \ldots & a_{mn} \end{array} \right] \left[ \begin{array}{c} b_1 \\ b_2 \\ b_3 \\ \vdots \\ b_n \end{array} \right] \\ &= \left[ \begin{array}{c} a_{11}b_1 + a_{12}b_2 + a_{13}b_3 + \cdots + a_{1n}b_n \\ a_{21}b_1 + a_{22}b_2 + a_{23}b_3 + \cdots + a_{2n}b_n \\ \vdots \\ a_{m1}b_1 + a_{m2}b_2 + a_{m3}b_3 + \cdots + a_{mn}b_n \end{array} \right]\end{align}$ |
| Transpose matrix <br><br> `procedure omvtrm(var a: ArbFloat; m, n, rwa: ArbInt; var c: ArbFloat; rwc: ArbInt);` | $\begin{align}A &= \left[ \begin{array}{cccc} a_{11} & a_{12} & a_{13} & \ldots & a_{1n} \\ a_{21} & a_{22} & a_{23} & \ldots & a_{2n} \\ \vdots & \vdots & \vdots & \ddots & \vdots \\ a_{m1} & a_{m2} & a_{m3} & \ldots & a_{mn} \end{array} \right] \quad \\ A^T &= \left[ \begin{array}{cccc} a_{11} & a_{21} & \ldots & a_{m1} \\ a_{12} & a_{22} & \ldots & a_{m2} \\ a_{13} & a_{23} & \ldots & a_{m3} \\ \vdots & \vdots & \ddots & \vdots \\ a_{1n} & a_{2n} & \ldots & a_{mn} \end{array} \right]\end{align}$ |
| 1-norm of a vector <br><br> `function omvn1v(var a: ArbFloat; n: ArbInt): ArbFloat; `   | $\|a\|_1 = \sum_{i=1}^n &#124;{a_i}&#124;$ |
| 2-norm of a vector <br><br> `function omvn2v(var a: ArbFloat; n: ArbInt): ArbFloat; `   | $\|a\|_2 = \sqrt{\sum_{i=1}^n {a_i}^2}$ |
| Maximum infinite norm of a vector <br><br> `function omvnmv(var a: ArbFloat; n: ArbInt): ArbFloat; ` | | $\|a\|_\infty = \max({a_1}, {a_2}, ... {a_n})$ |
| 1-norm of a matrix <br><br> `function omvn1m(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;  `   | $\|M\|_1 = \max_{1 \le j \le {n}} \sum_{i=1}^m&#124;M_{ij}&#124;$ |
| Maximum infinite norm of a matrix <br><br> `function omvnmm(var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat;`   | $\|M\|_\infty = \max_{1 \le i \le\ m} \sum_{j=1}^n &#124;M_{ij}&#124;$ |
| Frobenius norm of a matrix <br><br> `function omvnfm(Var a: ArbFloat; m, n, rwidth: ArbInt): ArbFloat; `   | $\|M\|_F = \sqrt{\sum_{i=1}^m \sum_{j=1}^n {M_{ij}}^2}$ |

## Unit `det`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Determinant of a standard matrix.  <br><br> `procedure detgen(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);` <br><br> `procedure detgsy(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);` <br><br> `procedure detgpd(n, rwidth: ArbInt; var a, f: ArbFloat; var k, term: ArbInt);`  |  For example: <br><br> $\begin{align}\det(A) &= \begin{vmatrix} a & b & c \\ d & e & f \\ g & h & i \end{vmatrix} \\ &= a \begin{vmatrix} e & f \\ h & i \end{vmatrix} - b \begin{vmatrix} d & f \\ g & i \end{vmatrix} + c \begin{vmatrix} d & e \\ g & h \end{vmatrix} \\ &= a(ei - fh) - b(di - fg) + c(de - dh)\end{align}$ <br><br> `detgen` - generic matrix <br><br> `detgsy` - symmetric matrix <br><br> `detgpd` - symmetric positive definite matrix |
| Determinant of a band matrix.  <br><br> `procedure detgba(n, l, r: ArbInt; var a, f: ArbFloat; var k, term:ArbInt);`      | |
| Determinant of a symmetric positive definite band matrix.  <br><br> `procedure detgpb(n, w: ArbInt; var a, f: ArbFloat; var k, term:ArbInt);`  | |
| Determinant of a tridiagonal matrix.  <br><br> `procedure detgtr(n: ArbInt; var l, d, u, f: ArbFloat; var k, term:ArbInt);`  | | 

## Unit `inv`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Inverse of a matrix. <br><br> `procedure invgen(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);` <br><br> `procedure invgsy(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);` <br><br> `procedure invgpd(n, rwidth: ArbInt; var ai: ArbFloat; var term: ArbInt);`  | Suppose a square matrix $A$. The matrix $A^{-1}$ is the inverse of $A$ if the product $A^{-1} A$ is the identity matrix $I$. <br><br> $\displaystyle{  A^{-1} A = I =  \begin{bmatrix} 1  &    & 0 \\ & \ddots  &   \\  0  &     & 1 \end{bmatrix} }$ |

## Unit `sle`

This unit has routines for solving linear equations with various conditions.

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Square matrices <br><br> `ArbFloat; var term: ArbInt);` <br><br> `procedure slegsy(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);` <br><br> `procedure slegpd(n, rwidth: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);`  | |
| Band matrix <br><br> `procedure slegba(n, l, r: ArbInt; var a, b, x, ca: ArbFloat; var term:ArbInt);`  | An optimised routine for solving band matrices.|
| Symmetric positive definite band matrix. <br><br> `procedure slegpb(n, w: ArbInt; var a, b, x, ca: ArbFloat; var term: ArbInt);`  | Optimised approach for a [symmetric positive band matrix](https://wiki.freepascal.org/NumLib_Documentation#Symmetric_positive_definite_band_matrix). |
| Tridiagonal matrix. <br><br> `procedure sledtr(n: ArbInt; var l, d, u, b, x: ArbFloat; var term: ArbInt);` <br><br> `procedure slegtr(n: ArbInt; var l, d, u, b, x, ca: ArbFloat; var term: ArbInt);`  | These are optimised procedures for solving system of linear equations based on tridiagonal matrices. |
| Least squares. <br><br> `procedure slegls(var a: ArbFloat; m, n, rwidtha: ArbInt; var b, x: ArbFloat; var term: ArbInt);`  | Solves linear systems of a rectangular matrix (has more equations than unknowns). |

## Unit `eig`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Calculates eigenvectors and eigenvalues of **generic** matrix.  <br><br> `procedure eigge1(var a: ArbFloat; n, rwidth: ArbInt; var lam: complex; var term: ArbInt);` <br><br> `procedure eigge3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: complex; rwidthx: ArbInt; var term: ArbInt);`  | `eigge1` calculates all eigenvalues. <br><br> `eigg3` calculates all eigenvalues and eigenvectors. |
| Calculates eigenvectors and eigenvalues of **generic symmetric** matrix. <br><br> `procedure eiggs1(var a: ArbFloat; n, rwidth: ArbInt; var lam: ArbFloat; var term: ArbInt); ` <br><br> `procedure eiggs2(var a: ArbFloat; n, rwidth, k1, k2: ArbInt; var lam: ArbFloat; var term: ArbInt);` <br><br>  `procedure eiggs3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: ArbFloat; var term: ArbInt);` <br><br>  `procedure eiggs4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var lam, x: ArbFloat; var term: ArbInt);`   | `eiggs1` finds all eigenvalues <br><br> `eiggs2` finds some eigenvalues (index k1..k2) <br><br> `eiggs3` finds all eigenvalues and eigenvectors <br><br> `eiggs4` finds some eigenvalues and eigenvectors (index k1..k2). |
| Calculates eigenvectors and eigenvalues of **generic symmetric positive definite** matrix. <br><br> `procedure eiggg1(var a: ArbFloat; n, rwidth: ArbInt; var lam: ArbFloat; var term: ArbInt);  ` <br><br>  `procedure eiggg2(var a: ArbFloat; n, rwidth, k1, k2: ArbInt; var lam: ArbFloat; var term: ArbInt); ` <br><br>  `procedure eiggg3(var a: ArbFloat; n, rwidtha: ArbInt; var lam, x: ArbFloat; var term: ArbInt);  `  <br><br>  `procedure eiggg4(var a: ArbFloat; n, rwidtha, k1, k2: ArbInt; var lam, x: ArbFloat; var term: ArbInt); `   | `eiggg1` finds all eigenvalues <br><br> `eiggg2` finds some eigenvalues (index k1..k2) <br><br> `eiggg3` finds all eigenvalues and eigenvectors <br><br> `eiggg4` finds some eigenvalues and eigenvectors (index k1..k2). |
| Calculates eigenvectors and eigenvalues of **symmetric band** matrices.  <br><br> `procedure eigbs1(var a: ArbFloat; n, w: ArbInt; var lam: ArbFloat; var term: ArbInt);` <br><br>  `procedure eigbs2(var a: ArbFloat; n, w, k1, k2: ArbInt; var lam: ArbFloat; var term: ArbInt);`  <br><br>  `procedure eigbs3(var a: ArbFloat; n, w: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt; var term: ArbInt);`  <br><br>  `procedure eigbs4(var a: ArbFloat; n, w, k1, k2: ArbInt; var lam, x: ArbFloat; rwidthx: ArbInt; var m2, term: ArbInt);`  | `eigbs1` finds all eigenvalues <br><br> `eigbs2` finds some eigenvalues (index k1..k2) <br><br> `eigbs3` finds all eigenvalues and eigenvectors <br><br> `eigbs4` finds some eigenvalues and eigenvectors (index k1..k2). |
| Calculates eigenvectors and eigenvalues of **symmetric tridiagonal** matrices  <br><br> `procedure eigts1(var d, cd: ArbFloat; n: ArbInt; var lam: ArbFloat; var term: ArbInt);` <br><br>  `procedure eigts2(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam: ArbFloat; var term: ArbInt);`  <br><br>  `procedure eigts3(var d, cd: ArbFloat; n: ArbInt; var lam, x: ArbFloat; rwidth: ArbInt; var term: ArbInt);` <br><br> `procedure eigts4(var d, cd: ArbFloat; n, k1, k2: ArbInt; var lam, x: ArbFloat; rwidth: ArbInt; var m2, term: ArbInt);`  | `eigts1` finds all eigenvalues <br><br> `eigts2` finds some eigenvalues (index k1..k2) <br><br> `eigts3` finds all eigenvalues and eigenvectors <br><br> `eigts4` finds some eigenvalues and eigenvectors (index k1..k2). |


## Unit `roo`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Polynomial of degree $n$. <br><br> `procedure roopol(var a: ArbFloat; n: ArbInt; var z: complex; var k, term: ArbInt);`  | A polynomial of degree n <br><br> $\displaystyle{  z^n + a_1 z^{n-1} + a_2 z^{n-2} + ... + a_{n-1} z + a_n = 0 }$ <br><br> always has $n$, not necessarily distinct, complex solutions. |
| Special polynomal of degree 2. <br><br> `procedure rooqua(p, q: ArbFloat; var z1, z2: complex);` | The quadratic equation <br><br> $\displaystyle{ {z}^2 + {p} {z} + {q} = 0}$ <br><br>  is a special polynomal of degree 2. It always has two, not necessarily different, complex roots.  |
| Solves polynomial that has exactly two terms. <br><br> `procedure roobin(n: ArbInt; a: complex; var z: complex; var term: ArbInt);` | $\displaystyle{ z^n = a }$ has $n$ complex solutions. |
| Bisection method for finding the root of a function. <br><br> `procedure roof1r(f: rfunc1r; a, b, ae, re: ArbFloat; var x: ArbFloat; var term: ArbInt);`  | |
| Finds root of a non-linear equations. <br><br> `procedure roofnr(f: roofnrfunc; n: ArbInt; var x, residu: ArbFloat; ra: ArbFloat; var term: ArbInt);`  | Finds the roots of a system of (nonlinear) equations: $\displaystyle{  f_{i}(x_1,x_2,\ldots,x_n)=0, \; i=1,2,\ldots,n }$ |


## Unit `int`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Calculates the integral of a function between $a$ and $b$ with an absolute accuracy $ae$. <br><br> `procedure int1fr(f: rfunc1r; a, b, ae: ArbFloat; var integral, err: ArbFloat; var term: ArbInt);` | The function `f` must take one real number (`ArbFloat`) as an input and return a real number (`ArbFloat`). <br><br> See the type `rfunc1r` declared in unit `typ`. |

## Unit `ode`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Solves a single first-order differential equation. <br><br> `procedure odeiv2(f: oderk1n; a: ArbFloat; var ya, b, yb: ArbFloat; n: ArbInt; ae: ArbFloat; var term: ArbInt);` | The adaptive algorithm uses a fifth-order explicit Runge-Kutta method, but it's unsuitable for stiff differential equations. <br><br> Accuracy is not guaranteed in all cases, and for unstable problems, small changes in $y(a)$ can lead to large errors in $y(b)$. To assess this, try different values of $ae$. <br><br> For solving initial value problems over multiple points (e.g., from $x=0$ to $x=1$ with step size 0.1), avoid restarting at $x=0$ with each step; instead, integrate continuously. |
| Solves a system of first-order differential equations. <br><br> `procedure odeiv2(f: oderk1n; a: ArbFloat; var ya, b, yb: ArbFloat; n: ArbInt; ae: ArbFloat; var term: ArbInt);` | The algorithm is based on an explicit one-step Runge-Kutta method of order five with variable step size. |

## Unit `ipf`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| Fits a set of data points with a polynomial. <br><br> `procedure ipfpol(m, n: ArbInt; var x, y, b: ArbFloat; var term: ArbInt);`  | |
| Use `ipfisn` to calculate the parameters of a spline. Then use the `ipfspn` procedure to find the value of the spline at any point. <br><br> `procedure ipfisn(n: ArbInt; var x, y, d2s: ArbFloat; var term: ArbInt);` <br><br> `function  ipfspn(n: ArbInt; var x, y, d2s: ArbFloat; t: ArbFloat; var term: ArbInt): ArbFloat;` | |


## Unit `spe`

| Operation and Routine       | Notes                                                   |
| --------------------------- | ------------------------------------------------------- |
| An efficient method for evaluating a polynomial at a specific x value using Horner's scheme. <br><br> `function spepol(x: ArbFloat; var a: ArbFloat; n: ArbInt): ArbFloat;`  | $\displaystyle{  \begin{align}\operatorname{p}(x) &= a_0 + a_1 x + a_2 x^2 + ... + a_n x^n \\ \operatorname{p}(x) &= a_0 + x (a_1 + x (a_2 + \dots + x (a_{n-1} + x a_n))) \end{align}}$ |
| Calculates error function $erf(x)$ and its complementary error function $erfc(x)$. <br><br>`function speerf(x: ArbFloat): ArbFloat; ` <br><br> `function speefc(x: ArbFloat): ArbFloat;`  | $erf(x)$ and $erfc(x)$ represent the lower and upper parts of the area under the Gaussian curve, adding up to 1 (100%). |
| Normal and inverse normal distribution. <br><br>`function normaldist(x: ArbFloat): ArbFloat;` <br><br> `function invnormaldist(y: ArbFloat): ArbFloat;`  | $\displaystyle{  \operatorname{N}(x) = \frac{1}{\sqrt{2\pi}} \int_{-\infty}^{x} \operatorname{exp}(\frac{t^2}{2}) dt = \frac{1}{2}[1 + \operatorname{erf} (\frac{x}{\sqrt{2}})]  }$ |
| Factorials to non-integer (and even complex) numbers. <br><br>`function spegam(x: ArbFloat): ArbFloat;` <br><br> `function spelga(x: ArbFloat): ArbFloat;`  | $\displaystyle{ \Gamma({x}) = \int_0^{\infty}t^{x-1} e^{-t} dt }$ <br><br>  `spemgam` - direct calculation <br><br> `spelga` - computes the natural logarithm of the Gamma function. |
| Incomplete gamma function. <br><br>`function gammap(s, x: ArbFloat): ArbFloat;` <br><br> `function gammaq(s, x: ArbFloat): ArbFloat;`  | $\displaystyle{ \begin{align} \operatorname{P}({s},{x}) &= \frac{1}{\Gamma({s})} \int_0^{x}t^{s-1} e^{-t} dt \\ \operatorname{Q}({s},{x}) &= \frac{1}{\Gamma({s})} \int_{x}^{\infty}t^{s-1} e^{-t} dt = 1 - \operatorname{P}({s}, {x}) \end{align}}$ |
| Beta function. <br><br>`function beta(a, b: ArbFloat): ArbFloat;`  | $\displaystyle{ \operatorname{B}(a, b) = \frac{{\Gamma(a)}{\Gamma(b)}}{\Gamma(a+b)} = \int_0^1{t^{x-1} (1-t)^{y-1} dt} }$ |
| Incomplete beta function (and the inverse). <br><br>`function betai(a, b, x: ArbFloat): ArbFloat;` <br><br> `function invbetai(a, b, y: ArbFloat): ArbFloat;`  | $\displaystyle{ \operatorname{I}_x(a,b) = \frac {1}{\operatorname{B}(ab)} \int_0^x{t^{x-1} (1-t)^{y-1} dt} }$ |
| Bessel functions of the first kind ($J_\alpha$), of the second kind ($Y_\alpha$), and modified first ($I_\alpha$) and second kind ($K_\alpha$). <br><br> <br><br> NumLib implements only the solutions for the parameters $α = 0$ and $α = 1$. <br><br>`function spebj0(x: ArbFloat): ArbFloat; ` <br><br> `function spebj1(x: ArbFloat): ArbFloat; ` <br><br> `function speby0(x: ArbFloat): ArbFloat;` <br><br> `function speby1(x: ArbFloat): ArbFloat; ` <br><br> `function spebi0(x: ArbFloat): ArbFloat;` <br><br> `function spebi1(x: ArbFloat): ArbFloat; ` <br><br> `function spebk0(x: ArbFloat): ArbFloat;` <br><br> `function spebk1(x: ArbFloat): ArbFloat; `  | The Bessel functions are solutions of the Bessel differential equation: <br><br> $\displaystyle{ {x}^2 y'' + x y' + ({x}^2 - \alpha^2) {y} = 0 }$  <br><br> `spebj0` - Bessel function $J_0$ (α = 0). <br><br> `spebj1` - Bessel function $J_1$ (α = 1). <br><br> `speby0` - Bessel function $Y_0$ (α = 0). <br><br> `speby1` - Bessel function $Y_1$ (α = 1). <br><br> `spebi0` - modified Bessel function $I_0$ (α = 0). <br><br> `spebi1` - modified Bessel function $I_1$ (α = 1). <br><br> `spebk0` - modified Bessel function $K_0$ (α= 0). <br><br> `spebk1` - modified Bessel function $K_1$ (α = 1).|

## Unit `spe` - Others

|                  Function                    |   Equivalent function in math     |                              Description                             |
| -------------------------------------------- | --------------------------------- | -------------------------------------------------------------------- |
| `function speent(x: ArbFloat): LongInt;`     | `floor(x)`                        | Entier function, calculates first integer smaller than or equal to x |
| `function spemax(a, b: Arbfloat): ArbFloat;` | `max(a, b)`                       | Maximum of two floating point values                                 |
| `function spepow(a, b: ArbFloat): ArbFloat;` | `power(a, b)`                     | Calculates $a^b$                                                     |
| `function spesgn(x: ArbFloat): ArbInt;`      | `sign(x)`                         | Returns the sign of x (-1 for x < 0, 0 for x = 0, +1 for x > 0)      |
| `function spears(x: ArbFloat): ArbFloat;`    | `arcsin(x)`                       | Inverse function of sin(x)                                           |
| `function spearc(x: ArbFloat): ArbFloat;`    | `arccos(x)`                       | Inverse function of cos(x)                                           |
| `function spesih(x: ArbFloat): ArbFloat;`    | `sinh(x)`                         | Hyperbolic sine                                                      |
| `function specoh(x: ArbFloat): ArbFloat;`    | `cosh(x)`                         | Hyperbolic cosine                                                    |
| `function spetah(x: ArbFloat): ArbFloat;`    | `tanh(x)`                         | Hyperbolic tangent                                                   |
| `function speash(x: ArbFloat): ArbFloat;`    | `arcsinh(x)`                      | Inverse of the hyperbolic sine                                       |
| `function speach(x: ArbFloat): ArbFloat;`    | `arccosh(x)`                      | Inverse of the hyperbolic cosine                                     |
| `function speath(x: ArbFloat): ArbFloat;`    | `arctanh(x)`                      | Inverse of the hyperbolic tangent                                    |